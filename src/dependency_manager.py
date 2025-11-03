#!/usr/bin/env python3
"""
Dependency Manager - Temporary installation and cleanup of external libraries

This module provides automatic dependency management for code execution:
- Detects required imports from code snippets
- Installs missing packages temporarily before execution
- Removes packages after execution to keep environment clean
- Supports multiple languages: Python, Go, Rust, R, Julia, etc.

Key Features:
- Per-execution isolation: dependencies are tracked and cleaned up
- Language-specific package managers: pip, go get, cargo, etc.
- Conda environment integration: installs within CLAP environment
- Rollback on failure: ensures clean state even if execution fails
"""

import os
import re
import subprocess
import json
import tempfile
from pathlib import Path
from typing import Dict, List, Set, Tuple, Optional
from dataclasses import dataclass, field
from datetime import datetime


@dataclass
class DependencyInfo:
    """Information about a package dependency"""
    name: str
    language: str
    version: Optional[str] = None
    installed: bool = False
    install_command: Optional[str] = None
    error: Optional[str] = None


@dataclass
class InstallationSession:
    """Tracks packages installed during a session for cleanup"""
    session_id: str
    language: str
    packages: List[DependencyInfo] = field(default_factory=list)
    start_time: datetime = field(default_factory=datetime.now)
    end_time: Optional[datetime] = None
    success: bool = False


class DependencyManager:
    """Manages temporary installation and removal of external dependencies
    
    Workflow:
    1. Parse code to extract import statements
    2. Check which packages are missing
    3. Install missing packages (with version pinning if needed)
    4. Track installation in session
    5. After execution, remove all installed packages
    
    Example:
        ```python
        manager = DependencyManager()
        session = manager.install_dependencies(code, 'python')
        try:
            # Execute code
            result = execute_code(code)
        finally:
            manager.cleanup_session(session)
        ```
    """
    
    def __init__(self, conda_env='CLAP', verbose=False):
        """Initialize dependency manager
        
        Args:
            conda_env: Name of conda environment to use (default: CLAP)
            verbose: If True, print detailed installation logs
        """
        self.conda_env = conda_env
        self.verbose = verbose
        self.sessions: Dict[str, InstallationSession] = {}
        
        # Languages that should NOT auto-install dependencies
        # Reason: complex build environments, global pollution, or instability
        self.no_auto_install = {
            'haskell',  # cabal --package-env issues in temp dirs, crashes
            'ocaml',    # opam requires dedicated switch, can conflict
            'csharp',   # requires .csproj file, complex setup
            'c',        # system package manager, requires sudo
            'cpp',      # system package manager, requires sudo
            'java',     # requires pom.xml/build.gradle
        }
        
        # Track pre-existing packages to avoid removing them
        self.preinstalled_packages = {
            'python': self._get_installed_python_packages(),
            'go': set(),  # Go modules are project-specific
            'rust': set(),  # Rust crates are project-specific
            'r': set(),  # R packages harder to detect, be conservative
            'julia': set()
        }
        
        # Common package name mappings (import name -> package name)
        self.package_mappings = {
            'python': {
                'cv2': 'opencv-python',
                'PIL': 'pillow',
                'sklearn': 'scikit-learn',
                'yaml': 'pyyaml',
                'bs4': 'beautifulsoup4',
            },
            'haskell': {
                # Common Haskell module -> package mappings
                'Data.Aeson': 'aeson',
                'Data.Text': 'text',
                'Data.ByteString': 'bytestring',
                'Data.Vector': 'vector',
                'Data.Map': 'containers',
                'Data.Set': 'containers',
                'Network.HTTP': 'HTTP',
                'Network.HTTP.Client': 'http-client',
                'Network.HTTP.Conduit': 'http-conduit',
                'Control.Monad.Trans': 'transformers',
                'Control.Lens': 'lens',
                'Text.Parsec': 'parsec',
                'Text.Megaparsec': 'megaparsec',
                'System.Random': 'random',
                'Test.QuickCheck': 'QuickCheck',
                'Test.Hspec': 'hspec',
            },
            'ocaml': {
                # Common OCaml module -> package mappings
                'Lwt': 'lwt',
                'Async': 'async',
                'Core': 'core',
                'Yojson': 'yojson',
                'Ppx_deriving': 'ppx_deriving',
                'Re': 're',
                'Cmdliner': 'cmdliner',
                'Cohttp': 'cohttp',
                'Batteries': 'batteries',
            },
            'csharp': {
                # Common C# namespace -> package mappings
                'Newtonsoft.Json': 'Newtonsoft.Json',
                'System.Text.Json': 'System.Text.Json',
                'Dapper': 'Dapper',
                'NUnit': 'NUnit',
                'NUnit.Framework': 'NUnit',
                'xUnit': 'xunit',
                'xunit': 'xunit',
                'Moq': 'Moq',
                'Serilog': 'Serilog',
                'AutoMapper': 'AutoMapper',
                'FluentValidation': 'FluentValidation',
                'EntityFramework': 'EntityFramework',
                'Microsoft.EntityFrameworkCore': 'Microsoft.EntityFrameworkCore',
            }
        }
    
    def _get_installed_python_packages(self) -> Set[str]:
        """Get list of currently installed Python packages"""
        try:
            cmd = ['conda', 'run', '-n', self.conda_env, 'pip', 'list', '--format=json']
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=10)
            
            if result.returncode == 0:
                packages = json.loads(result.stdout)
                return {pkg['name'].lower() for pkg in packages}
        except Exception as e:
            if self.verbose:
                print(f"⚠️  Could not get installed packages: {e}")
        
        return set()
    
    def extract_imports(self, code: str, language: str) -> List[str]:
        """Extract import statements from code
        
        Args:
            code: Source code to analyze
            language: Programming language
            
        Returns:
            List of package/module names imported
        """
        imports = []
        
        if language == 'python':
            imports = self._extract_python_imports(code)
        elif language in ['javascript', 'typescript']:
            imports = self._extract_js_ts_imports(code)
        elif language == 'ruby':
            imports = self._extract_ruby_imports(code)
        elif language == 'php':
            imports = self._extract_php_imports(code)
        elif language == 'haskell':
            imports = self._extract_haskell_imports(code)
        elif language == 'ocaml':
            imports = self._extract_ocaml_imports(code)
        elif language == 'csharp':
            imports = self._extract_csharp_imports(code)
        elif language == 'go':
            imports = self._extract_go_imports(code)
        elif language == 'rust':
            imports = self._extract_rust_imports(code)
        elif language == 'r':
            imports = self._extract_r_imports(code)
        elif language == 'julia':
            imports = self._extract_julia_imports(code)
        
        return imports
    
    def _extract_python_imports(self, code: str) -> List[str]:
        """Extract Python import statements"""
        imports = []
        
        # Match: import module, from module import ...
        patterns = [
            r'^\s*import\s+(\w+)',
            r'^\s*from\s+(\w+)\s+import',
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    module = match.group(1)
                    # Skip standard library modules (heuristic)
                    if module not in ['os', 'sys', 'math', 're', 'json', 'time', 
                                     'datetime', 'collections', 'itertools', 'functools',
                                     'random', 'string', 'typing', 'pathlib', 'io',
                                     'subprocess', 'tempfile', 'shutil', 'glob']:
                        imports.append(module)
        
        return list(set(imports))  # Remove duplicates
    
    def _extract_js_ts_imports(self, code: str) -> List[str]:
        """Extract JavaScript/TypeScript import statements"""
        imports = []
        
        # Patterns for ES6 imports and require()
        patterns = [
            r'^\s*import\s+.*\s+from\s+["\']([^"\']+)["\']',  # import ... from 'package'
            r'^\s*import\s+["\']([^"\']+)["\']',              # import 'package'
            r'^\s*(?:const|let|var)\s+.*\s*=\s*require\s*\(\s*["\']([^"\']+)["\']\s*\)',  # require('package')
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    module = match.group(1)
                    # Only external packages (not relative paths starting with . or /)
                    if not module.startswith('.') and not module.startswith('/'):
                        # Extract package name (before any subpath)
                        pkg = module.split('/')[0] if not module.startswith('@') else '/'.join(module.split('/')[:2])
                        imports.append(pkg)
        
        return list(set(imports))
    
    def _extract_ruby_imports(self, code: str) -> List[str]:
        """Extract Ruby require/gem statements"""
        imports = []
        
        patterns = [
            r'^\s*require\s+["\']([^"\']+)["\']',
            r'^\s*gem\s+["\']([^"\']+)["\']',
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    gem = match.group(1)
                    # Skip standard library (common ones)
                    if gem not in ['date', 'json', 'yaml', 'fileutils', 'pathname', 
                                  'tmpdir', 'stringio', 'csv', 'uri', 'net/http']:
                        imports.append(gem)
        
        return list(set(imports))
    
    def _extract_php_imports(self, code: str) -> List[str]:
        """Extract PHP use/require statements for Composer packages"""
        imports = []
        
        # Match use statements with namespace (vendor/package pattern)
        patterns = [
            r'^\s*use\s+([A-Z]\w+(?:\\[A-Z]\w+)+)',  # use Vendor\Package\Class
            r'^\s*new\s+([A-Z]\w+(?:\\[A-Z]\w+)+)',  # new Vendor\Package\Class
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    namespace = match.group(1)
                    # Extract vendor/package (first two parts)
                    parts = namespace.split('\\')
                    if len(parts) >= 2:
                        # Convert to composer format: vendor/package
                        vendor_package = f"{parts[0].lower()}/{parts[1].lower()}"
                        imports.append(vendor_package)
        
        return list(set(imports))
    
    def _extract_haskell_imports(self, code: str) -> List[str]:
        """Extract Haskell import statements
        
        Returns full module names which will be mapped to packages
        using package_mappings dictionary
        """
        imports = []
        
        # Match: import Module, import qualified Module as Alias
        patterns = [
            r'^\s*import\s+(?:qualified\s+)?([A-Z][\w\.]*)',
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    module = match.group(1)
                    # Skip Prelude and base library modules (included with GHC)
                    base_modules = ['Prelude', 'Data.List', 'Data.Maybe', 'Data.Either',
                                   'Data.Char', 'Data.Bool', 'Data.Int', 'Data.Word',
                                   'Control.Monad', 'Control.Applicative',
                                   'System.IO', 'System.Environment',
                                   'Text.Read', 'Text.Show']
                    if module not in base_modules:
                        imports.append(module)
        
        return list(set(imports))
    
    def _extract_ocaml_imports(self, code: str) -> List[str]:
        """Extract OCaml open/module statements
        
        Returns module names which will be mapped to packages
        using package_mappings dictionary
        """
        imports = []
        
        # Match: open Module, module M = Module
        patterns = [
            r'^\s*open\s+([A-Z]\w+)',
            r'^\s*#require\s+"([^"]+)"',  # #require "package" (toplevel)
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    module = match.group(1)
                    # Skip standard library modules
                    stdlib = ['List', 'Array', 'String', 'Printf', 'Hashtbl', 
                             'Map', 'Set', 'Queue', 'Stack', 'Sys', 'Unix',
                             'Buffer', 'Char', 'Int', 'Float', 'Bool']
                    if module not in stdlib:
                        imports.append(module)
        
        return list(set(imports))
    
    def _extract_csharp_imports(self, code: str) -> List[str]:
        """Extract C# using statements for NuGet packages"""
        imports = []
        
        # Match using statements with multiple namespace levels
        patterns = [
            r'^\s*using\s+([A-Z]\w+(?:\.[A-Z]\w+)*)\s*;',  # using Namespace.Package;
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    namespace = match.group(1)
                    # Skip System.* (built-in .NET) except specific packages
                    if namespace.startswith('System') and namespace not in ['System.Text.Json']:
                        continue
                    # Skip Microsoft.* core namespaces (but allow EF, etc)
                    if namespace.startswith('Microsoft.') and not namespace.startswith('Microsoft.EntityFrameworkCore'):
                        continue
                    # Return full namespace for mapping lookup
                    imports.append(namespace)
        
        return list(set(imports))
    
    def _extract_go_imports(self, code: str) -> List[str]:
        """Extract Go import statements"""
        imports = []
        in_import_block = False
        
        for line in code.split('\n'):
            line = line.strip()
            
            if line.startswith('import ('):
                in_import_block = True
                continue
            elif line == ')' and in_import_block:
                in_import_block = False
                continue
            elif in_import_block:
                # Extract package path from import line
                match = re.match(r'"([^"]+)"', line)
                if match:
                    pkg = match.group(1)
                    # Only external packages (contain /)
                    if '/' in pkg and not pkg.startswith('C'):
                        imports.append(pkg)
            elif line.startswith('import "'):
                match = re.match(r'import\s+"([^"]+)"', line)
                if match:
                    pkg = match.group(1)
                    if '/' in pkg and not pkg.startswith('C'):
                        imports.append(pkg)
        
        return list(set(imports))
    
    def _extract_rust_imports(self, code: str) -> List[str]:
        """Extract Rust use/extern crate statements"""
        imports = []
        
        patterns = [
            r'^\s*extern\s+crate\s+(\w+)',
            r'^\s*use\s+(\w+)::',
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    crate = match.group(1)
                    # Skip std library
                    if crate not in ['std', 'core', 'alloc']:
                        imports.append(crate)
        
        return list(set(imports))
    
    def _extract_r_imports(self, code: str) -> List[str]:
        """Extract R library/require statements"""
        imports = []
        
        patterns = [
            r'^\s*library\s*\(\s*["\']?(\w+)["\']?\s*\)',
            r'^\s*require\s*\(\s*["\']?(\w+)["\']?\s*\)',
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    pkg = match.group(1)
                    imports.append(pkg)
        
        return list(set(imports))
    
    def _extract_julia_imports(self, code: str) -> List[str]:
        """Extract Julia using/import statements"""
        imports = []
        
        patterns = [
            r'^\s*using\s+(\w+)',
            r'^\s*import\s+(\w+)',
        ]
        
        for line in code.split('\n'):
            for pattern in patterns:
                match = re.match(pattern, line)
                if match:
                    pkg = match.group(1)
                    imports.append(pkg)
        
        return list(set(imports))
    
    def check_missing_packages(self, imports: List[str], language: str) -> List[str]:
        """Check which packages are not installed
        
        Args:
            imports: List of package names
            language: Programming language
            
        Returns:
            List of missing package names
        """
        missing = []
        
        for pkg in imports:
            # Map import name to package name if needed
            if language in self.package_mappings:
                pkg_name = self.package_mappings[language].get(pkg, pkg)
            else:
                pkg_name = pkg
            
            if not self._is_package_installed(pkg_name, language):
                missing.append(pkg_name)
        
        return missing
    
    def _is_package_installed(self, package: str, language: str) -> bool:
        """Check if a package is already installed"""
        
        if language == 'python':
            # Check if in preinstalled list
            return package.lower() in self.preinstalled_packages['python']
        
        elif language == 'go':
            # For Go, we'll assume not installed (go get is idempotent)
            return False
        
        elif language == 'rust':
            # For Rust, check if crate exists in Cargo.toml (not implemented here)
            return False
        
        elif language == 'r':
            # For R, try to check if library can be loaded
            try:
                cmd = ['conda', 'run', '-n', self.conda_env, 'Rscript', '-e', 
                       f'library({package})']
                result = subprocess.run(cmd, capture_output=True, timeout=5)
                return result.returncode == 0
            except:
                return False
        
        elif language == 'julia':
            # For Julia, check if package is in environment
            return False
        
        return False
    
    def install_dependencies(self, code: str, language: str) -> InstallationSession:
        """Install all missing dependencies for code
        
        Args:
            code: Source code to analyze
            language: Programming language
            
        Returns:
            InstallationSession with tracking info
        """
        import uuid
        session_id = f"{language}_{uuid.uuid4().hex[:8]}"
        session = InstallationSession(session_id=session_id, language=language)
        
        # Check if language is blacklisted from auto-install
        if language in self.no_auto_install:
            if self.verbose:
                print(f"⚠️  Auto-install disabled for {language} (using pre-installed packages only)")
            session.success = True
            return session
        
        if self.verbose:
            print(f"📦 Analyzing dependencies for {language}...")
        
        # Extract imports
        imports = self.extract_imports(code, language)
        
        if not imports:
            if self.verbose:
                print("  ✓ No external dependencies detected")
            session.success = True
            return session
        
        if self.verbose:
            print(f"  Found imports: {', '.join(imports)}")
        
        # Check missing
        missing = self.check_missing_packages(imports, language)
        
        if not missing:
            if self.verbose:
                print("  ✓ All dependencies already installed")
            session.success = True
            return session
        
        if self.verbose:
            print(f"  Missing packages: {', '.join(missing)}")
        
        # Install each missing package
        for pkg in missing:
            dep_info = self._install_package(pkg, language)
            session.packages.append(dep_info)
        
        # Check if all installations succeeded
        session.success = all(dep.installed for dep in session.packages)
        self.sessions[session_id] = session
        
        return session
    
    def _get_install_command(self, package: str, language: str) -> Tuple[str, List[str]]:
        """Get installation command for a package
        
        Args:
            package: Package name
            language: Programming language
            
        Returns:
            Tuple of (human_readable_command, subprocess_command_list)
            
        Raises:
            ValueError: If language is not supported
        """
        # Configuration dictionary for all supported languages
        install_configs = {
            'python': {
                'display': f"pip install {package}",
                'command': ['conda', 'run', '-n', self.conda_env, 'pip', 'install', 
                           '--quiet', package]
            },
            'javascript': {
                'display': f"npm install {package}",
                'command': ['conda', 'run', '-n', self.conda_env, 'npm', 'install', 
                           '--silent', package]
            },
            'typescript': {
                'display': f"npm install {package}",
                'command': ['conda', 'run', '-n', self.conda_env, 'npm', 'install', 
                           '--silent', package]
            },
            'ruby': {
                'display': f"gem install {package}",
                'command': ['conda', 'run', '-n', self.conda_env, 'gem', 'install', 
                           '--quiet', package]
            },
            'php': {
                'display': f"composer require {package}",
                'command': ['composer', 'require', '--quiet', package],
                'note': 'Requires composer.json in working directory'
            },
            'go': {
                'display': f"go get {package}",
                'command': ['go', 'get', package]
            },
            'rust': {
                'display': f"cargo add {package}",
                'command': ['cargo', 'add', package],
                'note': 'Requires cargo-edit and Cargo.toml'
            },
            'r': {
                'display': f"install.packages('{package}')",
                'command': ['conda', 'run', '-n', self.conda_env, 'Rscript', '-e',
                           f"install.packages('{package}', repos='https://cloud.r-project.org/')"]
            },
            'julia': {
                'display': f'Pkg.add("{package}")',
                'command': ['conda', 'run', '-n', self.conda_env, 'julia', '-e',
                           f'using Pkg; Pkg.add("{package}")']
            },
            'haskell': {
                'display': f"cabal install {package}",
                'command': ['cabal', 'install', '--lib', package, '--package-env=.'],
                'note': 'Installs to local package environment (not global)'
            },
            'ocaml': {
                'display': f"opam install {package}",
                'command': ['opam', 'install', '-y', package],
                'note': 'Requires active opam switch'
            },
            'csharp': {
                'display': f"dotnet add package {package}",
                'command': ['dotnet', 'add', 'package', package],
                'note': 'Requires .csproj file in working directory'
            }
        }
        
        if language not in install_configs:
            raise ValueError(f"Unsupported language: {language}")
        
        config = install_configs[language]
        return config['display'], config['command']
    
    def _install_package(self, package: str, language: str) -> DependencyInfo:
        """Install a single package
        
        Args:
            package: Package name
            language: Programming language
            
        Returns:
            DependencyInfo with installation result
        """
        dep = DependencyInfo(name=package, language=language)
        
        if self.verbose:
            print(f"  📥 Installing {package}...")
        
        try:
            # Get installation command
            display_cmd, cmd = self._get_install_command(package, language)
            dep.install_command = display_cmd
            
            # Execute installation
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=120  # 2 minutes timeout for installation
            )
            
            if result.returncode == 0:
                dep.installed = True
                if self.verbose:
                    print(f"    ✓ {package} installed")
            else:
                dep.error = result.stderr[:200] if result.stderr else "Unknown error"
                if self.verbose:
                    print(f"    ✗ {package} failed: {dep.error}")
        
        except subprocess.TimeoutExpired:
            dep.error = "Installation timeout (>120s)"
            if self.verbose:
                print(f"    ✗ {package} timeout")
        
        except Exception as e:
            dep.error = str(e)[:200]
            if self.verbose:
                print(f"    ✗ {package} error: {e}")
        
        return dep
    
    def _get_uninstall_command(self, package: str, language: str) -> Optional[List[str]]:
        """Get uninstall command for a package
        
        Args:
            package: Package name
            language: Programming language
            
        Returns:
            Command list for subprocess, or None if no cleanup needed
        """
        # Configuration dictionary for cleanup commands
        uninstall_configs = {
            'python': ['conda', 'run', '-n', self.conda_env, 'pip', 'uninstall',
                      '-y', '--quiet', package],
            'javascript': ['conda', 'run', '-n', self.conda_env, 'npm', 'uninstall',
                          '--silent', package],
            'typescript': ['conda', 'run', '-n', self.conda_env, 'npm', 'uninstall',
                          '--silent', package],
            'ruby': ['conda', 'run', '-n', self.conda_env, 'gem', 'uninstall',
                    '-x', package],  # -x for executables
            'php': None,  # Composer requires manual removal or composer.json edit
            'go': None,  # Go modules are workspace-specific
            'rust': None,  # Rust crates are project-specific (Cargo.toml)
            'r': ['conda', 'run', '-n', self.conda_env, 'Rscript', '-e',
                 f"remove.packages('{package}')"],
            'julia': ['conda', 'run', '-n', self.conda_env, 'julia', '-e',
                     f'using Pkg; Pkg.rm("{package}")'],
            'haskell': ['ghc-pkg', 'unregister', '--package-env=.', '--force', package],
            'ocaml': ['opam', 'remove', '-y', package],
            'csharp': ['dotnet', 'remove', 'package', package]
        }
        
        return uninstall_configs.get(language)
    
    def cleanup_session(self, session: InstallationSession) -> bool:
        """Remove all packages installed during session
        
        Args:
            session: InstallationSession to clean up
            
        Returns:
            True if cleanup successful, False otherwise
        """
        if not session.packages:
            return True
        
        # Skip cleanup for blacklisted languages (no auto-install = no auto-cleanup)
        if session.language in self.no_auto_install:
            if self.verbose:
                print(f"⏭️  Skipping cleanup for {session.language} (no auto-install)")
            return True
        
        if self.verbose:
            print(f"🧹 Cleaning up session {session.session_id}...")
        
        success = True
        
        for dep in session.packages:
            if not dep.installed:
                continue  # Skip packages that weren't installed
            
            # Don't remove if it was preinstalled
            if dep.language in self.preinstalled_packages:
                if dep.name.lower() in self.preinstalled_packages[dep.language]:
                    if self.verbose:
                        print(f"  ⏭️  Skipping {dep.name} (preinstalled)")
                    continue
            
            try:
                if self.verbose:
                    print(f"  🗑️  Removing {dep.name}...")
                
                # Get uninstall command
                cmd = self._get_uninstall_command(dep.name, dep.language)
                
                if cmd is None:
                    # No cleanup needed (e.g., Go/Rust workspace-specific)
                    if self.verbose:
                        print(f"    ⏭️  {dep.name} ({dep.language} - no cleanup needed)")
                    continue
                
                result = subprocess.run(cmd, capture_output=True, timeout=60)
                
                if result.returncode == 0:
                    if self.verbose:
                        print(f"    ✓ {dep.name} removed")
                else:
                    success = False
                    if self.verbose:
                        print(f"    ✗ {dep.name} removal failed")
            
            except Exception as e:
                success = False
                if self.verbose:
                    print(f"    ✗ {dep.name} error: {e}")
        
        session.end_time = datetime.now()
        
        if self.verbose:
            duration = (session.end_time - session.start_time).total_seconds()
            print(f"  Session cleanup completed in {duration:.1f}s")
        
        return success
    
    def get_session_report(self, session: InstallationSession) -> str:
        """Get human-readable report of session
        
        Args:
            session: InstallationSession to report on
            
        Returns:
            Formatted report string
        """
        lines = []
        lines.append(f"Session: {session.session_id}")
        lines.append(f"Language: {session.language}")
        lines.append(f"Packages: {len(session.packages)}")
        
        if session.packages:
            lines.append("\nDependencies:")
            for dep in session.packages:
                status = "✓" if dep.installed else "✗"
                lines.append(f"  {status} {dep.name}")
                if dep.error:
                    lines.append(f"     Error: {dep.error}")
        
        if session.end_time:
            duration = (session.end_time - session.start_time).total_seconds()
            lines.append(f"\nDuration: {duration:.1f}s")
        
        return "\n".join(lines)


# Example usage
if __name__ == "__main__":
    # Test with Python code requiring numpy
    python_code = """
import numpy as np
import pandas as pd

def test():
    arr = np.array([1, 2, 3, 4, 5])
    print(arr.mean())

test()
"""
    
    manager = DependencyManager(verbose=True)
    
    print("="*60)
    print("Testing Dependency Manager")
    print("="*60)
    
    # Install dependencies
    session = manager.install_dependencies(python_code, 'python')
    
    print("\n" + manager.get_session_report(session))
    
    # Simulate execution
    print("\n[Simulating code execution...]")
    
    # Cleanup
    print()
    manager.cleanup_session(session)
