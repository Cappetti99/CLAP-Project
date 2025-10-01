"""
Modern Dependency Analyzer for SWAM Project
Single Responsibility: Intelligent dependency detection and management
Integrates with existing AutoDependencyInstaller logic
"""

import re
import os
import subprocess
import sys
from pathlib import Path
from typing import List, Dict, Set, Optional, Tuple
from dataclasses import dataclass


@dataclass
class DependencyInfo:
    """Single dependency information container"""
    name: str
    language: str
    is_external: bool
    install_command: Optional[str] = None
    version: Optional[str] = None


class ModernDependencyAnalyzer:
    """
    Single Responsibility: Analyze and manage code dependencies intelligently
    Combines pattern matching with installation capabilities
    """
    
    def __init__(self):
        self.supported_languages = {
            'python', 'javascript', 'java', 'ruby', 'php', 'go', 'rust', 'c', 'cpp'
        }
        
        # Import/require patterns for each language
        self.import_patterns = {
            'python': [
                r'^\s*import\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)',
                r'^\s*from\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+import',
                r'^\s*import\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+as\s+',
            ],
            'javascript': [
                r'require\([\'"]([^\'"]+)[\'"]\)',
                r'import.*from\s+[\'"]([^\'"]+)[\'"]'
            ],
            'java': [
                r'^\s*import\s+([\w\.]+)\s*;',
                r'^\s*import\s+static\s+([\w\.]+)\s*;'
            ],
            'go': [
                r'^\s*import\s+[\'"]([^\'"]+)[\'"]',
                r'^\s*import\s+\(\s*[\'"]([^\'"]+)[\'"]'
            ],
            'rust': [
                r'use\s+([\w:]+)',
                r'extern\s+crate\s+([\w]+)'
            ],
            'c': [
                r'#include\s*<([^>]+)>',
                r'#include\s*"([^"]+)"'
            ],
            'cpp': [
                r'#include\s*<([^>]+)>',
                r'#include\s*"([^"]+)"'
            ]
        }
        
        # Standard/builtin libraries for each language
        self.standard_libraries = {
            'python': {
                'os', 'sys', 'math', 'random', 'datetime', 'json', 're', 'collections',
                'itertools', 'functools', 'operator', 'string', 'io', 'pathlib',
                'urllib', 'http', 'email', 'html', 'xml', 'csv', 'sqlite3',
                'pickle', 'copy', 'threading', 'multiprocessing', 'subprocess',
                'time', 'calendar', 'hashlib', 'hmac', 'secrets', 'uuid',
                'typing', 'dataclasses', 'enum', 'abc', 'contextlib', 'warnings',
                'gc', 'weakref', 'platform', 'site', 'sysconfig'
            },
            'javascript': {
                'fs', 'path', 'os', 'crypto', 'util', 'events', 'stream',
                'http', 'https', 'url', 'querystring', 'buffer', 'child_process',
                'cluster', 'dgram', 'dns', 'net', 'tls', 'zlib', 'readline',
                'repl', 'vm', 'worker_threads', 'perf_hooks', 'async_hooks',
                'console', 'process', 'global'
            },
            'java': {
                'java.lang', 'java.util', 'java.io', 'java.nio', 'java.net',
                'java.text', 'java.time', 'java.math', 'java.security',
                'java.sql', 'java.awt', 'javax.swing', 'java.concurrent',
                'java.beans', 'java.rmi', 'javax.xml', 'javax.crypto'
            },
            'go': {
                'fmt', 'os', 'io', 'strings', 'strconv', 'time', 'math',
                'sort', 'sync', 'net', 'http', 'json', 'regexp', 'path',
                'filepath', 'bufio', 'bytes', 'context', 'errors', 'log'
            },
            'rust': {
                'std', 'core', 'alloc', 'proc_macro', 'test'
            },
            'c': {
                'stdio.h', 'stdlib.h', 'string.h', 'math.h', 'time.h',
                'ctype.h', 'limits.h', 'float.h', 'stddef.h', 'stdint.h',
                'stdbool.h', 'stdarg.h', 'signal.h', 'setjmp.h', 'locale.h',
                'errno.h', 'assert.h', 'iso646.h', 'wchar.h', 'wctype.h'
            },
            'cpp': {
                'iostream', 'vector', 'string', 'algorithm', 'map', 'set',
                'queue', 'stack', 'deque', 'list', 'unordered_map',
                'unordered_set', 'memory', 'functional', 'iterator',
                'numeric', 'utility', 'tuple', 'array', 'forward_list'
            }
        }
        
        # Package manager configurations
        self.package_managers = {
            'python': {
                'installer': 'pip',
                'command': [sys.executable, '-m', 'pip', 'install'],
                'check_cmd': [sys.executable, '-c']
            },
            'javascript': {
                'installer': 'npm',
                'command': ['npm', 'install'],
                'check_cmd': ['node', '-e']
            },
            'java': {
                'installer': 'maven',
                'command': ['mvn', 'dependency:resolve'],
                'check_cmd': ['javac', '-cp']
            }
        }

    def extract_dependencies(self, code: str, language: str) -> List[DependencyInfo]:
        """Extract all dependencies from code"""
        if language not in self.supported_languages:
            return []
        
        dependencies = []
        raw_deps = self._extract_raw_dependencies(code, language)
        
        for dep_name in raw_deps:
            is_external = self._is_external_dependency(dep_name, language)
            dep_info = DependencyInfo(
                name=dep_name,
                language=language,
                is_external=is_external,
                install_command=self._get_install_command(dep_name, language) if is_external else None
            )
            dependencies.append(dep_info)
        
        return dependencies
    
    def _extract_raw_dependencies(self, code: str, language: str) -> Set[str]:
        """Extract raw dependency names using regex patterns"""
        dependencies = set()
        patterns = self.import_patterns.get(language, [])
        
        for pattern in patterns:
            matches = re.findall(pattern, code, re.MULTILINE)
            dependencies.update(matches)
        
        return dependencies
    
    def _is_external_dependency(self, dep_name: str, language: str) -> bool:
        """Check if dependency is external (not in standard library)"""
        standard_libs = self.standard_libraries.get(language, set())
        
        # Handle hierarchical imports (e.g., package.module)
        base_package = dep_name.split('.')[0] if '.' in dep_name else dep_name
        
        return base_package not in standard_libs
    
    def _get_install_command(self, dep_name: str, language: str) -> Optional[str]:
        """Get installation command for dependency"""
        pkg_manager = self.package_managers.get(language)
        if not pkg_manager:
            return None
        
        # Map dependency name to package name if needed
        package_name = self._map_dependency_to_package(dep_name, language)
        
        if language == 'python':
            return f"pip install {package_name}"
        elif language == 'javascript':
            return f"npm install {package_name}"
        elif language == 'java':
            return f"mvn dependency:get -Dartifact={package_name}"
        
        return None
    
    def _map_dependency_to_package(self, dep_name: str, language: str) -> str:
        """Map import name to actual package name"""
        # Common mappings for popular packages
        mappings = {
            'python': {
                'cv2': 'opencv-python',
                'PIL': 'Pillow',
                'bs4': 'beautifulsoup4',
                'sklearn': 'scikit-learn',
                'yaml': 'PyYAML'
            },
            'javascript': {
                '@': dep_name,  # Scoped packages
            }
        }
        
        lang_mappings = mappings.get(language, {})
        return lang_mappings.get(dep_name, dep_name)
    
    def get_external_dependencies(self, code: str, language: str) -> List[DependencyInfo]:
        """Get only external dependencies that need installation"""
        all_deps = self.extract_dependencies(code, language)
        return [dep for dep in all_deps if dep.is_external]
    
    def check_dependency_availability(self, dep_name: str, language: str) -> bool:
        """Check if dependency is already available"""
        try:
            if language == 'python':
                subprocess.run(
                    [sys.executable, '-c', f'import {dep_name}'],
                    check=True, capture_output=True, timeout=5
                )
                return True
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
            pass
        
        return False
    
    def is_dependency_error(self, error_message: str) -> bool:
        """Determine if error is due to missing dependencies"""
        dependency_error_patterns = [
            'ModuleNotFoundError',
            'ImportError',
            'cannot find module',
            'package does not exist',
            'ClassNotFoundException',
            'NoClassDefFoundError',
            'undefined reference',
            'fatal error.*No such file'
        ]
        
        if not error_message:
            return False
        
        return any(re.search(pattern, error_message, re.IGNORECASE) 
                  for pattern in dependency_error_patterns)
    
    def suggest_fixes(self, code: str, language: str, error_message: str) -> List[str]:
        """Suggest dependency installation commands based on error"""
        suggestions = []
        
        if self.is_dependency_error(error_message):
            external_deps = self.get_external_dependencies(code, language)
            
            for dep in external_deps:
                if not self.check_dependency_availability(dep.name, language):
                    if dep.install_command:
                        suggestions.append(dep.install_command)
        
        return suggestions