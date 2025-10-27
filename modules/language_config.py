"""
Modern Language Configuration Manager for CLAP Project
Dynamic, conda-aware configuration system with single responsibility principle
"""

import os
import subprocess
from pathlib import Path
from typing import Dict, Optional, List


class LanguageConfigManager:
    """
    Single Responsibility: Manage language configurations dynamically
    Conda-aware, environment-adaptive configuration system
    """
    
    def __init__(self, conda_env: Optional[str] = None, use_conda: Optional[bool] = None):
        # By default prefer the CLAP environment only if explicitly requested via env or flag
        # use_conda can be set via environment variable CLAP_USE_CONDA ("1"/"true")
        if use_conda is None:
            env_val = os.environ.get('CLAP_USE_CONDA', '').lower()
            use_conda = env_val in ['1', 'true', 'yes']

        self.use_conda = use_conda

        if conda_env is None:
            conda_env = 'CLAP' if self.use_conda else None

        self.conda_env = conda_env
        self.conda_prefix = self._detect_conda_prefix() if self.use_conda else None
        
        # Detect MATLAB path once during initialization
        self._matlab_path = self._detect_matlab_path()
        
        # Core configuration constants
        self.DEFAULT_TIMEOUTS = {
            'interpreted': 10,
            'compiled': 30,
            'compilation': 60
        }
        
        self.MAX_SNIPPETS_PER_LANGUAGE = 500
        
    def _detect_conda_prefix(self) -> Optional[str]:
        """Detect conda environment prefix, prioritizing CLAP"""
        # If no conda env requested, skip detection
        if not self.conda_env:
            return None

        # If we are looking for CLAP, try common paths before using conda info
        if self.conda_env == 'CLAP':
            possible_paths = [
                '/home/lollo/miniconda3/envs/CLAP',
                '/Users/lorenzocappetti/miniconda3/envs/CLAP',
                os.path.expanduser('~/miniconda3/envs/CLAP'),
                os.path.expanduser('~/anaconda3/envs/CLAP')
            ]
            
            for path in possible_paths:
                if os.path.exists(path):
                    return path
        
        # Fallback to conda info only if conda_env is set
        if self.conda_env:
            try:
                result = subprocess.run(
                    ['conda', 'info', '--envs'], 
                    capture_output=True, text=True, timeout=10
                )
                for line in result.stdout.split('\n'):
                    if self.conda_env in line and not line.startswith('#'):
                        parts = line.split()
                        return parts[-1] if len(parts) > 1 else None
            except:
                pass
            return os.environ.get('CONDA_PREFIX')
    
    def _detect_matlab_path(self) -> Optional[str]:
        """Detect MATLAB executable path dynamically"""
        # 1. Check environment variable
        matlab_path = os.environ.get('MATLAB_PATH')
        if matlab_path:
            matlab_bin = os.path.join(matlab_path, 'bin', 'matlab')
            if os.path.isfile(matlab_bin) and os.access(matlab_bin, os.X_OK):
                return matlab_bin
        
        # 2. Check if matlab is in PATH
        try:
            result = subprocess.run(
                ['which', 'matlab'] if os.name != 'nt' else ['where', 'matlab'],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0 and result.stdout.strip():
                return result.stdout.strip().split('\n')[0]
        except:
            pass
        
        # 3. Check standard installation paths
        possible_paths = [
            '/usr/local/MATLAB/R2025b/bin/matlab',
            '/usr/local/MATLAB/R2025a/bin/matlab',
            '/usr/local/MATLAB/R2024b/bin/matlab',
            '/usr/local/MATLAB/R2024a/bin/matlab',
            '/usr/local/MATLAB/R2023b/bin/matlab',
            '/usr/local/MATLAB/R2023a/bin/matlab',
            '/opt/MATLAB/R2025b/bin/matlab',
            '/opt/MATLAB/R2024b/bin/matlab',
            '/opt/MATLAB/R2024a/bin/matlab',
            '/Applications/MATLAB_R2025b.app/bin/matlab',
            '/Applications/MATLAB_R2024b.app/bin/matlab',
        ]
        
        for path in possible_paths:
            if os.path.isfile(path) and os.access(path, os.X_OK):
                return path
        
        # 4. Search in common directories
        search_dirs = ['/usr/local/MATLAB', '/opt/MATLAB', '/Applications']
        for search_dir in search_dirs:
            if os.path.isdir(search_dir):
                try:
                    for entry in sorted(os.listdir(search_dir), reverse=True):
                        if 'MATLAB' in entry or entry.startswith('R20'):
                            matlab_candidate = os.path.join(search_dir, entry, 'bin', 'matlab')
                            if os.path.isfile(matlab_candidate) and os.access(matlab_candidate, os.X_OK):
                                return matlab_candidate
                except PermissionError:
                    pass
        
        return None
    
    def get_conda_command(self, command: str) -> str:
        """Get conda-aware command path"""
        # Return the binary inside the conda prefix only when use_conda is enabled
        if self.use_conda and self.conda_prefix:
            conda_bin = Path(self.conda_prefix) / 'bin' / command
            if conda_bin.exists():
                return str(conda_bin)
        return command
    
    def get_language_config(self, language: str) -> Dict:
        """Get dynamic configuration for a specific language"""
        base_configs = {
            'python': {
                'extension': '.py',
                'executor': self.get_conda_command('python'),
                'timeout': self.DEFAULT_TIMEOUTS['interpreted'],
                'test_code': 'print("Hello from Python!")',
                'type': 'interpreted',
                'compile_cmd': None
            },
            'javascript': {
                'extension': '.js',
                'executor': self.get_conda_command('node'),
                'timeout': self.DEFAULT_TIMEOUTS['interpreted'],
                'test_code': 'console.log("Hello from JavaScript!");',
                'type': 'interpreted',
                'compile_cmd': None
            },
            'java': {
                'extension': '.java',
                'compiler': self.get_conda_command('javac'),
                'executor': self.get_conda_command('java'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''public class Test {
    public static void main(String[] args) {
        System.out.println("Hello from Java!");
    }
}''',
                'type': 'compiled',
                'needs_class_name': True
            },
            'c': {
                'extension': '.c',
                'compiler': self.get_conda_command('gcc'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''#include <stdio.h>
int main() {
    printf("Hello from C!\\n");
    return 0;
}''',
                'type': 'compiled',
                'compile_flags': ['-std=c11', '-Wall']
            },
            'cpp': {
                'extension': '.cpp',
                'compiler': self.get_conda_command('g++'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''#include <iostream>
int main() {
    std::cout << "Hello from C++!" << std::endl;
    return 0;
}''',
                'type': 'compiled',
                'compile_flags': ['-std=c++20', '-Wall']
            },
            'go': {
                'extension': '.go',
                'compiler': self.get_conda_command('go'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''package main
import "fmt"
func main() {
    fmt.Println("Hello from Go!")
}''',
                'type': 'compiled',
                'build_cmd': 'build'
            },
            'rust': {
                'extension': '.rs',
                'compiler': self.get_conda_command('rustc'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''fn main() {
    println!("Hello from Rust!");
}''',
                'type': 'compiled',
                'compile_flags': ['-O']
            },
            'csharp': {
                'extension': '.cs',
                'compiler': self.get_conda_command('csc'),
                'executor': self.get_conda_command('mono'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''using System;
class Test {
    static void Main() {
        Console.WriteLine("Hello from C#!");
    }
}''',
                'type': 'compiled',
                'needs_class_name': True
            },
            'haskell': {
                'extension': '.hs',
                'compiler': self.get_conda_command('ghc'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': '''main :: IO ()
main = putStrLn "Hello from Haskell!"''',
                'type': 'compiled',
                'compile_flags': ['-O2']
            },
            'julia': {
                'extension': '.jl',
                'executor': self.get_conda_command('julia'),
                'timeout': self.DEFAULT_TIMEOUTS['interpreted'],
                'test_code': 'println("Hello from Julia!")',
                'type': 'interpreted',
                'compile_cmd': None
            },
            'matlab': {
                'extension': '.m',
                'executor': self._matlab_path or 'matlab',  # Dynamic detection
                'timeout': 60,  # MATLAB needs more time for startup
                'test_code': 'fprintf("Hello from MATLAB!\\n");',
                'type': 'interpreted',
                'compile_cmd': None,
                'executor_flags': ['-batch'],
                'available': self._matlab_path is not None
            },
            'ocaml': {
                'extension': '.ml',
                'compiler': self.get_conda_command('ocamlc'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': 'print_endline "Hello from OCaml!";;',
                'type': 'compiled',
                'compile_flags': []
            },
            'php': {
                'extension': '.php',
                'executor': self.get_conda_command('php'),
                'timeout': self.DEFAULT_TIMEOUTS['interpreted'],
                'test_code': '<?php\necho "Hello from PHP!\\n";\n?>',
                'type': 'interpreted',
                'compile_cmd': None
            },
            'r': {
                'extension': '.r',
                'executor': self.get_conda_command('Rscript'),
                'timeout': self.DEFAULT_TIMEOUTS['interpreted'],
                'test_code': 'cat("Hello from R!\\n")',
                'type': 'interpreted',
                'compile_cmd': None
            },
            'ruby': {
                'extension': '.rb',
                'executor': self.get_conda_command('ruby'),
                'timeout': self.DEFAULT_TIMEOUTS['interpreted'],
                'test_code': 'puts "Hello from Ruby!"',
                'type': 'interpreted',
                'compile_cmd': None
            },
            'typescript': {
                'extension': '.ts',
                'compiler': self.get_conda_command('tsc'),
                'executor': self.get_conda_command('node'),
                'timeout': self.DEFAULT_TIMEOUTS['compiled'],
                'test_code': 'console.log("Hello from TypeScript!");',
                'type': 'compiled',
                'compile_flags': []
            }
        }
        
        return base_configs.get(language.lower(), {})
    
    def get_all_supported_languages(self) -> List[str]:
        """Get list of all supported languages"""
        return ['python', 'javascript', 'java', 'c', 'cpp', 'go', 'rust', 
                'csharp', 'haskell', 'julia', 'matlab', 'ocaml', 'php', 'r', 'ruby', 'typescript']
    
    def get_file_extensions(self) -> Dict[str, str]:
        """Get mapping of languages to file extensions"""
        return {lang: self.get_language_config(lang).get('extension', '') 
                for lang in self.get_all_supported_languages()}
    
    def is_compiled_language(self, language: str) -> bool:
        """Check if language requires compilation"""
        config = self.get_language_config(language)
        return config.get('type') == 'compiled'
    
    def get_timeout(self, language: str, operation: str = 'execution') -> int:
        """Get timeout for specific language and operation"""
        if operation == 'compilation':
            return self.DEFAULT_TIMEOUTS['compilation']
        
        config = self.get_language_config(language)
        return config.get('timeout', self.DEFAULT_TIMEOUTS['interpreted'])
    
    def is_language_available(self, language: str) -> bool:
        """Check if a language is available on the system"""
        config = self.get_language_config(language)
        
        # Special handling for MATLAB
        if language.lower() == 'matlab':
            return config.get('available', False)
        
        # For other languages, check if executor/compiler exists
        if config.get('type') == 'compiled':
            compiler = config.get('compiler')
            if compiler:
                try:
                    result = subprocess.run(
                        ['which', compiler] if os.name != 'nt' else ['where', compiler],
                        capture_output=True,
                        timeout=5
                    )
                    return result.returncode == 0
                except:
                    return False
        else:
            executor = config.get('executor')
            if executor:
                try:
                    result = subprocess.run(
                        ['which', executor] if os.name != 'nt' else ['where', executor],
                        capture_output=True,
                        timeout=5
                    )
                    return result.returncode == 0
                except:
                    return False
        
        return False


# Legacy compatibility constants
TIMEOUT_SECONDS = 30
MAX_SNIPPETS_PER_LANGUAGE = 500
COMPILATION_TIMEOUT = 60


# Problematic patterns for language (codes to be filtered)
PROBLEMATIC_PATTERNS = {
 "Python": [
 r"import\s+tkinter",
 r"import\s+turtle",
 r"import\s+pygame",
 r"from\s+tkinter",
 r"input\s*\(",
 r"while\s+True:",
 r"for.*in.*range\(\s*\d{6,}"
 ],
 "Javascript": [
 r"document\.",
 r"window\.",
 r"prompt\s*\(",
 r"while\s*\(\s*true\s*\)"
 ],
 "Java": [
 r"JFrame",
 r"javax\.swing",
 r"Scanner.*System\.in",
 r"while\s*\(\s*true\s*\)"
 ],
 "C++": [
 r"getch\s*\(",
 r"cin\s*>>",
 r"while\s*\(\s*1\s*\)|while\s*\(\s*true\s*\)"
 ],
 "C": [
 r"getch\s*\(",
 r"scanf\s*\(",
 r"while\s*\(\s*1\s*\)"
 ],
 "C#": [
 r"Console\.ReadLine",
 r"Console\.ReadKey",
 r"System\.Windows\.Forms",
 r"while\s*\(\s*true\s*\)"
 ],
 "TypeScript": [
 r"document\.",
 r"window\.",
 r"prompt\s*\(",
 r"while\s*\(\s*true\s*\)"
 ],
 "Go": [
 r"fmt\.Scan",
 r"bufio\.NewReader.*os\.Stdin",
 r"for\s*{\s*}",  # Infinite loop
 r"for\s+true\s*{"
 ],
 "Rust": [
 r"std::io::stdin",
 r"loop\s*{",  # Infinite loop
 r"while\s+true\s*{"
 ],
 "Ruby": [
 r"gets",
 r"readline",
 r"require\s+['\"]tk['\"]",
 r"while\s+true"
 ],
 "PHP": [
 r"fgets\s*\(\s*STDIN",
 r"readline\s*\(",
 r"while\s*\(\s*true\s*\)"
 ],
 "Haskell": [
 r"getLine",
 r"getChar",
 r"interact"
 ],
 "OCaml": [
 r"read_line",
 r"input_line",
 r"Graphics\."
 ],
 "R": [
 r"readline\s*\(",
 r"scan\s*\(",
 r"while\s*\(\s*TRUE\s*\)"
 ],
 "MATLAB": [
 r"input\s*\(",
 r"ginput\s*\(",
 r"while\s+true",
 r"uicontrol"  # GUI
 ],
 "Julia": [
 r"readline\s*\(",
 r"read\s*\(\s*stdin",
 r"while\s+true"
 ]
}

# Problematic packages to avoid in automatic installation
PROBLEMATIC_PACKAGES = {
 "Python": ["tkinter", "turtle", "pygame", "numpy", "scipy", "matplotlib", "pandas"],
 "Javascript": ["fs", "path", "os", "crypto"],
 "TypeScript": ["fs", "path", "os", "crypto"],
 "Ruby": ["tk", "socket"],
 "PHP": ["socket", "ftp"],
 "R": ["base", "stats", "graphics"],
 "Java": ["javax.swing", "java.awt"],
 "C#": ["System.Windows.Forms"],
 "Go": ["syscall"],
 "Rust": ["std::io::stdin"],
 "Haskell": ["System.IO"],
 "OCaml": ["Graphics"],
 "MATLAB": ["uicontrol", "figure"],
 "Julia": ["Gtk", "Plots"]
}