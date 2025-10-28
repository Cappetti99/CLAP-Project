"""
Modern Language Configuration Manager for CLAP Project

This module provides a dynamic, conda-aware configuration system for managing
language support across the CLAP project. It handles:
- Language-specific compiler/interpreter configurations
- Timeout settings for different execution types
- Conda environment detection and integration
- Command path resolution with fallbacks
- Language capability detection
- Quality pattern definitions
"""

import os
import subprocess
from pathlib import Path
from typing import Dict, Optional, List


class LanguageConfigManager:
    """Single-responsibility configuration management for all supported languages.
    
    This class provides:
    - Centralized configuration for 15 programming languages
    - Conda environment support for isolated execution
    - Dynamic command resolution with fallback chains
    - Timeout configuration by language and operation type
    - Language capability detection and availability checking
    
    The class follows the single responsibility principle: it only manages
    language configurations and leaves execution to other modules.
    
    Attributes:
        use_conda: Whether to use conda environments for execution
        conda_env: Conda environment name to use (default: 'CLAP')
        conda_prefix: Full path to conda environment
        DEFAULT_TIMEOUTS: Timeout values for different operation types
    """
    
    def __init__(self, conda_env: Optional[str] = None, use_conda: Optional[bool] = None):
        """Initialize language configuration manager.
        
        Args:
            conda_env: Conda environment name to use (defaults to 'CLAP' if use_conda=True)
            use_conda: Whether to use conda environments (defaults to environment variable)
        """
        # ===== CONDA CONFIGURATION =====
        # By default prefer the CLAP environment only if explicitly requested via env or flag
        # use_conda can be set via environment variable CLAP_USE_CONDA ("1"/"true")
        if use_conda is None:
            env_val = os.environ.get('CLAP_USE_CONDA', '').lower()
            use_conda = env_val in ['1', 'true', 'yes']

        self.use_conda = use_conda

        if conda_env is None:
            conda_env = 'CLAP' if self.use_conda else None

        self.conda_env = conda_env
        # Detect conda environment path if conda is enabled
        self.conda_prefix = self._detect_conda_prefix() if self.use_conda else None
        
        # ===== CORE CONFIGURATION CONSTANTS =====
        # Timeout values for different types of operations
        # These can be overridden per-language in get_language_config()
        self.DEFAULT_TIMEOUTS = {
            'interpreted': 10,   # Interpreted languages usually run quickly
            'compiled': 30,      # Compiled binaries typically finish faster
            'compilation': 60    # Compilation can take longer
        }
        
        # Maximum code snippets per language to load in memory
        self.MAX_SNIPPETS_PER_LANGUAGE = 500
        
    def _detect_conda_prefix(self) -> Optional[str]:
        """Detect conda environment prefix, prioritizing CLAP environment.
        
        Strategy:
        1. Check common hardcoded paths for CLAP environment
        2. Query conda for environment information
        3. Fall back to CONDA_PREFIX environment variable
        
        Returns:
            str: Full path to conda environment, or None if not found
        """
        # ===== STEP 1: SKIP IF NO CONDA ENV REQUESTED =====
        # If no conda env requested, skip detection entirely
        if not self.conda_env:
            return None

        # ===== STEP 2: CHECK COMMON PATHS FOR CLAP ENVIRONMENT =====
        # If we are looking for CLAP, try common paths before using conda info
        # This is faster and more reliable than querying conda
        if self.conda_env == 'CLAP':
            possible_paths = [
                '/home/lollo/miniconda3/envs/CLAP',
                '/Users/lorenzocappetti/miniconda3/envs/CLAP',
                os.path.expanduser('~/miniconda3/envs/CLAP'),
                os.path.expanduser('~/anaconda3/envs/CLAP')
            ]
            
            # Return first existing path
            for path in possible_paths:
                if os.path.exists(path):
                    return path
        
        # ===== STEP 3: QUERY CONDA IF ENV NOT FOUND =====
        # Fallback to conda info only if conda_env is set
        if self.conda_env:
            try:
                result = subprocess.run(
                    ['conda', 'info', '--envs'], 
                    capture_output=True, text=True, timeout=10
                )
                # Parse conda output to find environment path
                for line in result.stdout.split('\n'):
                    if self.conda_env in line and not line.startswith('#'):
                        parts = line.split()
                        return parts[-1] if len(parts) > 1 else None
            except:
                # If conda query fails, try environment variable
                pass
            return os.environ.get('CONDA_PREFIX')
    
    def get_conda_command(self, command: str) -> str:
        """Get conda-aware command path for execution.
        
        Resolves command location with fallback chain:
        1. If conda enabled: check conda environment bin directory
        2. Fall back to system PATH
        
        Args:
            command: Command name to resolve (e.g., 'python3', 'gcc')
            
        Returns:
            str: Full command path or just the command name for system PATH lookup
        """
        # ===== CONDA-AWARE RESOLUTION =====
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
                'csharp', 'haskell', 'julia', 'ocaml', 'php', 'r', 'ruby', 'typescript']
    
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
 "Julia": ["Gtk", "Plots"]
}