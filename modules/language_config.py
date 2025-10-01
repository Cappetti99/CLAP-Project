"""
Modern Language Configuration Manager for SWAM Project
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
    
    def __init__(self, conda_env: Optional[str] = None):
        # SEMPRE preferisce SWAM se non specificato diversamente
        if conda_env is None:
            conda_env = 'SWAM'
        
        self.conda_env = conda_env
        self.conda_prefix = self._detect_conda_prefix()
        
        # Core configuration constants
        self.DEFAULT_TIMEOUTS = {
            'interpreted': 10,
            'compiled': 30,
            'compilation': 60
        }
        
        self.MAX_SNIPPETS_PER_LANGUAGE = 500
        
    def _detect_conda_prefix(self) -> Optional[str]:
        """Detect conda environment prefix, prioritizing SWAM"""
        # Se stiamo cercando SWAM, prova i path comuni prima di conda info
        if self.conda_env == 'SWAM':
            possible_paths = [
                '/home/lollo/miniconda3/envs/SWAM',
                '/Users/lorenzocappetti/miniconda3/envs/SWAM',
                os.path.expanduser('~/miniconda3/envs/SWAM'),
                os.path.expanduser('~/anaconda3/envs/SWAM')
            ]
            
            for path in possible_paths:
                if os.path.exists(path):
                    return path
        
        # Fallback al metodo conda info
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
    
    def get_conda_command(self, command: str) -> str:
        """Get conda-aware command path"""
        if self.conda_prefix:
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
            }
        }
        
        return base_configs.get(language.lower(), {})
    
    def get_all_supported_languages(self) -> List[str]:
        """Get list of all supported languages"""
        return ['python', 'javascript', 'java', 'c', 'cpp', 'go', 'rust']
    
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


# Legacy compatibility constants
TIMEOUT_SECONDS = 30
MAX_SNIPPETS_PER_LANGUAGE = 500
COMPILATION_TIMEOUT = 60

# Legacy configuration dictionary (deprecated - use LanguageConfigManager instead)
LEGACY_LANGUAGE_CONFIG = {
 "OOP": {
 "C++": {
 "ext": ".cpp",
 "compile_cmd": ["g++", "-std=c++20"],
 "run_cmd": "./",
 "install_deps": None,
 "common_includes": ["#include <iostream>", "#include <vector>", "#include <string>", "#include <algorithm>"],
 "folder": "../data/generated/code_snippets/oop/c++"
 },
 "C#": {
 "ext": ".cs",
 "compile_cmd": ["csc"],
 "run_cmd": "mono ",
 "install_deps": None,
 "common_includes": ["using System;"],
 "folder": "../data/generated/code_snippets/oop/csharp"
 },
 "Java": {
 "ext": ".java",
 "compile_cmd": ["javac"],
 "run_cmd": "java ",
 "install_deps": None,
 "common_includes": ["import java.util.*;", "import java.io.*;"],
 "folder": "../data/generated/code_snippets/oop/java"
 }
 },
 "Scripting": {
 "Python": {
 "ext": ".py",
 "compile_cmd": None,
 "run_cmd": "conda run -n SWAM python ",
 "install_deps": "conda run -n SWAM pip install",
 "common_imports": ["import sys", "import os", "import re", "import math", "import random"],
 "folder": "../data/generated/code_snippets/scripting/python"
 },
 "Ruby": {
 "ext": ".rb",
 "compile_cmd": None,
 "run_cmd": "ruby ",
 "install_deps": "gem install",
 "common_imports": ["require 'json'"],
 "folder": "../data/generated/code_snippets/scripting/ruby"
 },
 "JavaScript": {
 "ext": ".js",
 "compile_cmd": None,
 "run_cmd": "conda run -n SWAM node ",
 "install_deps": "conda run -n SWAM npm install -g",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scripting/javascript"
 },
 "TypeScript": {
 "ext": ".ts",
 "compile_cmd": ["conda", "run", "-n", "SWAM", "tsc"],
 "run_cmd": "conda run -n SWAM node ",
 "install_deps": "conda run -n SWAM npm install -g typescript",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scripting/typescript"
 }
 },
 "Imperative": {
 "C": {
 "ext": ".c",
 "compile_cmd": ["gcc", "-std=c99"],
 "run_cmd": "./",
 "install_deps": None,
 "common_includes": ["#include <stdio.h>", "#include <stdlib.h>", "#include <string.h>"],
 "folder": "../data/generated/code_snippets/imperative/c"
 },
 "Go": {
 "ext": ".go",
 "compile_cmd": ["conda", "run", "-n", "SWAM", "go", "build"],
 "run_cmd": "./",
 "install_deps": "conda run -n SWAM go get",
 "common_imports": ["import \"fmt\""],
 "folder": "../data/generated/code_snippets/imperative/go"
 },
 "Rust": {
 "ext": ".rs",
 "compile_cmd": ["conda", "run", "-n", "SWAM", "rustc"],
 "run_cmd": "./",
 "install_deps": "conda run -n SWAM cargo install",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/imperative/rust"
 },
 "PHP": {
 "ext": ".php",
 "compile_cmd": None,
 "run_cmd": "php ",
 "install_deps": None,
 "common_imports": ["<?php"],
 "folder": "../data/generated/code_snippets/imperative/php"
 }
 },
 "Functional": {
 "Haskell": {
 "ext": ".hs",
 "compile_cmd": ["ghc"],
 "run_cmd": "./",
 "install_deps": "cabal install",
 "common_imports": ["import Data.List"],
 "folder": "../data/generated/code_snippets/functional/haskell"
 },
 "OCaml": {
 "ext": ".ml",
 "compile_cmd": ["ocamlfind", "ocamlc"],
 "run_cmd": "./",
 "install_deps": "opam install",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/functional/ocaml"
 }
 },
 "Scientific": {
 "R": {
 "ext": ".r",
 "compile_cmd": None,
 "run_cmd": "conda run -n SWAM Rscript ",
 "install_deps": "conda run -n SWAM R -e 'install.packages'",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scientific/r"
 },
 "MATLAB": {
 "ext": ".m",
 "compile_cmd": None,
 "run_cmd": "octave ",
 "install_deps": None,
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scientific/matlab"
 },
 "Julia": {
 "ext": ".jl",
 "compile_cmd": None,
 "run_cmd": "julia ",
 "install_deps": "julia -e 'using Pkg; Pkg.add'",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scientific/julia"
 }
 }
}

# Pattern problematici per linguaggio (codici da filtrare)
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
 ]
}

# Pacchetti problematici da evitare nell'installazione automatica
PROBLEMATIC_PACKAGES = {
 "Python": ["tkinter", "turtle", "pygame", "numpy", "scipy", "matplotlib", "pandas"],
 "Javascript": ["fs", "path", "os", "crypto"],
 "Ruby": ["tk", "socket"],
 "R": ["base", "stats", "graphics"]
}