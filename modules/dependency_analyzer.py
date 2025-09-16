#!/usr/bin/env python3
"""
Analizzatore di dipendenze per codice multilingua
Identifica automaticamente le dipendenze necessarie senza modificare il codice
"""

import re
import os
from typing import List, Dict, Set, Tuple

class DependencyAnalyzer:
 """Analizza le dipendenze di file di codice in vari linguaggi"""

 def __init__(self):
 """Inizializza l'analizzatore con i pattern per ogni linguaggio"""

 # Pattern per identificare import/dipendenze per linguaggio
 self.dependency_patterns = {
 'python': {
 'patterns': [
 r'^\s*import\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)',
 r'^\s*from\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s+import',
 r'^\s*import\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+as\s+',
 ],
 'stdlib_modules': {
 'os', 'sys', 'math', 'random', 'datetime', 'json', 're', 'collections',
 'itertools', 'functools', 'operator', 'string', 'io', 'pathlib',
 'urllib', 'http', 'email', 'html', 'xml', 'csv', 'sqlite3',
 'pickle', 'copy', 'threading', 'multiprocessing', 'subprocess',
 'time', 'calendar', 'hashlib', 'hmac', 'secrets', 'uuid',
 'typing', 'dataclasses', 'enum', 'abc', 'contextlib'
 },
 'pip_mapping': {
 'numpy': 'numpy',
 'pandas': 'pandas',
 'matplotlib': 'matplotlib',
 'requests': 'requests',
 'scipy': 'scipy',
 'sklearn': 'scikit-learn',
 'cv2': 'opencv-python',
 'PIL': 'Pillow',
 'bs4': 'beautifulsoup4',
 'flask': 'Flask',
 'django': 'Django',
 'fastapi': 'fastapi',
 'sqlalchemy': 'SQLAlchemy',
 'pytest': 'pytest',
 'click': 'click',
 'jinja2': 'Jinja2',
 'yaml': 'PyYAML',
 'toml': 'toml',
 'tqdm': 'tqdm',
 'rich': 'rich',
 'typer': 'typer'
 }
 },

 'javascript': {
 'patterns': [
 r'^\s*import\s+.*?\s+from\s+[\'"]([^\'"]*)[\'"]\s*;?',
 r'^\s*const\s+.*?\s*=\s*require\s*\(\s*[\'"]([^\'"]*)[\'"]\s*\)',
 r'^\s*var\s+.*?\s*=\s*require\s*\(\s*[\'"]([^\'"]*)[\'"]\s*\)',
 r'^\s*let\s+.*?\s*=\s*require\s*\(\s*[\'"]([^\'"]*)[\'"]\s*\)',
 ],
 'builtin_modules': {
 'fs', 'path', 'os', 'crypto', 'util', 'events', 'stream',
 'http', 'https', 'url', 'querystring', 'buffer', 'child_process',
 'cluster', 'dgram', 'dns', 'net', 'tls', 'zlib', 'readline',
 'repl', 'vm', 'worker_threads', 'perf_hooks', 'async_hooks'
 },
 'npm_mapping': {
 'express': 'express',
 'lodash': 'lodash',
 'axios': 'axios',
 'react': 'react',
 'vue': 'vue',
 'angular': '@angular/core',
 'moment': 'moment',
 'chalk': 'chalk',
 'commander': 'commander',
 'inquirer': 'inquirer'
 }
 },

 'java': {
 'patterns': [
 r'^\s*import\s+([\w\.]+)\s*;',
 r'^\s*import\s+static\s+([\w\.]+)\s*;'
 ],
 'builtin_packages': {
 'java.lang', 'java.util', 'java.io', 'java.nio', 'java.net',
 'java.text', 'java.time', 'java.math', 'java.security',
 'java.sql', 'java.awt', 'javax.swing', 'java.concurrent'
 },
 'maven_mapping': {
 'org.apache.commons': 'commons-lang3',
 'com.google.gson': 'gson',
 'org.junit': 'junit',
 'org.springframework': 'spring-framework',
 'org.hibernate': 'hibernate-core'
 }
 },

 'cpp': {
 'patterns': [
 r'^\s*#include\s*<([^>]+)>',
 r'^\s*#include\s*"([^"]+)"'
 ],
 'standard_headers': {
 'iostream', 'vector', 'string', 'algorithm', 'map', 'set',
 'queue', 'stack', 'list', 'deque', 'unordered_map',
 'unordered_set', 'memory', 'functional', 'numeric',
 'iterator', 'utility', 'tuple', 'array', 'regex',
 'chrono', 'thread', 'mutex', 'condition_variable',
 'atomic', 'future', 'random', 'fstream', 'sstream'
 }
 },

 'c': {
 'patterns': [
 r'^\s*#include\s*<([^>]+)>',
 r'^\s*#include\s*"([^"]+)"'
 ],
 'standard_headers': {
 'stdio.h', 'stdlib.h', 'string.h', 'math.h', 'time.h',
 'ctype.h', 'limits.h', 'float.h', 'stddef.h', 'stdint.h',
 'stdbool.h', 'assert.h', 'errno.h', 'locale.h', 'signal.h',
 'setjmp.h', 'stdarg.h', 'unistd.h', 'sys/types.h'
 }
 },

 'csharp': {
 'patterns': [
 r'^\s*using\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)\s*;'
 ],
 'standard_namespaces': {
 'System', 'System.Collections', 'System.Collections.Generic',
 'System.IO', 'System.Text', 'System.Threading', 'System.Linq',
 'System.Net', 'System.Data', 'System.Xml'
 }
 },

 'ruby': {
 'patterns': [
 r'^\s*require\s+[\'"]([^\'"]*)[\'"]',
 r'^\s*require_relative\s+[\'"]([^\'"]*)[\'"]'
 ],
 'standard_libs': {
 'json', 'yaml', 'csv', 'uri', 'net', 'open', 'fileutils',
 'pathname', 'time', 'date', 'digest', 'base64', 'logger'
 }
 },

 'go': {
 'patterns': [
 r'^\s*import\s+"([^"]*)"',
 r'^\s*import\s+\(\s*"([^"]*)"'
 ],
 'standard_packages': {
 'fmt', 'os', 'io', 'strings', 'strconv', 'time', 'math',
 'sort', 'net', 'http', 'json', 'xml', 'crypto', 'log'
 }
 },

 'rust': {
 'patterns': [
 r'^\s*use\s+([a-zA-Z_][a-zA-Z0-9_]*(?:::[a-zA-Z_][a-zA-Z0-9_]*)*)',
 r'^\s*extern\s+crate\s+([a-zA-Z_][a-zA-Z0-9_]*)'
 ],
 'standard_crates': {
 'std', 'core', 'alloc', 'collections', 'env', 'fs', 'io',
 'net', 'process', 'thread', 'time', 'sync'
 }
 },

 'php': {
 'patterns': [
 r'^\s*require\s+[\'"]([^\'"]*)[\'"]',
 r'^\s*include\s+[\'"]([^\'"]*)[\'"]',
 r'^\s*use\s+([a-zA-Z_\\][a-zA-Z0-9_\\]*)'
 ],
 'builtin_functions': {
 'array', 'string', 'math', 'file', 'date', 'json', 'curl',
 'mysql', 'pdo', 'session', 'filter', 'hash'
 }
 },

 'r': {
 'patterns': [
 r'^\s*library\s*\(\s*([^)]*)\s*\)',
 r'^\s*require\s*\(\s*([^)]*)\s*\)'
 ],
 'base_packages': {
 'base', 'stats', 'graphics', 'grDevices', 'utils', 'datasets',
 'methods', 'grid', 'splines', 'stats4', 'tcltk', 'tools'
 }
 },

 'julia': {
 'patterns': [
 r'^\s*using\s+([a-zA-Z_][a-zA-Z0-9_]*)',
 r'^\s*import\s+([a-zA-Z_][a-zA-Z0-9_]*)'
 ],
 'standard_modules': {
 'Base', 'Core', 'LinearAlgebra', 'Statistics', 'Random',
 'Dates', 'Printf', 'Logging', 'Test', 'Pkg'
 }
 },

 'haskell': {
 'patterns': [
 r'^\s*import\s+([a-zA-Z_][a-zA-Z0-9_]*(?:\.[a-zA-Z_][a-zA-Z0-9_]*)*)'
 ],
 'standard_modules': {
 'Prelude', 'Data.List', 'Data.Maybe', 'Data.Either', 'Control.Monad',
 'System.IO', 'Text.Read', 'Data.Char', 'Data.Function'
 }
 },

 'ocaml': {
 'patterns': [
 r'^\s*open\s+([a-zA-Z_][a-zA-Z0-9_]*)',
 r'^\s*#require\s+"([^"]*)"'
 ],
 'standard_modules': {
 'List', 'Array', 'String', 'Printf', 'Random', 'Sys',
 'Unix', 'Filename', 'Buffer', 'Hashtbl'
 }
 }
 }

 def analyze_file(self, file_path: str, language: str) -> Dict[str, any]:
 """
 Analizza un file e determina le sue dipendenze

 Args:
 file_path: Percorso del file da analizzare
 language: Linguaggio del file

 Returns:
 Dict con informazioni sulle dipendenze
 """
 if not os.path.exists(file_path):
 return {'error': f'File non trovato: {file_path}'}

 if language.lower() not in self.dependency_patterns:
 return {'error': f'Linguaggio non supportato: {language}'}

 try:
 with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
 content = f.read()

 return self.analyze_content(content, language.lower())

 except Exception as e:
 return {'error': f'Errore lettura file: {e}'}

 def analyze_content(self, content: str, language: str) -> Dict[str, any]:
 """
 Analizza il contenuto e identifica le dipendenze

 Args:
 content: Contenuto del file
 language: Linguaggio del codice

 Returns:
 Dict con analisi delle dipendenze
 """
 lang_config = self.dependency_patterns.get(language, {})
 patterns = lang_config.get('patterns', [])

 # Set per raccogliere dipendenze uniche
 found_dependencies = set()

 # Analizza ogni riga
 for line in content.split('\n'):
 line = line.strip()
 if not line or line.startswith('#') or line.startswith('//'):
 continue

 # Applica tutti i pattern per il linguaggio
 for pattern in patterns:
 matches = re.findall(pattern, line, re.MULTILINE)
 for match in matches:
 if isinstance(match, tuple):
 match = match[0]
 found_dependencies.add(match.strip())

 # Classifica le dipendenze
 result = self._classify_dependencies(found_dependencies, language)
 result['total_dependencies'] = len(found_dependencies)
 result['language'] = language

 return result

 def _classify_dependencies(self, dependencies: Set[str], language: str) -> Dict[str, any]:
 """Classifica le dipendenze in built-in vs esterne"""

 lang_config = self.dependency_patterns.get(language, {})

 builtin_set = set()
 external_deps = set()
 install_commands = []

 if language == 'python':
 stdlib = lang_config.get('stdlib_modules', set())
 pip_mapping = lang_config.get('pip_mapping', {})

 for dep in dependencies:
 # Estrae il nome del modulo principale
 main_module = dep.split('.')[0]

 if main_module in stdlib:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 # Determina il comando di installazione
 pip_name = pip_mapping.get(main_module, main_module)
 install_commands.append(f"pip install {pip_name}")

 elif language == 'javascript':
 builtin_modules = lang_config.get('builtin_modules', set())
 npm_mapping = lang_config.get('npm_mapping', {})

 for dep in dependencies:
 if dep in builtin_modules or dep.startswith('./') or dep.startswith('../'):
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 npm_name = npm_mapping.get(dep, dep)
 install_commands.append(f"npm install {npm_name}")

 elif language == 'java':
 builtin_packages = lang_config.get('builtin_packages', set())

 for dep in dependencies:
 # Verifica se appartiene ai package standard
 is_builtin = any(dep.startswith(pkg) for pkg in builtin_packages)

 if is_builtin:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)

 elif language in ['cpp', 'c']:
 standard_headers = lang_config.get('standard_headers', set())

 for dep in dependencies:
 if dep in standard_headers:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)

 elif language == 'csharp':
 standard_namespaces = lang_config.get('standard_namespaces', set())

 for dep in dependencies:
 if any(dep.startswith(ns) for ns in standard_namespaces):
 builtin_set.add(dep)
 else:
 external_deps.add(dep)

 elif language == 'ruby':
 standard_libs = lang_config.get('standard_libs', set())

 for dep in dependencies:
 if dep in standard_libs:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"gem install {dep}")

 elif language == 'go':
 standard_packages = lang_config.get('standard_packages', set())

 for dep in dependencies:
 if dep in standard_packages or dep.startswith('golang.org/x/'):
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"go get {dep}")

 elif language == 'rust':
 standard_crates = lang_config.get('standard_crates', set())

 for dep in dependencies:
 main_crate = dep.split('::')[0]
 if main_crate in standard_crates:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"cargo add {main_crate}")

 elif language == 'php':
 builtin_functions = lang_config.get('builtin_functions', set())

 for dep in dependencies:
 # PHP ha molte funzioni built-in
 if any(dep.startswith(builtin) for builtin in builtin_functions):
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"composer require {dep}")

 elif language == 'r':
 base_packages = lang_config.get('base_packages', set())

 for dep in dependencies:
 dep_clean = dep.strip('\'"')
 if dep_clean in base_packages:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"install.packages('{dep_clean}')")

 elif language == 'julia':
 standard_modules = lang_config.get('standard_modules', set())

 for dep in dependencies:
 if dep in standard_modules:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"julia -e 'using Pkg; Pkg.add(\"{dep}\")'")

 elif language == 'haskell':
 standard_modules = lang_config.get('standard_modules', set())

 for dep in dependencies:
 if dep in standard_modules:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"cabal install {dep}")

 elif language == 'ocaml':
 standard_modules = lang_config.get('standard_modules', set())

 for dep in dependencies:
 if dep in standard_modules:
 builtin_set.add(dep)
 else:
 external_deps.add(dep)
 install_commands.append(f"opam install {dep}")

 return {
 'builtin_dependencies': sorted(list(builtin_set)),
 'external_dependencies': sorted(list(external_deps)),
 'install_commands': install_commands,
 'needs_installation': len(external_deps) > 0
 }

 def get_install_plan(self, file_path: str, language: str) -> Dict[str, any]:
 """
 Genera un piano di installazione per le dipendenze

 Args:
 file_path: Percorso del file
 language: Linguaggio del file

 Returns:
 Piano di installazione dettagliato
 """
 analysis = self.analyze_file(file_path, language)

 if 'error' in analysis:
 return analysis

 plan = {
 'file_path': file_path,
 'language': language,
 'analysis': analysis,
 'execution_ready': not analysis['needs_installation'],
 'install_steps': []
 }

 if analysis['needs_installation']:
 plan['install_steps'] = analysis['install_commands']

 return plan

def analyze_code_dependencies(file_path: str, language: str) -> Dict[str, any]:
 """
 Funzione di utilità per analizzare le dipendenze di un file

 Args:
 file_path: Percorso del file da analizzare
 language: Linguaggio del file

 Returns:
 Analisi delle dipendenze
 """
 analyzer = DependencyAnalyzer()
 return analyzer.get_install_plan(file_path, language)

if __name__ == "__main__":
 # Test dell'analizzatore
 analyzer = DependencyAnalyzer()

 # Test con contenuto Python di esempio
 test_python = """
import os
import sys
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from custom_module import my_function
"""

 print(" Test Analizzatore Dipendenze Python:")
 result = analyzer.analyze_content(test_python, 'python')
 print(f"Built-in: {result['builtin_dependencies']}")
 print(f"Esterni: {result['external_dependencies']}")
 print(f"Comandi install: {result['install_commands']}")
