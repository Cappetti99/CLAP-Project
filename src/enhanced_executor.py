#!/usr/bin/env python3
"""
Enhanced Executor - Sistema avanzato per esecuzione multi-linguaggio
Gestisce automaticamente dipendenze, esecuzione e cleanup
"""

import sys
import os
import subprocess
import json
import re
import tempfile
import time
from pathlib import Path
from datetime import datetime

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

class EnhancedExecutor:
    """Esecutore avanzato con gestione completa delle dipendenze"""
    
    def __init__(self):
        self.results_dir = "results/execution"
        self.analysis_dir = "results/task_analysis"
        self.temp_files = []
        self.installed_packages = []
        
        # Configurazione avanzata per ogni linguaggio
        self.language_config = {
            'python': {
                'extension': '.py',
                'executor': [sys.executable],
                'timeout': 10,
                'dependency_patterns': [
                    r'import\s+([a-zA-Z_][a-zA-Z0-9_]*)',
                    r'from\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+import'
                ],
                'install_cmd': [sys.executable, '-m', 'pip', 'install'],
                'check_cmd': [sys.executable, '-c']
            },
            'javascript': {
                'extension': '.js',
                'executor': ['node'],
                'timeout': 10,
                'dependency_patterns': [
                    r'require\([\'"]([^\'"]+)[\'"]\)',
                    r'import.*from\s+[\'"]([^\'"]+)[\'"]'
                ],
                'install_cmd': ['npm', 'install', '-g'],
                'check_cmd': ['node', '-e']
            },
            'java': {
                'extension': '.java',
                'compiler': ['javac'],
                'executor': ['java'],
                'timeout': 15,
                'dependency_patterns': [
                    r'import\s+([a-zA-Z_][a-zA-Z0-9_.]*);'
                ]
            },
            'c++': {
                'extension': '.cpp',
                'compiler': ['g++', '-std=c++20', '-o'],
                'executor': ['./'],
                'timeout': 15,
                'dependency_patterns': [
                    r'#include\s*[<"]([^>"]+)[>"]'
                ]
            },
            'c': {
                'extension': '.c',
                'compiler': ['gcc', '-std=c99', '-o'],
                'executor': ['./'],
                'timeout': 15,
                'dependency_patterns': [
                    r'#include\s*[<"]([^>"]+)[>"]'
                ]
            },
            'go': {
                'extension': '.go',
                'compiler': ['go', 'build', '-o'],
                'executor': ['./'],
                'timeout': 15,
                'dependency_patterns': [
                    r'import\s+"([^"]+)"'
                ]
            },
            'rust': {
                'extension': '.rs',
                'compiler': ['rustc', '-o'],
                'executor': ['./'],
                'timeout': 15,
                'dependency_patterns': [
                    r'use\s+([a-zA-Z_][a-zA-Z0-9_:]*);'
                ]
            }
        }
    
    def analyze_dependencies(self, code, language):
        """Analizza le dipendenze nel codice"""
        lang_key = language.lower()
        config = self.language_config.get(lang_key, {})
        patterns = config.get('dependency_patterns', [])
        
        dependencies = set()
        
        for pattern in patterns:
            matches = re.findall(pattern, code, re.MULTILINE | re.IGNORECASE)
            dependencies.update(matches)
        
        # Filtra dipendenze standard/built-in
        standard_libs = self.get_standard_libraries(language)
        external_deps = [dep for dep in dependencies if dep not in standard_libs]
        
        return {
            'all_dependencies': list(dependencies),
            'external_dependencies': external_deps,
            'standard_dependencies': [dep for dep in dependencies if dep in standard_libs]
        }
    
    def get_standard_libraries(self, language):
        """Restituisce le librerie standard per ogni linguaggio"""
        standard_libs = {
            'python': {
                'os', 'sys', 'math', 'random', 'datetime', 'json', 're', 
                'collections', 'itertools', 'functools', 'operator', 'typing',
                'pathlib', 'time', 'hashlib', 'pickle', 'copy', 'string'
            },
            'javascript': {
                'fs', 'path', 'util', 'crypto', 'events', 'stream', 'buffer',
                'url', 'querystring', 'os', 'process'
            },
            'java': {
                'java.util', 'java.lang', 'java.io', 'java.math', 'java.net',
                'java.text', 'java.time', 'java.security', 'java.nio'
            },
            'c++': {
                'iostream', 'vector', 'string', 'algorithm', 'memory', 'functional',
                'utility', 'map', 'set', 'queue', 'stack', 'list', 'array',
                'cmath', 'cstdlib', 'cstring', 'ctime'
            },
            'c': {
                'stdio.h', 'stdlib.h', 'string.h', 'math.h', 'time.h', 
                'ctype.h', 'limits.h', 'float.h', 'stdint.h', 'stdbool.h'
            }
        }
        
        return standard_libs.get(language.lower(), set())
    
    def install_dependencies(self, dependencies, language):
        """Installa dipendenze temporaneamente"""
        if not dependencies:
            return {'success': True, 'installed': [], 'failed': []}
        
        lang_key = language.lower()
        config = self.language_config.get(lang_key, {})
        install_cmd = config.get('install_cmd')
        
        if not install_cmd:
            return {'success': True, 'installed': [], 'failed': dependencies}
        
        results = {'success': True, 'installed': [], 'failed': []}
        
        print(f"📦 Installazione dipendenze per {language}...")
        
        for dep in dependencies:
            try:
                # Verifica se già installato
                if self.check_dependency_available(dep, language):
                    continue
                
                # Installa la dipendenza
                cmd = install_cmd + [dep]
                result = subprocess.run(
                    cmd, 
                    capture_output=True, 
                    text=True, 
                    timeout=60
                )
                
                if result.returncode == 0:
                    results['installed'].append(dep)
                    self.installed_packages.append(dep)
                    print(f"  ✅ {dep}")
                else:
                    results['failed'].append(dep)
                    print(f"  ❌ {dep}: {result.stderr}")
                    
            except subprocess.TimeoutExpired:
                results['failed'].append(dep)
                print(f"  ⏰ {dep}: timeout")
            except Exception as e:
                results['failed'].append(dep)
                print(f"  ❌ {dep}: {e}")
        
        if results['failed']:
            results['success'] = False
        
        return results
    
    def check_dependency_available(self, dependency, language):
        """Verifica se una dipendenza è disponibile"""
        lang_key = language.lower()
        config = self.language_config.get(lang_key, {})
        check_cmd = config.get('check_cmd')
        
        if not check_cmd:
            return False
        
        try:
            if language.lower() == 'python':
                test_code = f"import {dependency}"
                cmd = check_cmd + [test_code]
            elif language.lower() == 'javascript':
                test_code = f"require('{dependency}')"
                cmd = check_cmd + [test_code]
            else:
                return False
            
            result = subprocess.run(
                cmd, 
                capture_output=True, 
                text=True, 
                timeout=5
            )
            
            return result.returncode == 0
            
        except Exception:
            return False
    
    def execute_code(self, code, language, task_name):
        """Esegue il codice e restituisce il risultato"""
        lang_key = language.lower()
        config = self.language_config.get(lang_key, {})
        
        if not config:
            return {
                'success': False,
                'error': f'Linguaggio {language} non supportato',
                'output': '',
                'execution_time': 0
            }
        
        # Analizza dipendenze
        deps_analysis = self.analyze_dependencies(code, language)
        external_deps = deps_analysis['external_dependencies']
        
        # Installa dipendenze se necessarie
        if external_deps:
            dep_result = self.install_dependencies(external_deps, language)
            if not dep_result['success']:
                return {
                    'success': False,
                    'error': f'Impossibile installare dipendenze: {dep_result["failed"]}',
                    'output': '',
                    'execution_time': 0,
                    'dependencies': deps_analysis
                }
        
        # Crea file temporaneo
        temp_file = self.create_temp_file(code, config['extension'], task_name, language)
        
        try:
            start_time = time.time()
            
            # Compila se necessario
            if 'compiler' in config:
                compile_result = self.compile_code(temp_file, config, language)
                if not compile_result['success']:
                    return {
                        'success': False,
                        'error': f'Errore compilazione: {compile_result["error"]}',
                        'output': compile_result.get('output', ''),
                        'execution_time': time.time() - start_time,
                        'dependencies': deps_analysis
                    }
            
            # Esegue il codice
            exec_result = self.run_executable(temp_file, config, language)
            execution_time = time.time() - start_time
            
            return {
                'success': exec_result['success'],
                'error': exec_result.get('error', ''),
                'output': exec_result.get('output', ''),
                'execution_time': execution_time,
                'dependencies': deps_analysis
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': f'Errore esecuzione: {str(e)}',
                'output': '',
                'execution_time': time.time() - start_time if 'start_time' in locals() else 0,
                'dependencies': deps_analysis
            }
        finally:
            # Cleanup file temporanei
            self.cleanup_temp_files([temp_file])
    
    def create_temp_file(self, code, extension, task_name, language):
        """Crea un file temporaneo per il codice"""
        try:
            # Sanitizza il nome del task
            safe_task = re.sub(r'[^\w\s-]', '', task_name).strip()
            safe_task = re.sub(r'[-\s/]+', '_', safe_task)
            
            filename = f"temp_{safe_task}_{language.lower()}{extension}"
            
            # Assicurati che il path sia assoluto e corretto
            abs_results_dir = os.path.abspath(self.results_dir)
            os.makedirs(abs_results_dir, exist_ok=True)
            
            filepath = os.path.join(abs_results_dir, filename)
            
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(code)
            
            self.temp_files.append(filepath)
            return filepath
        except Exception as e:
            print(f"⚠️ Errore creazione file temporaneo: {e}")
            return None
    
    def compile_code(self, filepath, config, language):
        """Compila il codice se necessario"""
        try:
            # Verifica che il file esista
            if not os.path.exists(filepath):
                return {'success': False, 'error': f'File sorgente non trovato: {filepath}'}
            
            # Determina il nome dell'eseguibile
            base_name = Path(filepath).stem
            file_dir = os.path.dirname(filepath)
            
            if language.lower() == 'java':
                # Per Java, estrae il nome della classe
                with open(filepath, 'r') as f:
                    code = f.read()
                class_match = re.search(r'public\s+class\s+(\w+)', code)
                if class_match:
                    executable = class_match.group(1)
                else:
                    executable = base_name
                
                # Java compila nello stesso directory
                cmd = config['compiler'] + [filepath]
                executable_path = os.path.join(file_dir, f"{executable}.class")
            else:
                # Altri linguaggi compilati
                executable = base_name
                executable_path = os.path.join(file_dir, executable)
                cmd = config['compiler'] + [filepath] + ['-o', executable_path]
            
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.get('timeout', 30),
                cwd=file_dir
            )
            
            if result.returncode == 0:
                return {'success': True, 'executable': executable_path}
            else:
                return {
                    'success': False,
                    'error': result.stderr or result.stdout,
                    'output': result.stdout
                }
                
        except subprocess.TimeoutExpired:
            return {'success': False, 'error': 'Timeout durante compilazione'}
        except Exception as e:
            return {'success': False, 'error': str(e)}
    
    def run_executable(self, filepath, config, language):
        """Esegue il codice compilato o interpretato"""
        try:
            file_dir = os.path.dirname(filepath)
            
            if language.lower() == 'java':
                # Per Java, esegue la classe compilata
                with open(filepath, 'r') as f:
                    code = f.read()
                class_match = re.search(r'public\s+class\s+(\w+)', code)
                if class_match:
                    class_name = class_match.group(1)
                    cmd = config['executor'] + [class_name]
                else:
                    return {'success': False, 'error': 'Classe Java non trovata'}
            elif 'compiler' in config:
                # Per linguaggi compilati, usa il path dell'eseguibile dalla compilazione
                executable_name = Path(filepath).stem
                executable_path = os.path.join(file_dir, executable_name)
                
                # Verifica che l'eseguibile esista
                if not os.path.exists(executable_path):
                    return {'success': False, 'error': f'Eseguibile non trovato: {executable_path}'}
                
                cmd = [executable_path]
            else:
                # Per linguaggi interpretati
                if not os.path.exists(filepath):
                    return {'success': False, 'error': f'File sorgente non trovato: {filepath}'}
                cmd = config['executor'] + [filepath]
            
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.get('timeout', 30),
                cwd=file_dir
            )
            
            return {
                'success': result.returncode == 0,
                'output': result.stdout,
                'error': result.stderr if result.returncode != 0 else ''
            }
            
        except subprocess.TimeoutExpired:
            return {'success': False, 'error': 'Timeout durante esecuzione', 'output': ''}
        except Exception as e:
            return {'success': False, 'error': str(e), 'output': ''}
    
    def cleanup_temp_files(self, files):
        """Pulisce i file temporanei"""
        for filepath in files:
            try:
                if os.path.exists(filepath):
                    os.remove(filepath)
                
                # Rimuove anche eseguibili compilati
                if filepath.endswith(('.c', '.cpp', '.go', '.rs')):
                    executable = Path(filepath).stem
                    exec_path = os.path.join(os.path.dirname(filepath), executable)
                    if os.path.exists(exec_path):
                        os.remove(exec_path)
                
                # Rimuove file .class per Java
                if filepath.endswith('.java'):
                    class_file = filepath.replace('.java', '.class')
                    if os.path.exists(class_file):
                        os.remove(class_file)
                        
            except Exception as e:
                print(f"⚠️ Errore cleanup {filepath}: {e}")
    
    def execute_task_all_languages(self, task_name):
        """Esegue una task in tutti i linguaggi disponibili"""
        print(f"\n🎯 Esecuzione task: {task_name}")
        
        task_results = {}
        code_snippets_dir = os.path.join(self.analysis_dir, "code_snippets", task_name.replace('/', '_'))
        
        if not os.path.exists(code_snippets_dir):
            print(f"❌ Directory non trovata: {code_snippets_dir}")
            return task_results
        
        # Trova tutti i file di codice
        code_files = list(Path(code_snippets_dir).glob("*"))
        
        for code_file in code_files:
            if code_file.is_file():
                # Determina il linguaggio dal nome file
                language = self.detect_language_from_filename(code_file.name)
                
                if language:
                    print(f"  🔧 Esecuzione {language}...")
                    
                    try:
                        with open(code_file, 'r', encoding='utf-8') as f:
                            code = f.read()
                        
                        result = self.execute_code(code, language, task_name)
                        task_results[language] = result
                        
                        if result['success']:
                            print(f"    ✅ {language}: successo ({result['execution_time']:.2f}s)")
                        else:
                            print(f"    ❌ {language}: {result['error']}")
                            
                    except Exception as e:
                        task_results[language] = {
                            'success': False,
                            'error': f'Errore lettura file: {str(e)}',
                            'output': '',
                            'execution_time': 0
                        }
                        print(f"    ❌ {language}: errore lettura file")
        
        return task_results
    
    def detect_language_from_filename(self, filename):
        """Determina il linguaggio dal nome del file"""
        extension_map = {
            '.py': 'python',
            '.js': 'javascript',
            '.java': 'java',
            '.cpp': 'c++',
            '.c': 'c',
            '.go': 'go',
            '.rs': 'rust',
            '.cs': 'c#',
            '.rb': 'ruby',
            '.php': 'php',
            '.hs': 'haskell',
            '.ml': 'ocaml',
            '.r': 'r',
            '.m': 'matlab',
            '.jl': 'julia',
            '.ts': 'typescript'
        }
        
        for ext, lang in extension_map.items():
            if filename.endswith(ext):
                return lang
        
        return None
    
    def execute_all_common_tasks(self):
        """Esegue tutte le task comuni"""
        print("🚀 ENHANCED EXECUTOR - Esecuzione Completa")
        
        # Carica task comuni
        common_tasks_file = os.path.join(self.analysis_dir, "common_tasks.json")
        
        if not os.path.exists(common_tasks_file):
            print("❌ File common_tasks.json non trovato")
            return
        
        with open(common_tasks_file, 'r') as f:
            data = json.load(f)
        
        tasks = data.get('tasks', [])
        
        if not tasks:
            print("❌ Nessuna task comune trovata")
            return
        
        print(f"📊 Trovate {len(tasks)} task comuni")
        
        # Crea directory risultati
        os.makedirs(self.results_dir, exist_ok=True)
        
        overall_results = {}
        
        for task in tasks:
            task_results = self.execute_task_all_languages(task)
            overall_results[task] = task_results
        
        # Salva risultati
        self.save_execution_results(overall_results)
        
        # Report finale
        self.print_execution_summary(overall_results)
    
    def save_execution_results(self, results):
        """Salva i risultati dell'esecuzione"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = os.path.join(self.results_dir, f"execution_results_{timestamp}.json")
        
        try:
            with open(results_file, 'w') as f:
                json.dump(results, f, indent=2, default=str)
            print(f"💾 Risultati salvati in: {results_file}")
        except Exception as e:
            print(f"⚠️ Errore salvataggio risultati: {e}")
    
    def print_execution_summary(self, results):
        """Stampa un riassunto dell'esecuzione"""
        print(f"\n📊 RIASSUNTO ESECUZIONE:")
        
        total_executions = 0
        successful_executions = 0
        
        for task, task_results in results.items():
            print(f"\n🎯 {task}:")
            task_success = 0
            task_total = 0
            
            for language, result in task_results.items():
                task_total += 1
                total_executions += 1
                
                if result['success']:
                    task_success += 1
                    successful_executions += 1
                    print(f"  ✅ {language}: {result['execution_time']:.2f}s")
                else:
                    print(f"  ❌ {language}: {result['error'][:50]}...")
            
            success_rate = (task_success / task_total * 100) if task_total > 0 else 0
            print(f"  📈 Successo: {task_success}/{task_total} ({success_rate:.1f}%)")
        
        overall_success_rate = (successful_executions / total_executions * 100) if total_executions > 0 else 0
        print(f"\n🎉 TOTALE: {successful_executions}/{total_executions} ({overall_success_rate:.1f}%) esecuzioni riuscite")


if __name__ == "__main__":
    executor = EnhancedExecutor()
    executor.execute_all_common_tasks()
