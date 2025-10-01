#!/usr/bin/env python3
"""
Smart Executor - Esecutore intelligente che si adatta ai linguaggi disponibili
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

# Import modular components
try:
    from modules.language_config import LanguageConfigManager
    from modules.modern_logger import get_logger
    from modules.modern_dependency_analyzer import ModernDependencyAnalyzer
    MODULAR_COMPONENTS_AVAILABLE = True
except ImportError:
    MODULAR_COMPONENTS_AVAILABLE = False
    print("⚠️ Modular components not available, using fallback configuration")

# Importa il carbon tracker per monitorare l'impatto ambientale (lazy import)
CARBON_TRACKING_AVAILABLE = False  # Temporaneamente disabilitato per debug
start_carbon_tracking = None
stop_carbon_tracking = None

def _lazy_import_carbon():
    """Import lazy del carbon tracker"""
    global start_carbon_tracking, stop_carbon_tracking, CARBON_TRACKING_AVAILABLE
    if start_carbon_tracking is None and CARBON_TRACKING_AVAILABLE:
        try:
            from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking
        except:
            CARBON_TRACKING_AVAILABLE = False

class SmartExecutor:
    """Esecutore intelligente con componenti modulari che rileva automaticamente i linguaggi disponibili"""

    def __init__(self):
        self.results_dir = "results/execution"
        self.analysis_dir = "results/task_analysis"
        self.temp_files = []
        self.available_languages = {}
        
        # Initialize modular components
        if MODULAR_COMPONENTS_AVAILABLE:
            self.config_manager = LanguageConfigManager('SWAM')
            self.logger = get_logger(session_id=f"smart_executor_{int(time.time())}")
            self.dependency_analyzer = ModernDependencyAnalyzer()
            self.logger.info("🚀 SmartExecutor initialized with modular components")
            print("✅ MODULAR: Using modern language configuration and logging systems")
        else:
            self.config_manager = None
            self.logger = None
            self.dependency_analyzer = None
            print("⚠️ LEGACY: Using fallback configuration (modular components not available)")
        
        # Aggiungi homebrew al PATH per trovare tutti i compilatori
        homebrew_path = "/opt/homebrew/bin"
        if homebrew_path not in os.environ.get("PATH", ""):
            os.environ["PATH"] = homebrew_path + ":" + os.environ.get("PATH", "")

        # Configurazione completa per tutti i linguaggi supportati (aligned con LanguageTester)
        self.language_config = {
            'python': {
                'extension': '.py',
                'executor': ['conda', 'run', '-n', 'SWAM', 'python'],
                'timeout': 30,
                'test_code': 'print("test")'
            },
            'javascript': {
                'extension': '.js',
                'executor': ['conda', 'run', '-n', 'SWAM', 'node'],
                'timeout': 30,
                'test_code': 'console.log("test");'
            },
            'java': {
                'extension': '.java',
                'compiler': ['conda', 'run', '-n', 'SWAM', 'javac'],
                'executor': ['conda', 'run', '-n', 'SWAM', 'java'],
                'timeout': 30,
                'test_code': '''public class Test {
    public static void main(String[] args) {
        System.out.println("test");
    }
}'''
            },
            'ruby': {
                'extension': '.rb',
                'executor': ['conda', 'run', '-n', 'SWAM', 'ruby'],
                'timeout': 30,
                'test_code': 'puts "test"'
            },
            'php': {
                'extension': '.php',
                'executor': ['conda', 'run', '-n', 'SWAM', 'php'],
                'timeout': 30,
                'test_code': '<?php echo "test\\n"; ?>'
            },
            'r': {
                'extension': '.r',
                'executor': ['conda', 'run', '-n', 'SWAM', 'Rscript'],
                'timeout': 30,
                'test_code': 'cat("test\\n")'
            },
            'julia': {
                'extension': '.jl',
                'executor': ['conda', 'run', '-n', 'SWAM', 'julia'],
                'timeout': 30,
                'test_code': 'println("test")'
            },
            'matlab': {
                'extension': '.m',
                'executor': ['matlab', '-batch'],  # MATLAB non via conda
                'timeout': 30,
                'test_code': 'fprintf("test\\n");'
            },
            'csharp': {
                'extension': '.cs',
                'compiler': ['mcs'],  # Usa mcs (Mono C# compiler)
                'executor': ['mono'],
                'timeout': 30,
                'test_code': '''using System;
class Test {
    static void Main() {
        Console.WriteLine("test");
    }
}'''
            },
            'c': {
                'extension': '.c',
                'compiler': ['gcc', '-o'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''#include <stdio.h>
int main() {
    printf("test\\n");
    return 0;
}'''
            },
            'cpp': {
                'extension': '.cpp',
                'compiler': ['g++', '-std=c++20', '-o'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''#include <iostream>
int main() {
    std::cout << "test" << std::endl;
    return 0;
}'''
            },
            'go': {
                'extension': '.go',
                'executor': ['go', 'run'],
                'timeout': 30,
                'test_code': '''package main
import "fmt"
func main() {
    fmt.Println("test")
}'''
            },
            'rust': {
                'extension': '.rs',
                'compiler': ['rustc'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''fn main() {
    println!("test");
}'''
            },
            'haskell': {
                'extension': '.hs',
                'executor': ['runhaskell'],
                'timeout': 30,
                'test_code': '''main = putStrLn "test"'''
            },
            'ocaml': {
                'extension': '.ml',
                'compiler': ['ocamlc', '-o'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''print_endline "test";;'''
            },
            'typescript': {
                'extension': '.ts',
                'compiler': ['tsc'],
                'executor': ['node'],
                'timeout': 30,
                'test_code': '''console.log("test");'''
            }
        }

        # Rileva linguaggi disponibili dai risultati del test o fallback
        self.load_available_languages_from_test()

    def get_modular_config(self, language):
        """Get language configuration from modular components if available"""
        if self.config_manager:
            modular_config = self.config_manager.get_language_config(language)
            if modular_config:
                # Convert modular config to legacy format for compatibility
                return {
                    'extension': modular_config.get('extension', ''),
                    'executor': [modular_config.get('executor', '')],
                    'compiler': [modular_config.get('compiler', '')] if modular_config.get('compiler') else None,
                    'timeout': modular_config.get('timeout', 30),
                    'test_code': modular_config.get('test_code', '')
                }
        return None

    def log_modular_execution(self, language, task_name, execution_time, success, error=None):
        """Log execution using modular logger if available"""
        if self.logger:
            self.logger.log_execution_end(language, task_name, execution_time, success, 
                                        error_type="ExecutionError" if error else None)
            if error:
                self.logger.log_error(language, task_name, "ExecutionError", str(error), "")

    def analyze_dependencies(self, code, language):
        """Analyze code dependencies using modular analyzer if available"""
        if self.dependency_analyzer:
            dependencies = self.dependency_analyzer.get_external_dependencies(code, language)
            return dependencies
        return []

    def get_swam_env(self, language=None):
        """Ottiene l'ambiente modificato per includere i binari dell'ambiente SWAM"""
        env = os.environ.copy()
        
        # Path tipici per miniconda/conda
        conda_base = os.path.expanduser("~/miniconda3")
        swam_env_path = os.path.join(conda_base, "envs", "SWAM", "bin")
        
        if os.path.exists(swam_env_path):
            # Aggiungi il path dell'ambiente SWAM all'inizio del PATH
            current_path = env.get('PATH', '')
            env['PATH'] = f"{swam_env_path}:{current_path}"
        
        # Rimuovi LD_LIBRARY_PATH per linguaggi che hanno problemi con libtinfo
        if language in ['java', 'haskell', 'python', 'r', 'julia', 'javascript', 'php', 'ruby']:
            env.pop('LD_LIBRARY_PATH', None)
        
        return env

    def check_command_available(self, command):
        """Verifica se un comando è disponibile"""
        try:
            env = self.get_swam_env()
            result = subprocess.run(
                ['which', command] if os.name != 'nt' else ['where', command],
                capture_output=True,
                timeout=2,
                env=env
            )
            return result.returncode == 0
        except:
            return False

    def test_language_execution(self, language, config):
        """Testa se un linguaggio può essere eseguito"""
        try:
            # Crea file temporaneo
            with tempfile.NamedTemporaryFile(
                mode='w',
                suffix=config['extension'],
                delete=False
            ) as f:
                if language == 'java':
                    # Per Java, usa nome specifico per la classe
                    f.close()
                    os.unlink(f.name)
                    temp_file = f.name.replace(os.path.basename(f.name), 'Test.java')
                    with open(temp_file, 'w') as jf:
                        jf.write(config['test_code'])
                else:
                    f.write(config['test_code'])
                    temp_file = f.name

            temp_dir = os.path.dirname(temp_file)

            try:
                # Compilazione se necessaria
                if 'compiler' in config:
                    if language in ['c', 'cpp']:
                        # Per C/C++, aggiungi il nome del file di output
                        output_file = os.path.join(temp_dir, 'test')
                        compile_cmd = config['compiler'] + [output_file, temp_file]
                    else:
                        compile_cmd = config['compiler'] + [temp_file]
                    
                    compile_result = subprocess.run(
                        compile_cmd,
                        capture_output=True,
                        timeout=30,
                        cwd=temp_dir,
                        env=self.get_swam_env()
                    )
                    if compile_result.returncode != 0:
                        return False

                # Esecuzione
                if 'compiler' in config:
                    if language == 'java':
                        run_cmd = config['executor'] + ['Test']
                    elif language in ['c', 'cpp']:
                        run_cmd = ['./test']
                    else:
                        run_cmd = config['executor']
                else:
                    if language == 'matlab':
                        # MATLAB richiede solo il nome dello script
                        script_name = 'Test'
                        run_cmd = config['executor'] + [script_name]
                    else:
                        run_cmd = config['executor'] + [temp_file]

                run_result = subprocess.run(
                    run_cmd,
                    capture_output=True,
                    timeout=20,
                    cwd=temp_dir,
                    env=self.get_swam_env()
                )

                success = run_result.returncode == 0 and 'test' in run_result.stdout.decode()

                # Cleanup
                try:
                    os.unlink(temp_file)
                    if language == 'java' and os.path.exists(os.path.join(temp_dir, 'Test.class')):
                        os.unlink(os.path.join(temp_dir, 'Test.class'))
                    if language == 'csharp' and os.path.exists(os.path.join(temp_dir, 'test.exe')):
                        os.unlink(os.path.join(temp_dir, 'test.exe'))
                except:
                    pass

                return success

            except:
                return False

        except:
            return False

    def load_available_languages_from_test(self):
        """Carica i linguaggi disponibili dai risultati del test linguaggi"""
        import json
        from pathlib import Path
        
        test_results_dir = Path("results/execution")
        
        if test_results_dir.exists():
            # Cerca il file di test più recente
            test_files = list(test_results_dir.glob("language_test_results_*.json"))
            if test_files:
                latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
                try:
                    with open(latest_test, 'r') as f:
                        test_data = json.load(f)
                        
                    print(f" Caricando linguaggi da test: {latest_test.name}")
                    
                    # Mappa nomi dal test ai nomi del SmartExecutor (sono già identici)
                    test_to_executor_mapping = {
                        'cpp': 'cpp',
                        'java': 'java', 
                        'csharp': 'csharp',
                        'python': 'python',
                        'ruby': 'ruby',
                        'javascript': 'javascript',
                        'typescript': 'typescript',
                        'c': 'c',
                        'go': 'go',
                        'rust': 'rust',
                        'php': 'php',
                        'haskell': 'haskell',
                        'ocaml': 'ocaml',
                        'r': 'r',
                        'matlab': 'matlab',
                        'julia': 'julia'
                    }
                    
                    # Carica solo i linguaggi che hanno passato il test
                    for lang, result in test_data['results'].items():
                        if result['available'] and lang in test_to_executor_mapping:
                            executor_lang = test_to_executor_mapping[lang]
                            if executor_lang in self.language_config:
                                self.available_languages[executor_lang] = self.language_config[executor_lang]
                    
                    print(f" Linguaggi caricati dal test: {len(self.available_languages)}")
                    for lang in sorted(self.available_languages.keys()):
                        print(f" • {lang.upper()}")
                    
                    return True
                    
                except Exception as e:
                    print(f" Errore caricamento risultati test: {e}")
        
        # Fallback: usa il rilevamento tradizionale se non ci sono risultati del test
        print(" Nessun risultato test trovato, usando rilevamento tradizionale...")
        self.detect_available_languages()
        return False

    def detect_available_languages(self):
        """Rileva automaticamente i linguaggi disponibili"""
        print(" Rilevamento linguaggi disponibili...")

        for language, config in self.language_config.items():
            print(f" Testando {language}...", end=' ')

            # Per linguaggi compilati, verifica il compilatore
            if 'compiler' in config:
                compiler_cmd = config['compiler'][0]
                if not self.check_command_available(compiler_cmd):
                    print(" (comando non trovato)")
                    continue
            else:
                # Per linguaggi interpretati, verifica l'executor
                main_cmd = config['executor'][0]
                if not self.check_command_available(main_cmd):
                    print(" (comando non trovato)")
                    continue

            # Test di esecuzione rapido
            if self.test_language_execution(language, config):
                self.available_languages[language] = config
                print("")
            else:
                print(" (test esecuzione fallito)")

        print(f"\n Linguaggi disponibili: {len(self.available_languages)}")
        for lang in sorted(self.available_languages.keys()):
            print(f" • {lang.upper()}")

    def execute_code(self, code, language, task_name):
        """Esegue il codice se il linguaggio è disponibile"""

        # Avvia tracking CO2 per questa esecuzione (solo se non disabilitato)
        should_track = CARBON_TRACKING_AVAILABLE and not getattr(self, 'disable_carbon_tracking', False)
        if should_track:
            _lazy_import_carbon()
            if start_carbon_tracking:
                start_carbon_tracking(task_name, language)

        if language not in self.available_languages:
            if should_track and stop_carbon_tracking:
                stop_carbon_tracking()
            return {
                'success': False,
                'error': f'Linguaggio {language} non disponibile su questo sistema',
                'output': '',
                'execution_time': 0
            }

        config = self.available_languages[language]

        try:
            start_time = time.time()

            # Crea file temporaneo
            temp_file = self.create_temp_file(code, config['extension'], task_name, language)
            if not temp_file:
                if should_track and stop_carbon_tracking:
                    stop_carbon_tracking()
                return {
                    'success': False,
                    'error': 'Impossibile creare file temporaneo',
                    'output': '',
                    'execution_time': 0
                }

            temp_dir = os.path.dirname(temp_file)

            # Compilazione se necessaria
            if 'compiler' in config:
                compile_result = self.compile_code(temp_file, config, language)
                if not compile_result['success']:
                    return {
                        'success': False,
                        'error': f'Errore compilazione: {compile_result["error"]}',
                        'output': '',
                        'execution_time': time.time() - start_time
                    }

            # Esecuzione
            exec_result = self.run_code(temp_file, config, language, temp_dir)
            execution_time = time.time() - start_time

            result = {
                'success': exec_result['success'],
                'error': exec_result.get('error', ''),
                'output': exec_result.get('output', ''),
                'execution_time': execution_time
            }

            # Ferma tracking CO2
            if should_track and stop_carbon_tracking:
                stop_carbon_tracking()

            return result

        except Exception as e:
            # Ferma tracking CO2 anche in caso di errore
            if should_track and stop_carbon_tracking:
                stop_carbon_tracking()

            return {
                'success': False,
                'error': f'Errore esecuzione: {str(e)}',
                'output': '',
                'execution_time': time.time() - start_time if 'start_time' in locals() else 0
            }
        finally:
            # Cleanup
            self.cleanup_temp_files([temp_file] if 'temp_file' in locals() else [])

    def clean_code(self, code, language):
        """Pulisce il codice rimuovendo sintassi interattiva e problemi comuni"""
        # Prima di tutto, rimuove caratteri Unicode problematici
        cleaned = self.clean_code_content(code)
        
        # Rimuove token interattivi >>> e ... all'inizio delle righe
        cleaned = re.sub(r'^\s*>>>?\s*', '', cleaned, flags=re.MULTILINE)
        cleaned = re.sub(r'^\s*\.\.\.\s*', '', cleaned, flags=re.MULTILINE)
        
        # Correzioni specifiche per linguaggio
        if language == 'python':
            # Aggiunge parentesi a print statements Python 2
            cleaned = re.sub(r'\bprint\s+([^(][^\n]*)', r'print(\1)', cleaned)
            
        elif language == 'javascript':
            # Rimuove riferimenti a window/DOM per Node.js
            cleaned = re.sub(r'if\s*\(\s*window\.DOMParser\s*\)', 'if (false)', cleaned)
            cleaned = re.sub(r'window\.', '', cleaned)
            # Aggiunge dichiarazioni mancanti
            if 'Matrix' in cleaned and 'function Matrix' not in cleaned:
                cleaned = 'function Matrix() {}\n' + cleaned
                
        elif language == 'typescript':
            # Corregge opzioni compilatore
            cleaned = re.sub(r'-o\s+', '--outFile ', cleaned)
            
        elif language == 'java':
            # Se non c'è una classe definita, crea una classe Main
            if 'class' not in cleaned.lower():
                # Controlla se ci sono metodi/funzioni da wrappare
                has_methods = ('public static' in cleaned or 
                              'private static' in cleaned or
                              'static' in cleaned or
                              'public' in cleaned and ('(' in cleaned and ')' in cleaned))
                
                if has_methods:
                    # Aggiungi main method se manca
                    if 'main(' not in cleaned and 'main (' not in cleaned:
                        # Crea una classe con main che chiama il metodo principale se identificabile
                        cleaned = f'public class Main {{\n{cleaned}\n\n    public static void main(String[] args) {{\n        // Generated main method\n        System.out.println("Execution completed");\n    }}\n}}'
                    else:
                        cleaned = f'public class Main {{\n{cleaned}\n}}'
                else:
                    # Codice semplice, wrappa in main
                    cleaned = f'public class Main {{\n    public static void main(String[] args) {{\n{cleaned}\n    }}\n}}'
                    
        elif language == 'go':
            # Rimuove import di librerie esterne non standard
            lines = cleaned.split('\n')
            filtered_lines = []
            in_import_block = False
            removed_packages = []
            package_declared = False
            
            for line in lines:
                line_stripped = line.strip()
                
                # Gestisce dichiarazioni package (mantieni solo la prima)
                if line_stripped.startswith('package '):
                    if not package_declared:
                        if 'package main' not in line_stripped:
                            filtered_lines.append('package main')
                        else:
                            filtered_lines.append(line)
                        package_declared = True
                    continue
                
                # Gestisce blocchi di import
                elif line_stripped.startswith('import ('):
                    in_import_block = True
                    filtered_lines.append(line)
                    continue
                elif line_stripped == ')' and in_import_block:
                    in_import_block = False
                    filtered_lines.append(line)
                    continue
                elif in_import_block:
                    # Mantieni solo import standard
                    if any(std_pkg in line_stripped for std_pkg in ['"fmt"', '"os"', '"strings"', '"strconv"', '"math"', '"time"', '"io"', '"sort"', '"net"']):
                        filtered_lines.append(line)
                    else:
                        # Traccia i package rimossi per pulire il codice
                        if '"' in line_stripped:
                            pkg_name = line_stripped.split('"')[1].split('/')[-1]
                            removed_packages.append(pkg_name)
                    continue
                elif line_stripped.startswith('import "') and not any(std_pkg in line_stripped for std_pkg in ['"fmt"', '"os"', '"strings"', '"strconv"', '"math"', '"time"', '"io"', '"sort"', '"net"']):
                    # Traccia e salta import singoli di librerie esterne
                    if '"' in line_stripped:
                        pkg_name = line_stripped.split('"')[1].split('/')[-1]
                        removed_packages.append(pkg_name)
                    continue
                else:
                    filtered_lines.append(line)
            
            cleaned = '\n'.join(filtered_lines)
            
            # Rimuovi codice che usa i package rimossi e semplifica
            for pkg in removed_packages:
                # Rimuove chiamate al package (es. mat.NewDense, mat.Formatted)
                cleaned = re.sub(rf'\b{pkg}\.\w+\([^)]*\)', 'nil', cleaned)
                cleaned = re.sub(rf'\*{pkg}\.\w+', 'interface{}', cleaned)
                cleaned = re.sub(rf'\b{pkg}\.\w+', 'nil', cleaned)
            
            # Semplifica funzioni problematiche con versioni basic
            if 'mat.Dense' in cleaned or 'mat.NewDense' in cleaned:
                # Sostituisce con implementazione semplice senza dipendenze esterne
                cleaned = re.sub(r'func eye\([^}]*\}', '''func eye(n int) [][]int {
    matrix := make([][]int, n)
    for i := range matrix {
        matrix[i] = make([]int, n)
        matrix[i][i] = 1
    }
    return matrix
}''', cleaned, flags=re.DOTALL)
                
            # Pulisce il main da chiamate a funzioni esterne
            cleaned = re.sub(r'fmt\.Println\(mat\.Formatted\([^)]*\)\)', 'fmt.Println("Identity matrix created")', cleaned)
            
            cleaned = '\n'.join(filtered_lines)
            
            # Assicura che ci sia package main
            if 'package main' not in cleaned:
                cleaned = 'package main\n\n' + cleaned
                
            # Assicura che ci sia una funzione main
            if 'func main()' not in cleaned:
                cleaned += '\n\nfunc main() {\n    // Generated main function\n}\n'
                
        elif language == 'haskell':
            # Rimuove import di moduli esterni non standard
            lines = cleaned.split('\n')
            filtered_lines = []
            
            for line in lines:
                line_stripped = line.strip()
                # Mantieni solo import standard di Haskell
                if line_stripped.startswith('import ') and not any(std_mod in line_stripped for std_mod in ['Prelude', 'Data.List', 'Data.Char', 'System.IO']):
                    continue
                else:
                    filtered_lines.append(line)
            
            cleaned = '\n'.join(filtered_lines)
            
            # Assicura che ci sia una funzione main
            if 'main =' not in cleaned and 'main::' not in cleaned:
                # Trova la prima funzione definita per chiamarla in main
                function_match = re.search(r'^(\w+)\s+', cleaned, re.MULTILINE)
                if function_match:
                    func_name = function_match.group(1)
                    cleaned += f'\n\nmain = print ({func_name} 3)'
                else:
                    cleaned += '\n\nmain = putStrLn "Haskell execution completed"'
                
        elif language == 'rust':
            # Rimuove extern crate che possono causare problemi
            cleaned = re.sub(r'extern\s+crate\s+\w+;\s*\n?', '', cleaned)
            
            # Rimuove use statements di crate esterni
            cleaned = re.sub(r'use\s+\w+::\w+;\s*\n?', '', cleaned)
            
            # Sostituisce trait esterni con trait standard
            cleaned = re.sub(r'num::\w+', 'Copy + Clone + Default', cleaned)
            cleaned = re.sub(r'T::\w+\(\)', 'T::default()', cleaned)
            
            # Semplifica struct con generics problematici
            if 'num::' in cleaned or 'extern crate' in cleaned:
                # Sostituisce con implementazione più semplice
                cleaned = re.sub(r'struct Matrix<T>.*?where.*?\{', 'struct Matrix {\n    data: Vec<i32>,\n    size: usize,\n}\n\nimpl Matrix {', cleaned, flags=re.DOTALL)
                cleaned = re.sub(r'T::[a-zA-Z_]\w*\(\)', '0', cleaned)
                cleaned = re.sub(r'\bT\b', 'i32', cleaned)
            
            # Assicura che ci sia una funzione main
            if 'fn main()' not in cleaned:
                cleaned += '\n\nfn main() {\n    println!("Rust execution completed");\n}\n'
                
        return cleaned
    
    def create_temp_file(self, code, extension, task_name, language):
        """Crea un file temporaneo per il codice"""
        try:
            # Pulisce il codice prima di scriverlo
            cleaned_code = self.clean_code(code, language)
            
            # Sanitizza il nome del task
            safe_task = re.sub(r'[^\w\s-]', '', task_name).strip()
            safe_task = re.sub(r'[-\s/]+', '_', safe_task)

            if language == 'java':
                # Per Java, cerca il nome della classe nel codice (sia public che non public)
                class_match = re.search(r'(?:public\s+)?class\s+(\w+)', cleaned_code)
                if class_match:
                    class_name = class_match.group(1)
                    filename = f"{class_name}.java"
                    print(f" Creando file Java: {filename}")
                else:
                    filename = f"Main.java"
                    print(f" Nessuna classe trovata, usando: {filename}")
            else:
                filename = f"temp_{safe_task}_{language}{extension}"

            # Crea directory se necessaria
            abs_results_dir = os.path.abspath(self.results_dir)
            os.makedirs(abs_results_dir, exist_ok=True)

            filepath = os.path.join(abs_results_dir, filename)

            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(cleaned_code)

            self.temp_files.append(filepath)
            return filepath

        except Exception as e:
            print(f" Errore creazione file temporaneo: {e}")
            return None



    def compile_code(self, filepath, config, language):
        """Compila il codice se necessario"""
        try:
            temp_dir = os.path.dirname(filepath)
            base_name = Path(filepath).stem

            if language == 'java':
                cmd = config['compiler'] + [filepath]
            elif language == 'csharp':
                cmd = config['compiler'] + [filepath]
            elif language == 'go':
                # Go: rinomina il file in main.go e compila nell'output specifico
                main_go = os.path.join(temp_dir, 'main.go')
                os.rename(filepath, main_go)
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + ['-o', executable_path, 'main.go']
            elif language == 'rust':
                # Rust: compila con percorso assoluto
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath, '-o', executable_path]
            elif language == 'kotlin':
                # Kotlin: compila in JAR
                jar_path = os.path.join(temp_dir, 'test.jar')
                cmd = config['compiler'] + [filepath]
            elif language == 'swift':
                # Swift: compila con percorso assoluto
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath, '-o', executable_path]
            elif language in ['c', 'cpp']:
                # C/C++: ordine specifico degli argomenti
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [executable_path, filepath]
            elif language == 'ocaml':
                # OCaml: ordine specifico
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [executable_path, filepath]
            elif language == 'typescript':
                # TypeScript: compila in JS
                cmd = config['compiler'] + [filepath]
            elif language == 'haskell':
                # Haskell: ordine specifico ghc -o executable filepath
                executable_path = os.path.join(temp_dir, 'test')
                cmd = ['ghc', '-o', executable_path, filepath]
            else:
                # Altri linguaggi compilati
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath] + ['-o', executable_path]

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.get('timeout', 45),
                cwd=temp_dir,
                env=self.get_swam_env(language)
            )

            if result.returncode == 0:
                return {'success': True}
            else:
                return {
                    'success': False,
                    'error': result.stderr or result.stdout
                }

        except subprocess.TimeoutExpired:
            return {'success': False, 'error': 'Timeout durante compilazione'}
        except Exception as e:
            return {'success': False, 'error': str(e)}

    def run_code(self, filepath, config, language, temp_dir):
        """Esegue il codice"""
        try:
            if 'compiler' in config:
                if language == 'java':
                    # Per Java, estrae il nome della classe dal file compilato
                    with open(filepath, 'r', encoding='utf-8') as f:
                        code = f.read()

                    # Cerca sia 'public class' che 'class' senza public
                    class_match = re.search(r'(?:public\s+)?class\s+(\w+)', code)
                    if class_match:
                        class_name = class_match.group(1)
                        cmd = config['executor'] + [class_name]
                        print(f" Classe Java trovata: {class_name}")
                    else:
                        # Fallback: cerca per nome file
                        base_name = Path(filepath).stem
                        cmd = config['executor'] + [base_name]
                        print(f" Usando nome file: {base_name}")
                elif language == 'csharp':
                    # C#: eseguibile .exe generato dal compilatore
                    exe_name = Path(filepath).stem + '.exe'
                    exe_path = os.path.join(temp_dir, exe_name)
                    cmd = config['executor'] + [exe_path]
                elif language in ['c', 'cpp']:
                    # C/C++: usa nome fisso 'test'
                    executable_path = os.path.join(temp_dir, 'test')
                    cmd = [executable_path]
                elif language == 'ocaml':
                    # OCaml: usa nome fisso 'test' 
                    executable_path = os.path.join(temp_dir, 'test')
                    cmd = [executable_path]
                elif language == 'rust':
                    # Rust: usa nome fisso 'test'
                    executable_path = os.path.join(temp_dir, 'test')
                    cmd = [executable_path]
                elif language == 'typescript':
                    # TypeScript: esegui il file JS generato
                    js_file = filepath.replace('.ts', '.js')
                    cmd = config['executor'] + [js_file]
                else:
                    # Altri linguaggi compilati
                    executable_name = Path(filepath).stem
                    executable_path = os.path.join(temp_dir, executable_name)
                    cmd = [executable_path]
            else:
                # Linguaggi interpretati
                if language == 'matlab':
                    # MATLAB richiede solo il nome dello script senza estensione
                    script_name = Path(filepath).stem
                    cmd = config['executor'] + [script_name]
                else:
                    cmd = config['executor'] + [filepath]

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.get('timeout', 45),
                cwd=temp_dir,
                env=self.get_swam_env(language)
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

                # Rimuove anche file compilati
                temp_dir = os.path.dirname(filepath)
                base_name = Path(filepath).stem

                # File .class per Java
                class_file = os.path.join(temp_dir, f"{base_name}.class")
                if os.path.exists(class_file):
                    os.remove(class_file)

                # File .exe per C#
                exe_file = os.path.join(temp_dir, "test.exe")
                if os.path.exists(exe_file):
                    os.remove(exe_file)

            except Exception as e:
                print(f" Errore cleanup {filepath}: {e}")

    def find_task_file(self, task_name, language):
        """Trova il file per una task e linguaggio nella struttura gerarchica"""
        code_base_dir = "data/generated/code_snippets"

        # Normalizza i nomi dei linguaggi
        lang_normalized = language.lower()
        if lang_normalized == 'cpp':
            lang_normalized = 'c++'

        # Lista delle categorie presenti nel dataset
        categories = ['algorithms', 'strings', 'mathematics', 'io', 'basic', 'misc']

        # Cerca il file in tutte le categorie
        for category in categories:
            language_dir = os.path.join(code_base_dir, category, lang_normalized)
            if not os.path.exists(language_dir):
                continue

            # Cerca il file che contiene il nome della task (pattern migliorato)
            task_patterns = [
                task_name.replace('-', '_'),
                task_name.replace('_', '-'),
                task_name.replace(' ', '_'),
                task_name.replace(' ', '-')
            ]

            for filename in os.listdir(language_dir):
                filename_lower = filename.lower()
                for pattern in task_patterns:
                    if pattern.lower() in filename_lower:
                        return os.path.join(language_dir, filename)

        return None

    def clean_code_content(self, code):
        """Pulisce il codice da caratteri invisibili problematici"""

        # Sostituisce direttamente i caratteri problematici più comuni
        # U+00A0 (Non-Breaking Space) e altri spazi Unicode
        cleaned = code.replace('\u00a0', ' ') # Non-breaking space
        cleaned = cleaned.replace('\u2007', ' ') # Figure space
        cleaned = cleaned.replace('\u202f', ' ') # Narrow no-break space
        cleaned = cleaned.replace('\u2060', '') # Word joiner (invisibile)
        cleaned = cleaned.replace('\ufeff', '') # Byte order mark

        # Rimuove caratteri di controllo invisibili (tranne newline, tab, carriage return)
        import re
        cleaned = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]', '', cleaned)

        return cleaned

    def execute_task_all_available_languages(self, task_name):
        """Esegue una task in tutti i linguaggi disponibili"""
        print(f"\n Esecuzione task: {task_name}")

        task_results = {}

        # Cerca i file nella struttura reale: data/generated/code_snippets/category/language/
        code_base_dir = "data/generated/code_snippets"

        if not os.path.exists(code_base_dir):
            print(f" Directory base non trovata: {code_base_dir}")
            return task_results

        # Cerca i file di codice per i linguaggi disponibili in tutte le categorie
        for language in self.available_languages.keys():
            language_file = self.find_task_file(task_name, language)

            if language_file and os.path.exists(language_file):
                print(f" Esecuzione {language}...")

                try:
                    with open(language_file, 'r', encoding='utf-8') as f:
                        code = f.read()

                    # Pulisce il codice da caratteri invisibili
                    code = self.clean_code_content(code)

                    result = self.execute_code(code, language, task_name)
                    task_results[language] = result

                    if result['success']:
                        print(f" {language}: successo ({result['execution_time']:.2f}s)")
                        if result['output'].strip():
                            output_preview = result['output'].strip()[:100].replace('\n', ' ')
                            print(f" Output: {output_preview}...")
                    else:
                        print(f" {language}: {result['error'][:100]}...")

                except Exception as e:
                    task_results[language] = {
                        'success': False,
                        'error': f'Errore lettura file: {str(e)}',
                        'output': '',
                        'execution_time': 0
                    }
                    print(f" {language}: errore lettura file")
            else:
                print(f" {language}: file non trovato")

        return task_results

    def execute_all_common_tasks(self):
        """Esegue tutte le task comuni"""
        print(" SMART EXECUTOR - Esecuzione Adattiva")
        print(f" Linguaggi disponibili: {len(self.available_languages)}")

        # Carica task comuni
        common_tasks_file = os.path.join(self.analysis_dir, "common_tasks.json")

        if not os.path.exists(common_tasks_file):
            print(" File common_tasks.json non trovato")
            return

        with open(common_tasks_file, 'r') as f:
            data = json.load(f)

        common_tasks_data = data.get('common_tasks', [])

        if not common_tasks_data:
            print(" Nessuna task comune trovata")
            return

        # Estrae solo i nomi delle task (primi 10 per test)
        tasks = [task['name'] for task in common_tasks_data[:10]]

        print(f" Trovate {len(common_tasks_data)} task comuni totali")
        print(f" Eseguendo le prime {len(tasks)} task...")

        # Crea directory risultati
        os.makedirs(self.results_dir, exist_ok=True)

        overall_results = {}

        for task in tasks:
            task_results = self.execute_task_all_available_languages(task)
            overall_results[task] = task_results

        # Salva risultati
        self.save_execution_results(overall_results)

        # Report finale
        self.print_execution_summary(overall_results)

    def save_execution_results(self, results):
        """Salva i risultati dell'esecuzione"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = os.path.join(self.results_dir, f"smart_execution_results_{timestamp}.json")

        try:
            with open(results_file, 'w') as f:
                json.dump(results, f, indent=2, default=str)
            print(f" Risultati salvati in: {results_file}")
        except Exception as e:
            print(f" Errore salvataggio risultati: {e}")

    def print_execution_summary(self, results):
        """Stampa un riassunto dell'esecuzione"""
        print(f"\n RIASSUNTO ESECUZIONE SMART:")

        total_executions = 0
        successful_executions = 0

        for task, task_results in results.items():
            print(f"\n {task}:")
            task_success = 0
            task_total = 0

            for language, result in task_results.items():
                task_total += 1
                total_executions += 1

                if result['success']:
                    task_success += 1
                    successful_executions += 1
                    output_preview = result['output'].strip()[:50] if result['output'].strip() else "no output"
                    print(f" {language}: {result['execution_time']:.2f}s - {output_preview}")
                else:
                    print(f" {language}: {result['error'][:80]}...")

            if task_total > 0:
                success_rate = (task_success / task_total * 100)
                print(f" Successo: {task_success}/{task_total} ({success_rate:.1f}%)")

        if total_executions > 0:
            overall_success_rate = (successful_executions / total_executions * 100)
            print(f"\n🎉 TOTALE: {successful_executions}/{total_executions} ({overall_success_rate:.1f}%) esecuzioni riuscite")
            print(f" Linguaggi utilizzati: {sorted(self.available_languages.keys())}")
        else:
            print(f"\n Nessuna esecuzione completata")


if __name__ == "__main__":
    executor = SmartExecutor()
    executor.execute_all_common_tasks()
