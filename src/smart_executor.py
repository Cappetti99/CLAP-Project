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

# Importa il carbon tracker per monitorare l'impatto ambientale
try:
    from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking
    CARBON_TRACKING_AVAILABLE = True
except ImportError:
    CARBON_TRACKING_AVAILABLE = False

class SmartExecutor:
    """Esecutore intelligente che rileva automaticamente i linguaggi disponibili"""
    
    def __init__(self):
        self.results_dir = "results/execution"
        self.analysis_dir = "results/task_analysis"
        self.temp_files = []
        self.available_languages = {}
        
        # Configurazione completa per tutti i linguaggi supportati
        self.language_config = {
            'python': {
                'extension': '.py',
                'executor': ['python'],
                'timeout': 10,
                'test_code': 'print("test")'
            },
            'javascript': {
                'extension': '.js',
                'executor': ['node'],
                'timeout': 10,
                'test_code': 'console.log("test");'
            },
            'java': {
                'extension': '.java',
                'compiler': ['javac'],
                'executor': ['java'],
                'timeout': 30,
                'test_code': '''public class Test {
    public static void main(String[] args) {
        System.out.println("test");
    }
}'''
            },
            'ruby': {
                'extension': '.rb',
                'executor': ['ruby'],
                'timeout': 15,
                'test_code': 'puts "test"'
            },
            'php': {
                'extension': '.php',
                'executor': ['php'],
                'timeout': 15,
                'test_code': '<?php echo "test\\n"; ?>'
            },
            'r': {
                'extension': '.r',
                'executor': ['/Users/lorenzocappetti/miniconda3/envs/SWAM/bin/Rscript'],
                'timeout': 15,
                'test_code': 'cat("test\\n")'
            },
            'julia': {
                'extension': '.jl',
                'executor': ['julia'],
                'timeout': 20,
                'test_code': 'println("test")'
            },
            'matlab': {
                'extension': '.m',
                'executor': ['matlab', '-batch'],
                'timeout': 30,
                'test_code': 'fprintf("test\\n");'
            },
            'csharp': {
                'extension': '.cs',
                'compiler': ['csc', '-out:test.exe'],
                'executor': ['mono', 'test.exe'],
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
                'compiler': ['gcc', '-o', 'test'],
                'executor': ['./test'],
                'timeout': 25,
                'test_code': '''#include <stdio.h>
int main() {
    printf("test\\n");
    return 0;
}'''
            },
            'cpp': {
                'extension': '.cpp',
                'compiler': ['g++', '-o', 'test'],
                'executor': ['./test'],
                'timeout': 25,
                'test_code': '''#include <iostream>
int main() {
    std::cout << "test" << std::endl;
    return 0;
}'''
            },
            'go': {
                'extension': '.go',
                'compiler': ['go', 'build'],
                'executor': ['./test'],
                'timeout': 25,
                'test_code': '''package main
import "fmt"
func main() {
    fmt.Println("test")
}'''
            },
            'rust': {
                'extension': '.rs',
                'compiler': ['rustc'],
                'executor': ['./test'],
                'timeout': 30,
                'test_code': '''fn main() {
    println!("test");
}'''
            },
            'haskell': {
                'extension': '.hs',
                'compiler': ['ghc', '-o', 'test'],
                'executor': ['./test'],
                'timeout': 30,
                'test_code': '''main = putStrLn "test"'''
            },
            'ocaml': {
                'extension': '.ml',
                'compiler': ['ocamlc', '-o', 'test'],
                'executor': ['./test'],
                'timeout': 25,
                'test_code': '''print_endline "test";;'''
            },
            'typescript': {
                'extension': '.ts',
                'compiler': ['tsc', '--outFile', 'test.js'],
                'executor': ['node', 'test.js'],
                'timeout': 25,
                'test_code': '''console.log("test");'''
            }
        }
        
        # Rileva linguaggi disponibili
        self.detect_available_languages()
    
    def check_command_available(self, command):
        """Verifica se un comando è disponibile"""
        try:
            result = subprocess.run(
                ['which', command] if os.name != 'nt' else ['where', command],
                capture_output=True,
                timeout=5
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
                    compile_cmd = config['compiler'] + [temp_file]
                    compile_result = subprocess.run(
                        compile_cmd,
                        capture_output=True,
                        timeout=30,
                        cwd=temp_dir
                    )
                    if compile_result.returncode != 0:
                        return False
                
                # Esecuzione
                if 'compiler' in config:
                    if language == 'java':
                        run_cmd = config['executor'] + ['Test']
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
                    cwd=temp_dir
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
    
    def detect_available_languages(self):
        """Rileva automaticamente i linguaggi disponibili"""
        print("🔍 Rilevamento linguaggi disponibili...")
        
        for language, config in self.language_config.items():
            print(f"  📋 Testando {language}...", end=' ')
            
            # Per linguaggi compilati, verifica il compilatore
            if 'compiler' in config:
                compiler_cmd = config['compiler'][0]
                if not self.check_command_available(compiler_cmd):
                    print("❌ (comando non trovato)")
                    continue
            else:
                # Per linguaggi interpretati, verifica l'executor
                main_cmd = config['executor'][0]
                if not self.check_command_available(main_cmd):
                    print("❌ (comando non trovato)")
                    continue
            
            # Test di esecuzione rapido
            if self.test_language_execution(language, config):
                self.available_languages[language] = config
                print("✅")
            else:
                print("❌ (test esecuzione fallito)")
        
        print(f"\n🎯 Linguaggi disponibili: {len(self.available_languages)}")
        for lang in sorted(self.available_languages.keys()):
            print(f"  • {lang.upper()}")
    
    def execute_code(self, code, language, task_name):
        """Esegue il codice se il linguaggio è disponibile"""
        
        # Avvia tracking CO2 per questa esecuzione (solo se non disabilitato)
        should_track = CARBON_TRACKING_AVAILABLE and not getattr(self, 'disable_carbon_tracking', False)
        if should_track:
            start_carbon_tracking(task_name, language)
        
        if language not in self.available_languages:
            if should_track:
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
                if should_track:
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
            if should_track:
                stop_carbon_tracking()
                
            return result
            
        except Exception as e:
            # Ferma tracking CO2 anche in caso di errore
            if should_track:
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
    
    def create_temp_file(self, code, extension, task_name, language):
        """Crea un file temporaneo per il codice"""
        try:
            # Sanitizza il nome del task
            safe_task = re.sub(r'[^\w\s-]', '', task_name).strip()
            safe_task = re.sub(r'[-\s/]+', '_', safe_task)
            
            if language == 'java':
                # Per Java, cerca il nome della classe nel codice (sia public che non public)
                class_match = re.search(r'(?:public\s+)?class\s+(\w+)', code)
                if class_match:
                    class_name = class_match.group(1)
                    filename = f"{class_name}.java"
                    print(f"    🏗️ Creando file Java: {filename}")
                else:
                    filename = f"Main.java"
                    print(f"    🏗️ Nessuna classe trovata, usando: {filename}")
            else:
                filename = f"temp_{safe_task}_{language}{extension}"
            
            # Crea directory se necessaria
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
            temp_dir = os.path.dirname(filepath)
            base_name = Path(filepath).stem
            
            if language == 'java':
                cmd = config['compiler'] + [filepath]
            elif language == 'csharp':
                cmd = config['compiler'] + [filepath]
            elif language == 'go':
                # Go: rinomina il file in main.go e compila
                main_go = os.path.join(temp_dir, 'main.go')
                os.rename(filepath, main_go)
                cmd = config['compiler'] + ['-o', 'test', 'main.go']
            elif language == 'rust':
                # Rust: compila con nome specifico
                cmd = config['compiler'] + [filepath, '-o', 'test']
            else:
                # Altri linguaggi compilati (C, C++, Haskell, OCaml)
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath] + ['-o', executable_path]
            
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.get('timeout', 45),
                cwd=temp_dir
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
                        print(f"    🔍 Classe Java trovata: {class_name}")
                    else:
                        # Fallback: cerca per nome file
                        base_name = Path(filepath).stem
                        cmd = config['executor'] + [base_name]
                        print(f"    🔍 Usando nome file: {base_name}")
                elif language == 'csharp':
                    cmd = config['executor']
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
                cwd=temp_dir
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
                print(f"⚠️ Errore cleanup {filepath}: {e}")
    
    def find_task_file(self, task_name, language):
        """Trova il file per una task e linguaggio nella struttura gerarchica"""
        code_base_dir = "data/generated/code_snippets"
        
        # Mappa linguaggi alle loro categorie principali
        language_categories = {
            'python': 'scripting',
            'javascript': 'scripting', 
            'ruby': 'scripting',
            'typescript': 'scripting',
            'java': 'oop',
            'csharp': 'oop',
            'c++': 'oop',
            'c': 'imperative',
            'go': 'imperative',
            'rust': 'imperative',
            'php': 'imperative',
            'haskell': 'functional',
            'ocaml': 'functional',
            'r': 'scientific',
            'julia': 'scientific',
            'matlab': 'scientific'
        }
        
        # Normalizza i nomi dei linguaggi
        lang_normalized = language.lower()
        if lang_normalized == 'cpp':
            lang_normalized = 'c++'
        
        # Trova la categoria del linguaggio
        category = language_categories.get(lang_normalized)
        if not category:
            return None
        
        # Cerca il file nella directory del linguaggio
        language_dir = os.path.join(code_base_dir, category, lang_normalized)
        if not os.path.exists(language_dir):
            return None
        
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
        cleaned = code.replace('\u00a0', ' ')  # Non-breaking space
        cleaned = cleaned.replace('\u2007', ' ')  # Figure space  
        cleaned = cleaned.replace('\u202f', ' ')  # Narrow no-break space
        cleaned = cleaned.replace('\u2060', '')   # Word joiner (invisibile)
        cleaned = cleaned.replace('\ufeff', '')   # Byte order mark
        
        # Rimuove caratteri di controllo invisibili (tranne newline, tab, carriage return)
        import re
        cleaned = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]', '', cleaned)
        
        return cleaned

    def execute_task_all_available_languages(self, task_name):
        """Esegue una task in tutti i linguaggi disponibili"""
        print(f"\n🎯 Esecuzione task: {task_name}")
        
        task_results = {}
        
        # Cerca i file nella struttura reale: data/generated/code_snippets/category/language/
        code_base_dir = "data/generated/code_snippets"
        
        if not os.path.exists(code_base_dir):
            print(f"❌ Directory base non trovata: {code_base_dir}")
            return task_results
        
        # Cerca i file di codice per i linguaggi disponibili in tutte le categorie
        for language in self.available_languages.keys():
            language_file = self.find_task_file(task_name, language)
            
            if language_file and os.path.exists(language_file):
                print(f"  🔧 Esecuzione {language}...")
                
                try:
                    with open(language_file, 'r', encoding='utf-8') as f:
                        code = f.read()
                    
                    # Pulisce il codice da caratteri invisibili
                    code = self.clean_code_content(code)
                    
                    result = self.execute_code(code, language, task_name)
                    task_results[language] = result
                    
                    if result['success']:
                        print(f"    ✅ {language}: successo ({result['execution_time']:.2f}s)")
                        if result['output'].strip():
                            output_preview = result['output'].strip()[:100].replace('\n', ' ')
                            print(f"    📤 Output: {output_preview}...")
                    else:
                        print(f"    ❌ {language}: {result['error'][:100]}...")
                        
                except Exception as e:
                    task_results[language] = {
                        'success': False,
                        'error': f'Errore lettura file: {str(e)}',
                        'output': '',
                        'execution_time': 0
                    }
                    print(f"    ❌ {language}: errore lettura file")
            else:
                print(f"  ⚠️ {language}: file non trovato")
        
        return task_results
    
    def execute_all_common_tasks(self):
        """Esegue tutte le task comuni"""
        print("🚀 SMART EXECUTOR - Esecuzione Adattiva")
        print(f"📊 Linguaggi disponibili: {len(self.available_languages)}")
        
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
        
        print(f"🎯 Trovate {len(tasks)} task comuni")
        
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
            print(f"💾 Risultati salvati in: {results_file}")
        except Exception as e:
            print(f"⚠️ Errore salvataggio risultati: {e}")
    
    def print_execution_summary(self, results):
        """Stampa un riassunto dell'esecuzione"""
        print(f"\n📊 RIASSUNTO ESECUZIONE SMART:")
        
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
                    output_preview = result['output'].strip()[:50] if result['output'].strip() else "no output"
                    print(f"  ✅ {language}: {result['execution_time']:.2f}s - {output_preview}")
                else:
                    print(f"  ❌ {language}: {result['error'][:80]}...")
            
            if task_total > 0:
                success_rate = (task_success / task_total * 100)
                print(f"  📈 Successo: {task_success}/{task_total} ({success_rate:.1f}%)")
        
        if total_executions > 0:
            overall_success_rate = (successful_executions / total_executions * 100)
            print(f"\n🎉 TOTALE: {successful_executions}/{total_executions} ({overall_success_rate:.1f}%) esecuzioni riuscite")
            print(f"🔧 Linguaggi utilizzati: {sorted(self.available_languages.keys())}")
        else:
            print(f"\n⚠️ Nessuna esecuzione completata")


if __name__ == "__main__":
    executor = SmartExecutor()
    executor.execute_all_common_tasks()
