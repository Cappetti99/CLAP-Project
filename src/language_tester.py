#!/usr/bin/env python3
"""
Language Environment Tester - Testa la disponibilità di tutti i linguaggi di programmazione
Verifica compilatori, interpreti e capacità di esecuzione per 16 linguaggi supportati.
"""

import sys
import os
import subprocess
import tempfile
import json
import shutil
from pathlib import Path
from datetime import datetime

class LanguageTester:
 """
 Testa la disponibilità e funzionalità di tutti i linguaggi supportati dal sistema SWAM.

 Supporta 16 linguaggi:
 - Interpretati: Python, JavaScript, Ruby, PHP, R, Julia, MATLAB
 - Compilati: C, C++, Java, Go, Rust, Haskell, OCaml, TypeScript
 - VM-based: C# (Mono)
 """

 def __init__(self):
 self.test_results = {}
 self.temp_dirs = []

 # Configurazione test per ogni linguaggio
 self.test_programs = {
 # === LINGUAGGI INTERPRETATI ===
 'python': {
 'extension': '.py',
 'code': 'print("Hello from Python!")',
 'run_cmd': ['python'],
 'compile_cmd': None,
 'type': 'interpreted'
 },
 'javascript': {
 'extension': '.js',
 'code': 'console.log("Hello from JavaScript!");',
 'run_cmd': ['node'],
 'compile_cmd': None,
 'type': 'interpreted'
 },
 'ruby': {
 'extension': '.rb',
 'code': 'puts "Hello from Ruby!"',
 'run_cmd': ['ruby'],
 'compile_cmd': None,
 'type': 'interpreted'
 },
 'php': {
 'extension': '.php',
 'code': '<?php echo "Hello from PHP!\\n"; ?>',
 'run_cmd': ['php'],
 'compile_cmd': None,
 'type': 'interpreted'
 },
 'r': {
 'extension': '.r',
 'code': 'cat("Hello from R!\\n")',
 'run_cmd': ['Rscript'],
 'compile_cmd': None,
 'type': 'interpreted'
 },
 'julia': {
 'extension': '.jl',
 'code': 'println("Hello from Julia!")',
 'run_cmd': ['julia'],
 'compile_cmd': None,
 'type': 'interpreted'
 },
 'matlab': {
 'extension': '.m',
 'code': 'fprintf("Hello from MATLAB!\\n");',
 'run_cmd': ['matlab', '-batch'],
 'compile_cmd': None,
 'type': 'interpreted'
 },

 # === LINGUAGGI COMPILATI ===
 'c': {
 'extension': '.c',
 'code': '''#include <stdio.h>
int main() {
 printf("Hello from C!\\n");
 return 0;
}''',
 'run_cmd': ['./hello_c'],
 'compile_cmd': ['gcc', '-o', 'hello_c'],
 'type': 'compiled'
 },
 'cpp': {
 'extension': '.cpp',
 'code': '''#include <iostream>
int main() {
 std::cout << "Hello from C++!" << std::endl;
 return 0;
}''',
 'run_cmd': ['./hello_cpp'],
 'compile_cmd': ['g++', '-o', 'hello_cpp'],
 'type': 'compiled'
 },
 'java': {
 'extension': '.java',
 'code': '''public class HelloJava {
 public static void main(String[] args) {
 System.out.println("Hello from Java!");
 }
}''',
 'run_cmd': ['java', 'HelloJava'],
 'compile_cmd': ['javac'],
 'type': 'compiled'
 },
 'go': {
 'extension': '.go',
 'code': '''package main
import "fmt"
func main() {
 fmt.Println("Hello from Go!")
}''',
 'run_cmd': ['./hello_go'],
 'compile_cmd': ['go', 'build', '-o', 'hello_go'],
 'type': 'compiled'
 },
 'rust': {
 'extension': '.rs',
 'code': '''fn main() {
 println!("Hello from Rust!");
}''',
 'run_cmd': ['./hello_rust'],
 'compile_cmd': ['rustc', '-o', 'hello_rust'],
 'type': 'compiled'
 },
 'haskell': {
 'extension': '.hs',
 'code': '''main :: IO ()
main = putStrLn "Hello from Haskell!"''',
 'run_cmd': ['./hello_haskell'],
 'compile_cmd': ['ghc', '-o', 'hello_haskell'],
 'type': 'compiled'
 },
 'ocaml': {
 'extension': '.ml',
 'code': 'print_endline "Hello from OCaml!";;',
 'run_cmd': ['./hello_ocaml'],
 'compile_cmd': ['ocamlc', '-o', 'hello_ocaml'],
 'type': 'compiled'
 },
 'typescript': {
 'extension': '.ts',
 'code': 'console.log("Hello from TypeScript!");',
 'run_cmd': ['node', 'hello_typescript.js'],
 'compile_cmd': ['tsc'],
 'type': 'compiled'
 },

 # === LINGUAGGI VM-BASED ===
 'csharp': {
 'extension': '.cs',
 'code': '''using System;
class Program {
 static void Main() {
 Console.WriteLine("Hello from C#!");
 }
}''',
 'run_cmd': ['mono', 'hello.exe'],
 'compile_cmd': ['mcs', '-out:hello.exe'],
 'type': 'vm-based'
 }
 }

 def check_command_exists(self, command):
 """Verifica se un comando è disponibile nel sistema"""
 try:
 result = subprocess.run(
 ['which', command] if os.name != 'nt' else ['where', command],
 capture_output=True,
 text=True,
 timeout=5
 )
 return result.returncode == 0
 except Exception:
 return False

 def create_test_file(self, language, config):
 """Crea un file di test per il linguaggio specificato"""
 try:
 # Crea directory temporanea unica
 temp_dir = tempfile.mkdtemp(prefix=f"swam_test_{language}_")

 # Determina il nome del file
 if language == 'java':
 filename = f"HelloJava{config['extension']}"
 else:
 filename = f"hello_{language}{config['extension']}"

 # Crea il file con il codice di test
 filepath = os.path.join(temp_dir, filename)
 with open(filepath, 'w', encoding='utf-8') as f:
 f.write(config['code'])

 # Registra la directory per cleanup
 self.temp_dirs.append(temp_dir)
 return filepath, temp_dir

 except Exception as e:
 print(f" Errore creazione file: {e}")
 return None, None


 def test_language(self, language):
 """
 Testa un singolo linguaggio di programmazione.

 Processo:
 1. Verifica disponibilità comandi (interpreter/compiler)
 2. Crea file di test temporaneo
 3. Compila il codice (se necessario)
 4. Esegue il programma
 5. Analizza i risultati

 Returns:
 dict: Risultato dettagliato del test
 """
 config = self.test_programs[language]

 # Inizializza risultato
 result = {
 'language': language,
 'type': config['type'],
 'available': False,
 'interpreter_found': False,
 'compiler_found': False,
 'compile_success': False,
 'run_success': False,
 'output': '',
 'error': '',
 'execution_time': 0
 }

 print(f" Testing {language.upper()} ({config['type']})...")

 # === FASE 1: Verifica comandi ===
 if self._check_commands(config, result):
 return result

 # === FASE 2: Crea file di test ===
 filepath, temp_dir = self.create_test_file(language, config)
 if not filepath:
 result['error'] = "Impossibile creare file di test"
 return result

 try:
 # === FASE 3: Compilazione (se necessaria) ===
 if not self._compile_if_needed(config, filepath, temp_dir, result):
 return result

 # === FASE 4: Esecuzione ===
 self._execute_program(language, config, filepath, temp_dir, result)

 except subprocess.TimeoutExpired:
 result['error'] = "Timeout durante esecuzione"
 print(f" ⏰ Timeout")
 except Exception as e:
 result['error'] = f"Errore imprevisto: {str(e)}"
 print(f" Errore: {e}")

 return result

 def _check_commands(self, config, result):
 """Verifica la disponibilità dei comandi necessari"""
 run_command = config['run_cmd'][0]

 # Per linguaggi compilati con eseguibili locali, saltiamo il check iniziale
 if config['type'] == 'compiled' and run_command.startswith('./'):
 result['interpreter_found'] = True
 else:
 result['interpreter_found'] = self.check_command_exists(run_command)
 if not result['interpreter_found']:
 result['error'] = f"Comando '{run_command}' non trovato"
 print(f" {run_command} non disponibile")
 return True

 # Verifica compilatore se necessario
 if config['compile_cmd']:
 compile_command = config['compile_cmd'][0]
 result['compiler_found'] = self.check_command_exists(compile_command)
 if not result['compiler_found']:
 result['error'] = f"Compilatore '{compile_command}' non trovato"
 print(f" {compile_command} non disponibile")
 return True
 else:
 result['compiler_found'] = True

 return False

 def _compile_if_needed(self, config, filepath, temp_dir, result):
 """Compila il codice se necessario"""
 if not config['compile_cmd']:
 result['compile_success'] = True
 return True

 print(f" 🔨 Compilazione...")
 compile_cmd = config['compile_cmd'] + [filepath]

 compile_result = subprocess.run(
 compile_cmd,
 capture_output=True,
 text=True,
 timeout=30,
 cwd=temp_dir
 )

 result['compile_success'] = compile_result.returncode == 0

 if result['compile_success']:
 print(f" Compilazione riuscita")
 return True
 else:
 result['error'] = f"Errore compilazione: {compile_result.stderr[:200]}"
 print(f" Compilazione fallita")
 print(f" {compile_result.stderr[:100]}...")
 return False

 def _execute_program(self, language, config, filepath, temp_dir, result):
 """Esegue il programma compilato o interpretato"""
 print(f" Esecuzione...")

 # Determina comando di esecuzione
 if config['type'] == 'compiled':
 run_cmd = config['run_cmd']
 else:
 # Linguaggio interpretato
 if language == 'matlab':
 # MATLAB richiede solo il nome dello script
 script_name = os.path.splitext(os.path.basename(filepath))[0]
 run_cmd = config['run_cmd'] + [script_name]
 else:
 run_cmd = config['run_cmd'] + [filepath]

 # Esegui comando
 import time
 start_time = time.time()

 run_result = subprocess.run(
 run_cmd,
 capture_output=True,
 text=True,
 timeout=10,
 cwd=temp_dir
 )

 result['execution_time'] = round(time.time() - start_time, 3)
 result['run_success'] = run_result.returncode == 0
 result['output'] = run_result.stdout.strip()

 if result['run_success']:
 result['available'] = True
 print(f" Esecuzione riuscita ({result['execution_time']}s): {result['output']}")
 else:
 result['error'] = f"Errore esecuzione: {run_result.stderr[:200]}"
 print(f" Esecuzione fallita")
 print(f" {run_result.stderr[:100]}...")


 def test_all_languages(self):
 """Esegue il test completo di tutti i linguaggi supportati"""
 print(" LANGUAGE ENVIRONMENT TESTER")
 print("=" * 50)
 print("Verifico la disponibilità di tutti i linguaggi...\n")

 # Raggruppa linguaggi per tipo
 by_type = {}
 for lang, config in self.test_programs.items():
 lang_type = config['type']
 if lang_type not in by_type:
 by_type[lang_type] = []
 by_type[lang_type].append(lang)

 # Testa ogni gruppo
 for lang_type in ['interpreted', 'compiled', 'vm-based']:
 if lang_type in by_type:
 print(f"📂 Testing {lang_type.upper()} languages:")
 for language in by_type[lang_type]:
 result = self.test_language(language)
 self.test_results[language] = result
 print() # Spazio tra gruppi

 self.print_summary()
 self.save_results()
 self.cleanup()

 def print_summary(self):
 """Stampa un riassunto organizzato dei risultati"""
 print(" RIASSUNTO RISULTATI")
 print("=" * 50)

 # Categorizza risultati
 available = []
 unavailable = []
 by_type = {'interpreted': [], 'compiled': [], 'vm-based': []}

 for language, result in self.test_results.items():
 lang_type = self.test_programs[language]['type']

 if result['available']:
 available.append(language)
 by_type[lang_type].append((language, ''))
 else:
 unavailable.append(language)
 by_type[lang_type].append((language, ''))

 # Mostra risultati per tipo
 for lang_type, languages in by_type.items():
 if languages:
 print(f"\n🔹 {lang_type.upper()} LANGUAGES:")
 for lang, status in languages:
 if status == '':
 output = self.test_results[lang]['output'][:50]
 exec_time = self.test_results[lang]['execution_time']
 print(f" {status} {lang.upper()}: {output} ({exec_time}s)")
 else:
 error = self.test_results[lang]['error'][:60]
 print(f" {status} {lang.upper()}: {error}...")

 # Statistiche generali
 total = len(self.test_programs)
 success_rate = len(available) / total * 100

 print(f"\n STATISTICHE GENERALI:")
 print(f" Totale linguaggi testati: {total}")
 print(f" Linguaggi disponibili: {len(available)}")
 print(f" Linguaggi non disponibili: {len(unavailable)}")
 print(f" Tasso di successo: {success_rate:.1f}%")

 # Suggerimenti installazione
 if unavailable:
 print(f"\n SUGGERIMENTI INSTALLAZIONE:")
 missing_commands = set()
 for lang in unavailable:
 result = self.test_results[lang]
 config = self.test_programs[lang]

 if not result['interpreter_found'] and not config['run_cmd'][0].startswith('./'):
 missing_commands.add(config['run_cmd'][0])
 if config['compile_cmd'] and not result['compiler_found']:
 missing_commands.add(config['compile_cmd'][0])

 for cmd in sorted(missing_commands):
 if cmd == 'matlab':
 print(f" {cmd}: Licenza commerciale richiesta")
 else:
 print(f" {cmd}: brew install {cmd}")

 def save_results(self):
 """Salva i risultati in formato JSON con timestamp"""
 timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
 results_file = f"results/execution/language_test_results_{timestamp}.json"

 os.makedirs("results/execution", exist_ok=True)

 # Prepara dati per salvare
 save_data = {
 'timestamp': timestamp,
 'total_languages': len(self.test_programs),
 'available_count': sum(1 for r in self.test_results.values() if r['available']),
 'results': self.test_results
 }

 try:
 with open(results_file, 'w', encoding='utf-8') as f:
 json.dump(save_data, f, indent=2)
 print(f"\n💾 Risultati salvati in: {results_file}")
 except Exception as e:
 print(f"\n Errore salvataggio: {e}")

 def cleanup(self):
 """Pulisce tutti i file temporanei creati durante i test"""
 cleaned = 0
 for temp_dir in self.temp_dirs:
 try:
 shutil.rmtree(temp_dir)
 cleaned += 1
 except Exception as e:
 print(f" Errore cleanup {temp_dir}: {e}")

 if cleaned > 0:
 print(f" Puliti {cleaned} directory temporanee")

if __name__ == "__main__":
 tester = LanguageTester()
 tester.test_all_languages()
