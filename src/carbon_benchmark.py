#!/usr/bin/env python3
"""
Carbon Benchmark - Sistema di benchmarking per misurare emissioni CO2 medie
Esegue ogni codice 30 volte per calcolare statistiche accurate delle emissioni
"""

import os
import sys
import json
import time
import glob
import logging
from pathlib import Path
import re
import statistics
from datetime import datetime
from pathlib import Path
from tqdm import tqdm

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

try:
    from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
    from src.smart_executor import SmartExecutor
    CARBON_TRACKING_AVAILABLE = True
except ImportError:
    try:
        # Fallback per import diretto
        from carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
        from smart_executor import SmartExecutor
        CARBON_TRACKING_AVAILABLE = True
    except ImportError:
        CARBON_TRACKING_AVAILABLE = False


class CarbonBenchmark:
    """Sistema di benchmarking per misurare emissioni CO2 con ripetizioni multiple"""

    def __init__(self, iterations=30):
        self.iterations = iterations
        self.results_dir = "results/carbon_benchmark"
        self.code_base_path = "data/generated/code_snippets"
        self.executor = SmartExecutor()
        
        # Crea un logger semplice
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.DEBUG)
        if not self.logger.handlers:
            handler = logging.StreamHandler()
            handler.setLevel(logging.DEBUG)
            formatter = logging.Formatter('%(levelname)s - %(message)s')
            handler.setFormatter(formatter)
            self.logger.addHandler(handler)
            
        self.benchmark_results = {}
        
        # Statistiche sui fallimenti per linguaggio
        self.language_failures = {}

        # Crea directory per i risultati
        os.makedirs(self.results_dir, exist_ok=True)

        print(f" CARBON BENCHMARK SYSTEM")
        print(f" Configurato per {self.iterations} iterazioni per codice")
        print(f" Risultati salvati in: {self.results_dir}")

        if not CARBON_TRACKING_AVAILABLE:
            print(" Carbon tracking non disponibile")
            return
    
    def format_co2_value(self, kg_value, unit='mg'):
        """Converte e formatta i valori CO2 da kg a unità più leggibili."""
        if unit == 'mg':
            # Converte da kg a mg (1 kg = 1,000,000 mg)
            mg_value = kg_value * 1_000_000
            if mg_value < 0.001:
                return f"{mg_value:.6f} mg CO2eq"
            elif mg_value < 1:
                return f"{mg_value:.3f} mg CO2eq"
            else:
                return f"{mg_value:.2f} mg CO2eq"
        elif unit == 'g':
            # Converte da kg a g (1 kg = 1,000 g)
            g_value = kg_value * 1_000
            return f"{g_value:.6f} g CO2eq"
        else:
            # Mantiene in kg
            return f"{kg_value:.8f} kg CO2eq"

    def benchmark_single_execution(self, code, language, task_name, iteration):
        """Esegue un singolo benchmark di un codice"""
        session_id = f"{task_name}_{language}_iter{iteration:02d}_{datetime.now().strftime('%H%M%S')}"

        try:
            # Avvia tracking
            if CARBON_TRACKING_AVAILABLE:
                start_carbon_tracking(f"{task_name}_benchmark", f"{language}_iter{iteration}")

            start_time = time.time()

            # Disabilita temporaneamente il tracking nel smart_executor per evitare doppio tracking
            original_tracking = getattr(self.executor, 'disable_carbon_tracking', False)
            self.executor.disable_carbon_tracking = True

            # Esegui il codice
            result = self.executor.execute_code(code, language, f"{task_name}_benchmark_iter{iteration}")

            # Ripristina il tracking del smart_executor
            self.executor.disable_carbon_tracking = original_tracking

            execution_time = time.time() - start_time

            # Ferma tracking e ottieni emissioni
            emissions = 0.0
            if CARBON_TRACKING_AVAILABLE:
                emissions = stop_carbon_tracking()

            return {
                'iteration': iteration,
                'success': result['success'],
                'execution_time': execution_time,
                'emissions': emissions if emissions else 0.0,
                'session_id': session_id,
                'error': result.get('error', '') if not result['success'] else '',
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            if CARBON_TRACKING_AVAILABLE:
                stop_carbon_tracking()

            return {
                'iteration': iteration,
                'success': False,
                'execution_time': 0.0,
                'emissions': 0.0,
                'session_id': session_id,
                'error': str(e),
                'timestamp': datetime.now().isoformat()
            }

    def determine_category(self, task_name, language):
        """Determina la categoria di un task cercando in tutte le sottodirectory."""
        categories = ['functional', 'imperative', 'oop', 'scientific', 'scripting']
        
        for category in categories:
            category_path = os.path.join(self.code_base_path, category, language)
            if os.path.exists(category_path):
                # Cerca file in questa categoria
                files = glob.glob(os.path.join(category_path, "snippet_*.*"))
                for file_path in files:
                    filename = os.path.basename(file_path)
                    name_part = filename.replace("snippet_", "").rsplit(".", 1)[0]
                    
                    # Rimuovi il numero iniziale se presente
                    if "_" in name_part:
                        parts = name_part.split("_", 1)
                        if parts[0].isdigit():
                            task_in_filename = parts[1]
                        else:
                            task_in_filename = name_part
                    else:
                        task_in_filename = name_part
                    
                    # Confronta con il nome del task
                    normalized_task_name = task_name.replace(" ", "_")
                    
                    if (task_in_filename.lower() == normalized_task_name.lower() or 
                        normalized_task_name.lower() in task_in_filename.lower() or
                        task_in_filename.lower() in normalized_task_name.lower()):
                        return category
        
        # Default alla prima categoria se non trovato
        return 'scripting'
    
    def load_task_code(self, task_name, language, category_subdir):
        """Carica il codice per un task specifico dal dataset SWAM."""
        try:
            # Cerca in tutte le categorie del dataset SWAM
            categories = ['algorithms', 'basic', 'io', 'mathematics', 'misc', 'strings']
            
            # Mappa i nomi dei linguaggi al formato usato nel dataset
            language_mapping = {
                'python': 'python',
                'javascript': 'javascript', 
                'java': 'java',
                'cpp': 'cplusplus',
                'c': 'c',
                'ruby': 'ruby',
                'php': 'php',
                'r': 'r',
                'julia': 'julia',
                'csharp': 'c#',
                'go': 'go',
                'rust': 'rust',
                'haskell': 'haskell',
                'ocaml': 'ocaml',
                'typescript': 'typescript'
            }
            
            dataset_language = language_mapping.get(language, language)
            normalized_task_name = task_name.replace(" ", "_")
            
            # Cerca in tutte le categorie
            for category in categories:
                base_path = os.path.join(self.code_base_path, category, dataset_language)
                
                if not os.path.exists(base_path):
                    continue
                    
                # Pattern per trovare file che contengono il nome del task
                pattern = f"snippet_*_{normalized_task_name}.*"
                files = glob.glob(os.path.join(base_path, pattern))
                
                for file_path in files:
                    try:
                        with open(file_path, 'r', encoding='utf-8') as f:
                            content = f.read()
                            # Applica pulizia caratteri invisibili
                            content = self.executor.clean_code_content(content)
                            self.logger.debug(f"Trovato codice per '{task_name}' in {file_path}")
                            return content
                    except Exception as e:
                        self.logger.warning(f"Errore lettura file {file_path}: {e}")
                        continue
            
            # Se non trovato con il pattern esatto, prova pattern più flessibile
            for category in categories:
                base_path = os.path.join(self.code_base_path, category, dataset_language)
                
                if not os.path.exists(base_path):
                    continue
                    
                files = glob.glob(os.path.join(base_path, "snippet_*.*"))
                
                for file_path in files:
                    filename = os.path.basename(file_path)
                    # Controlla se il nome del task è contenuto nel nome del file
                    if normalized_task_name.lower() in filename.lower():
                        try:
                            with open(file_path, 'r', encoding='utf-8') as f:
                                content = f.read()
                                # Applica pulizia caratteri invisibili
                                content = self.executor.clean_code_content(content)
                                self.logger.debug(f"Trovato codice per '{task_name}' in {file_path} (match flessibile)")
                                return content
                        except Exception as e:
                            self.logger.warning(f"Errore lettura file {file_path}: {e}")
                            continue
            
            self.logger.debug(f"Nessun file trovato per task '{task_name}' in {language}")
            return None
                
        except Exception as e:
            self.logger.error(f"Errore nel caricamento del codice per {task_name}: {e}")
            return None

    def generate_missing_code(self, task_name, target_language, source_language=None):
        """
        Genera automaticamente il codice per un task mancante in un linguaggio target.
        Cerca prima un'implementazione di riferimento in un altro linguaggio.
        """
        try:
            # Se non specificato, cerca un linguaggio sorgente disponibile
            if not source_language:
                reference_languages = ['python', 'javascript', 'java', 'cpp', 'c']
                for ref_lang in reference_languages:
                    category = self.determine_category(task_name, ref_lang)
                    if category:
                        source_code = self.load_task_code(task_name, ref_lang, category)
                        if source_code:
                            source_language = ref_lang
                            break
                
                if not source_language:
                    self.logger.warning(f"Nessun codice di riferimento trovato per '{task_name}'")
                    return None

            # Carica il codice sorgente
            source_category = self.determine_category(task_name, source_language)
            source_code = self.load_task_code(task_name, source_language, source_category)
            
            if not source_code:
                self.logger.warning(f"Codice sorgente non trovato per '{task_name}' in {source_language}")
                return None

            # Genera il codice convertito usando dei template base
            converted_code = self.convert_code_to_language(source_code, source_language, target_language, task_name)
            
            if converted_code:
                # Salva il codice generato
                self.save_generated_code(task_name, target_language, converted_code)
                self.logger.info(f"Codice generato per '{task_name}' in {target_language}")
                return converted_code
            
            return None
            
        except Exception as e:
            self.logger.error(f"Errore nella generazione del codice per {task_name} in {target_language}: {e}")
            return None

    def convert_code_to_language(self, source_code, source_lang, target_lang, task_name):
        """
        Converte il codice da un linguaggio a un altro usando template e pattern.
        Questa è una versione semplificata - in futuro si potrebbe integrare con AI.
        """
        try:
            # Template base per diversi linguaggi
            if target_lang == 'python':
                return self.convert_to_python(source_code, source_lang, task_name)
            elif target_lang == 'javascript':
                return self.convert_to_javascript(source_code, source_lang, task_name)
            elif target_lang == 'java':
                return self.convert_to_java(source_code, source_lang, task_name)
            elif target_lang == 'cpp':
                return self.convert_to_cpp(source_code, source_lang, task_name)
            elif target_lang == 'c':
                return self.convert_to_c(source_code, source_lang, task_name)
            elif target_lang == 'php':
                return self.convert_to_php(source_code, source_lang, task_name)
            elif target_lang == 'ruby':
                return self.convert_to_ruby(source_code, source_lang, task_name)
            elif target_lang == 'r':
                return self.convert_to_r(source_code, source_lang, task_name)
            elif target_lang == 'julia':
                return self.convert_to_julia(source_code, source_lang, task_name)
            elif target_lang == 'haskell':
                return self.convert_to_haskell(source_code, source_lang, task_name)
            elif target_lang == 'ocaml':
                return self.convert_to_ocaml(source_code, source_lang, task_name)
            elif target_lang == 'go':
                return self.convert_to_go(source_code, source_lang, task_name)
            elif target_lang == 'rust':
                return self.convert_to_rust(source_code, source_lang, task_name)
            elif target_lang == 'typescript':
                return self.convert_to_typescript(source_code, source_lang, task_name)
            elif target_lang == 'csharp':
                return self.convert_to_csharp(source_code, source_lang, task_name)
            else:
                # Fallback: crea un template generico
                return self.create_generic_template(target_lang, task_name)
                
        except Exception as e:
            self.logger.error(f"Errore nella conversione da {source_lang} a {target_lang}: {e}")
            return None

    def convert_to_python(self, source_code, source_lang, task_name):
        """Converte codice in Python"""
        # Template Python base
        template = f'''#!/usr/bin/env python3
"""
{task_name} implementation
Auto-generated from {source_lang}
"""

def {task_name.lower().replace(' ', '_')}():
    """
    Implementation of {task_name}
    """
    # TODO: Implement the logic
    print("Executing {task_name}")
    return True

if __name__ == "__main__":
    result = {task_name.lower().replace(' ', '_')}()
    print(f"Result: {{result}}")
'''
        return template

    def convert_to_javascript(self, source_code, source_lang, task_name):
        """Converte codice in JavaScript"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */

function {func_name}() {{
    // TODO: Implement the logic
    console.log("Executing {task_name}");
    return true;
}}

// Execute the function
const result = {func_name}();
console.log(`Result: ${{result}}`);
'''
        return template

    def convert_to_java(self, source_code, source_lang, task_name):
        """Converte codice in Java"""
        class_name = ''.join(word.capitalize() for word in task_name.replace('-', ' ').split())
        template = f'''/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */
public class {class_name} {{
    
    public static void main(String[] args) {{
        {class_name} instance = new {class_name}();
        boolean result = instance.execute();
        System.out.println("Result: " + result);
    }}
    
    public boolean execute() {{
        // TODO: Implement the logic
        System.out.println("Executing {task_name}");
        return true;
    }}
}}
'''
        return template

    def convert_to_cpp(self, source_code, source_lang, task_name):
        """Converte codice in C++"""
        template = f'''/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */
#include <iostream>

bool {task_name.lower().replace(' ', '_').replace('-', '_')}() {{
    // TODO: Implement the logic
    std::cout << "Executing {task_name}" << std::endl;
    return true;
}}

int main() {{
    bool result = {task_name.lower().replace(' ', '_').replace('-', '_')}();
    std::cout << "Result: " << result << std::endl;
    return 0;
}}
'''
        return template

    def convert_to_c(self, source_code, source_lang, task_name):
        """Converte codice in C"""
        template = f'''/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */
#include <stdio.h>
#include <stdbool.h>

bool {task_name.lower().replace(' ', '_').replace('-', '_')}() {{
    // TODO: Implement the logic
    printf("Executing {task_name}\\n");
    return true;
}}

int main() {{
    bool result = {task_name.lower().replace(' ', '_').replace('-', '_')}();
    printf("Result: %s\\n", result ? "true" : "false");
    return 0;
}}
'''
        return template

    def convert_to_php(self, source_code, source_lang, task_name):
        """Converte codice in PHP"""
        template = f'''<?php
/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */

function {task_name.lower().replace(' ', '_').replace('-', '_')}() {{
    // TODO: Implement the logic
    echo "Executing {task_name}\\n";
    return true;
}}

$result = {task_name.lower().replace(' ', '_').replace('-', '_')}();
echo "Result: " . ($result ? 'true' : 'false') . "\\n";
?>
'''
        return template

    def convert_to_ruby(self, source_code, source_lang, task_name):
        """Converte codice in Ruby"""
        template = f'''#!/usr/bin/env ruby
# {task_name} implementation
# Auto-generated from {source_lang}

def {task_name.lower().replace(' ', '_').replace('-', '_')}
  # TODO: Implement the logic
  puts "Executing {task_name}"
  true
end

result = {task_name.lower().replace(' ', '_').replace('-', '_')}
puts "Result: #{{result}}"
'''
        return template

    def convert_to_r(self, source_code, source_lang, task_name):
        """Converte codice in R"""
        template = f'''# {task_name} implementation
# Auto-generated from {source_lang}

{task_name.lower().replace(' ', '_').replace('-', '_')} <- function() {{
  # TODO: Implement the logic
  cat("Executing {task_name}\\n")
  return(TRUE)
}}

result <- {task_name.lower().replace(' ', '_').replace('-', '_')}()
cat("Result:", result, "\\n")
'''
        return template

    def convert_to_julia(self, source_code, source_lang, task_name):
        """Converte codice in Julia"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''# {task_name} implementation
# Auto-generated from {source_lang}

function {func_name}()
    # TODO: Implement the logic
    println("Executing {task_name}")
    return true
end

result = {func_name}()
println("Result: $result")
'''
        return template

    def convert_to_haskell(self, source_code, source_lang, task_name):
        """Converte codice in Haskell"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''-- {task_name} implementation
-- Auto-generated from {source_lang}

{func_name} :: IO Bool
{func_name} = do
    -- TODO: Implement the logic
    putStrLn "Executing {task_name}"
    return True

main :: IO ()
main = do
    result <- {func_name}
    putStrLn $ "Result: " ++ show result
'''
        return template

    def convert_to_ocaml(self, source_code, source_lang, task_name):
        """Converte codice in OCaml"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''(* {task_name} implementation *)
(* Auto-generated from {source_lang} *)

let {func_name} () =
  (* TODO: Implement the logic *)
  print_endline "Executing {task_name}";
  true

let () =
  let result = {func_name} () in
  Printf.printf "Result: %b\\n" result
'''
        return template

    def convert_to_go(self, source_code, source_lang, task_name):
        """Converte codice in Go"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''// {task_name} implementation
// Auto-generated from {source_lang}
package main

import "fmt"

func {func_name}() bool {{
    // TODO: Implement the logic
    fmt.Println("Executing {task_name}")
    return true
}}

func main() {{
    result := {func_name}()
    fmt.Printf("Result: %t\\n", result)
}}
'''
        return template

    def convert_to_rust(self, source_code, source_lang, task_name):
        """Converte codice in Rust"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''// {task_name} implementation
// Auto-generated from {source_lang}

fn {func_name}() -> bool {{
    // TODO: Implement the logic
    println!("Executing {task_name}");
    true
}}

fn main() {{
    let result = {func_name}();
    println!("Result: {{}}", result);
}}
'''
        return template

    def convert_to_typescript(self, source_code, source_lang, task_name):
        """Converte codice in TypeScript"""
        func_name = task_name.lower().replace(' ', '_').replace('-', '_')
        template = f'''/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */

function {func_name}(): boolean {{
    // TODO: Implement the logic
    console.log("Executing {task_name}");
    return true;
}}

const result: boolean = {func_name}();
console.log(`Result: ${{result}}`);
'''
        return template

    def convert_to_csharp(self, source_code, source_lang, task_name):
        """Converte codice in C#"""
        class_name = ''.join(word.capitalize() for word in task_name.replace('-', ' ').split())
        template = f'''/**
 * {task_name} implementation
 * Auto-generated from {source_lang}
 */
using System;

class {class_name}
{{
    static void Main()
    {{
        {class_name} instance = new {class_name}();
        bool result = instance.Execute();
        Console.WriteLine($"Result: {{result}}");
    }}

    public bool Execute()
    {{
        // TODO: Implement the logic
        Console.WriteLine("Executing {task_name}");
        return true;
    }}
}}
'''
        return template

    def create_generic_template(self, language, task_name):
        """Crea un template generico per linguaggi non supportati"""
        template = f'''# {task_name} implementation
# Auto-generated template for {language}
# TODO: Implement {task_name} in {language}

# This is a placeholder implementation
print("Executing {task_name} in {language}")
'''
        return template

    def save_generated_code(self, task_name, language, code):
        """Salva il codice generato nella struttura appropriata"""
        try:
            # Determina la categoria (usa 'generated' come fallback)
            category = self.determine_category(task_name, language) or 'generated'
            
            # Crea il percorso per il file generato
            lang_dir = os.path.join(self.code_base_path, category, language)
            os.makedirs(lang_dir, exist_ok=True)
            
            # Nome del file
            extension = self.get_file_extension(language)
            filename = f"generated_{task_name.replace(' ', '_')}.{extension}"
            file_path = os.path.join(lang_dir, filename)
            
            # Salva il file
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)
            
            self.logger.info(f"Codice salvato in: {file_path}")
            return file_path
            
        except Exception as e:
            self.logger.error(f"Errore nel salvataggio del codice generato: {e}")
            return None
            
    def get_file_extension(self, language):
        """Restituisce l'estensione del file per il linguaggio."""
        extensions = {
            'python': 'py',
            'java': 'java',
            'javascript': 'js',
            'cpp': 'cpp',
            'c': 'c',
            'csharp': 'cs',
            'go': 'go',
            'rust': 'rs',
            'php': 'php',
            'ruby': 'rb',
            'swift': 'swift',
            'kotlin': 'kt',
            'typescript': 'ts',
            'scala': 'scala',
            'perl': 'pl',
            'lua': 'lua',
            'r': 'R',
            'haskell': 'hs',
            'ocaml': 'ml',
            'julia': 'jl'
        }
        return extensions.get(language, language)

    def is_code_executable(self, code, language):
        """Verifica se il codice è potenzialmente eseguibile"""
        if not code or len(code.strip()) < 10:
            return False, "Codice troppo corto"

        # Pattern che indicano codice non eseguibile
        problematic_patterns = {
            'python': [
                r'>>>\s',  # Interactive prompt
                r'^\s*\.\.\.\s',  # Continuation prompt
                r'Example:|example:|EXAMPLE:',  # Documentation
                r'Output:|output:|OUTPUT:',  # Expected output
            ],
            'javascript': [
                r'>\s',  # Browser console
                r'Example:|example:|EXAMPLE:',
                r'Output:|output:|OUTPUT:',
            ],
            'all': [
                r'^\s*#.*Example',  # Comments with examples
                r'^\s*//.*Example',
                r'Sample\s+(output|run)',
                r'Expected\s+(output|result)',
                r'^\s*\*\s.*example',  # Commented examples
            ]
        }

        # Verifica pattern problematici
        patterns_to_check = problematic_patterns.get(language, []) + problematic_patterns.get('all', [])

        for pattern in patterns_to_check:
            if re.search(pattern, code, re.MULTILINE | re.IGNORECASE):
                return False, f"Codice non eseguibile: contiene pattern '{pattern[:20]}...'"

        # Verifica se è principalmente commenti
        lines = code.split('\n')
        code_lines = 0
        comment_lines = 0

        for line in lines:
            line = line.strip()
            if not line:
                continue
            if line.startswith('#') or line.startswith('//') or line.startswith('/*'):
                comment_lines += 1
            else:
                code_lines += 1

        if code_lines == 0:
            return False, "Solo commenti, nessun codice eseguibile"

        if comment_lines > code_lines * 2:
            return False, "Troppi commenti rispetto al codice"

        return True, "OK"

    def benchmark_task_language(self, task_name, language, code):
        print(f"\n📊 Benchmark: {task_name} in {language}")

        iteration_results = []
        successful_runs = 0

        # Usa tqdm per la barra di progresso delle iterazioni
        desc = f"{language:>12}"
        with tqdm(total=self.iterations, desc=desc, unit="iter", 
                  bar_format="{desc}: {percentage:3.0f}%|{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}, {rate_fmt}]") as pbar:
            
            for i in range(1, self.iterations + 1):
                result = self.benchmark_single_execution(code, language, task_name, i)
                iteration_results.append(result)

                if result['success']:
                    successful_runs += 1
                    pbar.set_postfix({
                        'Status': '✅',
                        'Time': f"{result['execution_time']:.3f}s",
                        'CO2': self.format_co2_value(result['emissions'])[:8]
                    })
                else:
                    pbar.set_postfix({
                        'Status': '❌', 
                        'Error': result['error'][:20] + "..." if len(result['error']) > 20 else result['error']
                    })

                pbar.update(1)
                # Piccola pausa tra iterazioni per stabilizzare le misurazioni
                time.sleep(0.1)

        # Calcola statistiche
        success_rate = (successful_runs / self.iterations) * 100
        
        # Traccia i fallimenti per linguaggio
        if successful_runs == 0:
            if language not in self.language_failures:
                self.language_failures[language] = {
                    'failed_tasks': [],
                    'total_failures': 0,
                    'sample_errors': set()
                }
            
            self.language_failures[language]['failed_tasks'].append(task_name)
            self.language_failures[language]['total_failures'] += 1
            
            # Raccoglie esempi di errori (max 3 per linguaggio)
            failed_results = [r for r in iteration_results if not r['success']]
            if failed_results and len(self.language_failures[language]['sample_errors']) < 3:
                sample_error = failed_results[0]['error'][:100] + "..." if len(failed_results[0]['error']) > 100 else failed_results[0]['error']
                self.language_failures[language]['sample_errors'].add(sample_error)

        # Filtra solo le esecuzioni riuscite per le statistiche
        successful_results = [r for r in iteration_results if r['success']]

        statistics_data = {
            'total_iterations': self.iterations,
            'successful_runs': successful_runs,
            'success_rate': success_rate,
            'emissions_stats': {},
            'execution_time_stats': {},
            'all_iterations': iteration_results
        }

        if successful_results:
            emissions_values = [r['emissions'] for r in successful_results]
            execution_times = [r['execution_time'] for r in successful_results]

            statistics_data['emissions_stats'] = {
                'mean': statistics.mean(emissions_values),
                'median': statistics.median(emissions_values),
                'min': min(emissions_values),
                'max': max(emissions_values),
                'std_dev': statistics.stdev(emissions_values) if len(emissions_values) > 1 else 0,
                'total': sum(emissions_values),
                'samples': len(emissions_values)
            }

            statistics_data['execution_time_stats'] = {
                'mean': statistics.mean(execution_times),
                'median': statistics.median(execution_times),
                'min': min(execution_times),
                'max': max(execution_times),
                'std_dev': statistics.stdev(execution_times) if len(execution_times) > 1 else 0,
                'total': sum(execution_times)
            }

        # Stampa risultati
        self.print_benchmark_summary(task_name, language, statistics_data)

        return statistics_data

    def print_benchmark_summary(self, task_name, language, stats):
        """Stampa un riassunto del benchmark"""
        print(f"\n RISULTATI BENCHMARK: {task_name} - {language}")
        print("-" * 60)
        print(f" Esecuzioni riuscite: {stats['successful_runs']}/{stats['total_iterations']} ({stats['success_rate']:.1f}%)")

        if stats['emissions_stats']:
            em = stats['emissions_stats']
            print(f" EMISSIONI CO2:")
            print(f" • Media: {self.format_co2_value(em['mean'])}")
            print(f" • Mediana: {self.format_co2_value(em['median'])}")
            print(f" • Min-Max: {self.format_co2_value(em['min'])} - {self.format_co2_value(em['max'])}")
            print(f" • Deviazione std: {self.format_co2_value(em['std_dev'])}")
            print(f" • Totale {self.iterations} esecuzioni: {self.format_co2_value(em['total'])}")

            et = stats['execution_time_stats']
            print(f" TEMPI ESECUZIONE:")
            print(f" • Media: {et['mean']:.3f}s")
            print(f" • Mediana: {et['median']:.3f}s")
            print(f" • Min-Max: {et['min']:.3f} - {et['max']:.3f}s")
            print(f" • Deviazione std: {et['std_dev']:.3f}s")
        else:
            print(" Nessuna esecuzione riuscita - impossibile calcolare statistiche")

    def benchmark_all_tasks(self):
        """Esegue benchmark su TUTTE le task disponibili nel dataset (modalità COMPLETO)"""
        
        print(f"\n AVVIO CARBON BENCHMARK SYSTEM - MODALITÀ COMPLETA")
        print("=" * 60)

        if not CARBON_TRACKING_AVAILABLE:
            print(" Carbon tracking non disponibile - uscita")
            return {}

        # Rileva linguaggi disponibili dall'executor
        available_languages = list(self.executor.available_languages.keys())

        print(f" Linguaggi disponibili: {len(available_languages)}")
        print(f" Linguaggi: {', '.join(available_languages)}")

        if not available_languages:
            print(" Nessun linguaggio disponibile per il benchmark")
            return {}

        # Scansiona direttamente il dataset per trovare TUTTE le task
        print(" Scansionando dataset completo per trovare tutte le task...")
        all_tasks = set()
        
        # Scansiona tutte le categorie
        categories = ['algorithms', 'basic', 'io', 'mathematics', 'misc', 'strings']
        
        for category in categories:
            category_path = os.path.join(self.code_base_path, category)
            if os.path.exists(category_path):
                # Per ogni linguaggio nella categoria
                for lang_dir in os.listdir(category_path):
                    lang_path = os.path.join(category_path, lang_dir)
                    if os.path.isdir(lang_path):
                        # Trova tutti i file snippet
                        for file_name in os.listdir(lang_path):
                            if file_name.startswith('snippet_') and '_' in file_name:
                                # Estrae il nome della task dal filename
                                task_name = self.extract_task_name_from_filename(file_name)
                                if task_name:
                                    all_tasks.add(task_name)

        # Filtra solo le task che hanno almeno 2 linguaggi disponibili
        valid_tasks = []
        print(" Verificando disponibilità task per linguaggi disponibili...")
        
        for task_name in sorted(all_tasks):
            available_count = 0
            for language in available_languages:
                if self.task_exists_for_language(task_name, language):
                    available_count += 1
            
            # Includi task che hanno almeno 2 linguaggi disponibili
            if available_count >= 2:
                valid_tasks.append({
                    'name': task_name,
                    'available_languages': available_count
                })

        # Ordina per numero di linguaggi disponibili (discendente)
        valid_tasks.sort(key=lambda x: x['available_languages'], reverse=True)
        selected_tasks = [task['name'] for task in valid_tasks]

        print(f" Task totali trovate nel dataset: {len(all_tasks)}")
        print(f" Task valide (≥2 linguaggi disponibili): {len(selected_tasks)}")
        print(f" Tempo stimato: ~{len(selected_tasks) * len(available_languages) * self.iterations * 0.5 / 3600:.1f} ore")
        
        if not selected_tasks:
            print(" Nessuna task valida trovata")
            return {}

        # Conferma dall'utente per benchmark molto grandi
        if len(selected_tasks) > 100:
            print(f"\n⚠️  ATTENZIONE: Benchmark molto esteso!")
            print(f"   • {len(selected_tasks)} task")
            print(f"   • {len(available_languages)} linguaggi") 
            print(f"   • {self.iterations} iterazioni per combinazione")
            print(f"   • {len(selected_tasks) * len(available_languages) * self.iterations:,} esecuzioni totali")
            
            try:
                confirm = input("Procedere comunque? [s/N]: ").strip().lower()
                if confirm not in ['s', 'si', 'sì', 'y', 'yes']:
                    print("❌ Benchmark annullato dall'utente")
                    return {}
            except (EOFError, KeyboardInterrupt):
                print("\n❌ Benchmark annullato")
                return {}

        # Esegui benchmark per ogni task/linguaggio
        benchmark_data = {}
        total_combinations = len(selected_tasks) * len(available_languages)
        
        print(f"\n🚀 Avvio benchmark completo: {len(selected_tasks)} task × {len(available_languages)} linguaggi")
        print(f"📊 Totale combinazioni: {total_combinations:,}")
        print("=" * 60)

        # Usa tqdm per il progresso generale
        with tqdm(total=total_combinations, desc="🔬 Benchmark Progress", unit="combo",
                  bar_format="{desc}: {percentage:3.0f}%|{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}, {rate_fmt}]") as main_pbar:
            
            for task_name in selected_tasks:
                benchmark_data[task_name] = {}
                main_pbar.set_description(f"🔬 Task: {task_name[:25]}")

                for language in available_languages:
                    # Verifica se la task esiste per questo linguaggio
                    if not self.task_exists_for_language(task_name, language):
                        benchmark_data[task_name][language] = {
                            'total_iterations': self.iterations,
                            'successful_runs': 0,
                            'success_rate': 0.0,
                            'error': 'Task non disponibile per questo linguaggio'
                        }
                        main_pbar.set_postfix({'Lang': language, 'Status': '⚠️ Skip'})
                        main_pbar.update(1)
                        continue

                    # Carica il codice per questa task/linguaggio
                    category_subdir = self.determine_category(task_name, language)
                    code = self.load_task_code(task_name, language, category_subdir)

                    if not code:
                        benchmark_data[task_name][language] = {
                            'total_iterations': self.iterations,
                            'successful_runs': 0,
                            'success_rate': 0.0,
                            'error': 'Codice non trovato nel dataset'
                        }
                        main_pbar.set_postfix({'Lang': language, 'Status': '⚠️ No Code'})
                        main_pbar.update(1)
                        continue

                    # Aggiorna postfix con info corrente
                    main_pbar.set_postfix({'Lang': language, 'Status': '🔄 Running'})
                    
                    # Esegui benchmark per questo linguaggio
                    stats = self.benchmark_task_language(task_name, language, code)
                    benchmark_data[task_name][language] = stats
                    
                    # Aggiorna con risultato
                    success_rate = stats.get('success_rate', 0)
                    status = '✅' if success_rate > 50 else '❌' if success_rate == 0 else '⚠️'
                    main_pbar.set_postfix({'Lang': language, 'Status': f'{status} {success_rate:.0f}%'})
                    main_pbar.update(1)

        # Salva risultati
        self.save_benchmark_results(benchmark_data)
        self.generate_benchmark_report(benchmark_data)

        return benchmark_data

    def extract_task_name_from_filename(self, filename):
        """Estrae il nome della task dal nome del file"""
        try:
            # Formato tipico: snippet_123_Task_name.ext
            if filename.startswith('snippet_') and '_' in filename:
                parts = filename.split('_', 2)  # Divide in max 3 parti
                if len(parts) >= 3:
                    # Rimuove l'estensione dal nome della task
                    task_name = parts[2].rsplit('.', 1)[0]
                    return task_name
            return None
        except:
            return None

    def task_exists_for_language(self, task_name, language):
        """Verifica se una task esiste per un determinato linguaggio"""
        try:
            category_subdir = self.determine_category(task_name, language)
            code = self.load_task_code(task_name, language, category_subdir)
            return code is not None
        except:
            return False

    def benchmark_common_tasks(self, max_tasks=10):
        """Esegue benchmark su task comuni tra linguaggi"""
        
        # Controlla se esiste il file di analisi task comuni
        common_tasks_files = glob.glob("results/task_analysis/common_tasks.json")
        if not common_tasks_files:
            print(" File task comuni non trovato.")
            print(" Per generare il file common_tasks.json, esegui prima il comando:")
            print(" python main.py analyze")
            return {}
        
        # Ora procede con la logica esistente
        if not common_tasks_files:
            print(" Impossibile trovare o generare file task comuni")
            return {}
            
        common_tasks_file = common_tasks_files[0]
        
        print(f"\n AVVIO CARBON BENCHMARK SYSTEM")
        print("=" * 60)

        if not CARBON_TRACKING_AVAILABLE:
            print(" Carbon tracking non disponibile - uscita")
            return {}

        # Rileva linguaggi disponibili dall'executor
        available_languages = list(self.executor.available_languages.keys())

        print(f" Linguaggi disponibili: {len(available_languages)}")
        print(f" Linguaggi: {', '.join(available_languages)}")

        if not available_languages:
            print(" Nessun linguaggio disponibile per il benchmark")
            return {}

        # Carica task comuni dal file
        try:
            with open(common_tasks_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
                
                if 'common_tasks' in data and isinstance(data['common_tasks'], list):
                    available_tasks = [task['name'] for task in data['common_tasks'] if 'name' in task]
                    
                    # Seleziona task secondo max_tasks
                    if max_tasks and len(available_tasks) > max_tasks:
                        selected_tasks = available_tasks[:max_tasks]
                        print(f" Task selezionate per benchmark: {len(selected_tasks)} (prime {max_tasks} task)")
                    else:
                        selected_tasks = available_tasks
                        print(f" Task selezionate per benchmark: {len(selected_tasks)}")
                    
                    if not selected_tasks:
                        print(" Nessuna task comune trovata")
                        return {}
                else:
                    print(" Formato task comuni non valido")
                    return {}
        except Exception as e:
            print(f" Errore caricamento task: {e}")
            return {}

        # Esegui benchmark per ogni task/linguaggio
        benchmark_data = {}
        total_combinations = len(selected_tasks) * len(available_languages)
        current_combination = 0

        for task_name in selected_tasks:
            benchmark_data[task_name] = {}
            print(f"\n TASK: {task_name}")
            print("=" * 40)

            for language in available_languages:
                current_combination += 1
                print(f"\n Progresso: {current_combination}/{total_combinations} ({(current_combination/total_combinations)*100:.1f}%)")

                # Carica il codice per questa task/linguaggio
                category_subdir = self.determine_category(task_name, language)
                code = self.load_task_code(task_name, language, category_subdir)

                if not code:
                    print(f" ⚠️  File non trovato per {task_name} in {language} - saltato")
                    # Salta questo task/linguaggio se non è disponibile nel dataset
                    benchmark_data[task_name][language] = {
                        'total_iterations': self.iterations,
                        'successful_runs': 0,
                        'success_rate': 0.0,
                        'error': 'File non disponibile nel dataset'
                    }
                    continue

                print(f"\n Benchmark: {task_name} in {language}")

                # Esegui benchmark per questo linguaggio
                stats = self.benchmark_task_language(task_name, language, code)
                benchmark_data[task_name][language] = stats

        # Salva risultati
        self.save_benchmark_results(benchmark_data)
        self.generate_benchmark_report(benchmark_data)

        return benchmark_data

    def save_benchmark_results(self, benchmark_data):
        """Salva i risultati del benchmark"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Salva dati completi
        detailed_file = os.path.join(self.results_dir, f"carbon_benchmark_detailed_{timestamp}.json")
        with open(detailed_file, 'w') as f:
            json.dump(benchmark_data, f, indent=2)

        # Salva summary
        summary_data = {}
        for task_name, task_data in benchmark_data.items():
            summary_data[task_name] = {}
            for language, stats in task_data.items():
                if stats.get('emissions_stats'):
                    summary_data[task_name][language] = {
                        'success_rate': stats['success_rate'],
                        'mean_emissions': stats['emissions_stats']['mean'],
                        'mean_execution_time': stats['execution_time_stats']['mean'],
                        'total_emissions': stats['emissions_stats']['total']
                    }

        summary_file = os.path.join(self.results_dir, f"carbon_benchmark_summary_{timestamp}.json")
        with open(summary_file, 'w') as f:
            json.dump(summary_data, f, indent=2)

        print(f"\n💾 Risultati salvati:")
        print(f" Dettagliati: {detailed_file}")
        print(f" Summary: {summary_file}")

    def generate_benchmark_report(self, benchmark_data):
        """Genera un report finale del benchmark"""
        print(f"\n CARBON BENCHMARK REPORT FINALE")
        print("=" * 60)

        total_tasks = len(benchmark_data)
        total_emissions = 0.0
        total_executions = 0
        language_stats = {}

        for task_name, task_data in benchmark_data.items():
            for language, stats in task_data.items():
                if stats.get('emissions_stats'):
                    emissions = stats['emissions_stats']['total']
                    executions = stats['successful_runs']

                    total_emissions += emissions
                    total_executions += executions

                    if language not in language_stats:
                        language_stats[language] = {
                            'total_emissions': 0.0,
                            'total_executions': 0,
                            'avg_emissions_per_execution': 0.0
                        }

                    language_stats[language]['total_emissions'] += emissions
                    language_stats[language]['total_executions'] += executions

        # Calcola medie per linguaggio
        for language in language_stats:
            if language_stats[language]['total_executions'] > 0:
                language_stats[language]['avg_emissions_per_execution'] = \
                    language_stats[language]['total_emissions'] / language_stats[language]['total_executions']

        # Lista completa dei linguaggi supportati
        all_languages = [
            'python', 'javascript', 'java', 'cpp', 'c', 'ruby', 'php', 'r', 'julia', 
            'csharp', 'go', 'rust', 'haskell', 'ocaml', 'typescript'
        ]
        
        # Calcola statistiche sui linguaggi
        working_languages = set(language_stats.keys())
        failed_languages = set(all_languages) - working_languages
        
        print(f" STATISTICHE GENERALI:")
        print(f" Task benchmarked: {total_tasks}")
        print(f" Linguaggi supportati: {len(all_languages)}")
        print(f" Linguaggi funzionanti: {len(working_languages)}/{len(all_languages)} ({len(working_languages)/len(all_languages)*100:.1f}%)")
        print(f" Esecuzioni totali riuscite: {total_executions}")
        print(f" Emissioni totali: {self.format_co2_value(total_emissions)}")
        print(f" Media per esecuzione: {self.format_co2_value(total_emissions/total_executions)}" if total_executions > 0 else " Media per esecuzione: N/A")

        print(f"\n✅ LINGUAGGI FUNZIONANTI ({len(working_languages)}):")
        if working_languages:
            sorted_languages = sorted(language_stats.items(),
                                      key=lambda x: x[1]['avg_emissions_per_execution'])
            for i, (language, stats) in enumerate(sorted_languages, 1):
                print(f" {i:2d}. {language:12s}: {self.format_co2_value(stats['avg_emissions_per_execution'])}/run "
                      f"({stats['total_executions']} runs)")
        else:
            print("   Nessun linguaggio ha completato con successo il benchmark")
            
        print(f"\n❌ LINGUAGGI CON PROBLEMI ({len(failed_languages)}):")
        if failed_languages:
            for language in sorted(failed_languages):
                if language in self.language_failures:
                    failure_info = self.language_failures[language]
                    failed_tasks = len(failure_info['failed_tasks'])
                    print(f"   • {language:12s}: {failed_tasks} task falliti")
                    
                    # Mostra alcuni esempi di errori
                    if failure_info['sample_errors']:
                        sample_error = list(failure_info['sample_errors'])[0]
                        if 'Errore compilazione:' in sample_error:
                            error_type = "Errori di compilazione"
                        elif 'libtinfo' in sample_error.lower():
                            error_type = "Problemi libreria libtinfo"
                        elif 'no input files' in sample_error.lower():
                            error_type = "Problemi configurazione compilatore"
                        elif 'unknown start of token' in sample_error.lower():
                            error_type = "Errori di sintassi nel codice"
                        elif 'traceback' in sample_error.lower():
                            error_type = "Errori runtime Python"
                        elif 'error:' in sample_error.lower():
                            error_type = "Errori di compilazione"
                        else:
                            error_type = "Vari errori"
                        print(f"     → {error_type}")
                else:
                    print(f"   • {language:12s}: Non testato")
        else:
            print("   Tutti i linguaggi funzionano correttamente!")
            
        # Statistiche riassuntive dei problemi
        if self.language_failures:
            print(f"\n DETTAGLIO PROBLEMI:")
            total_failed_tasks = sum(len(info['failed_tasks']) for info in self.language_failures.values())
            print(f"   Task totali falliti: {total_failed_tasks}")
            
            # Categorizza i tipi di errori per linguaggio
            error_categories = {
                'Compilazione': set(),
                'Librerie/Dipendenze': set(), 
                'Sintassi': set(),
                'Runtime': set(),
                'Configurazione': set(),
                'Altri': set()
            }
            
            for language, failure_info in self.language_failures.items():
                for error in failure_info['sample_errors']:
                    error_lower = error.lower()
                    if 'compilazione' in error_lower or 'compile' in error_lower:
                        error_categories['Compilazione'].add(language)
                    elif 'libtinfo' in error_lower or 'library' in error_lower:
                        error_categories['Librerie/Dipendenze'].add(language)
                    elif 'syntax' in error_lower or 'token' in error_lower:
                        error_categories['Sintassi'].add(language)
                    elif 'traceback' in error_lower or 'runtime' in error_lower:
                        error_categories['Runtime'].add(language)
                    elif 'no input files' in error_lower or 'command not found' in error_lower:
                        error_categories['Configurazione'].add(language)
                    else:
                        error_categories['Altri'].add(language)
            
            for category, language_set in error_categories.items():
                if len(language_set) > 0:
                    print(f"   {category}: {len(language_set)} linguaggi")

        # Stima impatto annuale
        if total_emissions > 0:
            daily_estimate = total_emissions * (86400 / (self.iterations * len(language_stats)))  # 24h estimate
            yearly_estimate = daily_estimate * 365
            print(f"\n🔮 STIME IMPATTO:")
            print(f" Stima giornaliera (uso continuo): {self.format_co2_value(daily_estimate, 'g')}/giorno")
            print(f" Stima annuale (uso continuo): {self.format_co2_value(yearly_estimate, 'g')}/anno")


def benchmark_quick_test(iterations=5):
    """Test rapido del sistema di benchmark"""
    print(" QUICK BENCHMARK TEST")
    benchmark = CarbonBenchmark(iterations=iterations)

    # Test con codice Python semplice
    test_code = '''
import time
import math

# Calcolo matematico semplice
result = 0
for i in range(1000):
    result += math.sqrt(i) * math.sin(i)

print(f"Risultato: {result:.2f}")
'''

    stats = benchmark.benchmark_task_language("test_math", "python", test_code)
    return stats


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "test":
        # Test rapido
        benchmark_quick_test(5)
    elif len(sys.argv) > 1 and sys.argv[1] == "debug":
        # Debug veloce (3 iterazioni, 2 task) - per test rapidi
        benchmark = CarbonBenchmark(iterations=3)
        benchmark.benchmark_common_tasks(max_tasks=2)
    elif len(sys.argv) > 1 and sys.argv[1] == "quick":
        # Benchmark veloce (10 iterazioni, 3 task)
        benchmark = CarbonBenchmark(iterations=10)
        benchmark.benchmark_common_tasks(max_tasks=3)
    else:
        # Default: Top10 (5 iterazioni, 10 task)
        benchmark = CarbonBenchmark(iterations=30)
        benchmark.benchmark_common_tasks(max_tasks=10)