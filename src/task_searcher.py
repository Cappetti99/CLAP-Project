#!/usr/bin/env python3
"""
Task Searcher - Sistema di ricerca e esecuzione mirata per task specifiche
Permette di cercare task per nome, mostrare linguaggi disponibili e misurare CO2
"""

import os
import sys
import json
import glob
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from collections import defaultdict

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

try:
    from carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
    from smart_executor import SmartExecutor
    DEPENDENCIES_AVAILABLE = True
except ImportError:
    DEPENDENCIES_AVAILABLE = False
    print(" Alcune dipendenze non disponibili - funzionalità limitata")


class TaskSearcher:
    """
    Single Responsibility: Ricerca e esecuzione mirata di task specifiche
    Cerca task per nome, mostra linguaggi disponibili e misura impatto CO2
    """
    
    def __init__(self):
        self.code_base_path = "data/generated/code_snippets"
        self.results_dir = "results/task_search"
        
        # Carica linguaggi testati e disponibili
        self.available_languages = self._load_tested_languages()
        
        # Inizializza componenti se disponibili
        if DEPENDENCIES_AVAILABLE:
            self.executor = SmartExecutor()
        else:
            self.executor = None
            
        # Crea directory risultati
        os.makedirs(self.results_dir, exist_ok=True)
        
        # Mapping linguaggi comuni per la ricerca (nomi standardizzati)
        all_language_mapping = {
            'python': ['python'],
            'java': ['java'],
            'javascript': ['javascript'],
            'c': ['c'],
            'cpp': ['cpp'],                    # C++ standardizzato su 'cpp'  
            'c++': ['cpp'],
            'go': ['go'],
            'rust': ['rust'],
            'ruby': ['ruby'],
            'php': ['php'],
            'csharp': ['csharp'],             # C# standardizzato su 'csharp'
            'c#': ['csharp'],
            'haskell': ['haskell'],
            'ocaml': ['ocaml'],
            'r': ['r'],
            'julia': ['julia'],
            'typescript': ['typescript']
        }
        
        # Filtra solo i linguaggi disponibili
        self.language_mapping = {k: v for k, v in all_language_mapping.items() 
                               if k.lower() in [lang.lower() for lang in self.available_languages]}
    
    def _load_tested_languages(self) -> List[str]:
        """Carica la lista dei linguaggi testati e disponibili"""
        import json
        from pathlib import Path
        
        test_results_dir = Path("results/execution")
        available_languages = []
        
        if test_results_dir.exists():
            # Cerca il file di test più recente
            test_files = list(test_results_dir.glob("language_test_results_*.json"))
            if test_files:
                latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
                try:
                    with open(latest_test, 'r') as f:
                        test_data = json.load(f)
                    
                    # Carica silenziosamente (l'output dettagliato è già mostrato da SmartExecutor)
                    
                    # Mappa nomi del test ai nomi nel dataset
                    test_to_dataset_mapping = {
                        'cpp': 'cpp',             # C++ standardizzato su 'cpp'
                        'c++': 'cpp',             # C++ alternativo
                        'cplusplus': 'cpp',       # C++ forma completa
                        'java': 'java',
                        'csharp': 'csharp',       # C# standardizzato su 'csharp'
                        'c#': 'csharp',           # C# simbolo diretto
                        'cs': 'csharp',           # C# abbreviazione
                        'python': 'python',
                        'python3': 'python',      # Python 3 specifico
                        'py': 'python',           # Python abbreviazione
                        'ruby': 'ruby',
                        'rb': 'ruby',             # Ruby abbreviazione
                        'javascript': 'javascript',
                        'js': 'javascript',       # JavaScript abbreviazione
                        'node': 'javascript',     # Node.js
                        'nodejs': 'javascript',   # Node.js forma completa
                        'typescript': 'typescript',
                        'ts': 'typescript',       # TypeScript abbreviazione
                        'c': 'c',
                        'go': 'go',
                        'golang': 'go',           # Go nome completo
                        'rust': 'rust',
                        'rs': 'rust',             # Rust abbreviazione
                        'php': 'php',
                        'haskell': 'haskell',
                        'hs': 'haskell',          # Haskell abbreviazione
                        'ocaml': 'ocaml',
                        'ml': 'ocaml',            # OCaml/ML abbreviazione
                        'r': 'r',
                        'rlang': 'r',             # R linguaggio completo
                        'julia': 'julia',
                        'jl': 'julia'             # Julia abbreviazione
                    }
                    
                    # Carica solo i linguaggi che hanno passato il test
                    for lang, result in test_data['results'].items():
                        if result['available'] and lang in test_to_dataset_mapping:
                            dataset_lang = test_to_dataset_mapping[lang]
                            available_languages.append(dataset_lang)
                    
                    if available_languages:
                        # Output ridotto - dettagli già mostrati da SmartExecutor
                        return available_languages
                        
                except Exception as e:
                    print(f" Errore caricamento risultati test: {e}")
        
        # Fallback: usa tutti i linguaggi principali se non ci sono risultati del test
        print(" Nessun risultato test trovato, usando linguaggi principali di fallback")
        fallback_languages = ['python', 'java', 'javascript', 'c', 'cplusplus', 'go', 'rust']
        print(f"Linguaggi fallback: {', '.join(fallback_languages)}")
        return fallback_languages
    
    def normalize_task_name(self, task_name: str) -> str:
        """Normalizza il nome della task per la ricerca"""
        # Rimuove caratteri speciali e converte in lowercase
        normalized = re.sub(r'[^a-zA-Z0-9\s]', '', task_name.lower())
        # Sostituisce spazi con underscore
        normalized = re.sub(r'\s+', '_', normalized.strip())
        return normalized
    
    def _get_file_extensions(self, language: str) -> List[str]:
        """Ottiene le estensioni file supportate per un linguaggio"""
        # Mappa linguaggi alle loro estensioni (nomi standardizzati)
        extension_mapping = {
            'python': ['.py', '.txt'],
            'java': ['.java', '.txt'],
            'javascript': ['.js', '.txt'],
            'typescript': ['.ts', '.txt'],
            'c': ['.c', '.txt'],
            'cpp': ['.cpp', '.cxx', '.cc', '.c++', '.txt'],  # C++ standardizzato su 'cpp'
            'csharp': ['.cs', '.txt'],                       # C# standardizzato su 'csharp'
            'go': ['.go', '.txt'],
            'rust': ['.rs', '.txt'],
            'ruby': ['.rb', '.txt'],
            'php': ['.php', '.txt'],
            'haskell': ['.hs', '.lhs', '.txt'],  # .lhs per Literate Haskell
            'ocaml': ['.ml', '.mli', '.txt'],   # .mli per interface files
            'julia': ['.jl', '.txt'],
            'r': ['.r', '.R', '.txt']
        }
        
        return extension_mapping.get(language.lower(), ['.txt'])  # Default to .txt
    
    def search_tasks(self, task_name: str, fuzzy: bool = True) -> Dict[str, List[str]]:
        """
        Cerca task per nome nel dataset (solo nei linguaggi testati e disponibili)
        
        Args:
            task_name: Nome della task da cercare
            fuzzy: Se True, usa ricerca fuzzy (contenimento), altrimenti esatta
            
        Returns:
            Dict con linguaggi come chiavi e lista di file come valori
        """
        print(f"\nRicerca task: '{task_name}' (solo linguaggi testati)")
        print("=" * 50)
        
        normalized_name = self.normalize_task_name(task_name)
        found_tasks = defaultdict(list)
        
        # Filtra solo i linguaggi disponibili
        if not self.available_languages:
            print(" Nessun linguaggio disponibile - esegui prima 'python main.py test'")
            return dict(found_tasks)
        
        print(f"Cerco in {len(self.available_languages)} linguaggi testati...")
        
        # Cerca in tutte le directory di linguaggi
        for category_dir in glob.glob(os.path.join(self.code_base_path, "*")):
            if not os.path.isdir(category_dir):
                continue
                
            # Cerca in ogni linguaggio nella categoria
            for lang_dir in glob.glob(os.path.join(category_dir, "*")):
                if not os.path.isdir(lang_dir):
                    continue
                    
                lang_name = os.path.basename(lang_dir).lower()
                
                # FILTRO: Considera solo linguaggi testati e disponibili
                if lang_name not in [lang.lower() for lang in self.available_languages]:
                    continue
                
                # Cerca file che corrispondono al nome della task
                # Supporta multiple estensioni basate sul linguaggio
                extensions = self._get_file_extensions(lang_name)
                
                for ext in extensions:
                    pattern = os.path.join(lang_dir, f"*{ext}")
                    for file_path in glob.glob(pattern):
                        file_name = os.path.basename(file_path)
                        file_name_normalized = self.normalize_task_name(file_name)
                        
                        # Controllo corrispondenza (esatta o fuzzy)
                        if fuzzy:
                            if normalized_name in file_name_normalized:
                                found_tasks[lang_name].append(file_path)
                        else:
                            # Rimuove l'estensione per confronto esatto
                            name_without_ext = file_name_normalized
                            for extension in extensions:
                                name_without_ext = name_without_ext.replace(extension.lower(), '')
                            if normalized_name == name_without_ext:
                                found_tasks[lang_name].append(file_path)
        
        return dict(found_tasks)
    
    def display_search_results(self, found_tasks: Dict[str, List[str]], task_name: str):
        """Mostra i risultati della ricerca in formato user-friendly"""
        if not found_tasks:
            print(f"Nessuna task trovata per '{task_name}' nei linguaggi testati")
            print("\n Suggerimenti:")
            print("   • Prova una ricerca più generica (es: 'sort' invece di 'quicksort')")
            print("   • Usa parole chiave in inglese")
            print("   • Verifica che i linguaggi siano stati testati con 'python main.py test'")
            return
        
        total_tasks = sum(len(files) for files in found_tasks.values())
        print(f"Trovate {total_tasks} task in {len(found_tasks)} linguaggi testati:")
        print(f"Su {len(self.available_languages)} linguaggi disponibili dal test")
        print()
        
        # Raggruppa per linguaggio
        for lang, files in sorted(found_tasks.items()):
            print(f"{lang.upper()} ({len(files)} file):")
            for file_path in files[:3]:  # Mostra max 3 file per linguaggio
                file_name = os.path.basename(file_path)
                print(f"   • {file_name}")
            if len(files) > 3:
                print(f"   • ... e altri {len(files) - 3} file")
            print()
    
    def select_task_for_execution(self, found_tasks: Dict[str, List[str]]) -> Optional[Tuple[str, str]]:
        """
        Permette all'utente di selezionare una task specifica per l'esecuzione
        
        Returns:
            Tupla (linguaggio, file_path) o None se cancellato
        """
        if not found_tasks:
            return None
        
        print("Seleziona task da eseguire:")
        print()
        
        # Crea lista ordinata di opzioni
        options = []
        for lang, files in sorted(found_tasks.items()):
            for file_path in files:
                file_name = os.path.basename(file_path)
                options.append((lang, file_path, file_name))
        
        # Mostra opzioni
        for i, (lang, file_path, file_name) in enumerate(options, 1):
            print(f"  {i}. [{lang.upper()}] {file_name}")
        
        print(f"  0. Cancella")
        print()
        
        # Input utente
        try:
            choice = input(f"Seleziona opzione (1-{len(options)}) [0]: ").strip()
            if choice == '' or choice == '0':
                return None
            
            choice_num = int(choice)
            if 1 <= choice_num <= len(options):
                lang, file_path, _ = options[choice_num - 1]
                return lang, file_path
            else:
                print("Scelta non valida")
                return None
                
        except (ValueError, KeyboardInterrupt):
            print("Operazione cancellata")
            return None
    
    def execute_task_with_carbon_tracking(self, language: str, file_path: str) -> bool:
        """
        Esegue una task specifica con monitoraggio CO2
        
        Args:
            language: Nome del linguaggio
            file_path: Path al file della task
            
        Returns:
            True se l'esecuzione ha successo, False altrimenti
        """
        if not DEPENDENCIES_AVAILABLE or not self.executor:
            print("Dipendenze non disponibili per l'esecuzione")
            return False
        
        task_name = os.path.basename(file_path)
        print(f"\nEsecuzione task: '{task_name}' in {language.upper()}")
        print("=" * 50)
        
        try:
            # Legge il codice
            with open(file_path, 'r', encoding='utf-8') as f:
                code = f.read()

            print(f"File: {os.path.basename(file_path)}")
            print(f"Linguaggio: {language}")
            print(f"Dimensione: {len(code)} caratteri")
            
            # Analisi qualitativa del codice prima dell'esecuzione
            print("\nANALISI QUALITATIVA")
            print("-" * 30)
            quality_analysis = self._analyze_code_quality(code, language)
            self._display_quality_results(quality_analysis)
            print()

            # Avvia tracking CO2
            if CODECARBON_AVAILABLE:
                carbon_session = start_carbon_tracking(task_name, language)
                print("Monitoraggio CO2 avviato")
            else:
                carbon_session = None
                print("Monitoraggio CO2 non disponibile")
            
            print("Esecuzione in corso...")
            print("-" * 40)
            
            # Esegue il codice
            result = self.executor.execute_code(
                code=code,
                language=language,
                task_name=task_name
            )
            
            print("-" * 40)
            
            # Ferma tracking CO2
            emissions = None
            if carbon_session and CODECARBON_AVAILABLE:
                emissions = stop_carbon_tracking()
                if emissions:
                    print(f"Emissioni CO2: {emissions:.6f} kg ({emissions * 1000:.3f} g)")
                else:
                    print("Dati CO2 non disponibili")
            
            # Salva risultati
            self._save_execution_results(task_name, language, file_path, result.get('success', False), emissions)
            
            if result.get('success', False):
                print("Esecuzione completata con successo")
                if result.get('output'):
                    print(f"Output: {result['output'][:100]}...")
            else:
                print("Esecuzione fallita")
                if result.get('error'):
                    print(f"Errore: {result['error'][:100]}...")
            
            return result.get('success', False)
            
        except Exception as e:
            print(f"Errore durante l'esecuzione: {e}")
            return False
    
    def _save_execution_results(self, task_name: str, language: str, file_path: str, 
                              success: bool, emissions: Optional[float]):
        """Salva i risultati dell'esecuzione"""
        try:
            from datetime import datetime
            
            result = {
                'task_name': task_name,
                'language': language,
                'file_path': file_path,
                'success': success,
                'co2_emissions_kg': emissions,
                'timestamp': datetime.now().isoformat(),
                'execution_type': 'task_search'
            }
            
            # Salva in file JSON
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            result_file = os.path.join(self.results_dir, f"task_execution_{timestamp}.json")
            
            with open(result_file, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2, ensure_ascii=False)
            
            print(f" Risultati salvati in: {result_file}")
            
        except Exception as e:
            print(f" Errore nel salvataggio: {e}")
    
    def interactive_task_search(self, initial_query: Optional[str] = None):
        """
        Interfaccia interattiva per la ricerca e esecuzione di task
        
        Args:
            initial_query: Query iniziale opzionale
        """
        print("\nTASK SEARCHER - Ricerca e Esecuzione Mirata")
        print("=" * 50)
        print(f"Limitato ai {len(self.available_languages)} linguaggi testati e disponibili")
        print()
        
        # Query iniziale
        if initial_query:
            query = initial_query
            print(f"Query iniziale: '{query}'")
        else:
            query = input(" Inserisci nome task da cercare: ").strip()
        
        if not query:
            print("Query vuota, operazione cancellata")
            return False
        
        # Cerca task (solo nei linguaggi testati)
        found_tasks = self.search_tasks(query, fuzzy=True)
        self.display_search_results(found_tasks, query)
        
        if not found_tasks:
            return False
        
        # Selezione per esecuzione
        print("Vuoi eseguire una di queste task?")
        execute_choice = input("Conferma [s/N]: ").strip().lower()
        
        if execute_choice not in ['s', 'si', 'y', 'yes']:
            print("Operazione cancellata")
            print("\nRicerca e esecuzione task completata!")
            print("Controlla i risultati in results/task_search/")
            print("=" * 60)
            return True
        
        # Selezione task specifica
        selection = self.select_task_for_execution(found_tasks)
        if not selection:
            print("Nessuna task selezionata")
            print("\nRicerca e esecuzione task completata!")
            print("Controlla i risultati in results/task_search/")
            print("=" * 60)
            return True
        
        language, file_path = selection
        
        # Esecuzione con monitoraggio CO2
        success = self.execute_task_with_carbon_tracking(language, file_path)
        
        print("\nRicerca e esecuzione task completata!")
        print("Controlla i risultati in results/task_search/")
        print("=" * 60)
        
        return success
    
    def _analyze_code_quality(self, code: str, language: str) -> Dict:
        """
        Analizza la qualità del codice
        
        Args:
            code: Codice sorgente da analizzare
            language: Linguaggio di programmazione
            
        Returns:
            Dizionario con metriche di qualità
        """
        quality_patterns = {
            'comments': [
                r'//.*',           # C, C++, Java, JavaScript, Go, Rust
                r'/\*.*?\*/',      # C, C++, Java, JavaScript
                r'#.*',            # Python, Ruby, Perl, Bash
                r'<!--.*?-->',     # HTML, XML
                r'--.*',           # SQL, Haskell
                r';.*',            # Scheme, Lisp
                r'\(\*.*?\*\)'     # OCaml, F#
            ],
            'functions': [
                r'def\s+\w+',      # Python
                r'function\s+\w+', # JavaScript
                r'func\s+\w+',     # Go
                r'fn\s+\w+',       # Rust
                r'\w+\s+\w+\s*\([^)]*\)\s*{',  # C, C++, Java
                r'sub\s+\w+',      # Perl
                r'class\s+\w+',    # OOP languages
                r'module\s+\w+'    # F#, Haskell
            ],
            'error_handling': [
                r'try\s*{',        # Java, C#
                r'try:',           # Python
                r'catch',          # Java, C#, JavaScript
                r'except',         # Python
                r'finally',        # Java, Python
                r'panic',          # Go, Rust
                r'Result<',        # Rust
                r'Option<',        # Rust
                r'if.*error',      # Go
                r'rescue',         # Ruby
                r'ensure'          # Ruby
            ],
            'imports': [
                r'import\s+',      # Python, Java, JavaScript
                r'#include\s*<',   # C, C++
                r'using\s+',       # C#
                r'require\s+',     # Ruby, JavaScript
                r'use\s+',         # Rust
                r'from\s+\w+\s+import', # Python
                r'package\s+',     # Go, Java
                r'extern\s+crate'  # Rust
            ]
        }
        
        quality_metrics = {
            'has_comments': False,
            'has_functions': False,
            'has_error_handling': False,
            'has_imports': False,
            'code_length': len(code),
            'line_count': len(code.split('\n')),
            'non_empty_lines': len([line for line in code.split('\n') if line.strip()])
        }
        
        # Verifica pattern di qualità
        import re
        for pattern_type, patterns in quality_patterns.items():
            metric_name = f"has_{pattern_type}"
            
            for pattern in patterns:
                if re.search(pattern, code, re.MULTILINE | re.DOTALL):
                    quality_metrics[metric_name] = True
                    break
        
        # Calcola quality score (0-100)
        score = 0
        if quality_metrics['has_comments']: score += 25     # Documentazione
        if quality_metrics['has_functions']: score += 30    # Struttura modulare  
        if quality_metrics['has_error_handling']: score += 25  # Robustezza
        if quality_metrics['has_imports']: score += 10     # Uso librerie
        
        # Bonus per codice ben strutturato
        if quality_metrics['non_empty_lines'] > 10: score += 10  # Non triviale
        
        quality_metrics['quality_score'] = min(score, 100)
        quality_metrics['language'] = language
        
        return quality_metrics
    
    def _display_quality_results(self, quality_analysis: Dict):
        """
        Mostra i risultati dell'analisi qualitativa
        
        Args:
            quality_analysis: Risultati dell'analisi qualitativa
        """
        score = quality_analysis.get('quality_score', 0)
        
        # Testo per il punteggio
        if score >= 80:
            score_text = "ECCELLENTE"
        elif score >= 60:
            score_text = "BUONO"
        elif score >= 40:
            score_text = "DISCRETO"
        else:
            score_text = "MIGLIORABILE"
        
        print(f"Quality Score: {score}/100 ({score_text})")
        print(f"Righe: {quality_analysis['line_count']} (effettive: {quality_analysis['non_empty_lines']})")
        
        # Dettagli feature
        features = []
        if quality_analysis.get('has_comments'): features.append("Commenti")
        if quality_analysis.get('has_functions'): features.append("Funzioni")
        if quality_analysis.get('has_error_handling'): features.append("Error handling")
        if quality_analysis.get('has_imports'): features.append("Import/Include")
        
        if features:
            print(f"Caratteristiche: {', '.join(features)}")
        else:
            print("Caratteristiche base: nessuna rilevata")


def search_and_execute_task(task_name: Optional[str] = None) -> bool:
    """
    Funzione principale per la ricerca e esecuzione di task
    
    Args:
        task_name: Nome della task da cercare (opzionale, se None chiede input)
        
    Returns:
        True se l'operazione ha successo, False altrimenti
    """
    try:
        searcher = TaskSearcher()
        return searcher.interactive_task_search(task_name)
    except KeyboardInterrupt:
        print("\nOperazione interrotta dall'utente")
        return False
    except Exception as e:
        print(f"Errore nel task searcher: {e}")
        return False


if __name__ == "__main__":
    # Test del modulo
    if len(sys.argv) > 1:
        task_name = " ".join(sys.argv[1:])
        search_and_execute_task(task_name)
    else:
        search_and_execute_task()