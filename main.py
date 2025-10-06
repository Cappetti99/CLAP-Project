#!/usr/bin/env python3
"""
SWAM Project - Main Entry Point
Sistema completo per l'analisi e esecuzione di codici multi-linguaggio con monitoraggio CO2
"""

import sys
import os
import argparse
import glob
import shutil
from pathlib import Path

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
modules_path = os.path.join(script_dir, 'modules')
src_path = os.path.join(script_dir, 'src')
sys.path.insert(0, modules_path)
sys.path.insert(0, src_path)

def print_banner():
    """Stampa il banner del progetto con informazioni di versione"""
    print("=" * 40)
    print("SWAM PROJECT")
    print("Author: Lorenzo Cappetti")
    print("=" * 40)

def print_help():
    """Stampa l'aiuto conciso per l'utilizzo del sistema"""
    print("\nCOMANDI PRINCIPALI:")
    print("  test       - Verifica linguaggi disponibili")
    print("  analyze    - Trova task comuni")
    print("  smart      - Esecuzione intelligente (RACCOMANDATO)")
    print("  benchmark  - Misurazione CO2")
    print("  carbon     - Report emissioni")
    
    print("\nCOMANDI AGGIUNTIVI:")
    print("  execute    - Esecuzione completa (tutti i linguaggi)")
    print("  find       - Cerca task + analisi qualità + CO2")
    print("  quality    - Analisi qualitativa (experimental)")
    print("  clean      - Pulizia cache")
    print("  status     - Stato progetto")
    
    print("\n AVVIO RAPIDO:")
    print("  python main.py test && python main.py analyze && python main.py smart")
    
    print("\n Per dettagli: README.md e SETUP_GUIDE.md")

def analyze_tasks():
    """Esegue l'analisi delle task comuni"""
    print("\nANALISI TASK COMUNI")
    print("-" * 40)
    
    # Prima controlla se ci sono risultati di test recenti
    available_languages = []
    test_results_dir = Path("results/execution")
    
    if test_results_dir.exists():
        # Cerca il file di test più recente
        test_files = list(test_results_dir.glob("language_test_results_*.json"))
        if test_files:
            latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
            try:
                import json
                with open(latest_test, 'r') as f:
                    test_data = json.load(f)
                    for lang, result in test_data['results'].items():
                        if result['available']:
                            available_languages.append(lang)
                
                print(f"Utilizzando risultati test da: {latest_test.name}")
                print(f"Linguaggi disponibili rilevati: {len(available_languages)}")
                print(f"Linguaggi: {', '.join(sorted(available_languages))}")
                print()
            except Exception as e:
                print(f"Errore lettura risultati test: {e}")
                available_languages = []
    
    # Verifica che abbiamo linguaggi disponibili
    if not available_languages:
        print("  Nessun risultato di test trovato.")
        print("  AZIONE RICHIESTA: Esegui prima 'python main.py test' per rilevare i linguaggi disponibili.")
        print("  Il comando 'analyze' richiede risultati del test per funzionare correttamente.")
        return False
    
    try:
        from src.finder import UnifiedTaskFinder
        finder = UnifiedTaskFinder()
        
        # Usa la logica TOP 10 con linguaggi disponibili
        print(f"Cercando le TOP 10 task con più linguaggi tra quelli disponibili...")
        print(f"Linguaggi disponibili: {len(available_languages)}")
        
        # Usa la nuova logica per TOP 10 task con output ottimizzato
        common_tasks = finder.find_common_tasks(
            min_languages=1, 
            available_languages=available_languages,
            verbose=False  # Output conciso per workflow normale
        )
        
        if common_tasks:
            print(f"TOP 10 task con più linguaggi disponibili:")
                
            for i, task in enumerate(common_tasks[:10], 1):  # Mostra prime 10
                lang_info = f"{task['language_count']} linguaggi"
                print(f"  {i:2d}. {task['name']} - {lang_info}")
            
            # Salva i risultati su file
            import json
            import os
            os.makedirs("results/task_analysis", exist_ok=True)
            
            result_data = {
                "analysis_type": "top_10_available_languages",
                "total_tasks": len(common_tasks),
                "available_languages_count": len(available_languages),
                "all_languages_count": len(finder.supported_languages),
                "languages": list(finder.supported_languages),
                "available_languages": available_languages,
                "common_tasks": common_tasks,
                "min_languages_filter": 1,
                "search_strategy": "top_10_by_coverage"
            }
            
            with open("results/task_analysis/common_tasks.json", 'w') as f:
                json.dump(result_data, f, indent=2)
                
        else:
            print("Nessuna task comune trovata")
            
    except ImportError as e:
        print(f"Errore importazione modulo analisi: {e}")
        return False
    except Exception as e:
        print(f"Errore durante analisi: {e}")
        return False
    
    return True

def execute_codes():
    """Esegue i codici delle task comuni"""
    print("\nESECUZIONE CODICI")
    print("-" * 40)
    
    try:
        from src.smart_executor import SmartExecutor
        executor = SmartExecutor()
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"Errore importazione modulo esecutore: {e}")
        return False
    except Exception as e:
        print(f"Errore durante esecuzione: {e}")
        return False
    
    return True

def test_languages():
    """Testa la disponibilità di tutti i linguaggi"""
    print("\nTEST LINGUAGGI")
    print("-" * 40)
    
    try:
        from language_tester import LanguageTester
        tester = LanguageTester()
        tester.test_all_languages()
        
        # Mostra risultati dettagliati
        print("\n" + "="*60)
        print(" RISULTATI DETTAGLIATI DEL TEST")
        print("="*60)
        
        working = []
        not_working = []
        
        for language, result in tester.test_results.items():
            if result['available']:
                working.append(language)
            else:
                not_working.append(language)
        
        # Linguaggi funzionanti
        if working:
            print(f"\n[OK] LINGUAGGI FUNZIONANTI ({len(working)}/{len(tester.test_results)}):")
            for lang in sorted(working):
                print(f"   [OK] {lang.upper()}")
        
        # Linguaggi non funzionanti  
        if not_working:
            print(f"\n[FAIL] LINGUAGGI NON FUNZIONANTI ({len(not_working)}/{len(tester.test_results)}):")
            for lang in sorted(not_working):
                result = tester.test_results[lang]
                error_msg = result.get('error', 'Errore sconosciuto')[:60]
                print(f"   [FAIL] {lang.upper()}: {error_msg}")
        
        # Statistiche finali
        success_rate = len(working) / len(tester.test_results) * 100
        print(f"\n STATISTICHE:")
        print(f"   - Totale testati: {len(tester.test_results)}")
        print(f"   - Funzionanti: {len(working)}")
        print(f"   - Non funzionanti: {len(not_working)}")
        print(f"   - Tasso successo: {success_rate:.1f}%")
        
        if not_working:
            print(f"\n SUGGERIMENTI PER INSTALLAZIONE:")
            print(f"   Comandi per installare i linguaggi mancanti su Linux:")
            
            for lang in sorted(not_working):
                print(f"   - {lang.upper()}:", end=" ")
                
                if lang == 'c':
                    print("sudo apt install gcc build-essential")
                elif lang == 'cpp':
                    print("sudo apt install g++ build-essential")
                elif lang == 'java':
                    print("sudo apt install openjdk-11-jdk")
                elif lang == 'javascript':
                    print("sudo apt install nodejs")
                elif lang == 'python':
                    print("sudo apt install python3 python3-pip")
                elif lang == 'go':
                    print("sudo apt install golang-go")
                elif lang == 'rust':
                    print("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
                elif lang == 'csharp':
                    print("sudo apt install mono-complete")
                elif lang == 'ruby':
                    print("sudo apt install ruby-full")
                elif lang == 'php':
                    print("sudo apt install php-cli")
                elif lang == 'perl':
                    print("sudo apt install perl")
                elif lang == 'lua':
                    print("sudo apt install lua5.3")
                elif lang == 'swift':
                    print("Installazione complessa - vedi swift.org/download")
                elif lang == 'kotlin':
                    print("sudo snap install kotlin --classic")
                elif lang == 'dart':
                    print("sudo apt install dart")
                elif lang == 'scala':
                    print("sudo apt install scala")
                elif lang == 'haskell':
                    print("sudo apt install ghc")
                elif lang == 'ocaml':
                    print("sudo apt install ocaml")
                elif lang == 'fortran':
                    print("sudo apt install gfortran")
                elif lang == 'matlab':
                    print(" Licenza commerciale richiesta - mathworks.com")
                elif lang == 'r':
                    print("sudo apt install r-base")
                elif lang == 'julia':
                    print("wget https://julialang.org/downloads/ (installazione manuale)")
                else:
                    print(f"sudo apt install {lang} (verificare nome pacchetto)")
            
            print(f"\n  Su altri sistemi operativi:")
            print(f"   • macOS: usa 'brew install <linguaggio>'")
            print(f"   • Windows: usa Windows Package Manager 'winget install'")
            print(f"   • Arch Linux: usa 'pacman -S <linguaggio>'")
            print(f"   • Fedora/RHEL: usa 'dnf install <linguaggio>'")
            
            print(f"\n  Nota: Alcuni linguaggi potrebbero richiedere riavvio del terminale")
        
        print("\n" + "="*60)
        
    except ImportError as e:
        print(f"Errore importazione modulo test: {e}")
        return False
    except Exception as e:
        print(f"Errore durante test: {e}")
        return False
    
    return True

def smart_execute():
    """Esegue le TOP 10 task comuni solo nei linguaggi testati e disponibili"""
    print("\nESECUZIONE ADATTIVA")
    print("-" * 40)
    
    try:
        from src.smart_executor import SmartExecutor
        executor = SmartExecutor()
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"Errore importazione modulo smart executor: {e}")
        return False
    except Exception as e:
        print(f"Errore durante esecuzione adattiva: {e}")
        return False
    
    return True

def benchmark_carbon(mode=None):
    """Esegue benchmark CO2 con ripetizioni multiple con input interattivo"""
    print("\n CARBON BENCHMARK - Sistema di Misurazione CO2")
    print("=" * 55)
    
    try:
        from src.carbon_benchmark import CarbonBenchmark
        
        # Mostra opzioni disponibili
        print("Modalità benchmark disponibili:")
        print()
        print("  TOP10 - Analisi Task Principali")
        print("   • Solo le top 10 task più frequenti")
        print("   • 30 ripetizioni per task per calcolare la media")
        print("   • Tempo stimato: ~45-60 minuti")
        print("   • Ideale per analisi regolari")
        print()
        print("  VELOCE - Test Funzionalità")
        print("   • Solo 3 task di esempio")
        print("   • 3 ripetizioni per task")
        print("   • Tempo stimato: ~3-5 minuti")
        print("   • Perfetto per verificare che tutto funzioni")
        print()
        print("  COMPLETO - Analisi Esaustiva")
        print("   • TUTTE le task del dataset (1000+ task)")
        print("   • 3 ripetizioni per task")
        print("   • Tempo stimato: ~6-8 ore")
        print("   • Copertura completa per ricerca scientifica")
        print()
        
        # Gestione input
        if mode is None:
            print("Seleziona modalità:")
            choice = input("Inserisci [1/top10] [2/veloce] [3/completo] (default: top10): ").strip().lower()
        else:
            choice = mode.lower()
            print(f"Modalità selezionata da parametro: {choice}")
        
        # Configurazione modalità
        if choice in ["2", "veloce", "speed", "fast"]:
            iterations = 3
            max_tasks = 3
            mode_name = "VELOCE"
            description = "Test funzionalità - 3 task, 3 ripetizioni"
        elif choice in ["3", "completo", "full", "complete", "tutto"]:
            iterations = 3
            max_tasks = None  # Tutte le task disponibili
            mode_name = "COMPLETO"
            description = "Analisi esaustiva - tutte le task, 3 ripetizioni"
        else:
            # Default: top10
            iterations = 30
            max_tasks = 10
            mode_name = "TOP10"
            description = "Analisi task principali - 10 task, 30 ripetizioni"
        
        # Conferma configurazione
        print(f"\n MODALITÀ SELEZIONATA: {mode_name}")
        print(f" {description}")
        print(f" Configurazione: {iterations} iterazioni per task")
        print(f" Task da testare: {'TUTTE le disponibili' if max_tasks is None else f'Prime {max_tasks} task'}")
        
        # Conferma dall'utente
        if mode is None:  # Solo se interattivo
            print()
            try:
                confirm = input("Procedere con il benchmark? [s/N]: ").strip().lower()
                if confirm not in ['s', 'si', 'sì', 'y', 'yes']:
                    print("❌ Benchmark annullato dall'utente")
                    return False
            except EOFError:
                # Input non interattivo, procedi automaticamente
                print(" Modalità non-interattiva rilevata, procedo automaticamente...")
                pass
        
        print(f"\n Avvio benchmark in modalità {mode_name}...")
        print(" Usa Ctrl+C per interrompere in qualsiasi momento")
        
        # Avvia benchmark
        benchmark = CarbonBenchmark(iterations=iterations)
        
        # Seleziona il metodo di benchmark in base alla modalità
        if max_tasks is None:  # Modalità COMPLETO
            print(" Modalità COMPLETO: utilizzando TUTTE le task del dataset")
            benchmark.benchmark_all_tasks()
        else:  # Modalità TOP10 o VELOCE
            print(f" Modalità {mode_name}: utilizzando task comuni")
            benchmark.benchmark_common_tasks(max_tasks=max_tasks)
        
    except ImportError as e:
        print(f"Errore importazione modulo benchmark: {e}")
        return False
    except KeyboardInterrupt:
        print("\nBenchmark interrotto dall'utente")
        return False
    except Exception as e:
        print(f"Errore durante benchmark: {e}")
        return False
    
    return True

def clean_project():
    """Pulisce file temporanei e cache del progetto"""
    print("\nPULIZIA PROGETTO")
    print("-" * 40)
    
    try:
        import shutil
        import glob
        
        cleaned_files = 0
        cleaned_dirs = 0
        
        # File temporanei da rimuovere
        temp_patterns = [
            "results/execution/temp_*",
            "results/execution/*.class",
            "results/execution/a.out",
            "**/__pycache__",
            "**/*.pyc",
            "**/*.pyo",
            "**/*.o",
            "**/*.obj",
            "**/*.exe",
            "**/*.out",
            "**/*.class",
            "**/*.hi",
            "**/*.cmo",
            "**/*.cmi"
        ]
        
        print("Rimozione file temporanei...")
        
        for pattern in temp_patterns:
            files = glob.glob(pattern, recursive=True)
            for file_path in files:
                try:
                    if os.path.isfile(file_path):
                        os.remove(file_path)
                        cleaned_files += 1
                        print(f"  Rimosso: {file_path}")
                    elif os.path.isdir(file_path):
                        shutil.rmtree(file_path)
                        cleaned_dirs += 1
                        print(f"  Rimossa directory: {file_path}")
                except Exception as e:
                    print(f"  Errore rimozione {file_path}: {e}")
        
        # Pulizia cache specifiche
        cache_dirs = [
            ".pytest_cache",
            "node_modules",
            "target",  # Rust
            "dist",    # Go/Python
            "_build"   # OCaml
        ]
        
        print("\nPulizia cache...")
        for cache_dir in cache_dirs:
            if os.path.exists(cache_dir):
                try:
                    shutil.rmtree(cache_dir)
                    cleaned_dirs += 1
                    print(f"  Rimossa cache: {cache_dir}")
                except Exception as e:
                    print(f"  Errore rimozione cache {cache_dir}: {e}")
        
        print(f"\nPulizia completata!")
        print(f"File rimossi: {cleaned_files}")
        print(f" Directory rimosse: {cleaned_dirs}")
        
        # Pulizia duplicati CSV in results/
        print("\nPulizia duplicati CSV...")
        try:
            # Import del modulo cleaner se esiste
            if os.path.exists("src/cleaner.py"):
                sys.path.insert(0, 'src')
                import cleaner
                csv_removed = cleaner.cleanup_csv_duplicates()
                print(f"File CSV duplicati rimossi: {csv_removed}")
            elif os.path.exists("cleaner.py"):
                import cleaner
                csv_removed = cleaner.cleanup_csv_duplicates()
                print(f"File CSV duplicati rimossi: {csv_removed}")
            else:
                print("Script cleaner.py non trovato")
        except Exception as e:
            print(f"Errore pulizia CSV: {e}")
        
        return True
            
    except Exception as e:
        print(f"Errore durante pulizia: {e}")
        return False

def show_status():
    """Mostra lo stato del progetto"""
    print("\nSTATO PROGETTO")
    print("-" * 40)
    
    # Verifica directory principali
    directories = [
        "data/generated/code_snippets",
        "results/task_analysis", 
        "results/execution",
        "modules",
        "src"
    ]
    
    print("Directory:")
    for directory in directories:
        path = Path(directory)
        if path.exists():
            if path.is_dir():
                file_count = len(list(path.rglob("*")))
                print(f"  {directory} ({file_count} file)")
            else:
                print(f"   {directory} (non è una directory)")
        else:
            print(f"  {directory} (non trovata)")
    
    # Verifica file di analisi
    analysis_files = [
        "results/task_analysis/common_tasks.json",
        "results/task_analysis/dependency_analysis.json"
    ]
    
    print("\nFile di Analisi:")
    for file_path in analysis_files:
        path = Path(file_path)
        if path.exists():
            size = path.stat().st_size
            print(f"  {file_path} ({size} byte)")
        else:
            print(f"  {file_path} (non trovato)")
    
    # Conta task disponibili
    task_count = 0
    common_tasks_file = Path("results/task_analysis/common_tasks.json")
    if common_tasks_file.exists():
        try:
            import json
            with open(common_tasks_file, 'r') as f:
                data = json.load(f)
                task_count = data.get('total_tasks', 0)
        except:
            pass
    
    print(f"\nTask Analizzate: {task_count} (esegui 'analyze' prima)")
    
    # Verifica risultati esecuzione
    exec_dir = Path("results/execution")
    if exec_dir.exists():
        exec_files = list(exec_dir.glob("execution_results_*.json"))
        print(f"\nEsecuzioni Completate: {len(exec_files)}")
        if exec_files:
            latest = max(exec_files, key=lambda x: x.stat().st_mtime)
            print(f"  Ultima esecuzione: {latest.name}")
    else:
        print("\nEsecuzioni Completate: 0 (esegui 'execute' prima)")

def quality_analysis():
    """Esegue analisi qualitativa avanzata del dataset"""
    print("\\nANALISI QUALITATIVA AVANZATA")
    print("-" * 40)
    
    try:
        from src.finder import UnifiedTaskFinder
        finder = UnifiedTaskFinder()
        
        print(" Creazione dataset con analisi qualitativa...")
        
        # Crea DataFrame con quality analysis (output conciso)
        finder.create_dataset_dataframe(include_quality_analysis=True, verbose=False)
        
        if finder.df is None or len(finder.df) == 0:
            print("❌ Nessun dato trovato per l'analisi")
            return False
        
        # Analisi task di alta qualità
        print("\\n TASK DI ALTA QUALITÀ")
        quality_tasks = finder.find_common_tasks(
            min_languages=5,
            include_quality=True,
            min_quality_score=70,
            verbose=False
        )
        
        if quality_tasks:
            print(f"Le migliori {min(5, len(quality_tasks))} task per qualità:")
            for i, task in enumerate(quality_tasks[:5], 1):
                quality_score = task.get('avg_quality_score', 0)
                print(f"  {i}. {task['name']} - {quality_score:.1f}/100 ({task['language_count']} linguaggi)")
        else:
            print("  Nessuna task di alta qualità trovata")
        
        # Statistiche generali (verbose per dettagli)
        print("\\n STATISTICHE QUALITATIVE")
        stats = finder.get_quality_statistics(verbose=True)
        
        # Suggerimenti per miglioramenti
        if stats:
            features = stats['feature_coverage']
            print("\\n SUGGERIMENTI MIGLIORAMENTO:")
            
            if features['has_error_handling_pct'] < 10:
                print(f"  Error handling molto basso ({features['has_error_handling_pct']:.1f}%) - Considerare task più robuste")
            
            if features['has_comments_pct'] < 30:
                print(f"  Documentazione scarsa ({features['has_comments_pct']:.1f}%) - Migliorare commenti")
            
            if features['has_functions_pct'] < 50:
                print(f" Codice poco strutturato ({features['has_functions_pct']:.1f}%) - Più funzioni modulari")
        
        print("\\n✅ Analisi qualitativa completata!")
        
    except ImportError as e:
        print(f"Errore importazione modulo finder: {e}")
        return False
    except Exception as e:
        print(f"Errore durante analisi qualitativa: {e}")
        return False
    
    return True

def main():
    """Funzione principale"""
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument(
        'command', 
        nargs='?',
        choices=['analyze', 'execute', 'smart', 'test', 'clean', 'status', 'carbon', 'benchmark', 'quality', 'find', 'help'],
        default='help',
        help='Comando da eseguire'
    )
    
    parser.add_argument(
        '--mode',
        choices=['veloce', 'top10', 'completo'],
        help='Modalità per il comando benchmark (veloce/top10/completo)'
    )
    
    parser.add_argument(
        '--task',
        help='Nome della task da cercare (per comando find)'
    )
    
    args = parser.parse_args()
    
    print_banner()
    
    if args.command == 'help':
        print_help()
    elif args.command == 'analyze':
        success = analyze_tasks()
        if success:
            print("\nAnalisi completata con successo!")
            print("Ora puoi eseguire 'python main.py execute' per testare i codici")
        else:
            print("\nAnalisi fallita")
            sys.exit(1)
    elif args.command == 'execute':
        success = execute_codes()
        if success:
            print("\nEsecuzione completata!")
            print("Controlla i risultati in results/execution/")
        else:
            print("\nEsecuzione fallita")
            sys.exit(1)
    elif args.command == 'smart':
        success = smart_execute()
        if success:
            print("\nEsecuzione adattiva completata!")
            print("Controlla i risultati in results/execution/")
        else:
            print("\nEsecuzione adattiva fallita")
            sys.exit(1)
    elif args.command == 'find':
        # Comando per ricerca e esecuzione mirata di task specifiche
        try:
            from src.task_searcher import search_and_execute_task
            success = search_and_execute_task(args.task)
            if not success:
                print("\nRicerca task fallita o cancellata")
        except ImportError:
            print("Task Searcher non disponibile")
            print("Verifica che tutti i moduli siano correttamente installati")
    elif args.command == 'test':
        success = test_languages()
        if success:
            print("\nTest linguaggi completato!")
            print("Controlla i risultati per vedere quali linguaggi sono disponibili")
        else:
            print("\nTest linguaggi fallito")
            sys.exit(1)
    elif args.command == 'clean':
        success = clean_project()
        if success:
            print("\nPulizia completata!")
        else:
            print("\nPulizia fallita")
            sys.exit(1)
    elif args.command == 'status':
        show_status()
    elif args.command == 'carbon':
        # Comando per visualizzare il report dell'impatto ambientale
        try:
            from src.carbon_tracker import print_carbon_report
            print_carbon_report()
        except ImportError:
            print("Carbon tracker non disponibile")
            print("Installa CodeCarbon con: pip install codecarbon")
    elif args.command == 'benchmark':
        # Comando per eseguire benchmark CO2 con ripetizioni multiple
        success = benchmark_carbon(mode=args.mode)
        if success:
            print("\nBenchmark CO2 completato!")
            print("Controlla i risultati in results/carbon_benchmark/")
        else:
            print("\nBenchmark CO2 fallito")
            sys.exit(1)
    elif args.command == 'quality':
        # Comando per analisi qualitativa avanzata
        success = quality_analysis()
        if success:
            print("\nAnalisi qualitativa completata!")
            print("I risultati mostrano la qualità del codice nel dataset")
        else:
            print("\nAnalisi qualitativa fallita")
            sys.exit(1)
    else:
        print(f"Comando non riconosciuto: {args.command}")
        print("Usa 'python main.py help' per vedere i comandi disponibili")

if __name__ == "__main__":
    main()
