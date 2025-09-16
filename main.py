#!/usr/bin/env python3
"""
SWAM Project - Main Entry Point
Sistema completo per l'analisi e esecuzione di codici multi-linguaggio con monitoraggio CO2

Version: 2.0.0
Author: Lorenzo Cappetti
Features: Multi-language analysis, Smart execution, CO2 tracking, Statistical benchmarking
"""

__version__ = "2.0.0"
__author__ = "Lorenzo Cappetti"

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
    print("=" * 65)
    print("SWAM PROJECT - Cross-Language Code Analysis System")
    print("   Sistema per Analisi e Esecuzione Multi-Linguaggio + CO2 Tracking")
    print("   Author: Lorenzo Cappetti | New: CodeCarbon Integration")
    print("=" * 65)

def print_help():
    """Stampa l'aiuto completo per l'utilizzo del sistema"""
    print("\nCOMANDI DISPONIBILI:")
    print("  analyze    - Analizza task comuni tra linguaggi (completa)")
    print("  execute    - Esegue codici task comuni (tutti i linguaggi)")
    print("  smart      - Esegue codici solo nei linguaggi disponibili")
    print("  test       - Testa disponibilità di tutti i linguaggi")
    print("  clean      - Pulisce file temporanei e cache")
    print("  status     - Mostra stato dettagliato del progetto")
    print("  carbon     - Report impatto ambientale (CodeCarbon)")
    print("  benchmark  - Benchmark CO2 statistico (30 run per precisione)")
    print("  install    - Installa dipendenze del progetto")
    print("  help       - Mostra questo aiuto completo")
    print("\nESEMPI D'USO:")
    print("  python main.py test        # Prima verifica i linguaggi")
    print("  python main.py analyze     # Analizza task comuni")
    print("  python main.py smart       # Esegue solo linguaggi funzionanti")
    print("  python main.py benchmark   # Benchmark preciso CO2 (30 esecuzioni)")
    print("  python main.py carbon      # Visualizza emissioni CO2")
    print("  python main.py install     # Installa codecarbon e deps")
    print("  python main.py status      # Stato completo progetto")

    print("\nSUGGERIMENTI:")
    print("  • Usa 'test' prima del primo run per verificare linguaggi")
    print("  • 'smart' è raccomandato per esecuzioni quotidiane")
    print("  • 'benchmark' per dati statistici accurati (più lento)")
    print("  • 'carbon' per monitoring ambientale continuo")

def analyze_tasks():
    """Esegue l'analisi delle task comuni"""
    print("\nANALISI TASK COMUNI")
    print("-" * 40)
    
    try:
        from advanced_task_finder import AdvancedTaskFinder
        finder = AdvancedTaskFinder()
        common_tasks = finder.find_common_tasks(min_languages=8)
        
        if common_tasks:
            print(f"Trovate {len(common_tasks)} task comuni")
            for task in common_tasks[:10]:  # Mostra prime 10
                print(f"  • {task['name']} ({task['language_count']} linguaggi)")
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
        from enhanced_executor import EnhancedExecutor
        executor = EnhancedExecutor()
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
    except ImportError as e:
        print(f"Errore importazione modulo test: {e}")
        return False
    except Exception as e:
        print(f"Errore durante test: {e}")
        return False
    
    return True

def smart_execute():
    """Esegue i codici usando l'esecutore intelligente"""
    print("\nESECUZIONE INTELLIGENTE")
    print("-" * 40)
    
    try:
        from smart_executor import SmartExecutor
        executor = SmartExecutor()
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"Errore importazione modulo smart executor: {e}")
        return False
    except Exception as e:
        print(f"Errore durante esecuzione intelligente: {e}")
        return False
    
    return True

def benchmark_carbon():
    """Esegue benchmark CO2 con ripetizioni multiple"""
    print("\nCARBON BENCHMARK")
    print("-" * 40)
    
    try:
        from carbon_benchmark import CarbonBenchmark
        
        # Chiedi all'utente il numero di iterazioni
        print("Configurazione benchmark:")
        print("  • Standard: 30 iterazioni (raccomandato per precisione)")
        print("  • Veloce: 10 iterazioni (per test rapidi)")
        print("  • Test: 5 iterazioni (per debug)")
        print("  • Debug: 3 iterazioni (super veloce)")
        
        choice = input("\nScegli modalità [standard/veloce/test/debug] (default: standard): ").strip().lower()
        
        if choice == "debug":
            iterations = 3
            max_tasks = 2
        elif choice == "veloce":
            iterations = 10
            max_tasks = 3
        elif choice == "test":
            iterations = 5
            max_tasks = 2
        else:
            iterations = 30
            max_tasks = 5
        
        print(f"\nConfigurazione: {iterations} iterazioni, {max_tasks} task")
        
        benchmark = CarbonBenchmark(iterations=iterations)
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
        print(f"📂 Directory rimosse: {cleaned_dirs}")
        
        # Pulizia duplicati CSV in results/
        print("\nPulizia duplicati CSV...")
        try:
            # Import del modulo cleanup_results se esiste
            if os.path.exists("src/cleanup_results.py"):
                sys.path.insert(0, 'src')
                import cleanup_results
                csv_removed = cleanup_results.cleanup_csv_duplicates()
                print(f"File CSV duplicati rimossi: {csv_removed}")
            elif os.path.exists("cleanup_results.py"):
                import cleanup_results
                csv_removed = cleanup_results.cleanup_csv_duplicates()
                print(f"File CSV duplicati rimossi: {csv_removed}")
            else:
                print("Script cleanup_results.py non trovato")
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
    task_dir = Path("results/task_analysis/code_snippets")
    if task_dir.exists():
        task_count = len([d for d in task_dir.iterdir() if d.is_dir()])
        print(f"\nTask Analizzate: {task_count}")
        
        # Mostra prime 5 task
        tasks = sorted([d.name for d in task_dir.iterdir() if d.is_dir()])[:5]
        for task in tasks:
            print(f"  • {task}")
        if len(tasks) == 5 and task_count > 5:
            print(f"  ... e altre {task_count - 5} task")
    else:
        print("\nTask Analizzate: 0 (esegui 'analyze' prima)")
    
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

def main():
    """Funzione principale"""
    parser = argparse.ArgumentParser(
        description="SWAM Project - Sistema per Analisi e Esecuzione Multi-Linguaggio",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument(
        'command', 
        nargs='?',
        choices=['analyze', 'execute', 'smart', 'test', 'clean', 'status', 'carbon', 'install', 'benchmark', 'help'],
        default='help',
        help='Comando da eseguire'
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
            print("\nEsecuzione intelligente completata!")
            print("Controlla i risultati in results/execution/")
        else:
            print("\nEsecuzione intelligente fallita")
            sys.exit(1)
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
        success = benchmark_carbon()
        if success:
            print("\nBenchmark CO2 completato!")
            print("Controlla i risultati in results/carbon_benchmark/")
        else:
            print("\nBenchmark CO2 fallito")
            sys.exit(1)
    elif args.command == 'install':
        # Comando per installare le dipendenze
        print("\nINSTALLAZIONE DIPENDENZE SWAM")
        print("=" * 50)
        
        # Controlla se siamo in environment conda
        conda_env = os.environ.get('CONDA_DEFAULT_ENV')
        if conda_env:
            print(f"Ambiente conda attivo: {conda_env}")
        else:
            print("Nessun ambiente conda rilevato")
            
        # Installa requirements
        import subprocess
        try:
            subprocess.run([sys.executable, "-m", "pip", "install", "-r", "requirements.txt"], check=True)
            print("Dipendenze installate con successo")
        except subprocess.CalledProcessError as e:
            print(f"Errore installazione: {e}")
    else:
        print(f"Comando non riconosciuto: {args.command}")
        print("Usa 'python main.py help' per vedere i comandi disponibili")
    
    print("\n" + "=" * 60)

if __name__ == "__main__":
    main()
