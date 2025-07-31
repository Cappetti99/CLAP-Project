#!/usr/bin/env python3
"""
SWAM Project - Main Entry Point
Sistema completo per l'analisi e esecuzione di codici multi-linguaggio
"""

import sys
import os
import argparse
from pathlib import Path

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
modules_path = os.path.join(script_dir, 'modules')
src_path = os.path.join(script_dir, 'src')
sys.path.insert(0, modules_path)
sys.path.insert(0, src_path)

def print_banner():
    """Stampa il banner del progetto"""
    print("=" * 60)
    print("🌊 SWAM PROJECT - Cross-Language Code Analysis System")
    print("   Sistema per Analisi e Esecuzione Multi-Linguaggio")
    print("=" * 60)

def print_help():
    """Stampa l'aiuto per l'utilizzo"""
    print("\n📋 COMANDI DISPONIBILI:")
    print("  analyze    - Analizza le task comuni tra linguaggi")
    print("  execute    - Esegue i codici delle task comuni (tutti i linguaggi)")
    print("  smart      - Esegue i codici solo nei linguaggi disponibili")
    print("  test       - Testa la disponibilità di tutti i linguaggi")
    print("  clean      - Pulisce i file temporanei e cache")
    print("  status     - Mostra lo stato del progetto")
    print("  help       - Mostra questo aiuto")
    print("\n📖 ESEMPI D'USO:")
    print("  python main.py test      # Prima verifica i linguaggi disponibili")
    print("  python main.py smart     # Esegue solo i linguaggi funzionanti")
    print("  python main.py analyze")
    print("  python main.py status")

def analyze_tasks():
    """Esegue l'analisi delle task comuni"""
    print("\n🔍 ANALISI TASK COMUNI")
    print("-" * 40)
    
    try:
        from advanced_task_finder import AdvancedTaskFinder
        finder = AdvancedTaskFinder()
        common_tasks = finder.find_common_tasks(min_languages=8)
        
        if common_tasks:
            print(f"✅ Trovate {len(common_tasks)} task comuni")
            for task in common_tasks[:10]:  # Mostra prime 10
                print(f"  • {task['name']} ({task['language_count']} linguaggi)")
        else:
            print("❌ Nessuna task comune trovata")
            
    except ImportError as e:
        print(f"❌ Errore importazione modulo analisi: {e}")
        return False
    except Exception as e:
        print(f"❌ Errore durante analisi: {e}")
        return False
    
    return True

def execute_codes():
    """Esegue i codici delle task comuni"""
    print("\n🚀 ESECUZIONE CODICI")
    print("-" * 40)
    
    try:
        from enhanced_executor import EnhancedExecutor
        executor = EnhancedExecutor()
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"❌ Errore importazione modulo esecutore: {e}")
        return False
    except Exception as e:
        print(f"❌ Errore durante esecuzione: {e}")
        return False
    
    return True

def test_languages():
    """Testa la disponibilità di tutti i linguaggi"""
    print("\n🧪 TEST LINGUAGGI")
    print("-" * 40)
    
    try:
        from language_tester import LanguageTester
        tester = LanguageTester()
        tester.test_all_languages()
    except ImportError as e:
        print(f"❌ Errore importazione modulo test: {e}")
        return False
    except Exception as e:
        print(f"❌ Errore durante test: {e}")
        return False
    
    return True

def smart_execute():
    """Esegue i codici usando l'esecutore intelligente"""
    print("\n🧠 ESECUZIONE INTELLIGENTE")
    print("-" * 40)
    
    try:
        from smart_executor import SmartExecutor
        executor = SmartExecutor()
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"❌ Errore importazione modulo smart executor: {e}")
        return False
    except Exception as e:
        print(f"❌ Errore durante esecuzione intelligente: {e}")
        return False
    
    return True

def clean_project():
    """Pulisce file temporanei e cache del progetto"""
    print("\n🧹 PULIZIA PROGETTO")
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
        
        print("🗑️ Rimozione file temporanei...")
        
        for pattern in temp_patterns:
            files = glob.glob(pattern, recursive=True)
            for file_path in files:
                try:
                    if os.path.isfile(file_path):
                        os.remove(file_path)
                        cleaned_files += 1
                        print(f"  ✅ Rimosso: {file_path}")
                    elif os.path.isdir(file_path):
                        shutil.rmtree(file_path)
                        cleaned_dirs += 1
                        print(f"  ✅ Rimossa directory: {file_path}")
                except Exception as e:
                    print(f"  ⚠️ Errore rimozione {file_path}: {e}")
        
        # Pulizia cache specifiche
        cache_dirs = [
            ".pytest_cache",
            "node_modules",
            "target",  # Rust
            "dist",    # Go/Python
            "_build"   # OCaml
        ]
        
        print("\n🧹 Pulizia cache...")
        for cache_dir in cache_dirs:
            if os.path.exists(cache_dir):
                try:
                    shutil.rmtree(cache_dir)
                    cleaned_dirs += 1
                    print(f"  ✅ Rimossa cache: {cache_dir}")
                except Exception as e:
                    print(f"  ⚠️ Errore rimozione cache {cache_dir}: {e}")
        
        print(f"\n✅ Pulizia completata!")
        print(f"📁 File rimossi: {cleaned_files}")
        print(f"📂 Directory rimosse: {cleaned_dirs}")
        
        return True
            
    except Exception as e:
        print(f"❌ Errore durante pulizia: {e}")
        return False

def show_status():
    """Mostra lo stato del progetto"""
    print("\n📊 STATO PROGETTO")
    print("-" * 40)
    
    # Verifica directory principali
    directories = [
        "data/generated/code_snippets",
        "results/task_analysis", 
        "results/execution",
        "modules",
        "src"
    ]
    
    print("📁 Directory:")
    for directory in directories:
        path = Path(directory)
        if path.exists():
            if path.is_dir():
                file_count = len(list(path.rglob("*")))
                print(f"  ✅ {directory} ({file_count} file)")
            else:
                print(f"  ⚠️  {directory} (non è una directory)")
        else:
            print(f"  ❌ {directory} (non trovata)")
    
    # Verifica file di analisi
    analysis_files = [
        "results/task_analysis/common_tasks.json",
        "results/task_analysis/dependency_analysis.json"
    ]
    
    print("\n📄 File di Analisi:")
    for file_path in analysis_files:
        path = Path(file_path)
        if path.exists():
            size = path.stat().st_size
            print(f"  ✅ {file_path} ({size} byte)")
        else:
            print(f"  ❌ {file_path} (non trovato)")
    
    # Conta task disponibili
    task_dir = Path("results/task_analysis/code_snippets")
    if task_dir.exists():
        task_count = len([d for d in task_dir.iterdir() if d.is_dir()])
        print(f"\n🎯 Task Analizzate: {task_count}")
        
        # Mostra prime 5 task
        tasks = sorted([d.name for d in task_dir.iterdir() if d.is_dir()])[:5]
        for task in tasks:
            print(f"  • {task}")
        if len(tasks) == 5 and task_count > 5:
            print(f"  ... e altre {task_count - 5} task")
    else:
        print("\n🎯 Task Analizzate: 0 (esegui 'analyze' prima)")
    
    # Verifica risultati esecuzione
    exec_dir = Path("results/execution")
    if exec_dir.exists():
        exec_files = list(exec_dir.glob("execution_results_*.json"))
        print(f"\n🏃 Esecuzioni Completate: {len(exec_files)}")
        if exec_files:
            latest = max(exec_files, key=lambda x: x.stat().st_mtime)
            print(f"  📅 Ultima esecuzione: {latest.name}")
    else:
        print("\n🏃 Esecuzioni Completate: 0 (esegui 'execute' prima)")

def main():
    """Funzione principale"""
    parser = argparse.ArgumentParser(
        description="SWAM Project - Sistema per Analisi e Esecuzione Multi-Linguaggio",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument(
        'command', 
        nargs='?',
        choices=['analyze', 'execute', 'smart', 'test', 'clean', 'status', 'help'],
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
            print("\n✅ Analisi completata con successo!")
            print("💡 Ora puoi eseguire 'python main.py execute' per testare i codici")
        else:
            print("\n❌ Analisi fallita")
            sys.exit(1)
    elif args.command == 'execute':
        success = execute_codes()
        if success:
            print("\n✅ Esecuzione completata!")
            print("💡 Controlla i risultati in results/execution/")
        else:
            print("\n❌ Esecuzione fallita")
            sys.exit(1)
    elif args.command == 'smart':
        success = smart_execute()
        if success:
            print("\n✅ Esecuzione intelligente completata!")
            print("💡 Controlla i risultati in results/execution/")
        else:
            print("\n❌ Esecuzione intelligente fallita")
            sys.exit(1)
    elif args.command == 'test':
        success = test_languages()
        if success:
            print("\n✅ Test linguaggi completato!")
            print("💡 Controlla i risultati per vedere quali linguaggi sono disponibili")
        else:
            print("\n❌ Test linguaggi fallito")
            sys.exit(1)
    elif args.command == 'clean':
        success = clean_project()
        if success:
            print("\n✅ Pulizia completata!")
        else:
            print("\n❌ Pulizia fallita")
            sys.exit(1)
    elif args.command == 'status':
        show_status()
    
    print("\n" + "=" * 60)

if __name__ == "__main__":
    main()
