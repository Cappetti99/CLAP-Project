#!/usr/bin/env python3
"""
SWAM Results Cleanup - Mantiene solo i file più recenti
Rimuove automaticamente file CSV duplicati con timestamp diversi
"""

import os
import glob
from pathlib import Path

def cleanup_csv_duplicates(results_dir="results"):
    """Mantiene solo il file più recente per ogni tipo di CSV"""
    print("🧹 PULIZIA FILE CSV DUPLICATI")
    print("-" * 40)
    
    # Pattern di file da pulire
    patterns = [
        "common_tasks_*.csv",
        "task_statistics_*.csv", 
        "task_language_matrix_*.csv"
    ]
    
    total_removed = 0
    
    for pattern in patterns:
        pattern_path = os.path.join(results_dir, pattern)
        files = sorted(glob.glob(pattern_path), key=os.path.getmtime, reverse=True)
        
        if len(files) > 1:
            # Mantieni solo il più recente (primo nella lista ordinata)
            files_to_remove = files[1:]
            
            print(f"\n📄 Pattern: {pattern}")
            print(f"  📁 Trovati: {len(files)} file")
            print(f"  ✅ Mantieni: {os.path.basename(files[0])}")
            
            for file_to_remove in files_to_remove:
                try:
                    os.remove(file_to_remove)
                    print(f"  🗑️ Rimosso: {os.path.basename(file_to_remove)}")
                    total_removed += 1
                except OSError as e:
                    print(f"  ❌ Errore rimozione {file_to_remove}: {e}")
        else:
            print(f"\n📄 Pattern: {pattern}")
            if files:
                print(f"  ✅ Solo 1 file trovato: {os.path.basename(files[0])}")
            else:
                print(f"  ℹ️ Nessun file trovato")
    
    print(f"\n✅ Pulizia completata!")
    print(f"🗑️ File rimossi: {total_removed}")
    
    return total_removed

def cleanup_old_execution_results(results_dir="results", keep_recent=3):
    """Mantiene solo gli ultimi N risultati di esecuzione"""
    exec_dir = os.path.join(results_dir, "execution")
    if not os.path.exists(exec_dir):
        return 0
    
    print(f"\n🧹 PULIZIA RISULTATI ESECUZIONE")
    print("-" * 40)
    
    # Pattern di file esecuzione
    patterns = [
        "language_test_results_*.json",
        "execution_results_*.json",
        "smart_execution_results_*.json"
    ]
    
    total_removed = 0
    
    for pattern in patterns:
        pattern_path = os.path.join(exec_dir, pattern)
        files = sorted(glob.glob(pattern_path), key=os.path.getmtime, reverse=True)
        
        if len(files) > keep_recent:
            files_to_remove = files[keep_recent:]
            
            print(f"\n📄 Pattern: {pattern}")
            print(f"  📁 Trovati: {len(files)} file")
            print(f"  ✅ Mantieni: {keep_recent} più recenti")
            
            for file_to_remove in files_to_remove:
                try:
                    os.remove(file_to_remove)
                    print(f"  🗑️ Rimosso: {os.path.basename(file_to_remove)}")
                    total_removed += 1
                except OSError as e:
                    print(f"  ❌ Errore rimozione {file_to_remove}: {e}")
    
    return total_removed

def main():
    """Funzione principale di pulizia"""
    print("🌊 SWAM RESULTS CLEANUP")
    print("=" * 50)
    
    # Verifica che la directory results esista
    if not os.path.exists("results"):
        print("❌ Cartella 'results' non trovata")
        print("💡 Esegui prima: python main.py simple")
        return
    
    # Pulizia CSV duplicati
    csv_removed = cleanup_csv_duplicates()
    
    # Pulizia risultati esecuzione vecchi  
    exec_removed = cleanup_old_execution_results()
    
    # Riassunto finale
    total_removed = csv_removed + exec_removed
    print(f"\n🎉 PULIZIA COMPLETATA")
    print("=" * 50)
    print(f"🗑️ Totale file rimossi: {total_removed}")
    
    if total_removed == 0:
        print("✨ Cartella results già pulita!")
    else:
        print("✅ Spazio liberato e duplicati rimossi")
    
    # Mostra struttura finale
    print(f"\n📁 STRUTTURA FINALE:")
    for root, dirs, files in os.walk("results"):
        level = root.replace("results", "").count(os.sep)
        indent = " " * 2 * level
        print(f"{indent}{os.path.basename(root)}/")
        subindent = " " * 2 * (level + 1)
        for file in files:
            print(f"{subindent}{file}")

if __name__ == "__main__":
    main()
