#!/usr/bin/env python3
"""
Script di pulizia intelligente per cartella results/
Mantiene solo i dati più recenti e importanti
"""

import os
import glob
import shutil
from datetime import datetime, timedelta
from pathlib import Path


def cleanup_results():
    """Pulizia intelligente della cartella results"""
    results_dir = "results"

    print(" PULIZIA INTELLIGENTE CARTELLA RESULTS")
    print("=" * 50)

    # 1. Backup cartella importante
    backup_essential_data()

    # 2. Pulizia carbon/ (mantieni solo ultimi 30 giorni)
    cleanup_carbon_sessions()

    # 3. Pulizia carbon_benchmark/ (mantieni solo 3 più recenti)
    cleanup_carbon_benchmarks()

    # 4. Pulizia execution/ (rimuovi file temporanei)
    cleanup_execution_temp()

    # 5. Report finale
    show_cleanup_report()


def backup_essential_data():
    """Backup dei dati essenziali"""
    print("\n BACKUP DATI ESSENZIALI")

    backup_dir = "results_backup_essential"
    os.makedirs(backup_dir, exist_ok=True)

    # Backup CSV principali
    csv_files = glob.glob("results/*.csv")
    for csv_file in csv_files:
        shutil.copy2(csv_file, backup_dir)
        print(f" Backup: {os.path.basename(csv_file)}")

    # Backup summary più recenti
    summary_files = glob.glob("results/carbon_benchmark/*summary*.json")
    summary_files.sort(key=os.path.getmtime, reverse=True)

    for i, summary in enumerate(summary_files[:3]):  # Ultimi 3
        dest = os.path.join(backup_dir, f"summary_{i+1}_{os.path.basename(summary)}")
        shutil.copy2(summary, dest)
        print(f" Backup: {os.path.basename(summary)}")


def cleanup_carbon_sessions():
    """Rimuovi sessioni carbon vecchie (mantieni ultimi 30 giorni)"""
    print("\n PULIZIA SESSIONI CARBON")

    carbon_dir = "results/carbon"
    if not os.path.exists(carbon_dir):
        return

    # Data limite (30 giorni fa)
    cutoff_date = datetime.now() - timedelta(days=30)

    session_files = glob.glob(f"{carbon_dir}/session_*.json")
    removed_count = 0

    for session_file in session_files:
        file_time = datetime.fromtimestamp(os.path.getmtime(session_file))
        if file_time < cutoff_date:
            os.remove(session_file)
            removed_count += 1

    print(f" Rimosse {removed_count} sessioni vecchie (>30 giorni)")
    print(f" Mantenute {len(session_files) - removed_count} sessioni recenti")


def cleanup_carbon_benchmarks():
    """Mantieni solo l'ultimo file detailed (più pesante)"""
    print("\n PULIZIA BENCHMARK CARBON")

    benchmark_dir = "results/carbon_benchmark"
    if not os.path.exists(benchmark_dir):
        return

    # Trova file detailed (i più grandi)
    detailed_files = glob.glob(f"{benchmark_dir}/carbon_benchmark_detailed_*.json")
    detailed_files.sort(key=os.path.getmtime, reverse=True)

    # Mantieni solo il più recente
    files_to_keep = detailed_files[:1]
    files_to_remove = detailed_files[1:]

    removed_size = 0
    for file_to_remove in files_to_remove:
        file_size = os.path.getsize(file_to_remove)
        removed_size += file_size
        os.remove(file_to_remove)
        print(f" Rimosso: {os.path.basename(file_to_remove)} ({file_size//1024//1024} MB)")

    print(f" Spazio liberato: {removed_size//1024//1024} MB")
    print(f" Mantenuti {len(files_to_keep)} benchmark recenti")


def cleanup_execution_temp():
    """Rimuovi file temporanei da execution/"""
    print("\n PULIZIA FILE TEMPORANEI")

    execution_dir = "results/execution"
    if not os.path.exists(execution_dir):
        return

    # Pattern file temporanei
    temp_patterns = [
        "temp_*.hi", "temp_*.o", "temp_*.cmi", "temp_*.cmo",
        "*.dat", "test.txt", "filename.txt"
    ]

    removed_count = 0
    for pattern in temp_patterns:
        temp_files = glob.glob(f"{execution_dir}/{pattern}")
        for temp_file in temp_files:
            os.remove(temp_file)
            removed_count += 1

    print(f" Rimossi {removed_count} file temporanei")


def show_cleanup_report():
    """Mostra report post-pulizia"""
    print("\n REPORT POST-PULIZIA")
    print("=" * 30)

    # Dimensioni attuali
    import subprocess

    try:
        result = subprocess.run(['du', '-sh', 'results'],
                                capture_output=True, text=True)
        if result.returncode == 0:
            size = result.stdout.strip().split()[0]
            print(f" Dimensione results/: {size}")

        # Dettaglio sottocartelle
        result = subprocess.run(['du', '-sh', 'results/*'],
                                shell=True, capture_output=True, text=True)
        if result.returncode == 0:
            print(" Dettaglio cartelle:")
            for line in result.stdout.strip().split('\n'):
                if line.strip():
                    print(f" {line}")

    except Exception as e:
        print(f" Errore calcolo dimensioni: {e}")

    print("\n PULIZIA COMPLETATA!")
    print(" I dati essenziali sono salvati in 'results_backup_essential/'")


if __name__ == "__main__":
    cleanup_results()