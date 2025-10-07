#!/usr/bin/env python3
"""
Smart cleaning script for the results/ folder
Keeps only the most recent and important data
"""

import os
import glob
import shutil
from datetime import datetime, timedelta
from pathlib import Path


def cleanup_results():
    """Smart cleaning of the results folder"""
    results_dir = "results"

    print(" SMART CLEANING OF RESULTS FOLDER")
    print("=" * 50)

    # 1. Backup essential data
    backup_essential_data()

    # 2. Cleanup carbon/ (keep only last 30 days)
    cleanup_carbon_sessions()

    # 3. Cleanup carbon_benchmark/ (keep only 3 most recent)
    cleanup_carbon_benchmarks()

    # 4. Cleanup execution/ (remove temporary files)
    cleanup_execution_temp()

    # 5. Final report
    show_cleanup_report()


def backup_essential_data():
    """Backup essential data"""
    print("\n ESSENTIAL DATA BACKUP")

    backup_dir = "results_backup_essential"
    os.makedirs(backup_dir, exist_ok=True)

    # Backup main CSV files
    csv_files = glob.glob("results/*.csv")
    for csv_file in csv_files:
        shutil.copy2(csv_file, backup_dir)
        print(f" Backup: {os.path.basename(csv_file)}")

    # Backup summary files
    summary_files = glob.glob("results/carbon_benchmark/*summary*.json")
    summary_files.sort(key=os.path.getmtime, reverse=True)

    for i, summary in enumerate(summary_files[:3]):  # Last 3
        dest = os.path.join(backup_dir, f"summary_{i+1}_{os.path.basename(summary)}")
        shutil.copy2(summary, dest)
        print(f" Backup: {os.path.basename(summary)}")


def cleanup_carbon_sessions():
    """Remove old carbon sessions (keep only last 30 days)"""
    print("\n CLEANUP CARBON SESSIONS")

    carbon_dir = "results/carbon"
    if not os.path.exists(carbon_dir):
        return

    # Cutoff date (30 days ago)
    cutoff_date = datetime.now() - timedelta(days=30)

    session_files = glob.glob(f"{carbon_dir}/session_*.json")
    removed_count = 0

    for session_file in session_files:
        file_time = datetime.fromtimestamp(os.path.getmtime(session_file))
        if file_time < cutoff_date:
            os.remove(session_file)
            removed_count += 1

    print(f" Removed {removed_count} old sessions (>30 days)")
    print(f" Kept {len(session_files) - removed_count} recent sessions")


def cleanup_carbon_benchmarks():
    """Keep only the latest detailed file (largest)"""
    print("\n CLEANUP CARBON BENCHMARKS")

    benchmark_dir = "results/carbon_benchmark"
    if not os.path.exists(benchmark_dir):
        return

    # Find detailed files (the largest)
    detailed_files = glob.glob(f"{benchmark_dir}/carbon_benchmark_detailed_*.json")
    detailed_files.sort(key=os.path.getmtime, reverse=True)

    # Keep only the latest
    files_to_keep = detailed_files[:1]
    files_to_remove = detailed_files[1:]

    removed_size = 0
    for file_to_remove in files_to_remove:
        file_size = os.path.getsize(file_to_remove)
        removed_size += file_size
        os.remove(file_to_remove)
        print(f" Removed: {os.path.basename(file_to_remove)} ({file_size//1024//1024} MB)")

    print(f" Freed space: {removed_size//1024//1024} MB")
    print(f" Kept {len(files_to_keep)} recent benchmarks")


def cleanup_execution_temp():
    """Remove temporary files from execution/"""
    print("\n CLEANUP TEMPORARY FILES")

    execution_dir = "results/execution"
    if not os.path.exists(execution_dir):
        return

    # Temporary file patterns
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

    print(f" Removed {removed_count} temporary files")


def show_cleanup_report():
    """Show post-cleanup report"""
    print("\n POST-CLEANUP REPORT")
    print("=" * 30)

    # Current sizes
    import subprocess

    try:
        result = subprocess.run(['du', '-sh', 'results'],
                                capture_output=True, text=True)
        if result.returncode == 0:
            size = result.stdout.strip().split()[0]
            print(f" Size of results/: {size}")

        # Subdirectory details
        result = subprocess.run(['du', '-sh', 'results/*'],
                                shell=True, capture_output=True, text=True)
        if result.returncode == 0:
            print(" Subdirectory details:")
            for line in result.stdout.strip().split('\n'):
                if line.strip():
                    print(f" {line}")

    except Exception as e:
        print(f" Error calculating sizes: {e}")

    print("\n CLEANUP COMPLETED!")
    print(" Essential data has been saved to 'results_backup_essential/'")


if __name__ == "__main__":
    cleanup_results()