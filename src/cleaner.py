#!/usr/bin/env python3
"""Smart Cleanup System for CLAP Project"""

import os
import glob
import shutil
from datetime import datetime, timedelta
from collections import defaultdict


def cleanup_carbon_sessions(days_to_keep=2, dry_run=True):
    """Clean up session files, keeping only those from the last N days."""
    carbon_dir = "results/carbon"
    
    if not os.path.exists(carbon_dir):
        print(f"⚠️  Directory {carbon_dir} not found")
        return 0
    
    cutoff_date = datetime.now() - timedelta(days=days_to_keep)
    cutoff_timestamp = cutoff_date.timestamp()
    
    print(f"\n🗑️  CLEANUP CARBON SESSIONS")
    print(f"📅 Cutoff date: {cutoff_date.strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 50)
    
    session_files = glob.glob(os.path.join(carbon_dir, "session_*.json"))
    print(f"📊 Session files found: {len(session_files)}")
    
    files_to_delete = [(f, datetime.fromtimestamp(os.path.getmtime(f))) 
                       for f in session_files 
                       if os.path.getmtime(f) < cutoff_timestamp]
    
    print(f"🗑️  Files to delete: {len(files_to_delete)}")
    
    if not files_to_delete:
        print("\n✨ No files to delete.")
        return 0
    
    total_size = sum(os.path.getsize(f) for f, _ in files_to_delete)
    print(f"💾 Size to free: {total_size / (1024 * 1024):.2f} MB")
    
    if dry_run:
        print("\n⚠️  DRY-RUN MODE: No files deleted")
        print("💡 Use --execute to actually delete")
        return len(files_to_delete)
    
    deleted_count = 0
    for file_path, _ in files_to_delete:
        try:
            os.remove(file_path)
            deleted_count += 1
        except Exception as e:
            print(f"❌ Error: {e}")
    
    print(f"✅ Deleted {deleted_count} files")
    return deleted_count


def show_session_statistics():
    """Show session file statistics."""
    carbon_dir = "results/carbon"
    
    if not os.path.exists(carbon_dir):
        print(f"⚠️  Directory {carbon_dir} not found")
        return
    
    print("\n📊 SESSION FILE STATISTICS")
    print("=" * 50)
    
    session_files = glob.glob(os.path.join(carbon_dir, "session_*.json"))
    
    if not session_files:
        print("📭 No session files found")
        return
    
    files_by_date = defaultdict(list)
    for f in session_files:
        date = datetime.fromtimestamp(os.path.getmtime(f)).date()
        files_by_date[date].append(f)
    
    total_files = len(session_files)
    total_size = sum(os.path.getsize(f) for f in session_files)
    
    print(f"📁 Total files: {total_files}")
    print(f"💾 Total size: {total_size / (1024 * 1024):.2f} MB\n")
    
    for date in sorted(files_by_date.keys(), reverse=True)[:10]:
        count = len(files_by_date[date])
        pct = (count / total_files) * 100
        bar = "█" * int(pct / 2)
        print(f"{date} │ {bar} {count:4d} files ({pct:5.1f}%)")


def cleanup_temp_files(dry_run=True):
    """Remove temporary files."""
    print(f"\n🗑️  CLEANUP TEMPORARY FILES")
    print("=" * 50)
    
    patterns = [
        "results/execution/temp_*",
        "**/__pycache__",
        "**/*.pyc"
    ]
    
    files_to_delete = []
    for pattern in patterns:
        files_to_delete.extend(glob.glob(pattern, recursive=True))
    
    if not files_to_delete:
        print("✨ No temporary files found")
        return 0
    
    print(f"📁 Temporary files found: {len(files_to_delete)}")
    
    if dry_run:
        print("\n⚠️  DRY-RUN MODE")
        return len(files_to_delete)
    
    deleted_count = 0
    for f in files_to_delete:
        try:
            if os.path.isfile(f):
                os.remove(f)
            elif os.path.isdir(f):
                shutil.rmtree(f)
            deleted_count += 1
        except Exception as e:
            print(f"❌ Error: {e}")
    
    print(f"✅ Deleted {deleted_count} items")
    return deleted_count


def get_directory_stats():
    """Show directory statistics."""
    print(f"\n📊 DIRECTORY STATISTICS")
    print("=" * 50)
    
    if not os.path.exists("results"):
        print("⚠️  Results directory not found")
        return
    
    subdirs = ["carbon", "carbon_benchmark", "csv", "execution"]
    stats = {}
    total_size = 0
    
    for subdir in subdirs:
        path = os.path.join("results", subdir)
        if os.path.exists(path):
            size = sum(os.path.getsize(os.path.join(r, f)) 
                      for r, _, files in os.walk(path) for f in files)
            count = sum(len(files) for _, _, files in os.walk(path))
            stats[subdir] = (size, count)
            total_size += size
    
    print(f"📁 Total size: {total_size / (1024 * 1024):.2f} MB\n")
    
    for subdir, (size, count) in sorted(stats.items(), key=lambda x: x[1][0], reverse=True):
        size_mb = size / (1024 * 1024)
        pct = (size / total_size * 100) if total_size > 0 else 0
        bar = "█" * int(pct / 2)
        print(f"{subdir:20s} │ {bar:50s} {size_mb:6.2f} MB ({count:5d} files)")


def deep_clean(days_to_keep=2, keep_benchmarks=3, dry_run=True):
    """Comprehensive cleanup."""
    print("\n" + "=" * 50)
    print("🧹 DEEP CLEAN")
    print("=" * 50)
    
    cleanup_carbon_sessions(days_to_keep=days_to_keep, dry_run=dry_run)
    cleanup_temp_files(dry_run=dry_run)
    get_directory_stats()
    
    print("\n✅ CLEANUP COMPLETED!")


if __name__ == "__main__":
    deep_clean(days_to_keep=2, keep_benchmarks=3, dry_run=True)
