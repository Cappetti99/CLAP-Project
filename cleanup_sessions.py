#!/usr/bin/env python3
"""
Cleanup script for CLAP session files
Keeps only session_*.json files from the last 2 days
"""

import os
import glob
import time
from datetime import datetime, timedelta
from pathlib import Path

def cleanup_session_files(days_to_keep=2, dry_run=True):
    """
    Cleans up session files, keeping only those from the last N days.
    
    Args:
        days_to_keep (int): Days to keep (default: 2)
        dry_run (bool): If True, only shows what would be deleted without actually deleting
    """
    
    # Directory containing session files
    carbon_dir = "results/carbon"
    
    if not os.path.exists(carbon_dir):
        print(f" Directory {carbon_dir} not found")
        return
    
    # Calculate cutoff timestamp (2 days ago)
    cutoff_date = datetime.now() - timedelta(days=days_to_keep)
    cutoff_timestamp = cutoff_date.timestamp()
    
    print(f" CLEANUP SESSION FILES")
    print(f" Cutoff date: {cutoff_date.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f" Directory: {carbon_dir}")
    print("=" * 50)
    
    # Find all session_*.json files
    session_pattern = os.path.join(carbon_dir, "session_*.json")
    session_files = glob.glob(session_pattern)
    
    print(f" Session file found: {len(session_files)}")
    
    # Separate files to keep and delete
    files_to_keep = []
    files_to_delete = []
    
    for file_path in session_files:
        # Get modification timestamp
        file_mtime = os.path.getmtime(file_path)
        file_date = datetime.fromtimestamp(file_mtime)
        
        if file_mtime > cutoff_timestamp:
            files_to_keep.append((file_path, file_date))
        else:
            files_to_delete.append((file_path, file_date))
    
    # Report
    print(f" File da mantenere: {len(files_to_keep)}")
    print(f"  File da eliminare: {len(files_to_delete)}")
    
    if not files_to_delete:
        print("\n No file to delete. Cleanup not needed.")
        return 0
    
    
    if len(files_to_delete) > 5:
        print(f"  ... and {len(files_to_delete) - 5} more files")
    
    # Show some examples of files to keep
    print(f"\n Examples of files to keep:")
    for i, (file_path, file_date) in enumerate(files_to_keep[:5]):
        filename = os.path.basename(file_path)
        print(f"  • {filename} ({file_date.strftime('%Y-%m-%d %H:%M')})")
    
    if len(files_to_keep) > 5:
        print(f"  ... e altri {len(files_to_keep) - 5} file")
    
    # Calculate total size to be freed
    total_size = 0
    for file_path, _ in files_to_delete:
        try:
            total_size += os.path.getsize(file_path)
        except:
            pass
    
    size_mb = total_size / (1024 * 1024)
    print(f"\n Total size to be freed: ~{size_mb:.1f} MB")
    
    if dry_run:
        print("\n  DRY-RUN MODE: No files were deleted")
        print(" To actually delete files, use: --execute")
        return len(files_to_delete)
    
    # Effectively delete files
    print(f"\n Deleting {len(files_to_delete)} files...")
    deleted_count = 0
    
    for file_path, file_date in files_to_delete:
        try:
            os.remove(file_path)
            deleted_count += 1
            if deleted_count % 100 == 0:  # Progress every 100 files
                print(f"   Deleted {deleted_count}/{len(files_to_delete)} files...")
        except Exception as e:
            print(f" Error deleting {os.path.basename(file_path)}: {e}")
    
    print(f"\n Cleanup completed!")
    print(f" Files deleted: {deleted_count}")
    print(f" Files kept: {len(files_to_keep)}")
    print(f" Space freed: ~{size_mb:.1f} MB")
    
    return deleted_count

def show_file_distribution():
    """ Shows the distribution of session files over time"""
    carbon_dir = "results/carbon"
    session_pattern = os.path.join(carbon_dir, "session_*.json")
    session_files = glob.glob(session_pattern)
    
    if not session_files:
        print(" No session files found.")
        return
    print(f" SESSION FILE TIME DISTRIBUTION")
    print("=" * 50)
    
    # Group by date
    dates = {}
    for file_path in session_files:
        file_mtime = os.path.getmtime(file_path)
        file_date = datetime.fromtimestamp(file_mtime)
        date_key = file_date.strftime("%Y-%m-%d")
        
        if date_key not in dates:
            dates[date_key] = 0
        dates[date_key] += 1
    
    # Display distribution
    total_files = len(session_files)
    for date_key in sorted(dates.keys(), reverse=True):
        count = dates[date_key]
        percentage = (count / total_files) * 100
        print(f" {date_key}: {count:4d} files ({percentage:5.1f}%)")
    
    print(f"\n Total: {total_files} file")
    
    # Suggestion
    if len(dates) > 3:
        oldest_dates = sorted(dates.keys())[:-2]  # Tutte tranne le ultime 2
        files_to_clean = sum(dates[date] for date in oldest_dates)
        print(f"\n Suggestion: keeping 2 days would delete {files_to_clean} files")

def main():
    """Main function"""
    import sys
    
    # Check arguments
    dry_run = True
    days_to_keep = 2
    show_stats = False
    
    if "--execute" in sys.argv:
        dry_run = False
        print(" EXECUTION MODE: Files will be deleted!")
    
    if "--stats" in sys.argv:
        show_stats = True
    
    if "--help" in sys.argv or "-h" in sys.argv:
        print(" SCRIPT CLEANUP SESSION SWAM")
        print("=" * 40)
        print("Keeps only session_*.json files from the last N days")
        print()
        print("Options:")
        print("  --stats              Show file time distribution")
        print("  --days N             Days to keep (default: 2)")
        print("  --execute            Actually perform the cleanup")
        print("  --help, -h           Show this help message")
        print()
        print("Examples:")
        print("  python cleanup_sessions.py                    # Dry-run with 2 days")
        print("  python cleanup_sessions.py --stats            # Show statistics")
        print("  python cleanup_sessions.py --days 1           # Dry-run with 1 day")
        print("  python cleanup_sessions.py --days 3 --execute # Delete keeping 3 days")
        print()
        print("  SAFETY:")
        print("• Default dry-run mode (no deletion)")
        print("• ALWAYS keeps benchmark and analysis files")
        print("• ONLY deletes older session_*.json files")
        return
    
    if "--days" in sys.argv:
        try:
            idx = sys.argv.index("--days")
            days_to_keep = int(sys.argv[idx + 1])
        except (IndexError, ValueError):
            print(" Error: specify a valid number of days after --days")
            sys.exit(1)
    
    # Check we are in the right directory
    if not os.path.exists("main.py") or not os.path.exists("results"):
        print(" Error: Run the script from the CLAP-Project directory")
        sys.exit(1)
    
    # Show stats or perform cleanup
    if show_stats:
        show_file_distribution()
        return
    
    try:
        deleted_count = cleanup_session_files(days_to_keep=days_to_keep, dry_run=dry_run)
        
        if dry_run and deleted_count > 0:
            print(f"\n To perform the cleanup:")
            print(f"   python cleanup_sessions.py --execute")
            print(f"\n To change days to keep:")
            print(f"   python cleanup_sessions.py --days 3 --execute")
        elif dry_run and deleted_count == 0:
            print(f"\n Available options:")
            print(f"   python cleanup_sessions.py --stats          # Show file distribution")
            print(f"   python cleanup_sessions.py --days 1         # Test with 1 day") 
            print(f"   python cleanup_sessions.py --help           # Show help")
            
    except KeyboardInterrupt:
        print("\n  Process interrupted by user")
    except Exception as e:
        print(f" Unexpected error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
