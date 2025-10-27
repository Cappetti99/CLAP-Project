#!/usr/bin/env python3
"""
Script to download and organize the Rosetta Code dataset
from Hugging Face in the format expected by the CLAP system
"""

import os
import sys
from pathlib import Path
import re
from collections import defaultdict

def clean_filename(name):
    """Cleans the file name to make it filesystem-safe"""
    # Remove special characters and spaces
    name = re.sub(r'[^\w\s-]', '', name)
    name = re.sub(r'[-\s]+', '_', name)
    return name.strip('_')

def get_file_extension(language):
    """Maps the language to the file extension"""
    extensions = {
        'python': '.py',
        'java': '.java',
        'javascript': '.js',
        'c': '.c',
        'c++': '.cpp',
        'cpp': '.cpp',
        'csharp': '.cs',
        'c#': '.cs',
        'go': '.go',
        'rust': '.rs',
        'ruby': '.rb',
        'php': '.php',
        'haskell': '.hs',
        'ocaml': '.ml',
        'r': '.r',
        'julia': '.jl',
        'typescript': '.ts',
        'matlab': '.m'
    }
    return extensions.get(language.lower(), '.txt')

def organize_by_category(task_name):
    """Organizes tasks into categories"""
    task_lower = task_name.lower()
    
    if any(word in task_lower for word in ['sort', 'search', 'tree', 'graph', 'hash']):
        return 'algorithms'
    elif any(word in task_lower for word in ['string', 'text', 'char', 'word']):
        return 'strings'
    elif any(word in task_lower for word in ['math', 'number', 'calc', 'prime', 'factorial', 'fibonacci']):
        return 'mathematics'
    elif any(word in task_lower for word in ['file', 'io', 'read', 'write']):
        return 'io'
    elif any(word in task_lower for word in ['hello', 'world', 'print']):
        return 'basic'
    else:
        return 'misc'

def download_rosetta_code():
    """Downloads and organizes the Rosetta Code dataset"""
    print("[*] Downloading Rosetta Code dataset from Hugging Face...")
    
    try:
        # Try to import datasets
        try:
            from datasets import load_dataset
        except ImportError:
            print("[ERROR] The 'datasets' package is not installed.")
            print("[*] Please install it with: pip install datasets")
            return False
        
        # Load the dataset
        dataset = load_dataset("christopher/rosetta-code")
        print(f"[OK] Dataset loaded! Examples found: {len(dataset['train'])}")
        
        # Output directory
        output_dir = Path("data/generated/code_snippets")
        output_dir.mkdir(parents=True, exist_ok=True)
        print(f"[*] Output directory: {output_dir.absolute()}")
        
        # Statistics
        stats = defaultdict(lambda: defaultdict(int))
        total_files = 0
        skipped_files = 0
        
        # Process each example
        for idx, example in enumerate(dataset['train']):
            task_name = example.get('task_name', f'task_{idx}')
            language = example.get('language_name', 'unknown')
            code = example.get('code', '')
            
            if not code.strip():
                skipped_files += 1
                continue
                
            # Clean names
            clean_task = clean_filename(task_name)
            clean_lang = language.lower().replace(' ', '').replace('+', 'plus')
            
            # Determine category and extension
            category = organize_by_category(task_name)
            extension = get_file_extension(language)
            
            # Create directory structure: category/language/
            lang_dir = output_dir / category / clean_lang
            lang_dir.mkdir(parents=True, exist_ok=True)
            
            # File name in the expected format: snippet_N_TaskName.ext
            filename = f"snippet_{stats[category][clean_lang] + 1}_{clean_task}{extension}"
            filepath = lang_dir / filename
            
            # Write the file with error handling
            try:
                with open(filepath, 'w', encoding='utf-8', errors='replace') as f:
                    f.write(code)
                
                stats[category][clean_lang] += 1
                total_files += 1
                
                if total_files % 100 == 0:
                    print(f"[*] Processed {total_files} files...")
                    
            except OSError as e:
                print(f"[WARNING] Error writing {filepath}: {e}")
                skipped_files += 1
                continue
            except Exception as e:
                print(f"[WARNING] Unexpected error with {filepath}: {e}")
                skipped_files += 1
                continue
        
        # Print final statistics
        print(f"\n[OK] Download completed!")
        print(f"[*] Statistics:")
        print(f"    Total files created: {total_files}")
        print(f"    Skipped files: {skipped_files}")
        print(f"\n[*] Files by category:")
        
        for category in sorted(stats.keys()):
            languages = stats[category]
            total_in_category = sum(languages.values())
            print(f"    {category}: {total_in_category} files")
            for lang, count in sorted(languages.items()):
                print(f"      - {lang}: {count} files")
        
        return True
        
    except Exception as e:
        print(f"[ERROR] Error during download: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    print("=" * 60)
    print("CLAP - Rosetta Code Dataset Downloader")
    print("=" * 60)
    print()
    
    success = download_rosetta_code()
    
    if success:
        print("\n[OK] Dataset downloaded and organized successfully!")
    else:
        print("\n[ERROR] Download failed!")
        sys.exit(1)