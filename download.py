#!/usr/bin/env python3
"""
Script per scaricare e organizzare il dataset Rosetta Code
da Hugging Face nel formato atteso dal sistema SWAM
"""

import os
import sys
from pathlib import Path
from datasets import load_dataset
import re
from collections import defaultdict

def clean_filename(name):
    """Pulisce il nome del file per renderlo filesystem-safe"""
    # Rimuove caratteri speciali e spazi
    name = re.sub(r'[^\w\s-]', '', name)
    name = re.sub(r'[-\s]+', '_', name)
    return name.strip('_')

def get_file_extension(language):
    """Mappa il linguaggio all'estensione file"""
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
    """Organizza le task in categorie"""
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
    """Scarica e organizza il dataset Rosetta Code"""
    print(" Scaricamento dataset Rosetta Code da Hugging Face...")
    
    try:
        # Carica il dataset
        dataset = load_dataset("christopher/rosetta-code")
        print(f" Dataset caricato! Esempi trovati: {len(dataset['train'])}")
        
        # Directory di output
        output_dir = Path("data/generated/code_snippets")
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Statistiche
        stats = defaultdict(lambda: defaultdict(int))
        total_files = 0
        
        # Processa ogni esempio
        for idx, example in enumerate(dataset['train']):
            task_name = example.get('task_name', f'task_{idx}')
            language = example.get('language_name', 'unknown')
            code = example.get('code', '')
            
            if not code.strip():
                continue
                
            # Pulisce i nomi
            clean_task = clean_filename(task_name)
            clean_lang = language.lower().replace(' ', '').replace('+', 'plus')
            
            # Determina categoria e estensione
            category = organize_by_category(task_name)
            extension = get_file_extension(language)
            
            # Crea la struttura di directory: category/language/
            lang_dir = output_dir / category / clean_lang
            lang_dir.mkdir(parents=True, exist_ok=True)
            
            # Nome file nel formato atteso: snippet_N_TaskName.ext
            filename = f"snippet_{stats[category][clean_lang] + 1}_{clean_task}{extension}"
            filepath = lang_dir / filename
            
            # Scrive il file
            try:
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(code)
                
                stats[category][clean_lang] += 1
                total_files += 1
                
                if total_files % 100 == 0:
                    print(f"   Processati {total_files} file...")
                    
            except Exception as e:
                print(f" Errore scrittura {filepath}: {e}")
                continue
        
        # Stampa statistiche finali
        print(f"\n Download completato!")
        print(f" Statistiche:")
        print(f"   Total file creati: {total_files}")
        
        for category, languages in stats.items():
            print(f"    {category}:")
            for lang, count in sorted(languages.items()):
                print(f"      {lang}: {count} file")
        
        return True
        
    except Exception as e:
        print(f" Errore durante il download: {e}")
        return False

if __name__ == "__main__":
    print("=" * 50)
    print("SWAM - Rosetta Code Dataset Downloader")
    print("=" * 50)
    
    success = download_rosetta_code()
    
    if success:
        print("\n Dataset scaricato e organizzato con successo!")
        print("Ora puoi eseguire: python main.py analyze")
    else:
        print("\n Download fallito!")
        sys.exit(1)
