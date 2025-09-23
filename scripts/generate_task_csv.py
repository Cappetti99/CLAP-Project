#!/usr/bin/env python3
"""
Script per generare un CSV con statistiche sui task del dataset SWAM
"""

import os
import csv
import glob
import json
from collections import defaultdict

def count_lines_of_code(file_path):
    """Conta le linee di codice (escludendo righe vuote e commenti)"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        
        code_lines = 0
        for line in lines:
            line = line.strip()
            # Salta righe vuote
            if not line:
                continue
            # Salta commenti comuni
            if (line.startswith('//') or line.startswith('#') or 
                line.startswith('/*') or line.startswith('*') or
                line.startswith('<!--') or line.startswith('%')):
                continue
            code_lines += 1
        
        return code_lines
    except Exception as e:
        print(f"Errore lettura file {file_path}: {e}")
        return 0

def normalize_task_name(task_name):
    """Normalizza il nome del task"""
    # Rimuove numeri e caratteri speciali dall'inizio
    task_name = task_name.strip()
    # Rimuove prefissi come "snippet_XX_"
    if '_' in task_name:
        parts = task_name.split('_')
        if len(parts) >= 3 and parts[0] == 'snippet' and parts[1].isdigit():
            task_name = '_'.join(parts[2:])
    
    # Normalizza caratteri speciali
    task_name = task_name.replace('_', ' ').replace('-', ' ')
    # Capitalizza ogni parola
    task_name = ' '.join(word.capitalize() for word in task_name.split())
    
    return task_name

def get_language_display_name(lang_dir):
    """Converte il nome della directory nel nome display del linguaggio"""
    mapping = {
        'c': 'C',
        'cplusplus': 'C++', 
        'cpp': 'C++',
        'csharp': 'C#',
        'c#': 'C#',
        'javascript': 'JavaScript',
        'typescript': 'TypeScript',
        'python': 'Python',
        'java': 'Java',
        'go': 'Go',
        'rust': 'Rust',
        'php': 'PHP',
        'ruby': 'Ruby',
        'r': 'R',
        'julia': 'Julia',
        'haskell': 'Haskell',
        'ocaml': 'OCaml',
        'matlab': 'MATLAB'
    }
    return mapping.get(lang_dir.lower(), lang_dir.capitalize())

def generate_task_url(task_name):
    """Genera un URL placeholder per il task"""
    # Converte il nome in formato URL-friendly
    url_name = task_name.lower().replace(' ', '_').replace("'", "")
    return f"https://swam-dataset.org/tasks/{url_name}"

def analyze_swam_dataset():
    """Analizza il dataset SWAM e genera statistiche sui task"""
    
    dataset_path = "data/generated/code_snippets"
    
    if not os.path.exists(dataset_path):
        print(f"❌ Dataset path non trovato: {dataset_path}")
        return None
    
    # Struttura per raccogliere dati
    task_data = defaultdict(lambda: {
        'languages': set(),
        'code_lengths': [],
        'categories': set()
    })
    
    print(f"🔍 Analizzando dataset in: {dataset_path}")
    
    # Scansiona tutte le categorie
    categories = [d for d in os.listdir(dataset_path) 
                 if os.path.isdir(os.path.join(dataset_path, d))]
    
    print(f"📁 Categorie trovate: {categories}")
    
    total_files = 0
    processed_files = 0
    
    for category in categories:
        category_path = os.path.join(dataset_path, category)
        
        # Scansiona tutti i linguaggi nella categoria
        languages = [d for d in os.listdir(category_path) 
                    if os.path.isdir(os.path.join(category_path, d))]
        
        for language in languages:
            lang_path = os.path.join(category_path, language)
            
            # Trova tutti i file di codice
            pattern = os.path.join(lang_path, "snippet_*.*")
            files = glob.glob(pattern)
            total_files += len(files)
            
            for file_path in files:
                try:
                    filename = os.path.basename(file_path)
                    
                    # Estrai nome task dal filename
                    # Formato: snippet_XX_TaskName.ext
                    name_without_ext = os.path.splitext(filename)[0]
                    
                    if '_' in name_without_ext:
                        parts = name_without_ext.split('_')
                        if len(parts) >= 3 and parts[0] == 'snippet' and parts[1].isdigit():
                            raw_task_name = '_'.join(parts[2:])
                            task_name = normalize_task_name(raw_task_name)
                            
                            # Conta linee di codice
                            code_lines = count_lines_of_code(file_path)
                            
                            if code_lines > 0:  # Solo se il file ha contenuto valido
                                lang_display = get_language_display_name(language)
                                
                                task_data[task_name]['languages'].add(lang_display)
                                task_data[task_name]['code_lengths'].append(code_lines)
                                task_data[task_name]['categories'].add(category)
                                
                                processed_files += 1
                                
                except Exception as e:
                    print(f"⚠️  Errore processando {file_path}: {e}")
                    continue
    
    print(f"📊 File totali: {total_files}, processati: {processed_files}")
    print(f"🎯 Task unici trovati: {len(task_data)}")
    
    return task_data

def create_task_csv(task_data, output_file="results/csv/swam_tasks_analysis.csv"):
    """Crea il file CSV con le statistiche dei task"""
    
    # Crea directory se non esiste
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    
    # Ordina i task per nome
    sorted_tasks = sorted(task_data.items())
    
    with open(output_file, 'w', newline='', encoding='utf-8') as csvfile:
        fieldnames = [
            'task_name',
            'avg_code_length', 
            'min_code_length',
            'max_code_length',
            'total_implementations',
            'task_url',
            'languages'
        ]
        
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        
        for task_name, data in sorted_tasks:
            if not data['code_lengths']:  # Salta task senza implementazioni valide
                continue
                
            code_lengths = data['code_lengths']
            languages_sorted = sorted(list(data['languages']))
            
            row = {
                'task_name': task_name,
                'avg_code_length': round(sum(code_lengths) / len(code_lengths)),
                'min_code_length': min(code_lengths),
                'max_code_length': max(code_lengths),
                'total_implementations': len(code_lengths),
                'task_url': generate_task_url(task_name),
                'languages': ','.join(languages_sorted)
            }
            
            writer.writerow(row)
    
    print(f"✅ CSV creato: {output_file}")
    return output_file

def main():
    """Funzione principale"""
    
    print("🔬 ANALISI DATASET SWAM - GENERAZIONE CSV TASK")
    print("=" * 60)
    
    # Analizza il dataset
    task_data = analyze_swam_dataset()
    
    if not task_data:
        print("❌ Impossibile analizzare il dataset")
        return
    
    # Crea il CSV
    csv_file = create_task_csv(task_data)
    
    # Mostra anteprima
    print(f"\n📊 ANTEPRIMA RISULTATI:")
    print("-" * 40)
    
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            if i < 10:  # Prime 10 righe
                print(f"{row['task_name']:<25} | {row['total_implementations']:2s} impl | "
                      f"Avg: {row['avg_code_length']:2s} lines | "
                      f"{len(row['languages'].split(','))} langs")
    
    print(f"\n🎉 Analisi completata!")
    print(f"📄 File generato: {csv_file}")

if __name__ == "__main__":
    main()
