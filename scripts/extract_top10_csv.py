#!/usr/bin/env python3
"""
Script per estrarre solo i TOP10 task dal CSV completo SWAM
"""

import json
import csv
import os

def load_top10_tasks():
    """Carica i nomi dei TOP10 task dal file common_tasks.json"""
    
    common_tasks_file = "results/task_analysis/common_tasks.json"
    
    if not os.path.exists(common_tasks_file):
        print(f"❌ File non trovato: {common_tasks_file}")
        return None
    
    with open(common_tasks_file, 'r') as f:
        data = json.load(f)
    
    # Estrai i nomi dei task dalla sezione "common_tasks"
    top10_tasks = []
    for task in data.get('common_tasks', []):
        task_name = task['name']
        # Normalizza il nome per il matching con il CSV
        normalized_name = task_name.replace('_', ' ').replace('-', ' ')
        normalized_name = ' '.join(word.capitalize() for word in normalized_name.split())
        top10_tasks.append(normalized_name)
    
    print(f"📋 TOP10 task trovati:")
    for i, task in enumerate(top10_tasks, 1):
        print(f"  {i:2d}. {task}")
    
    return top10_tasks

def extract_top10_csv(input_csv, output_csv, top10_tasks):
    """Estrae solo i TOP10 task dal CSV completo"""
    
    if not os.path.exists(input_csv):
        print(f"❌ File CSV non trovato: {input_csv}")
        return False
    
    # Crea directory output se non esiste
    os.makedirs(os.path.dirname(output_csv), exist_ok=True)
    
    extracted_count = 0
    total_tasks = len(top10_tasks)
    
    with open(input_csv, 'r', encoding='utf-8') as infile, \
         open(output_csv, 'w', newline='', encoding='utf-8') as outfile:
        
        reader = csv.DictReader(infile)
        writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)
        writer.writeheader()
        
        for row in reader:
            task_name = row['task_name']
            
            # Verifica se questo task è nei TOP10
            if task_name in top10_tasks:
                writer.writerow(row)
                extracted_count += 1
                print(f"✅ Estratto: {task_name}")
    
    print(f"\n📊 ESTRAZIONE COMPLETATA:")
    print(f"   Task estratti: {extracted_count}/{total_tasks}")
    print(f"   File creato: {output_csv}")
    
    if extracted_count < total_tasks:
        missing = set(top10_tasks) - {row['task_name'] for row in csv.DictReader(open(input_csv))}
        print(f"\n⚠️  Task non trovati nel CSV:")
        for task in missing:
            print(f"     - {task}")
    
    return extracted_count > 0

def add_benchmark_results(top10_csv, benchmark_json):
    """Aggiunge i risultati del benchmark al CSV TOP10"""
    
    if not os.path.exists(benchmark_json):
        print(f"⚠️  File benchmark non trovato: {benchmark_json}")
        return
    
    with open(benchmark_json, 'r') as f:
        benchmark_data = json.load(f)
    
    # Leggi il CSV esistente
    with open(top10_csv, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = reader.fieldnames
    
    # Aggiungi nuovi campi
    new_fieldnames = list(fieldnames) + [
        'benchmark_tested',
        'successful_languages',
        'avg_emissions_mg_co2eq',
        'fastest_language',
        'most_efficient_language'
    ]
    
    # Aggiorna ogni riga con dati benchmark
    for row in rows:
        task_name = row['task_name']
        
        # Mapping dei nomi task tra CSV e benchmark
        name_mapping = {
            'Balanced Brackets': 'Balanced_brackets',
            'Loop Over Multiple Arrays Simultaneously': 'Loop_over_multiple_arrays_simultaneously', 
            'Fractal Tree': 'Fractal_tree',
            'Greatest Common Divisor': 'Greatest_common_divisor',
            'Gray Code': 'Gray_code',
            'Function Composition': 'Function_composition',
            'Sorting Algorithmsquicksort': 'Sorting_algorithmsQuicksort',
            'Pascals Triangle': 'Pascals_triangle',
            'Identity Matrix': 'Identity_matrix',
            'Palindrome Detection': 'Palindrome_detection'
        }
        
        benchmark_task_name = name_mapping.get(task_name, task_name.replace(' ', '_'))
        
        if benchmark_task_name in benchmark_data:
            task_data = benchmark_data[benchmark_task_name]
            
            # Calcola statistiche
            successful_langs = []
            emissions_data = []
            time_data = []
            
            for lang, stats in task_data.items():
                if stats.get('success_rate', 0) == 100.0:
                    successful_langs.append(lang)
                    emissions_data.append((lang, stats['mean_emissions']))
                    time_data.append((lang, stats['mean_execution_time']))
            
            # Trova il più veloce e più efficiente
            fastest_lang = min(time_data, key=lambda x: x[1])[0] if time_data else "N/A"
            most_efficient_lang = min(emissions_data, key=lambda x: x[1])[0] if emissions_data else "N/A"
            avg_emissions = sum(x[1] for x in emissions_data) / len(emissions_data) if emissions_data else 0
            
            row.update({
                'benchmark_tested': 'Yes',
                'successful_languages': ','.join(successful_langs),
                'avg_emissions_mg_co2eq': f"{avg_emissions * 1000:.3f}",
                'fastest_language': fastest_lang,
                'most_efficient_language': most_efficient_lang
            })
        else:
            row.update({
                'benchmark_tested': 'No',
                'successful_languages': '',
                'avg_emissions_mg_co2eq': '',
                'fastest_language': '',
                'most_efficient_language': ''
            })
    
    # Riscrivi il file con i nuovi dati
    with open(top10_csv, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=new_fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    
    print(f"✅ Dati benchmark aggiunti al CSV")

def main():
    """Funzione principale"""
    
    print("🔬 ESTRAZIONE TOP10 TASK CSV")
    print("=" * 50)
    
    # 1. Carica i TOP10 task
    top10_tasks = load_top10_tasks()
    if not top10_tasks:
        return
    
    # 2. Estrai dal CSV completo
    input_csv = "results/csv/swam_tasks_analysis.csv"
    output_csv = "results/csv/top10_tasks_analysis.csv"
    
    success = extract_top10_csv(input_csv, output_csv, top10_tasks)
    if not success:
        return
    
    # 3. Aggiungi risultati benchmark se disponibili
    benchmark_json = "results/carbon_benchmark/carbon_benchmark_summary_20250923_174626.json"
    if os.path.exists(benchmark_json):
        print(f"\n🔬 Aggiungendo risultati benchmark...")
        add_benchmark_results(output_csv, benchmark_json)
    
    print(f"\n🎉 TOP10 CSV creato con successo!")
    print(f"📄 File: {output_csv}")
    
    # Mostra anteprima
    print(f"\n📊 ANTEPRIMA:")
    with open(output_csv, 'r') as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            if i < 5:
                print(f"{row['task_name']:<30} | {row['total_implementations']:3s} impl | Avg: {row['avg_code_length']:2s} lines")

if __name__ == "__main__":
    main()
