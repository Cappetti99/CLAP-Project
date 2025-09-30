#!/usr/bin/env python3
"""
Script per convertire i risultati del benchmark da JSON a CSV
"""

import json
import csv
import os
import sys
import glob
from datetime import datetime

def find_latest_summary():
    """Trova il file summary più recente"""
    pattern = "results/carbon_benchmark/carbon_benchmark_summary_*.json"
    files = glob.glob(pattern)
    if not files:
        return None
    return max(files, key=os.path.getmtime)

def json_to_csv_detailed(json_file, output_dir="results/csv"):
    """Converte JSON summary in CSV dettagliato (una riga per task/linguaggio)"""
    
    # Crea directory se non esiste
    os.makedirs(output_dir, exist_ok=True)
    
    # Carica dati JSON
    with open(json_file, 'r') as f:
        data = json.load(f)
    
    # Estrai timestamp dal nome file
    timestamp = os.path.basename(json_file).split('_')[-1].replace('.json', '')
    
    # Nome file CSV
    csv_file = os.path.join(output_dir, f"benchmark_detailed_{timestamp}.csv")
    
    # Campi CSV
    fieldnames = [
        'Task',
        'Language', 
        'Success_Rate_%',
        'Mean_Emissions_kg_CO2eq',
        'Mean_Execution_Time_s',
        'Total_Emissions_kg_CO2eq',
        'Emissions_mg_CO2eq',  # Per leggibilità
        'Execution_Time_ms'    # Per leggibilità
    ]
    
    with open(csv_file, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        
        for task_name, task_data in data.items():
            for language, stats in task_data.items():
                row = {
                    'Task': task_name,
                    'Language': language,
                    'Success_Rate_%': f"{stats['success_rate']:.1f}",
                    'Mean_Emissions_kg_CO2eq': f"{stats['mean_emissions']:.8f}",
                    'Mean_Execution_Time_s': f"{stats['mean_execution_time']:.6f}",
                    'Total_Emissions_kg_CO2eq': f"{stats['total_emissions']:.8f}",
                    'Emissions_mg_CO2eq': f"{stats['mean_emissions'] * 1000:.3f}",
                    'Execution_Time_ms': f"{stats['mean_execution_time'] * 1000:.1f}"
                }
                writer.writerow(row)
    
    print(f"✅ CSV dettagliato creato: {csv_file}")
    return csv_file

def json_to_csv_summary(json_file, output_dir="results/csv"):
    """Converte JSON in CSV riassuntivo per linguaggio"""
    
    # Crea directory se non esiste
    os.makedirs(output_dir, exist_ok=True)
    
    # Carica dati JSON
    with open(json_file, 'r') as f:
        data = json.load(f)
    
    # Estrai timestamp dal nome file
    timestamp = os.path.basename(json_file).split('_')[-1].replace('.json', '')
    
    # Calcola statistiche per linguaggio
    language_stats = {}
    
    for task_name, task_data in data.items():
        for language, stats in task_data.items():
            if language not in language_stats:
                language_stats[language] = {
                    'total_emissions': 0.0,
                    'total_execution_time': 0.0,
                    'task_count': 0,
                    'total_success_rate': 0.0
                }
            
            language_stats[language]['total_emissions'] += stats['mean_emissions']
            language_stats[language]['total_execution_time'] += stats['mean_execution_time']
            language_stats[language]['total_success_rate'] += stats['success_rate']
            language_stats[language]['task_count'] += 1
    
    # Calcola medie
    for language in language_stats:
        count = language_stats[language]['task_count']
        language_stats[language]['avg_emissions'] = language_stats[language]['total_emissions'] / count
        language_stats[language]['avg_execution_time'] = language_stats[language]['total_execution_time'] / count
        language_stats[language]['avg_success_rate'] = language_stats[language]['total_success_rate'] / count
    
    # Ordina per efficienza energetica
    sorted_languages = sorted(language_stats.items(), 
                            key=lambda x: x[1]['avg_emissions'])
    
    # Nome file CSV
    csv_file = os.path.join(output_dir, f"benchmark_summary_{timestamp}.csv")
    
    # Campi CSV
    fieldnames = [
        'Rank',
        'Language',
        'Tasks_Count',
        'Avg_Success_Rate_%',
        'Avg_Emissions_kg_CO2eq',
        'Avg_Execution_Time_s',
        'Emissions_mg_CO2eq',
        'Execution_Time_ms',
        'Efficiency_Score'  # Rapporto prestazioni/emissioni
    ]
    
    with open(csv_file, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        
        for rank, (language, stats) in enumerate(sorted_languages, 1):
            # Calcola efficiency score (inverso delle emissioni normalizzato)
            efficiency = 1.0 / stats['avg_emissions'] if stats['avg_emissions'] > 0 else 0
            
            row = {
                'Rank': rank,
                'Language': language,
                'Tasks_Count': stats['task_count'],
                'Avg_Success_Rate_%': f"{stats['avg_success_rate']:.1f}",
                'Avg_Emissions_kg_CO2eq': f"{stats['avg_emissions']:.8f}",
                'Avg_Execution_Time_s': f"{stats['avg_execution_time']:.6f}",
                'Emissions_mg_CO2eq': f"{stats['avg_emissions'] * 1000:.3f}",
                'Execution_Time_ms': f"{stats['avg_execution_time'] * 1000:.1f}",
                'Efficiency_Score': f"{efficiency:.0f}"
            }
            writer.writerow(row)
    
    print(f"✅ CSV riassuntivo creato: {csv_file}")
    return csv_file

def main():
    """Funzione principale"""
    
    # Trova il file JSON più recente se non specificato
    if len(sys.argv) > 1:
        json_file = sys.argv[1]
    else:
        json_file = find_latest_summary()
        if not json_file:
            print("❌ Nessun file summary trovato in results/carbon_benchmark/")
            print("Usa: python scripts/json_to_csv.py <file.json>")
            sys.exit(1)
    
    if not os.path.exists(json_file):
        print(f"❌ File non trovato: {json_file}")
        sys.exit(1)
    
    print(f" Convertendo: {json_file}")
    
    # Genera entrambi i CSV
    csv_detailed = json_to_csv_detailed(json_file)
    csv_summary = json_to_csv_summary(json_file)
    
    print(f"\n Conversione completata!")
    print(f" File dettagliato: {csv_detailed}")
    print(f" File riassuntivo: {csv_summary}")
    
    # Mostra anteprima del riassuntivo
    print(f"\n ANTEPRIMA RANKING:")
    with open(csv_summary, 'r') as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            if i < 15:  # Prime 5 righe
                print(f"{row['Rank']:2s}. {row['Language']:12s} - {row['Emissions_mg_CO2eq']:6s} mg CO2eq")

if __name__ == "__main__":
    main()
