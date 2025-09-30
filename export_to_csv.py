#!/usr/bin/env python3
"""
CSV Exporter per i risultati SWAM
Esporta i risultati dell'ultima run in formato CSV per analisi
"""

import json
import pandas as pd
import os
from pathlib import Path
import argparse
from datetime import datetime
import glob

class SWAMCSVExporter:
    """Esporta i risultati SWAM in formato CSV"""
    
    def __init__(self):
        self.results_dir = Path("results")
        self.csv_dir = self.results_dir / "csv"
        self.csv_dir.mkdir(exist_ok=True)
        
    def find_latest_results(self):
        """Trova i file di risultati più recenti"""
        results = {}
        
        # Carbon benchmark results
        carbon_files = list((self.results_dir / "carbon_benchmark").glob("carbon_benchmark_detailed_*.json"))
        if carbon_files:
            latest_carbon = max(carbon_files, key=lambda x: x.stat().st_mtime)
            results['carbon_detailed'] = latest_carbon
            
            # Trova anche il summary corrispondente
            timestamp = latest_carbon.stem.split('_')[-1]
            summary_file = latest_carbon.parent / f"carbon_benchmark_summary_{timestamp}.json"
            if summary_file.exists():
                results['carbon_summary'] = summary_file
        
        # Language test results  
        test_files = list((self.results_dir / "execution").glob("language_test_results_*.json"))
        if test_files:
            latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
            results['language_test'] = latest_test
            
        return results
    
    def export_carbon_detailed(self, json_file, output_name=None):
        """Esporta risultati carbon dettagliati in CSV"""
        print(f"📊 Caricamento carbon benchmark da: {json_file.name}")
        
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        # Prepara i dati per il DataFrame
        rows = []
        
        for task_name, task_data in data['results'].items():
            for lang, lang_results in task_data.items():
                if isinstance(lang_results, dict) and 'runs' in lang_results:
                    # Statistiche aggregate per linguaggio/task
                    base_row = {
                        'task': task_name,
                        'language': lang,
                        'total_runs': len(lang_results['runs']),
                        'successful_runs': lang_results.get('successful_runs', 0),
                        'success_rate': lang_results.get('success_rate', 0),
                        'avg_co2_mg': lang_results.get('avg_co2_mg', 0),
                        'median_co2_mg': lang_results.get('median_co2_mg', 0),
                        'min_co2_mg': lang_results.get('min_co2_mg', 0),
                        'max_co2_mg': lang_results.get('max_co2_mg', 0),
                        'std_co2_mg': lang_results.get('std_co2_mg', 0),
                        'total_co2_mg': lang_results.get('total_co2_mg', 0),
                        'avg_time_s': lang_results.get('avg_time_s', 0),
                        'median_time_s': lang_results.get('median_time_s', 0),
                        'min_time_s': lang_results.get('min_time_s', 0),
                        'max_time_s': lang_results.get('max_time_s', 0),
                        'std_time_s': lang_results.get('std_time_s', 0)
                    }
                    rows.append(base_row)
        
        df = pd.DataFrame(rows)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"carbon_benchmark_summary_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV esportato: {output_path}")
        print(f"   📈 {len(df)} righe | {len(df.columns)} colonne")
        
        return output_path
    
    def export_carbon_individual_runs(self, json_file, output_name=None):
        """Esporta ogni singola run in CSV"""
        print(f"📊 Caricamento run individuali da: {json_file.name}")
        
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        rows = []
        
        for task_name, task_data in data['results'].items():
            for lang, lang_results in task_data.items():
                if isinstance(lang_results, dict) and 'runs' in lang_results:
                    for i, run in enumerate(lang_results['runs'], 1):
                        row = {
                            'task': task_name,
                            'language': lang,
                            'run_number': i,
                            'success': run.get('success', False),
                            'co2_mg': run.get('co2_mg', 0),
                            'execution_time_s': run.get('execution_time_s', 0),
                            'session_id': run.get('session_id', ''),
                            'timestamp': run.get('timestamp', '')
                        }
                        rows.append(row)
        
        df = pd.DataFrame(rows)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"carbon_individual_runs_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV run individuali: {output_path}")
        print(f"   📈 {len(df)} righe | {len(df.columns)} colonne")
        
        return output_path
    
    def export_language_rankings(self, json_file, output_name=None):
        """Esporta ranking linguaggi per efficienza energetica"""
        print(f"📊 Caricamento per ranking da: {json_file.name}")
        
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        # Calcola statistiche per linguaggio
        lang_stats = {}
        
        for task_name, task_data in data['results'].items():
            for lang, lang_results in task_data.items():
                if isinstance(lang_results, dict) and lang_results.get('successful_runs', 0) > 0:
                    if lang not in lang_stats:
                        lang_stats[lang] = {
                            'total_runs': 0,
                            'successful_runs': 0,
                            'total_co2_mg': 0,
                            'total_time_s': 0,
                            'tasks_count': 0,
                            'co2_values': [],
                            'time_values': []
                        }
                    
                    stats = lang_stats[lang]
                    stats['total_runs'] += lang_results.get('total_runs', 0)
                    stats['successful_runs'] += lang_results.get('successful_runs', 0)
                    stats['total_co2_mg'] += lang_results.get('total_co2_mg', 0)
                    stats['total_time_s'] += lang_results.get('avg_time_s', 0) * lang_results.get('successful_runs', 0)
                    stats['tasks_count'] += 1
                    
                    if 'runs' in lang_results:
                        for run in lang_results['runs']:
                            if run.get('success', False):
                                stats['co2_values'].append(run.get('co2_mg', 0))
                                stats['time_values'].append(run.get('execution_time_s', 0))
        
        # Crea DataFrame ranking
        ranking_rows = []
        for lang, stats in lang_stats.items():
            if stats['successful_runs'] > 0:
                row = {
                    'language': lang,
                    'total_runs': stats['total_runs'],
                    'successful_runs': stats['successful_runs'],
                    'success_rate': stats['successful_runs'] / stats['total_runs'] * 100,
                    'tasks_count': stats['tasks_count'],
                    'avg_co2_per_run_mg': stats['total_co2_mg'] / stats['successful_runs'],
                    'avg_time_per_run_s': stats['total_time_s'] / stats['successful_runs'],
                    'total_co2_mg': stats['total_co2_mg'],
                    'co2_std_mg': pd.Series(stats['co2_values']).std() if stats['co2_values'] else 0,
                    'time_std_s': pd.Series(stats['time_values']).std() if stats['time_values'] else 0
                }
                ranking_rows.append(row)
        
        df = pd.DataFrame(ranking_rows)
        df = df.sort_values('avg_co2_per_run_mg')  # Ordina per efficienza energetica
        df['energy_rank'] = range(1, len(df) + 1)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"language_energy_ranking_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV ranking linguaggi: {output_path}")
        print(f"   📈 {len(df)} linguaggi | {len(df.columns)} metriche")
        
        return output_path
    
    def export_language_test_results(self, json_file, output_name=None):
        """Esporta risultati test linguaggi"""
        print(f"📊 Caricamento test linguaggi da: {json_file.name}")
        
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        rows = []
        for lang, result in data['results'].items():
            row = {
                'language': lang,
                'available': result.get('available', False),
                'type': result.get('type', ''),
                'interpreter_found': result.get('interpreter_found', False),
                'compiler_found': result.get('compiler_found', False),
                'compile_success': result.get('compile_success', False),
                'run_success': result.get('run_success', False),
                'output': result.get('output', '')[:100],  # Tronca output lungo
                'error': result.get('error', '')[:200],    # Tronca errore lungo
                'execution_time': result.get('execution_time', 0)
            }
            rows.append(row)
        
        df = pd.DataFrame(rows)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"language_test_results_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV test linguaggi: {output_path}")
        print(f"   📈 {len(df)} linguaggi | {len(df.columns)} colonne")
        
        return output_path
    
    def export_all_latest(self):
        """Esporta tutti i risultati più recenti"""
        print("🚀 EXPORT RISULTATI ULTIMA RUN")
        print("=" * 50)
        
        latest = self.find_latest_results()
        
        if not latest:
            print("❌ Nessun risultato trovato!")
            return
        
        exported_files = []
        
        # Carbon benchmark
        if 'carbon_detailed' in latest:
            print(f"\n📊 CARBON BENCHMARK")
            print(f"File sorgente: {latest['carbon_detailed']}")
            
            # Summary aggregate
            summary_csv = self.export_carbon_detailed(latest['carbon_detailed'])
            exported_files.append(summary_csv)
            
            # Run individuali
            runs_csv = self.export_carbon_individual_runs(latest['carbon_detailed'])
            exported_files.append(runs_csv)
            
            # Ranking linguaggi
            ranking_csv = self.export_language_rankings(latest['carbon_detailed'])
            exported_files.append(ranking_csv)
        
        # Language test
        if 'language_test' in latest:
            print(f"\n🔧 LANGUAGE TEST")
            print(f"File sorgente: {latest['language_test']}")
            
            test_csv = self.export_language_test_results(latest['language_test'])
            exported_files.append(test_csv)
        
        # Summary finale
        print(f"\n✅ EXPORT COMPLETATO!")
        print(f"📁 Directory CSV: {self.csv_dir}")
        print(f"📄 File esportati: {len(exported_files)}")
        for f in exported_files:
            size_kb = f.stat().st_size / 1024
            print(f"   • {f.name} ({size_kb:.1f} KB)")
        
        return exported_files

def main():
    parser = argparse.ArgumentParser(description="Esporta risultati SWAM in CSV")
    parser.add_argument("--type", choices=["all", "carbon", "test"], default="all",
                       help="Tipo di export (default: all)")
    parser.add_argument("--latest", action="store_true", default=True,
                       help="Usa risultati più recenti (default)")
    
    args = parser.parse_args()
    
    exporter = SWAMCSVExporter()
    
    if args.type == "all":
        exporter.export_all_latest()
    else:
        print(f"Tipo {args.type} non ancora implementato per singolo export")

if __name__ == "__main__":
    main()