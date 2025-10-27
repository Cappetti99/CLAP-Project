#!/usr/bin/env python3
"""
CSV Exporter
Export the results of the last run in CSV format for analysis
"""

import json
import pandas as pd
import os
from pathlib import Path
import argparse
from datetime import datetime
import glob

class SWAMCSVExporter:
    """Export the results in CSV format"""
    
    def __init__(self, mode=None):
        """
        Initialize CSV exporter
        
        Args:
            mode: Benchmark mode ('fast', 'top10', 'complete') for organized output.
                  If None, auto-detects from source file or uses root csv/ directory.
        """
        self.results_dir = Path("results")
        self.mode = mode.lower() if mode else None
        
        # Create mode-specific CSV directory
        if self.mode:
            self.csv_dir = self.results_dir / "csv" / self.mode
        else:
            self.csv_dir = self.results_dir / "csv"
        self.csv_dir.mkdir(parents=True, exist_ok=True)
        
    def find_latest_results(self, mode=None):
        """Find the latest result files, optionally filtered by mode
        
        Args:
            mode: Filter results by mode ('fast', 'top10', 'complete').
                  If None, searches all mode directories and root.
        """
        results = {}
        
        # Carbon benchmark results - search in mode-specific directories
        search_dirs = []
        if mode:
            # Search only in specified mode directory
            mode_dir = self.results_dir / "carbon_benchmark" / mode.lower()
            if mode_dir.exists():
                search_dirs.append(mode_dir)
        else:
            # Search in all mode directories and root
            benchmark_dir = self.results_dir / "carbon_benchmark"
            for subdir in ['fast', 'top10', 'complete']:
                mode_dir = benchmark_dir / subdir
                if mode_dir.exists():
                    search_dirs.append(mode_dir)
            # Also check root for legacy files
            if benchmark_dir.exists():
                search_dirs.append(benchmark_dir)
        
        # Find latest across all search directories
        carbon_files = []
        for search_dir in search_dirs:
            carbon_files.extend(list(search_dir.glob("carbon_benchmark_detailed_*.json")))
        
        if carbon_files:
            latest_carbon = max(carbon_files, key=lambda x: x.stat().st_mtime)
            results['carbon_detailed'] = latest_carbon
            
            # Detect mode from file path
            detected_mode = None
            for mode_name in ['fast', 'top10', 'complete']:
                if f"/{mode_name}/" in str(latest_carbon):
                    detected_mode = mode_name
                    break
            results['detected_mode'] = detected_mode
            
            # Find the corresponding summary file
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
        """Export carbon detailed results to CSV"""
        print(f" Loading carbon benchmark from: {json_file.name}")

        with open(json_file, 'r') as f:
            data = json.load(f)

        # Prepare data for DataFrame
        rows = []
        
        # Our JSON starts directly with the tasks, it does not have “results”.
        for task_name, task_data in data.items():
            for lang, lang_results in task_data.items():
                if isinstance(lang_results, dict) and 'all_iterations' in lang_results:
                    # Statistics aggregated by language/task from our format
                    emissions_stats = lang_results.get('emissions_stats', {})
                    time_stats = lang_results.get('execution_time_stats', {})
                    
                    base_row = {
                        'task': task_name,
                        'language': lang,
                        'total_runs': lang_results.get('total_iterations', 0),
                        'successful_runs': lang_results.get('successful_runs', 0),
                        'success_rate': lang_results.get('success_rate', 0),
                        'avg_co2_mg': emissions_stats.get('mean', 0) * 1000000,  # Converti kg a mg
                        'median_co2_mg': emissions_stats.get('median', 0) * 1000000,
                        'min_co2_mg': emissions_stats.get('min', 0) * 1000000,
                        'max_co2_mg': emissions_stats.get('max', 0) * 1000000,
                        'std_co2_mg': emissions_stats.get('std_dev', 0) * 1000000,
                        'total_co2_mg': emissions_stats.get('total', 0) * 1000000,
                        'avg_time_s': time_stats.get('mean', 0),
                        'median_time_s': time_stats.get('median', 0),
                        'min_time_s': time_stats.get('min', 0),
                        'max_time_s': time_stats.get('max', 0),
                        'std_time_s': time_stats.get('std_dev', 0)
                    }
                    rows.append(base_row)
        
        df = pd.DataFrame(rows)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"carbon_benchmark_summary_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV exported: {output_path}")
        print(f" {len(df)} rows | {len(df.columns)} columns")

        return output_path
    
    def export_carbon_individual_runs(self, json_file, output_name=None):
        """Export individual runs to CSV"""
        print(f" Loading individual runs from: {json_file.name}")
        
        with open(json_file, 'r') as f:
            data = json.load(f)
        
        rows = []

        # Our JSON starts directly with the tasks, it does not have 'results'
        for task_name, task_data in data.items():
            for lang, lang_results in task_data.items():
                if isinstance(lang_results, dict) and 'all_iterations' in lang_results:
                    for run in lang_results['all_iterations']:
                        row = {
                            'task': task_name,
                            'language': lang,
                            'run_number': run.get('iteration', 0),
                            'success': run.get('success', False),
                            'co2_mg': run.get('emissions', 0) * 1000000,  # kg to mg
                            'execution_time_s': run.get('execution_time', 0),
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
        print(f"✅ CSV exported: {output_path}")
        print(f" {len(df)} rows | {len(df.columns)} columns")

        return output_path
    
    def export_language_rankings(self, json_file, output_name=None):
        """Export language rankings for energy efficiency"""
        print(f" Loading rankings from: {json_file.name}")
        
        with open(json_file, 'r') as f:
            data = json.load(f)

        # Calculate statistics for each language
        lang_stats = {}
        
        for task_name, task_data in data.items():
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
                    stats['total_runs'] += lang_results.get('total_iterations', 0)
                    stats['successful_runs'] += lang_results.get('successful_runs', 0)
                    stats['total_co2_mg'] += lang_results.get('emissions_stats', {}).get('total', 0) * 1000000  # Convert kg to mg
                    stats['total_time_s'] += lang_results.get('execution_time_stats', {}).get('total', 0)
                    stats['tasks_count'] += 1
                    
                    if 'all_iterations' in lang_results:
                        for run in lang_results['all_iterations']:
                            if run.get('success', False):
                                stats['co2_values'].append(run.get('emissions', 0) * 1000000)  # Convert kg to mg
                                stats['time_values'].append(run.get('execution_time', 0))

        # Create DataFrame ranking
        ranking_rows = []
        for lang, stats in lang_stats.items():
            if stats['successful_runs'] > 0 and stats['total_runs'] > 0:
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
        df = df.sort_values('avg_co2_per_run_mg')  # Sort by energy efficiency
        df['energy_rank'] = range(1, len(df) + 1)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"language_energy_ranking_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV language ranking: {output_path}")
        print(f" {len(df)} languages | {len(df.columns)} metrics")

        return output_path
    
    def export_language_test_results(self, json_file, output_name=None):
        """Export language test results"""
        print(f" Loading language tests from: {json_file.name}")
        
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
                'output': result.get('output', '')[:100],  # Truncate long output
                'error': result.get('error', '')[:200],    # Truncate long error
                'execution_time': result.get('execution_time', 0)
            }
            rows.append(row)
        
        df = pd.DataFrame(rows)
        
        if output_name is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_name = f"language_test_results_{timestamp}.csv"
        
        output_path = self.csv_dir / output_name
        df.to_csv(output_path, index=False)
        print(f"✅ CSV language test results: {output_path}")
        print(f" {len(df)} languages | {len(df.columns)} columns")

        return output_path
    
    def export_all_latest(self):
        """Export all the latest results"""
        print("EXPORT RESULTS OF LAST RUN")
        print("=" * 50)
        
        latest = self.find_latest_results()
        
        if not latest:
            print("❌ No results found!")
            return
        
        # Update csv_dir if mode was auto-detected
        if 'detected_mode' in latest and latest['detected_mode']:
            detected_mode = latest['detected_mode']
            print(f"ℹ️  Using mode-specific directory: {detected_mode}")
            self.csv_dir = self.results_dir / "csv" / detected_mode
            self.csv_dir.mkdir(parents=True, exist_ok=True)
        
        exported_files = []
        
        # Carbon benchmark
        if 'carbon_detailed' in latest:
            print(f"\n CARBON BENCHMARK")
            print(f"File sorgente: {latest['carbon_detailed']}")
            
            # Summary aggregate
            summary_csv = self.export_carbon_detailed(latest['carbon_detailed'])
            exported_files.append(summary_csv)
            
            # Individual runs
            runs_csv = self.export_carbon_individual_runs(latest['carbon_detailed'])
            exported_files.append(runs_csv)
            
            # Language ranking
            ranking_csv = self.export_language_rankings(latest['carbon_detailed'])
            exported_files.append(ranking_csv)
        
        # Language test
        if 'language_test' in latest:
            print(f"\n LANGUAGE TEST")
            print(f"Source file: {latest['language_test']}")
            
            test_csv = self.export_language_test_results(latest['language_test'])
            exported_files.append(test_csv)
        
        # Final summary
        print(f"\n✅ EXPORT COMPLETE!")
        print(f" Directory CSV: {self.csv_dir}")
        print(f" Export File: {len(exported_files)}")
        for f in exported_files:
            size_kb = f.stat().st_size / 1024
            print(f"   • {f.name} ({size_kb:.1f} KB)")
        
        return exported_files

def main():
    parser = argparse.ArgumentParser(description="Export SWAM results to CSV")
    parser.add_argument("--type", choices=["all", "carbon", "test"], default="all",
                       help="Type of export (default: all)")
    parser.add_argument("--latest", action="store_true", default=True,
                       help="Use latest results (default)")
    parser.add_argument("--mode", choices=["FAST", "TOP10", "COMPLETE"], default=None,
                       help="Benchmark mode (FAST/TOP10/COMPLETE). If not specified, auto-detected from results.")
    
    args = parser.parse_args()
    
    exporter = SWAMCSVExporter(mode=args.mode)
    
    if args.type == "all":
        exporter.export_all_latest()
    else:
        print(f"Type {args.type} not yet implemented for single export")

if __name__ == "__main__":
    main()