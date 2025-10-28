#!/usr/bin/env python3
"""
Ranking Analyzer - Multi-Tier Ranking System for Carbon Benchmark
Generates rankings by category, complexity, and performance metrics
"""

import json
import statistics
from pathlib import Path
from collections import defaultdict
from datetime import datetime


class RankingAnalyzer:
    """Analyzes benchmark results and generates paradigm-based rankings"""
    
    def __init__(self, benchmark_results=None, results_dir='results/carbon_benchmark'):
        """
        Initialize the ranking analyzer
        
        Args:
            benchmark_results: Dictionary with benchmark data (optional)
            results_dir: Directory containing benchmark results
        """
        self.benchmark_results = benchmark_results
        self.results_dir = Path(results_dir)
        
        # Define programming paradigms
        self.paradigms = {
            'OOP': ['cpp', 'csharp', 'java'],
            'Scripting': ['python', 'ruby', 'javascript', 'typescript'],
            'Imperative': ['c', 'go', 'rust', 'php'],
            'Functional': ['haskell', 'ocaml'],
            'Scientific': ['r', 'julia']
        }
        
        # Reverse mapping: language -> paradigm
        self.lang_to_paradigm = {}
        for paradigm, langs in self.paradigms.items():
            for lang in langs:
                self.lang_to_paradigm[lang.lower()] = paradigm
    
    def load_latest_benchmark(self, mode=None):
        """Load the most recent benchmark results"""
        if mode:
            search_dir = self.results_dir / mode.lower()
        else:
            # Search in all mode directories
            search_dirs = [
                self.results_dir / 'fast',
                self.results_dir / 'top10',
                self.results_dir / 'complete'
            ]
            search_dir = self.results_dir
        
        if mode and (self.results_dir / mode.lower()).exists():
            json_files = list((self.results_dir / mode.lower()).glob('carbon_benchmark_detailed_*.json'))
        else:
            json_files = []
            for dir_path in [self.results_dir / 'fast', self.results_dir / 'top10', self.results_dir / 'complete']:
                if dir_path.exists():
                    json_files.extend(list(dir_path.glob('carbon_benchmark_detailed_*.json')))
        
        if not json_files:
            print(f"⚠️  No benchmark files found in {self.results_dir}")
            return None
        
        latest_file = max(json_files, key=lambda p: p.stat().st_mtime)
        print(f"📂 Loading benchmark from: {latest_file}")
        
        with open(latest_file, 'r') as f:
            self.benchmark_results = json.load(f)
        
        return self.benchmark_results
    
    def determine_category(self, task_name):
        """
        Determine which category a task belongs to
        
        Args:
            task_name: Name of the task
            
        Returns:
            Category name or 'other'
        """
        task_lower = task_name.lower()
        
        # Check predefined categories
        for category, keywords in self.task_categories.items():
            for keyword in keywords:
                if keyword.lower() in task_lower:
                    return category
        
        # Pattern matching for common task types
        if any(word in task_lower for word in ['loop', 'iterate', 'for', 'while']):
            return 'basic'
        elif any(word in task_lower for word in ['sort', 'search', 'algorithm', 'recursive']):
            return 'algorithms'
        elif any(word in task_lower for word in ['file', 'read', 'write', 'io']):
            return 'io'
        elif any(word in task_lower for word in ['math', 'number', 'prime', 'fibonacci', 'factorial']):
            return 'mathematics'
        elif any(word in task_lower for word in ['string', 'text', 'char', 'parse']):
            return 'strings'
        elif any(word in task_lower for word in ['array', 'list', 'tree', 'graph', 'hash']):
            return 'data_structures'
        elif any(word in task_lower for word in ['date', 'time', 'calendar', 'day', 'week']):
            return 'date_time'
        elif any(word in task_lower for word in ['encode', 'decode', 'base64', 'hex', 'binary']):
            return 'encoding'
        elif any(word in task_lower for word in ['hash', 'crypt', 'encrypt', 'signature', 'md5', 'sha']):
            return 'crypto'
        else:
            return 'other'
    
    def get_all_tested_languages(self):
        """Extract all languages from benchmark results"""
        if not self.benchmark_results:
            return []
        
        languages = set()
        for task_data in self.benchmark_results.values():
            languages.update(task_data.keys())
        
        return sorted(languages)
    
    def get_paradigm_single_task(self):
        """
        Find ONE common task per paradigm where ALL languages succeeded
        Selects the task with most balanced emissions (lowest coefficient of variation)
        
        Returns:
            Dictionary mapping paradigm -> single task with stats
        """
        if not self.benchmark_results:
            print("❌ No benchmark data available. Load data first.")
            return None
        
        paradigm_complete_tasks = self.get_paradigm_complete_tasks()
        if not paradigm_complete_tasks:
            return None
        
        paradigm_single_task = {}
        
        for paradigm, tasks in paradigm_complete_tasks.items():
            if not tasks:
                continue
            
            # Select the task with most balanced emissions (lowest CV)
            best_task = None
            best_cv = float('inf')
            
            for task_info in tasks:
                emissions = [stats['emissions_mg'] for stats in task_info['stats'].values()]
                if len(emissions) > 1:
                    mean_emissions = statistics.mean(emissions)
                    std_emissions = statistics.stdev(emissions)
                    cv = std_emissions / mean_emissions if mean_emissions > 0 else float('inf')
                    
                    if cv < best_cv:
                        best_cv = cv
                        best_task = task_info
            
            if best_task:
                paradigm_single_task[paradigm] = best_task
        
        return paradigm_single_task
    
    def get_paradigm_complete_tasks(self):
        """
        Find tasks where ALL languages of a paradigm succeeded
        
        Returns:
            Dictionary mapping paradigm -> list of complete tasks with stats
        """
        if not self.benchmark_results:
            print("❌ No benchmark data available. Load data first.")
            return None
        
        paradigm_complete_tasks = defaultdict(list)
        
        for task_name, task_data in self.benchmark_results.items():
            # Check each paradigm
            for paradigm, paradigm_langs in self.paradigms.items():
                # Check if ALL languages in this paradigm completed this task
                all_complete = True
                paradigm_stats = {}
                
                for lang in paradigm_langs:
                    lang_lower = lang.lower()
                    if lang_lower not in task_data:
                        all_complete = False
                        break
                    
                    lang_stats = task_data[lang_lower]
                    if not lang_stats.get('emissions_stats'):
                        all_complete = False
                        break
                    
                    # Store the stats for this language on this task
                    paradigm_stats[lang] = {
                        'emissions_kg': lang_stats['emissions_stats']['mean'],
                        'emissions_mg': lang_stats['emissions_stats']['mean'] * 1_000_000,
                        'time_s': lang_stats['execution_time_stats']['mean'],
                        'successful_runs': lang_stats.get('successful_runs', 0),
                        'total_runs': lang_stats.get('total_iterations', 0)
                    }
                
                # If all languages in this paradigm completed the task, add it
                if all_complete:
                    paradigm_complete_tasks[paradigm].append({
                        'task': task_name,
                        'stats': paradigm_stats
                    })
        
        return paradigm_complete_tasks
    
    def paradigm_ranking(self):
        """
        Create rankings by programming paradigm
        Uses ONE common task per paradigm (most balanced emissions)
        
        Returns:
            Dictionary with paradigm rankings and selected task info
        """
        if not self.benchmark_results:
            print("❌ No benchmark data available. Load data first.")
            return None
        
        print("\n📊 Generating Paradigm-Based Rankings (Single Task per Paradigm)...")
        print("=" * 60)
        
        # Get ONE task per paradigm
        paradigm_single_task = self.get_paradigm_single_task()
        
        if not paradigm_single_task:
            print("❌ No complete tasks found for any paradigm")
            return None
        
        print(f"\n📁 Selected Task by Paradigm:")
        for paradigm, task_info in sorted(paradigm_single_task.items()):
            print(f"   • {paradigm}: '{task_info['task']}'")
        
        # Calculate rankings for each paradigm using the selected task
        paradigm_rankings = {}
        
        for paradigm, task_info in paradigm_single_task.items():
            paradigm_rankings[paradigm] = {
                'task': task_info['task'],
                'languages': {}
            }
            
            # Get stats for each language in this paradigm on this specific task
            for lang in self.paradigms[paradigm]:
                lang_stats = task_info['stats'].get(lang)
                if lang_stats:
                    paradigm_rankings[paradigm]['languages'][lang] = {
                        'emissions_mg': lang_stats['emissions_mg'],
                        'time_s': lang_stats['time_s'],
                        'successful_runs': lang_stats['successful_runs'],
                        'total_runs': lang_stats['total_runs'],
                        'success_rate': (lang_stats['successful_runs'] / lang_stats['total_runs'] * 100) 
                                       if lang_stats['total_runs'] > 0 else 0
                    }
        
        return paradigm_rankings
    
    def paradigm_summary(self):
        """
        Create summary ranking comparing paradigms
        
        Returns:
            Dictionary with paradigm-level comparisons
        """
        paradigm_rankings = self.paradigm_ranking()
        if not paradigm_rankings:
            return None
        
        paradigm_summary = {}
        
        for paradigm, paradigm_data in paradigm_rankings.items():
            lang_stats = paradigm_data.get('languages', {})
            if not lang_stats:
                continue
            
            # Aggregate statistics across all languages in the paradigm
            all_emissions = [stats['emissions_mg'] for stats in lang_stats.values()]
            all_times = [stats['time_s'] for stats in lang_stats.values()]
            
            paradigm_summary[paradigm] = {
                'task': paradigm_data['task'],
                'paradigm_avg_emissions_mg': statistics.mean(all_emissions),
                'paradigm_avg_time_s': statistics.mean(all_times),
                'best_language_co2': min(lang_stats.items(), key=lambda x: x[1]['emissions_mg'])[0],
                'best_language_time': min(lang_stats.items(), key=lambda x: x[1]['time_s'])[0],
                'languages_count': len(lang_stats),
                'language_details': lang_stats
            }
        
        return paradigm_summary
    
    def overall_ranking(self):
        """
        Create overall ranking across all tasks (legacy compatibility)
        Now redirects to paradigm_ranking
        
        Returns:
            Dictionary with paradigm rankings
        """
        return self.paradigm_ranking()
    
    def _calculate_efficiency_score(self, avg_co2_mg, avg_time_s):
        """
        Calculate efficiency score (lower is better)
        Combines CO2 emissions and execution time
        """
        # Normalize and combine (weighted: 60% CO2, 40% time)
        # Lower values are better
        co2_weight = 0.6
        time_weight = 0.4
        
        # Use log scale to handle wide ranges
        import math
        co2_score = math.log1p(avg_co2_mg) if avg_co2_mg > 0 else 0
        time_score = math.log1p(avg_time_s) if avg_time_s > 0 else 0
        
        return (co2_weight * co2_score) + (time_weight * time_score)
    
    def print_paradigm_rankings(self, paradigm_rankings):
        """Print formatted rankings for each paradigm"""
        if not paradigm_rankings:
            print("❌ No paradigm rankings available")
            return
        
        for paradigm, paradigm_data in sorted(paradigm_rankings.items()):
            lang_rankings = paradigm_data.get('languages', {})
            if not lang_rankings:
                continue
            
            print(f"\n🏆 {paradigm.upper()} PARADIGM")
            print("=" * 60)
            print(f"   Task: '{paradigm_data['task']}'")
            print(f"   Languages: {', '.join([l.upper() for l in self.paradigms[paradigm]])}")
            print()
            
            # Sort by CO2 emissions
            sorted_langs_co2 = sorted(
                lang_rankings.items(),
                key=lambda x: x[1]['emissions_mg']
            )
            
            print("   Ranking by CO₂ Emissions:")
            for i, (lang, stats) in enumerate(sorted_langs_co2, 1):
                co2 = stats['emissions_mg']
                success_rate = stats['success_rate']
                print(f"      {i}. {lang.upper()}: {co2:.2f} mg (Success: {success_rate:.1f}%)")
            
            # Sort by execution time
            sorted_langs_time = sorted(
                lang_rankings.items(),
                key=lambda x: x[1]['time_s']
            )
            
            print("\n   Ranking by Execution Time:")
            for i, (lang, stats) in enumerate(sorted_langs_time, 1):
                time = stats['time_s']
                success_rate = stats['success_rate']
                print(f"      {i}. {lang.upper()}: {time:.3f}s (Success: {success_rate:.1f}%)")
            print()
    
    def print_paradigm_summary(self, paradigm_summary):
        """Print paradigm-level comparison"""
        if not paradigm_summary:
            print("❌ No paradigm summary available")
            return
        
        print(f"\n🏆 PARADIGM COMPARISON SUMMARY")
        print("=" * 60)
        
        # Sort by paradigm average CO2
        print("\nBy Average CO₂ Emissions:")
        sorted_co2 = sorted(
            paradigm_summary.items(),
            key=lambda x: x[1]['paradigm_avg_emissions_mg']
        )
        
        for i, (paradigm, stats) in enumerate(sorted_co2, 1):
            print(f"   {i}. {paradigm.upper()}: {stats['paradigm_avg_emissions_mg']:.2f} mg")
            print(f"      Task: '{stats['task']}' | Best: {stats['best_language_co2'].upper()}")
        
        # Sort by paradigm average time
        print("\nBy Average Execution Time:")
        sorted_time = sorted(
            paradigm_summary.items(),
            key=lambda x: x[1]['paradigm_avg_time_s']
        )
        
        for i, (paradigm, stats) in enumerate(sorted_time, 1):
            print(f"   {i}. {paradigm.upper()}: {stats['paradigm_avg_time_s']:.3f} s")
            print(f"      Task: '{stats['task']}' | Best: {stats['best_language_time'].upper()}")
    
    def print_overall_rankings(self, overall_rankings, top_n=10):
        """Print formatted overall rankings (redirects to paradigm rankings)"""
        return self.print_paradigm_rankings(overall_rankings)
    
    def save_rankings(self, paradigm_rankings, paradigm_summary, output_dir='results/rankings'):
        """Save rankings to JSON files"""
        output_path = Path(output_dir)
        output_path.mkdir(parents=True, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save paradigm rankings
        paradigm_file = output_path / f"paradigm_rankings_{timestamp}.json"
        with open(paradigm_file, 'w') as f:
            json.dump(paradigm_rankings, f, indent=2)
        print(f"\n💾 Paradigm rankings saved: {paradigm_file}")
        
        # Save paradigm summary
        summary_file = output_path / f"paradigm_summary_{timestamp}.json"
        with open(summary_file, 'w') as f:
            json.dump(paradigm_summary, f, indent=2)
        print(f"💾 Paradigm summary saved: {summary_file}")
        
        return paradigm_file, summary_file
    
    def generate_all_rankings(self, save=True):
        """Generate all rankings and optionally save them"""
        if not self.benchmark_results:
            self.load_latest_benchmark()
        
        if not self.benchmark_results:
            print("❌ No benchmark data available")
            return None, None
        
        # Generate rankings
        paradigm_rankings = self.paradigm_ranking()
        paradigm_summary = self.paradigm_summary()
        
        # Print rankings
        self.print_paradigm_rankings(paradigm_rankings)
        self.print_paradigm_summary(paradigm_summary)
        
        # Save rankings
        if save:
            self.save_rankings(paradigm_rankings, paradigm_summary)
        
        return paradigm_rankings, paradigm_summary


def main():
    """Command-line interface for ranking analysis"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Analyze carbon benchmark rankings')
    parser.add_argument('--mode', choices=['FAST', 'TOP10', 'COMPLETE'],
                       help='Benchmark mode to analyze')
    parser.add_argument('--no-save', action='store_true',
                       help='Do not save rankings to files')
    parser.add_argument('--top', type=int, default=10,
                       help='Number of top languages to show (default: 10)')
    
    args = parser.parse_args()
    
    analyzer = RankingAnalyzer()
    
    # Load benchmark
    if args.mode:
        analyzer.load_latest_benchmark(mode=args.mode)
    else:
        analyzer.load_latest_benchmark()
    
    # Generate and display rankings
    analyzer.generate_all_rankings(save=not args.no_save)


if __name__ == '__main__':
    main()
