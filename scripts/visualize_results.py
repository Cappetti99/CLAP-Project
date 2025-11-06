#!/usr/bin/env python3
"""
CLAP Results Visualization Tool
Generate professional charts from CLAP project CSV and JSON results
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import json
import glob
import statistics
from pathlib import Path
import numpy as np
from datetime import datetime

# Chart style configuration
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")
plt.rcParams['figure.figsize'] = (14, 8)
plt.rcParams['font.size'] = 11


class CLAPVisualizer:
    def __init__(self, results_dir='results', mode=None):
        """
        Initialize visualizer with results directory
        
        Args:
            results_dir: Base results directory
            mode: Benchmark mode ('FAST', 'TOP10', 'COMPLETE')
                  If specified, organizes outputs in mode-specific subdirectories
        """
        self.results_dir = Path(results_dir)
        self.mode = mode
        
        # Setup mode-specific directories
        if mode:
            mode_subdir = mode.lower()
            self.csv_dir = self.results_dir / 'csv' / mode_subdir
            self.carbon_dir = self.results_dir / 'carbon_benchmark' / mode_subdir
            self.output_dir = self.results_dir / 'visualizations' / mode_subdir
        else:
            # Try to auto-detect mode from latest results
            detected_mode = self._detect_latest_mode()
            if detected_mode:
                print(f"ℹ️  Auto-detected mode: {detected_mode}")
                mode_subdir = detected_mode.lower()
                self.csv_dir = self.results_dir / 'csv' / mode_subdir
                self.carbon_dir = self.results_dir / 'carbon_benchmark' / mode_subdir
                self.output_dir = self.results_dir / 'visualizations' / mode_subdir
            else:
                # Fallback to root directories
                self.csv_dir = self.results_dir / 'csv'
                self.carbon_dir = self.results_dir / 'carbon'
                self.output_dir = self.results_dir / 'visualizations'
        
        self.output_dir.mkdir(parents=True, exist_ok=True)
    
    def _detect_latest_mode(self):
        """Auto-detect the mode from latest benchmark results"""
        benchmark_dir = self.results_dir / 'carbon_benchmark'
        if not benchmark_dir.exists():
            return None
        
        # Find latest JSON file across all mode directories
        latest_file = None
        latest_time = 0
        detected_mode = None
        
        for mode_name in ['fast', 'top10', 'complete']:
            mode_dir = benchmark_dir / mode_name
            if mode_dir.exists():
                json_files = list(mode_dir.glob('carbon_benchmark_detailed_*.json'))
                for json_file in json_files:
                    mtime = json_file.stat().st_mtime
                    if mtime > latest_time:
                        latest_time = mtime
                        latest_file = json_file
                        detected_mode = mode_name.upper()
        
        return detected_mode
        
    def load_language_energy_ranking(self):
        """Load language energy ranking CSV"""
        csv_files = list(self.csv_dir.glob('language_energy_ranking_*.csv'))
        if csv_files:
            latest = max(csv_files, key=lambda p: p.stat().st_mtime)
            return pd.read_csv(latest)
        return None
    
    def load_top10_tasks(self):
        """Load top tasks data from carbon_benchmark_summary CSV"""
        # Try loading from summary CSV
        csv_files = list(self.csv_dir.glob('carbon_benchmark_summary_*.csv'))
        if csv_files:
            latest = max(csv_files, key=lambda p: p.stat().st_mtime)
            try:
                df = pd.read_csv(latest)
                # Group by task and aggregate
                if 'task' in df.columns:
                    task_stats = df.groupby('task').agg({
                        'avg_co2_mg': 'mean',
                        'language': 'count'
                    }).reset_index()
                    task_stats.columns = ['task', 'avg_co2_mg', 'language_count']
                    # Sort by language count (most common tasks) and limit to top 10
                    task_stats = task_stats.sort_values('language_count', ascending=False).head(10)
                    return task_stats
            except Exception as e:
                print(f"⚠️  Error loading summary CSV: {e}")
        
        # Fallback: legacy file
        csv_file = self.csv_dir / 'top10_tasks_analysis.csv'
        if csv_file.exists():
            return pd.read_csv(csv_file)
        
        return None
    
    def load_carbon_sessions(self, limit=100):
        """Load carbon benchmark JSON files or use CSV data"""
        # Try loading from individual runs CSV first (more reliable)
        csv_files = list(self.csv_dir.glob('carbon_individual_runs_*.csv'))
        if csv_files:
            latest_csv = max(csv_files, key=lambda p: p.stat().st_mtime)
            try:
                return pd.read_csv(latest_csv)
            except Exception as e:
                print(f"⚠️  Error loading CSV {latest_csv.name}: {e}")
        
        # Fallback: try loading JSON files
        json_files = list(self.carbon_dir.glob('carbon_benchmark_detailed_*.json'))
        if not json_files:
            json_files = list(self.carbon_dir.glob('session_*.json'))
        
        if not json_files:
            return None
        
        # Load the most recent JSON and convert to DataFrame
        latest_json = max(json_files, key=lambda p: p.stat().st_mtime)
        try:
            with open(latest_json, 'r') as f:
                data = json.load(f)
            
            # Convert nested JSON to flat rows
            rows = []
            for task_name, task_data in data.items():
                for lang, lang_results in task_data.items():
                    if isinstance(lang_results, dict) and 'all_iterations' in lang_results:
                        for iteration in lang_results['all_iterations']:
                            rows.append({
                                'task': task_name,
                                'language': lang,
                                'co2_mg': iteration.get('emissions', 0) * 1000000,
                                'execution_time_s': iteration.get('execution_time', 0),
                                'success': iteration.get('success', False)
                            })
            
            return pd.DataFrame(rows) if rows else None
        except Exception as e:
            print(f"⚠️  Error loading JSON {latest_json.name}: {e}")
            return None
    
    def plot_language_energy_ranking(self, save=True):
        """Chart 1: Language energy ranking"""
        df = self.load_language_energy_ranking()
        if df is None or df.empty:
            print("❌ No data available for language_energy_ranking")
            return
        
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(18, 8))
        
        # Chart 1a: Average CO2 per language
        colors = sns.color_palette("RdYlGn_r", len(df))
        bars1 = ax1.barh(df['language'], df['avg_co2_per_run_mg'], color=colors)
        ax1.set_xlabel('Average CO₂ per Execution (mg)', fontsize=12, fontweight='bold')
        ax1.set_ylabel('Language', fontsize=12, fontweight='bold')
        ax1.set_title('Energy Ranking: Average CO₂ by Language', 
                     fontsize=14, fontweight='bold', pad=20)
        ax1.invert_yaxis()
        
        # Add values on bars
        for i, (bar, val) in enumerate(zip(bars1, df['avg_co2_per_run_mg'])):
            ax1.text(val + 200, bar.get_y() + bar.get_height()/2, 
                    f'{val:.0f} mg', va='center', fontsize=9)
        
        # Chart 1b: Average time per language
        colors2 = sns.color_palette("YlOrRd", len(df))
        bars2 = ax2.barh(df['language'], df['avg_time_per_run_s'], color=colors2)
        ax2.set_xlabel('Average Time per Execution (s)', fontsize=12, fontweight='bold')
        ax2.set_ylabel('Language', fontsize=12, fontweight='bold')
        ax2.set_title('Performance: Average Time by Language', 
                     fontsize=14, fontweight='bold', pad=20)
        ax2.invert_yaxis()
        
        # Add values on bars
        for bar, val in zip(bars2, df['avg_time_per_run_s']):
            ax2.text(val + 0.05, bar.get_y() + bar.get_height()/2, 
                    f'{val:.3f}s', va='center', fontsize=9)
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'language_energy_ranking.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
        
    def plot_co2_vs_time_scatter(self, save=True):
        """Chart 2: CO2 vs Time scatter plot by language"""
        df = self.load_language_energy_ranking()
        if df is None or df.empty:
            print("❌ No data available")
            return
        
        # Add energy_rank if not present
        if 'energy_rank' not in df.columns:
            df['energy_rank'] = range(len(df))
        
        # Ensure required columns exist
        if 'successful_runs' not in df.columns:
            df['successful_runs'] = df.get('total_runs', 10)
        
        fig, ax = plt.subplots(figsize=(14, 10))
        
        # Scatter plot with size based on successful_runs
        scatter = ax.scatter(df['avg_time_per_run_s'], 
                           df['avg_co2_per_run_mg'],
                           s=df['successful_runs']*3,  # Size proportional to runs
                           c=df['energy_rank'],
                           cmap='RdYlGn_r',
                           alpha=0.7,
                           edgecolors='black',
                           linewidth=1.5)
        
        # Labels for each language
        for idx, row in df.iterrows():
            ax.annotate(row['language'].upper() if isinstance(row['language'], str) else str(row['language']), 
                       (row['avg_time_per_run_s'], row['avg_co2_per_run_mg']),
                       xytext=(5, 5), textcoords='offset points',
                       fontsize=10, fontweight='bold',
                       bbox=dict(boxstyle='round,pad=0.3', 
                               facecolor='yellow', alpha=0.3))
        
        ax.set_xlabel('Average Time per Run (seconds)', fontsize=13, fontweight='bold')
        ax.set_ylabel('Average CO₂ per Run (mg)', fontsize=13, fontweight='bold')
        ax.set_title('CO₂ vs Execution Time by Language\n(Bubble size = number of runs)', 
                    fontsize=15, fontweight='bold', pad=20)
        ax.grid(True, alpha=0.3)
        
        # Colorbar
        cbar = plt.colorbar(scatter, ax=ax)
        cbar.set_label('Energy Ranking', fontsize=11, fontweight='bold')
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'co2_vs_time_scatter.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def plot_top_tasks_comparison(self, save=True):
        """Chart 3: Top Tasks comparison by CO2"""
        # Try loading summary CSV first
        csv_files = list(self.csv_dir.glob('carbon_benchmark_summary_*.csv'))
        if not csv_files:
            print("❌ No data available for top10_tasks")
            return
        
        latest = max(csv_files, key=lambda p: p.stat().st_mtime)
        try:
            df = pd.read_csv(latest)
        except Exception as e:
            print(f"❌ Error loading CSV: {e}")
            return
        
        # Check required columns
        if 'task' not in df.columns or 'avg_co2_mg' not in df.columns:
            print("❌ Required columns (task, avg_co2_mg) not found in CSV")
            return
        
        # Filter valid data
        df_valid = df[df['avg_co2_mg'] > 0].copy()
        
        if df_valid.empty:
            print("❌ No valid data for tasks comparison")
            return
        
        # Get top 10 tasks by CO2 or by language count
        if 'language' in df.columns:
            # Count languages per task
            task_counts = df_valid.groupby('task')['language'].count()
            top_tasks = task_counts.nlargest(10).index
        else:
            # Fallback: top by CO2
            task_co2 = df_valid.groupby('task')['avg_co2_mg'].mean()
            top_tasks = task_co2.nlargest(10).index
        
        df_top = df_valid[df_valid['task'].isin(top_tasks)]
        
        if df_top.empty:
            print("❌ No data for top tasks")
            return
        
        fig, ax = plt.subplots(figsize=(18, 12))
        
        # Pivot for heatmap
        if 'language' in df_top.columns:
            pivot = df_top.pivot_table(values='avg_co2_mg', 
                                       index='task', 
                                       columns='language',
                                       aggfunc='mean',
                                       fill_value=0)
        else:
            # Simple bar chart if no language column
            task_co2 = df_top.groupby('task')['avg_co2_mg'].mean().sort_values(ascending=False)
            ax.barh(range(len(task_co2)), task_co2.values)
            ax.set_yticks(range(len(task_co2)))
            ax.set_yticklabels(task_co2.index)
            ax.set_xlabel('Average CO₂ (mg)', fontsize=12, fontweight='bold')
            ax.set_ylabel('Task', fontsize=12, fontweight='bold')
            ax.set_title('Top Tasks by CO₂ Emissions', fontsize=15, fontweight='bold', pad=20)
            plt.tight_layout()
            if save:
                output_path = self.output_dir / 'top_tasks_co2_heatmap.png'
                plt.savefig(output_path, dpi=300, bbox_inches='tight')
                print(f"✅ Saved: {output_path}")
            plt.show()
            return
        
        # Heatmap
        sns.heatmap(pivot, annot=True, fmt='.0f', cmap='YlOrRd', 
                   linewidths=0.5, ax=ax, cbar_kws={'label': 'Average CO₂ (mg)'})
        
        ax.set_title('CO₂ Comparison by Task and Language (Top 10 Tasks)', 
                    fontsize=15, fontweight='bold', pad=20)
        ax.set_xlabel('Language', fontsize=12, fontweight='bold')
        ax.set_ylabel('Task', fontsize=12, fontweight='bold')
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'top_tasks_co2_heatmap.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def load_latest_benchmark_results(self):
        """Load the most recent detailed benchmark results from carbon_dir"""
        if not self.carbon_dir.exists():
            print(f"⚠️  Carbon directory not found: {self.carbon_dir}")
            return None
        
        # Find the latest detailed file in the mode-specific directory
        detailed_files = list(self.carbon_dir.glob("carbon_benchmark_detailed_*.json"))
        
        if not detailed_files:
            # Try also checking session files as fallback
            detailed_files = list(self.carbon_dir.glob("session_*.json"))
        
        if not detailed_files:
            print(f"⚠️  No benchmark files found in: {self.carbon_dir}")
            return None
        
        latest_file = max(detailed_files, key=lambda x: x.stat().st_mtime)
        print(f"📂 Loading benchmark results from: {latest_file.name}")
        
        try:
            with open(latest_file, 'r') as f:
                return json.load(f)
        except Exception as e:
            print(f"❌ Error loading {latest_file.name}: {e}")
            return None
    
    def calculate_language_stats(self, data):
        """Calculate success statistics for each language"""
        lang_stats = {}
        
        for task_name, task_data in data.items():
            for lang, lang_data in task_data.items():
                if lang not in lang_stats:
                    lang_stats[lang] = {
                        'total': 0,
                        'success': 0,
                        'fail': 0,
                        'partial': 0,
                        'tasks': []
                    }
                
                success_rate = lang_data.get('success_rate', 0)
                lang_stats[lang]['total'] += 1
                lang_stats[lang]['tasks'].append({
                    'name': task_name,
                    'success_rate': success_rate
                })
                
                if success_rate == 100:
                    lang_stats[lang]['success'] += 1
                elif success_rate > 0:
                    lang_stats[lang]['partial'] += 1
                else:
                    lang_stats[lang]['fail'] += 1
        
        return lang_stats
    
    def plot_language_success_rates(self, save=True):
        """Chart: Language Success Rates (task-based)"""
        data = self.load_latest_benchmark_results()
        if not data:
            print("❌ No benchmark data available")
            return
        
        lang_stats = self.calculate_language_stats(data)
        
        # Prepare data
        languages = []
        success_rates = []
        colors = []
        
        for lang in sorted(lang_stats.keys()):
            stats = lang_stats[lang]
            total = stats['total']
            success_rate = (stats['success'] / total * 100) if total > 0 else 0
            
            languages.append(lang.upper())
            success_rates.append(success_rate)
            
            # Color based on success rate
            if success_rate >= 80:
                colors.append('#2ecc71')  # Green
            elif success_rate >= 50:
                colors.append('#f39c12')  # Orange
            else:
                colors.append('#e74c3c')  # Red
        
        # Sort by success rate
        sorted_indices = np.argsort(success_rates)[::-1]
        languages = [languages[i] for i in sorted_indices]
        success_rates = [success_rates[i] for i in sorted_indices]
        colors = [colors[i] for i in sorted_indices]
        
        # Create figure
        fig, ax = plt.subplots(figsize=(14, 8))
        
        # Create bar chart
        bars = ax.barh(languages, success_rates, color=colors, edgecolor='black', linewidth=0.5)
        
        # Add value labels on bars
        for i, (bar, rate) in enumerate(zip(bars, success_rates)):
            width = bar.get_width()
            ax.text(width + 2, bar.get_y() + bar.get_height()/2, 
                    f'{rate:.0f}%',
                    ha='left', va='center', fontweight='bold', fontsize=9)
        
        # Styling
        ax.set_xlabel('Success Rate (%)', fontsize=12, fontweight='bold')
        ax.set_ylabel('Programming Language', fontsize=12, fontweight='bold')
        ax.set_title('Language Success Rates\n(Tasks with 100% successful iterations)', 
                     fontsize=14, fontweight='bold', pad=20)
        ax.set_xlim(0, 110)
        ax.grid(axis='x', alpha=0.3)
        
        # Add legend
        from matplotlib.patches import Patch
        legend_elements = [
            Patch(facecolor='#2ecc71', label='≥80% (Excellent)'),
            Patch(facecolor='#f39c12', label='50-79% (Good)'),
            Patch(facecolor='#e74c3c', label='<50% (Poor)')
        ]
        ax.legend(handles=legend_elements, loc='lower right', framealpha=0.9)
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'language_success_rates.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def plot_success_breakdown(self, save=True):
        """Chart: Detailed success breakdown (full/partial/failed)"""
        data = self.load_latest_benchmark_results()
        if not data:
            print("❌ No benchmark data available")
            return
        
        lang_stats = self.calculate_language_stats(data)
        
        # Prepare data
        languages = []
        full_success = []
        partial_success = []
        failures = []
        
        for lang in sorted(lang_stats.keys()):
            stats = lang_stats[lang]
            
            languages.append(lang.upper())
            full_success.append(stats['success'])
            partial_success.append(stats['partial'])
            failures.append(stats['fail'])
        
        # Sort by total success (full + partial)
        total_success = [f + p for f, p in zip(full_success, partial_success)]
        sorted_indices = np.argsort(total_success)[::-1]
        
        languages = [languages[i] for i in sorted_indices]
        full_success = [full_success[i] for i in sorted_indices]
        partial_success = [partial_success[i] for i in sorted_indices]
        failures = [failures[i] for i in sorted_indices]
        
        # Create figure
        fig, ax = plt.subplots(figsize=(14, 8))
        
        # Create stacked bars
        y_pos = np.arange(len(languages))
        
        p1 = ax.barh(y_pos, full_success, color='#2ecc71', label='100% Success', edgecolor='black', linewidth=0.5)
        p2 = ax.barh(y_pos, partial_success, left=full_success, color='#f39c12', label='Partial Success', edgecolor='black', linewidth=0.5)
        p3 = ax.barh(y_pos, failures, left=[f+p for f,p in zip(full_success, partial_success)], 
                     color='#e74c3c', label='Failed', edgecolor='black', linewidth=0.5)
        
        # Add value labels
        for i, (f, p, fail) in enumerate(zip(full_success, partial_success, failures)):
            total = f + p + fail
            # Full success label
            if f > 0:
                ax.text(f/2, i, str(f), ha='center', va='center', fontweight='bold', color='white', fontsize=9)
            # Partial success label
            if p > 0:
                ax.text(f + p/2, i, str(p), ha='center', va='center', fontweight='bold', color='white', fontsize=9)
            # Failure label
            if fail > 0:
                ax.text(f + p + fail/2, i, str(fail), ha='center', va='center', fontweight='bold', color='white', fontsize=9)
        
        # Styling
        ax.set_yticks(y_pos)
        ax.set_yticklabels(languages)
        ax.set_xlabel('Number of Tasks', fontsize=12, fontweight='bold')
        ax.set_ylabel('Programming Language', fontsize=12, fontweight='bold')
        ax.set_title('Task Success Breakdown by Language\n(Full Success / Partial Success / Failed)', 
                     fontsize=14, fontweight='bold', pad=20)
        ax.legend(loc='lower right', framealpha=0.9)
        ax.grid(axis='x', alpha=0.3)
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'language_success_breakdown.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def plot_task_language_heatmap(self, save=True):
        """Chart: Task × Language success rate heatmap"""
        data = self.load_latest_benchmark_results()
        if not data:
            print("❌ No benchmark data available")
            return
        
        lang_stats = self.calculate_language_stats(data)
        
        # Get all tasks and languages
        all_tasks = sorted(set(data.keys()))
        all_languages = sorted(lang_stats.keys())
        
        # Create matrix
        matrix = np.zeros((len(all_tasks), len(all_languages)))
        
        for i, task in enumerate(all_tasks):
            for j, lang in enumerate(all_languages):
                if lang in data[task]:
                    task_data = data[task][lang]
                    # Check if file actually exists (not just attempted)
                    if task_data.get('error') == 'File not available in dataset':
                        matrix[i, j] = np.nan  # Gray = file doesn't exist
                    else:
                        matrix[i, j] = task_data.get('success_rate', 0)  # Red-Green = exists
                else:
                    matrix[i, j] = np.nan
        
        # Create figure
        fig, ax = plt.subplots(figsize=(16, 10))
        
        # Create custom colormap: Gray for NaN, Red-Yellow-Green for 0-100%
        from matplotlib.colors import LinearSegmentedColormap
        import matplotlib.patches as mpatches
        
        # Create heatmap with NaN shown in gray
        cmap = sns.color_palette("RdYlGn", as_cmap=True)
        cmap.set_bad(color='#CCCCCC')  # Set NaN color to darker gray for better visibility
        
        sns.heatmap(matrix, 
                    xticklabels=[l.upper() for l in all_languages],
                    yticklabels=all_tasks,
                    cmap=cmap,
                    vmin=0, vmax=100,
                    cbar_kws={'label': 'Success Rate (%)'},
                    linewidths=0.5,
                    linecolor='gray',
                    ax=ax)
        
        # Styling
        ax.set_xlabel('Programming Language', fontsize=12, fontweight='bold')
        ax.set_ylabel('Task', fontsize=12, fontweight='bold')
        ax.set_title('Carbon Benchmark: Success Rate Heatmap\n(Task × Language)', 
                     fontsize=14, fontweight='bold', pad=20)
        
        # Add legend for NaN values
        legend_elements = [mpatches.Patch(facecolor='#CCCCCC', edgecolor='gray', 
                                         label='Task not available in dataset')]
        ax.legend(handles=legend_elements, loc='upper left', bbox_to_anchor=(1.15, 1))
        
        plt.xticks(rotation=45, ha='right')
        plt.yticks(rotation=0)
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'language_task_heatmap.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def plot_co2_distribution_boxplot(self, save=True):
        """Chart 5: CO2 Distribution Boxplot by Language"""
        # Load individual runs CSV for detailed distribution
        csv_files = list(self.csv_dir.glob('carbon_individual_runs_*.csv'))
        if not csv_files:
            # Fallback to summary
            csv_files = list(self.csv_dir.glob('carbon_benchmark_summary_*.csv'))
        
        if not csv_files:
            print("❌ No data available")
            return
        
        latest = max(csv_files, key=lambda p: p.stat().st_mtime)
        try:
            df = pd.read_csv(latest)
        except Exception as e:
            print(f"❌ Error loading CSV: {e}")
            return
        
        # Check which columns are available
        if 'co2_mg' in df.columns and 'language' in df.columns:
            # Individual runs format
            df_valid = df[df['co2_mg'] > 0].copy()
            co2_col = 'co2_mg'
        elif 'avg_co2_mg' in df.columns and 'language' in df.columns:
            # Summary format
            df_valid = df[df['avg_co2_mg'] > 0].copy()
            co2_col = 'avg_co2_mg'
        else:
            print("❌ No CO2 data columns found in CSV")
            return
        
        if df_valid.empty:
            print("❌ No valid data available")
            return
        
        fig, ax = plt.subplots(figsize=(16, 8))
        
        # Prepare data for boxplot
        languages = sorted(df_valid['language'].unique())
        data_by_lang = [df_valid[df_valid['language'] == lang][co2_col].values 
                        for lang in languages]
        
        # Create boxplot
        bp = ax.boxplot(data_by_lang,
                        labels=[lang.upper() for lang in languages],
                        patch_artist=True,
                        showmeans=True,
                        meanline=True)
        
        # Color the boxes
        colors = sns.color_palette("Set3", len(bp['boxes']))
        for patch, color in zip(bp['boxes'], colors):
            patch.set_facecolor(color)
            patch.set_alpha(0.7)
        
        ax.set_xlabel('Language', fontsize=12, fontweight='bold')
        ax.set_ylabel('CO₂ (mg)', fontsize=12, fontweight='bold')
        ax.set_title('CO₂ Distribution by Language (Boxplot)', 
                    fontsize=14, fontweight='bold', pad=20)
        ax.grid(axis='y', alpha=0.3)
        plt.xticks(rotation=45, ha='right')
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'co2_distribution_boxplot.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def load_latest_rankings(self):
        """Load the most recent paradigm rankings JSON file"""
        rankings_dir = self.results_dir / 'rankings'
        if not rankings_dir.exists():
            print(f"❌ Rankings directory not found: {rankings_dir}")
            return None
        
        # Try to load paradigm rankings first (new format)
        json_files = list(rankings_dir.glob('paradigm_rankings_*.json'))
        if json_files:
            latest_file = max(json_files, key=lambda x: x.stat().st_mtime)
            print(f"📊 Loading paradigm rankings from: {latest_file.name}")
            
            with open(latest_file, 'r', encoding='utf-8') as f:
                return json.load(f)
        
        # Fallback to old overall_rankings format
        json_files = list(rankings_dir.glob('overall_rankings_*.json'))
        if not json_files:
            print(f"❌ No ranking files found in {rankings_dir}")
            return None
        
        latest_file = max(json_files, key=lambda x: x.stat().st_mtime)
        print(f"📊 Loading rankings from: {latest_file.name}")
        
        with open(latest_file, 'r', encoding='utf-8') as f:
            return json.load(f)
    
    def plot_efficiency_rankings(self, save=True):
        """Chart 8: Paradigm-based Efficiency Rankings with metrics visualization"""
        rankings = self.load_latest_rankings()
        if not rankings:
            print("❌ No rankings data available")
            return
        
        # Check if it's paradigm-based or legacy format
        is_paradigm_based = any(key in ['OOP', 'Scripting', 'Imperative', 'Functional', 'Scientific'] 
                                for key in rankings.keys())
        
        if is_paradigm_based:
            self._plot_paradigm_rankings(rankings, save)
        else:
            self._plot_legacy_rankings(rankings, save)
    
    def plot_energy_consumption_per_run(self, save=True):
        """Chart 9: Average Energy Consumption per Run by Language"""
        # Load individual runs CSV for detailed per-run statistics
        csv_files = list(self.csv_dir.glob('carbon_individual_runs_*.csv'))
        if not csv_files:
            # Fallback to summary
            csv_files = list(self.csv_dir.glob('carbon_benchmark_summary_*.csv'))
        
        if not csv_files:
            print("❌ No data available for energy consumption analysis")
            return
        
        latest = max(csv_files, key=lambda p: p.stat().st_mtime)
        try:
            df = pd.read_csv(latest)
        except Exception as e:
            print(f"❌ Error loading CSV: {e}")
            return
        
        # Check which columns are available and calculate per-run energy
        if 'co2_mg' in df.columns and 'language' in df.columns:
            # Individual runs format - already per-run
            df_valid = df[df['co2_mg'] > 0].copy()
            
            # Calculate average per language
            lang_stats = df_valid.groupby('language').agg({
                'co2_mg': ['mean', 'std', 'count']
            }).reset_index()
            
            lang_stats.columns = ['language', 'avg_co2_per_run', 'std_co2', 'run_count']
            
        elif 'avg_co2_mg' in df.columns and 'language' in df.columns:
            # Summary format - already has averages
            df_valid = df[df['avg_co2_mg'] > 0].copy()
            
            lang_stats = df_valid.groupby('language').agg({
                'avg_co2_mg': 'mean'
            }).reset_index()
            
            lang_stats.columns = ['language', 'avg_co2_per_run']
            lang_stats['std_co2'] = 0
            lang_stats['run_count'] = len(df_valid)
        else:
            print("❌ No CO2 data columns found in CSV")
            return
        
        if lang_stats.empty:
            print("❌ No valid data available")
            return
        
        # Sort by average CO2
        lang_stats = lang_stats.sort_values('avg_co2_per_run')
        
        # Create single large figure with more height for spacing
        num_langs = len(lang_stats)
        fig_height = max(10, num_langs * 0.8)  # Dynamic height based on number of languages
        fig = plt.figure(figsize=(16, fig_height))
        
        # Main title
        fig.suptitle('Energy Consumption per Execution Run', 
                     fontsize=18, fontweight='bold', y=0.98)
        
        # ===== MAIN CHART: Bar chart with gradient colors =====
        ax = plt.subplot(1, 1, 1)
        
        languages = [lang.upper() for lang in lang_stats['language']]
        avg_values = lang_stats['avg_co2_per_run'].values
        std_values = lang_stats['std_co2'].values if 'std_co2' in lang_stats.columns else np.zeros(len(avg_values))
        
        # Create beautiful gradient from green (efficient) to red (inefficient)
        norm = plt.Normalize(vmin=min(avg_values), vmax=max(avg_values))
        colors = plt.cm.RdYlGn_r(norm(avg_values))
        
        # Create horizontal bars with more spacing
        bars = ax.barh(range(len(languages)), avg_values, 
                      color=colors, edgecolor='black', linewidth=1.2,
                      height=0.6, alpha=0.85)
        
        # Add error bars if available (only if they won't clutter)
        if std_values.max() > 0 and std_values.max() < max(avg_values) * 0.3:
            ax.errorbar(avg_values, range(len(languages)), 
                       xerr=std_values, fmt='none',
                       ecolor='#555555', elinewidth=1.5, capsize=4, capthick=1.5,
                       alpha=0.6, zorder=10)
        
        # Customize axes with more padding
        ax.set_yticks(range(len(languages)))
        ax.set_yticklabels(languages, fontsize=12, fontweight='bold')
        ax.set_xlabel('Average CO₂ Emissions per Run (mg)', fontsize=13, fontweight='bold', labelpad=10)
        ax.set_title('Languages Ranked by Energy Efficiency (Lower = Better)', 
                    fontsize=14, fontweight='bold', pad=20)
        
        # Add grid
        ax.grid(axis='x', alpha=0.25, linestyle='--', linewidth=0.8)
        ax.set_axisbelow(True)
        
        # Extend x-axis to make room for labels
        ax.set_xlim(0, max(avg_values) * 1.25)
        
        # Add value labels on bars - simplified
        for i, (val, std) in enumerate(zip(avg_values, std_values)):
            # Position label outside the bar
            x_pos = val + max(avg_values) * 0.015
            
            # Simplified label - always black text
            label = f'{val:.1f} mg'
            
            ax.text(x_pos, i, label,
                   va='center', ha='left',
                   fontsize=10, fontweight='bold',
                   color='black',
                   bbox=dict(boxstyle='round,pad=0.3', 
                            facecolor='white', 
                            edgecolor='gray',
                            linewidth=0.5,
                            alpha=0.9))
        
        # Calculate statistics
        total_runs = lang_stats['run_count'].sum() if 'run_count' in lang_stats.columns else 0
        avg_all = lang_stats['avg_co2_per_run'].mean()
        min_co2 = lang_stats['avg_co2_per_run'].min()
        max_co2 = lang_stats['avg_co2_per_run'].max()
        
        # Add compact statistics in title area instead of overlapping box
        stats_line = f'Statistics: {len(lang_stats)} languages | Avg: {avg_all:.1f} mg | Range: {min_co2:.1f} - {max_co2:.1f} mg'
        if total_runs > 0:
            stats_line += f' | Total runs: {total_runs:,}'
        
        fig.text(0.5, 0.94, stats_line,
                ha='center', fontsize=11, style='italic', color='#555555',
                bbox=dict(boxstyle='round,pad=0.5', facecolor='#f0f0f0', 
                         edgecolor='#cccccc', alpha=0.8))
        
        plt.tight_layout(rect=[0, 0.02, 1, 0.92])
        
        if save:
            output_path = self.output_dir / 'energy_consumption_per_run.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight', facecolor='white')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def _plot_paradigm_rankings(self, paradigm_rankings, save=True):
        """Plot paradigm-based rankings with two separate charts: CO2 and Time"""
        # Define colors for each paradigm
        paradigm_colors = {
            'OOP': '#FF6B6B',
            'Scripting': '#4ECDC4',
            'Imperative': '#45B7D1',
            'Functional': '#FFA07A',
            'Scientific': '#98D8C8'
        }
        
        # Prepare data for CO2 plot
        all_langs_co2 = []
        all_co2 = []
        all_colors_co2 = []
        paradigm_tasks = {}
        
        # Track where each paradigm starts for adding task labels
        paradigm_positions_co2 = {}
        current_position = 0
        
        for paradigm, paradigm_data in sorted(paradigm_rankings.items()):
            task_name = paradigm_data.get('task', 'Unknown')
            paradigm_tasks[paradigm] = task_name
            lang_stats = paradigm_data.get('languages', {})
            
            # Sort by CO2
            sorted_by_co2 = sorted(lang_stats.items(), key=lambda x: x[1]['emissions_mg'])
            
            # Store starting position for this paradigm
            paradigm_positions_co2[paradigm] = current_position + len(sorted_by_co2) / 2
            
            for lang, stats in sorted_by_co2:
                all_langs_co2.append(lang.upper())
                all_co2.append(stats['emissions_mg'])
                all_colors_co2.append(paradigm_colors.get(paradigm, '#CCCCCC'))
                current_position += 1
        
        # Prepare data for Time plot
        all_langs_time = []
        all_time = []
        all_colors_time = []
        
        # Track where each paradigm starts for adding task labels
        paradigm_positions_time = {}
        current_position = 0
        
        for paradigm, paradigm_data in sorted(paradigm_rankings.items()):
            lang_stats = paradigm_data.get('languages', {})
            
            # Sort by Time
            sorted_by_time = sorted(lang_stats.items(), key=lambda x: x[1]['time_s'])
            
            # Store starting position for this paradigm
            paradigm_positions_time[paradigm] = current_position + len(sorted_by_time) / 2
            
            for lang, stats in sorted_by_time:
                all_langs_time.append(lang.upper())
                all_time.append(stats['time_s'])
                all_colors_time.append(paradigm_colors.get(paradigm, '#CCCCCC'))
                current_position += 1
        
        # Create figure with 2 subplots with dynamic height
        num_langs = len(all_langs_co2)
        fig_height = max(14, num_langs * 0.7)
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(16, fig_height))
        fig.suptitle('Paradigm-Based Language Rankings\n(Single Task per Paradigm)', 
                     fontsize=16, fontweight='bold', y=0.995)
        
        # ===== Plot 1: CO2 Emissions =====
        bars1 = ax1.barh(range(len(all_langs_co2)), all_co2, color=all_colors_co2, 
                         edgecolor='black', linewidth=1.0, height=0.6, alpha=0.85)
        
        ax1.set_yticks(range(len(all_langs_co2)))
        ax1.set_yticklabels(all_langs_co2, fontsize=11, fontweight='bold')
        ax1.set_xlabel('CO₂ Emissions (mg)', fontsize=13, fontweight='bold', labelpad=10)
        ax1.set_title('Ranking by CO₂ Emissions (100%)', fontsize=14, fontweight='bold', pad=15)
        ax1.invert_yaxis()
        ax1.grid(axis='x', alpha=0.25, linestyle='--', linewidth=0.8)
        ax1.set_axisbelow(True)
        
        # Extend x-axis to make room for labels
        ax1.set_xlim(0, max(all_co2) * 1.2)
        
        # Add value labels for CO2 - black text with white background
        for i, co2 in enumerate(all_co2):
            x_pos = co2 + max(all_co2) * 0.01
            ax1.text(x_pos, i, f'{co2:.0f} mg',
                    va='center', ha='left',
                    fontsize=10, fontweight='bold',
                    color='black',
                    bbox=dict(boxstyle='round,pad=0.3', 
                             facecolor='white', 
                             edgecolor='gray',
                             linewidth=0.5,
                             alpha=0.9))
        
        # Add paradigm + task labels on the left side - better positioned
        for paradigm, position in paradigm_positions_co2.items():
            task_name = paradigm_tasks.get(paradigm, 'N/A')
            ax1.text(-max(all_co2)*0.12, position, 
                    f'{paradigm}\n[{task_name}]',
                    va='center', ha='right', fontsize=9, fontweight='bold',
                    color=paradigm_colors.get(paradigm, '#000'),
                    bbox=dict(boxstyle='round,pad=0.4', facecolor='white', 
                             edgecolor=paradigm_colors.get(paradigm, '#000'), 
                             linewidth=1.5, alpha=0.9))
        
        # ===== Plot 2: Execution Time =====
        bars2 = ax2.barh(range(len(all_langs_time)), all_time, color=all_colors_time, 
                         edgecolor='black', linewidth=1.0, height=0.6, alpha=0.85)
        
        ax2.set_yticks(range(len(all_langs_time)))
        ax2.set_yticklabels(all_langs_time, fontsize=11, fontweight='bold')
        ax2.set_xlabel('Execution Time (seconds)', fontsize=13, fontweight='bold', labelpad=10)
        ax2.set_title('Ranking by Execution Time (100%)', fontsize=14, fontweight='bold', pad=15)
        ax2.invert_yaxis()
        ax2.grid(axis='x', alpha=0.25, linestyle='--', linewidth=0.8)
        ax2.set_axisbelow(True)
        
        # Extend x-axis to make room for labels
        ax2.set_xlim(0, max(all_time) * 1.2)
        
        # Add value labels for Time - black text with white background
        for i, time in enumerate(all_time):
            x_pos = time + max(all_time) * 0.01
            ax2.text(x_pos, i, f'{time:.3f}s',
                    va='center', ha='left',
                    fontsize=10, fontweight='bold',
                    color='black',
                    bbox=dict(boxstyle='round,pad=0.3', 
                             facecolor='white', 
                             edgecolor='gray',
                             linewidth=0.5,
                             alpha=0.9))
        
        # Add paradigm + task labels on the left side - better positioned
        for paradigm, position in paradigm_positions_time.items():
            task_name = paradigm_tasks.get(paradigm, 'N/A')
            ax2.text(-max(all_time)*0.12, position, 
                    f'{paradigm}\n[{task_name}]',
                    va='center', ha='right', fontsize=9, fontweight='bold',
                    color=paradigm_colors.get(paradigm, '#000'),
                    bbox=dict(boxstyle='round,pad=0.4', facecolor='white', 
                             edgecolor=paradigm_colors.get(paradigm, '#000'), 
                             linewidth=1.5, alpha=0.9))
        
        # Add methodology note
        fig.text(0.5, 0.01, 
                'Ranking Method: Each paradigm uses ONE common task where ALL its languages succeeded. '
                'Task selected based on most balanced emissions.',
                ha='center', fontsize=10, style='italic', color='#555555',
                bbox=dict(boxstyle='round,pad=0.5', facecolor='#f0f0f0', 
                         edgecolor='#cccccc', alpha=0.8))
        
        plt.tight_layout(rect=[0, 0.03, 1, 0.99])
        
        if save:
            output_path = self.output_dir / 'efficiency_rankings.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight', facecolor='white')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def _plot_legacy_rankings(self, rankings, save=True):
        """Plot legacy format rankings (for backward compatibility)"""
        print("ℹ️  Using legacy ranking format")
        
        # Prepare data sorted by efficiency score
        languages = []
        efficiency_scores = []
        avg_co2 = []
        avg_time = []
        
        for lang, stats in sorted(rankings.items(), key=lambda x: x[1].get('efficiency_score', 999)):
            languages.append(lang.upper())
            efficiency_scores.append(stats.get('efficiency_score', 0))
            avg_co2.append(stats['avg_emissions_mg'])
            avg_time.append(stats['avg_time_s'])
        
        # Take top 15 languages
        top_n = min(15, len(languages))
        languages = languages[:top_n]
        efficiency_scores = efficiency_scores[:top_n]
        avg_co2 = avg_co2[:top_n]
        avg_time = avg_time[:top_n]
        
        # Create simple barplot
        fig, ax = plt.subplots(figsize=(14, 10))
        colors = plt.cm.RdYlGn_r(np.linspace(0.2, 0.8, top_n))
        bars = ax.barh(languages, efficiency_scores, color=colors, edgecolor='black', linewidth=0.8)
        
        ax.set_xlabel('Efficiency Score (lower is better)', fontsize=12, fontweight='bold')
        ax.set_ylabel('Programming Language', fontsize=12, fontweight='bold')
        ax.set_title('Language Efficiency Rankings\n(60% CO₂ + 40% Execution Time)', 
                     fontsize=14, fontweight='bold', pad=20)
        ax.invert_yaxis()
        ax.grid(axis='x', alpha=0.3)
        
        # Add value labels
        for i, (bar, score) in enumerate(zip(bars, efficiency_scores)):
            ax.text(bar.get_width() + 0.05, bar.get_y() + bar.get_height()/2, 
                    f'{score:.2f}',
                    va='center', fontsize=9, fontweight='bold')
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'efficiency_rankings.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def plot_paradigm_energy_consumption(self, save=True):
        """Energy Consumption (Wh) calculated from CO2 emissions"""
        rankings = self.load_latest_rankings()
        if not rankings:
            print("❌ No rankings data available")
            return
        
        # Check if it's paradigm-based format
        is_paradigm_based = any(key in ['OOP', 'Scripting', 'Imperative', 'Functional', 'Scientific'] 
                                for key in rankings.keys())
        
        if not is_paradigm_based:
            print("❌ This chart requires paradigm-based rankings")
            return
        
        # Define colors for each paradigm
        paradigm_colors = {
            'OOP': '#FF6B6B',
            'Scripting': '#4ECDC4',
            'Imperative': '#45B7D1',
            'Functional': '#FFA07A',
            'Scientific': '#98D8C8'
        }
        
        # CodeCarbon conversion constants
        # FE = Emission Factor for Italy (average) in g CO₂ / Wh
        EMISSION_FACTOR = 0.231  # g CO₂ per Wh (Italy average)
        
        # Prepare data - calculate energy consumption in Wh
        all_langs = []
        all_co2 = []
        all_time = []
        all_energy_wh = []  # Energy in Wh
        all_colors = []
        paradigm_tasks = {}
        
        # Track where each paradigm starts
        paradigm_positions = {}
        current_position = 0
        
        for paradigm, paradigm_data in sorted(rankings.items()):
            task_name = paradigm_data.get('task', 'Unknown')
            paradigm_tasks[paradigm] = task_name
            lang_stats = paradigm_data.get('languages', {})
            
            lang_energy = []
            for lang, stats in lang_stats.items():
                co2_mg = stats['emissions_mg']
                time_s = stats['time_s']
                
                # Formula: E(Wh) = CO₂(mg) / (FE(g/Wh) × 10³)
                # This converts CO₂ emissions back to energy consumed
                energy_wh = co2_mg / (EMISSION_FACTOR * 1000)
                
                lang_energy.append((lang, co2_mg, time_s, energy_wh))
            
            # Sort by energy consumption (ascending - best first)
            lang_energy.sort(key=lambda x: x[3])
            
            # Store starting position for this paradigm
            paradigm_positions[paradigm] = current_position + len(lang_energy) / 2
            
            for lang, co2, time, energy_wh in lang_energy:
                all_langs.append(lang.upper())
                all_co2.append(co2)
                all_time.append(time)
                all_energy_wh.append(energy_wh)
                all_colors.append(paradigm_colors.get(paradigm, '#CCCCCC'))
                current_position += 1
        
        # Create figure with dynamic height
        num_langs = len(all_langs)
        fig_height = max(12, num_langs * 0.7)
        fig = plt.figure(figsize=(16, fig_height))
        
        # Main title
        fig.suptitle('Energy Consumption by Paradigm', 
                     fontsize=18, fontweight='bold', y=0.995)
        
        # Create main chart
        ax = plt.subplot(1, 1, 1)
        
        # Create horizontal bars with gradient colors
        bars = ax.barh(range(len(all_langs)), all_energy_wh, color=all_colors, 
                      edgecolor='black', linewidth=1.0, height=0.6, alpha=0.85)
        
        # Customize axes
        ax.set_yticks(range(len(all_langs)))
        ax.set_yticklabels(all_langs, fontsize=11, fontweight='bold')
        ax.set_xlabel('Energy Consumption (Wh)', fontsize=13, fontweight='bold', labelpad=10)
        ax.set_title('Languages Ranked by Energy Consumption (Lower = Better)', 
                    fontsize=14, fontweight='bold', pad=20)
        ax.invert_yaxis()
        
        # Add grid
        ax.grid(axis='x', alpha=0.25, linestyle='--', linewidth=0.8)
        ax.set_axisbelow(True)
        
        # Extend x-axis to make room for labels
        ax.set_xlim(0, max(all_energy_wh) * 1.3)
        
        # Add value labels with energy, CO2 and time details
        for i, (energy_wh, co2, time) in enumerate(zip(all_energy_wh, all_co2, all_time)):
            x_pos = energy_wh + max(all_energy_wh) * 0.01
            
            # Show energy (Wh) + CO2 (mg) + time (s)
            label = f'{energy_wh:.2f} Wh ({co2:.0f}mg, {time:.3f}s)'
            
            ax.text(x_pos, i, label,
                   va='center', ha='left',
                   fontsize=9, fontweight='bold',
                   color='black',
                   bbox=dict(boxstyle='round,pad=0.3', 
                            facecolor='white', 
                            edgecolor='gray',
                            linewidth=0.5,
                            alpha=0.9))
        
        # Add paradigm + task labels on the left
        for paradigm, position in paradigm_positions.items():
            task_name = paradigm_tasks.get(paradigm, 'N/A')
            ax.text(-max(all_energy_wh)*0.15, position, 
                   f'{paradigm}\n[{task_name}]',
                   va='center', ha='right', fontsize=9, fontweight='bold',
                   color=paradigm_colors.get(paradigm, '#000'),
                   bbox=dict(boxstyle='round,pad=0.4', facecolor='white', 
                            edgecolor=paradigm_colors.get(paradigm, '#000'), 
                            linewidth=1.5, alpha=0.9))
        
        # Calculate statistics
        avg_energy = sum(all_energy_wh) / len(all_energy_wh)
        min_energy = min(all_energy_wh)
        max_energy = max(all_energy_wh)
        avg_co2 = sum(all_co2) / len(all_co2)
        avg_time = sum(all_time) / len(all_time)
        total_energy = sum(all_energy_wh)
        
        # Add statistics
        stats_line = (f'Statistics: {len(all_langs)} languages across {len(paradigm_positions)} paradigms | '
                     f'Avg Energy: {avg_energy:.2f} Wh | Total: {total_energy:.2f} Wh | '
                     f'Avg CO₂: {avg_co2:.1f}mg | Avg Time: {avg_time:.3f}s')
        fig.text(0.5, 0.94, stats_line,
                ha='center', fontsize=11, style='italic', color='#555555',
                bbox=dict(boxstyle='round,pad=0.5', facecolor='#f0f0f0', 
                         edgecolor='#cccccc', alpha=0.8))
        
        # Add methodology note with CodeCarbon formula
        fig.text(0.5, 0.01, 
                f'Methodology: Energy (Wh) = CO₂(mg) / (FE × 10³), where FE = {EMISSION_FACTOR} g CO₂/Wh (Italy average). '
                'Based on CodeCarbon conversion standard.',
                ha='center', fontsize=10, style='italic', color='#555555',
                bbox=dict(boxstyle='round,pad=0.5', facecolor='#f0f0f1', 
                         edgecolor='#cccccc', alpha=0.8))
        
        plt.tight_layout(rect=[0, 0.03, 1, 0.92])
        
        if save:
            output_path = self.output_dir / 'paradigm_energy_consumption.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight', facecolor='white')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def generate_all_plots(self):
        """Generate all available charts"""
        print(" Generating all CLAP charts...\n")
        
        print("1 - Language Energy Ranking...")
        self.plot_language_energy_ranking()
        
        print("\n2 - CO2 vs Time Scatter...")
        self.plot_co2_vs_time_scatter()

        print("\n3 - Top Tasks Comparison...")
        self.plot_top_tasks_comparison()

        print("\n4 - CO2 Distribution Boxplot...")
        self.plot_co2_distribution_boxplot()
        
        print("\n5 - Language Success Rates...")
        self.plot_language_success_rates()
        
        print("\n6 - Success Breakdown...")
        self.plot_success_breakdown()
        
        print("\n7 - Task × Language Heatmap...")
        self.plot_task_language_heatmap()
        
        print("\n8 - Efficiency Rankings...")
        self.plot_efficiency_rankings()
        
        print("\n9 - Energy Consumption per Run...")
        self.plot_energy_consumption_per_run()
        
        print("\n10 - Paradigm Energy Consumption...")
        self.plot_paradigm_energy_consumption()
        
        print(f"\n✅ COMPLETE! All 10 charts saved in: {self.output_dir}")
        print(f"Directory: {self.output_dir.absolute()}")



def main():
    """Main function"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Visualize CLAP results')
    parser.add_argument('--all', action='store_true', help='Generate all charts (10 total)')
    parser.add_argument('--ranking', action='store_true', help='Energy ranking chart')
    parser.add_argument('--scatter', action='store_true', help='CO2 vs Time scatter chart')
    parser.add_argument('--tasks', action='store_true', help='Tasks comparison chart')
    parser.add_argument('--boxplot', action='store_true', help='CO2 boxplot chart')
    parser.add_argument('--success-rates', action='store_true', help='Language success rates chart')
    parser.add_argument('--success-breakdown', action='store_true', help='Success breakdown chart')
    parser.add_argument('--heatmap', action='store_true', help='Task × Language heatmap')
    parser.add_argument('--efficiency', action='store_true', help='Efficiency rankings chart')
    parser.add_argument('--energy-per-run', action='store_true', help='Energy consumption per run')
    parser.add_argument('--paradigm-energy', action='store_true', help='Paradigm energy consumption (NEW!)')
    parser.add_argument('--mode', choices=['FAST', 'TOP10', 'COMPLETE'], default=None,
                       help='Benchmark mode (FAST/TOP10/COMPLETE). If not specified, uses default paths.')
    
    args = parser.parse_args()
    
    viz = CLAPVisualizer(mode=args.mode)
    
    # If no specific chart selected, generate all
    if args.all or not any([args.ranking, args.scatter, args.tasks, args.boxplot, 
                            args.success_rates, args.success_breakdown, args.heatmap, 
                            args.efficiency, args.energy_per_run, args.paradigm_energy]):
        viz.generate_all_plots()
    else:
        if args.ranking:
            viz.plot_language_energy_ranking()
        if args.scatter:
            viz.plot_co2_vs_time_scatter()
        if args.tasks:
            viz.plot_top_tasks_comparison()
        if args.boxplot:
            viz.plot_co2_distribution_boxplot()
        if args.success_rates:
            viz.plot_language_success_rates()
        if args.success_breakdown:
            viz.plot_success_breakdown()
        if args.heatmap:
            viz.plot_task_language_heatmap()
        if args.efficiency:
            viz.plot_efficiency_rankings()
        if args.energy_per_run:
            viz.plot_energy_consumption_per_run()
        if args.paradigm_energy:
            viz.plot_paradigm_energy_consumption()


if __name__ == '__main__':
    main()
