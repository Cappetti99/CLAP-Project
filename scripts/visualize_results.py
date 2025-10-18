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
from pathlib import Path
import numpy as np
from datetime import datetime

# Chart style configuration
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")
plt.rcParams['figure.figsize'] = (14, 8)
plt.rcParams['font.size'] = 11


class CLAPVisualizer:
    def __init__(self, results_dir='results'):
        """Initialize visualizer with results directory"""
        self.results_dir = Path(results_dir)
        self.csv_dir = self.results_dir / 'csv'
        self.carbon_dir = self.results_dir / 'carbon'
        self.output_dir = self.results_dir / 'visualizations'
        self.output_dir.mkdir(exist_ok=True)
        
    def load_language_energy_ranking(self):
        """Load language energy ranking CSV"""
        csv_files = list(self.csv_dir.glob('language_energy_ranking_*.csv'))
        if csv_files:
            latest = max(csv_files, key=lambda p: p.stat().st_mtime)
            return pd.read_csv(latest)
        return None
    
    def load_top10_tasks(self):
        """Load top 10 tasks CSV"""
        csv_file = self.csv_dir / 'top10_tasks_analysis.csv'
        if csv_file.exists():
            return pd.read_csv(csv_file)
        return None
    
    def load_carbon_sessions(self, limit=100):
        """Load first N carbon session JSON files"""
        json_files = sorted(self.carbon_dir.glob('session_*.json'))[:limit]
        sessions = []
        
        for json_file in json_files:
            try:
                with open(json_file, 'r') as f:
                    data = json.load(f)
                    sessions.append(data)
            except Exception as e:
                print(f"Error loading {json_file}: {e}")
                
        return pd.DataFrame(sessions) if sessions else None
    
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
            ax.annotate(row['language'], 
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
        df = self.load_top10_tasks()
        if df is None or df.empty:
            print("❌ No data available for top10_tasks")
            return
        
        # Filter only successful tasks
        df_success = df[df['success_rate'] > 0].copy()
        
        # Get top 10 most common tasks
        top_tasks = df_success.groupby('task')['total_runs'].sum().nlargest(10).index
        df_top = df_success[df_success['task'].isin(top_tasks)]
        
        fig, ax = plt.subplots(figsize=(18, 12))
        
        # Pivot for heatmap
        pivot = df_top.pivot_table(values='avg_co2_mg', 
                                   index='task', 
                                   columns='language',
                                   fill_value=0)
        
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
        """Load the most recent detailed benchmark results"""
        carbon_benchmark_dir = self.results_dir / "carbon_benchmark"
        
        if not carbon_benchmark_dir.exists():
            return None
        
        # Find the latest detailed file
        detailed_files = sorted(carbon_benchmark_dir.glob("carbon_benchmark_detailed_*.json"), 
                               key=lambda x: x.stat().st_mtime, reverse=True)
        
        if not detailed_files:
            return None
        
        latest_file = detailed_files[0]
        print(f"Loading benchmark results from: {latest_file.name}")
        
        with open(latest_file, 'r') as f:
            return json.load(f)
    
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
        """Chart 5: CO2 Distribution Boxplot"""
        df = self.load_top10_tasks()
        if df is None or df.empty:
            print("❌ No data available")
            return
        
        # Filter only languages with data
        df_valid = df[df['avg_co2_mg'] > 0]
        
        fig, ax = plt.subplots(figsize=(16, 8))
        
        # Boxplot
        bp = ax.boxplot([df_valid[df_valid['language'] == lang]['avg_co2_mg'].values 
                         for lang in df_valid['language'].unique()],
                        labels=df_valid['language'].unique(),
                        patch_artist=True,
                        showmeans=True)
        
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
    
    def generate_all_plots(self):
        """Generate all available charts"""
        print("📊 Generating all CLAP charts...\n")
        
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
        
        print(f"\n✅ COMPLETE! All 7 charts saved in: {self.output_dir}")
        print(f"Directory: {self.output_dir.absolute()}")



def main():
    """Main function"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Visualize CLAP results')
    parser.add_argument('--all', action='store_true', help='Generate all charts (7 total)')
    parser.add_argument('--ranking', action='store_true', help='Energy ranking chart')
    parser.add_argument('--scatter', action='store_true', help='CO2 vs Time scatter chart')
    parser.add_argument('--tasks', action='store_true', help='Tasks comparison chart')
    parser.add_argument('--boxplot', action='store_true', help='CO2 boxplot chart')
    parser.add_argument('--success-rates', action='store_true', help='Language success rates chart')
    parser.add_argument('--success-breakdown', action='store_true', help='Success breakdown chart')
    parser.add_argument('--heatmap', action='store_true', help='Task × Language heatmap')
    
    args = parser.parse_args()
    
    viz = CLAPVisualizer()
    
    # If no specific chart selected, generate all
    if args.all or not any([args.ranking, args.scatter, args.tasks, args.boxplot, 
                            args.success_rates, args.success_breakdown, args.heatmap]):
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


if __name__ == '__main__':
    main()
