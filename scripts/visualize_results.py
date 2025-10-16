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
        ax1.set_title('🌍 Energy Ranking: Average CO₂ by Language', 
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
        ax2.set_title('⏱️ Performance: Average Time by Language', 
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
        
        ax.set_xlabel('⏱️ Average Time per Run (seconds)', fontsize=13, fontweight='bold')
        ax.set_ylabel('🌍 Average CO₂ per Run (mg)', fontsize=13, fontweight='bold')
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
        
        ax.set_title('📊 CO₂ Comparison by Task and Language (Top 10 Tasks)', 
                    fontsize=15, fontweight='bold', pad=20)
        ax.set_xlabel('Language', fontsize=12, fontweight='bold')
        ax.set_ylabel('Task', fontsize=12, fontweight='bold')
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'top_tasks_co2_heatmap.png'
            plt.savefig(output_path, dpi=300, bbox_inches='tight')
            print(f"✅ Saved: {output_path}")
        
        plt.show()
    
    def plot_success_rate_comparison(self, save=True):
        """Chart 4: Success Rate by Language"""
        df = self.load_language_energy_ranking()
        if df is None or df.empty:
            print("❌ No data available")
            return
        
        fig, ax = plt.subplots(figsize=(14, 8))
        
        # Sort by success rate
        df_sorted = df.sort_values('success_rate', ascending=True)
        
        colors = ['green' if x == 100.0 else 'orange' for x in df_sorted['success_rate']]
        bars = ax.barh(df_sorted['language'], df_sorted['success_rate'], color=colors, alpha=0.7)
        
        ax.set_xlabel('Success Rate (%)', fontsize=12, fontweight='bold')
        ax.set_ylabel('Language', fontsize=12, fontweight='bold')
        ax.set_title('✅ Success Rate by Language', fontsize=14, fontweight='bold', pad=20)
        ax.set_xlim(0, 105)
        
        # Add percentages
        for bar, val in zip(bars, df_sorted['success_rate']):
            ax.text(val + 1, bar.get_y() + bar.get_height()/2, 
                   f'{val:.1f}%', va='center', fontsize=10, fontweight='bold')
        
        plt.tight_layout()
        
        if save:
            output_path = self.output_dir / 'success_rate_comparison.png'
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
        ax.set_title('📦 CO₂ Distribution by Language (Boxplot)', 
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
        print("🚀 Generating CLAP charts...\n")
        
        print("1️⃣ Language Energy Ranking...")
        self.plot_language_energy_ranking()
        
        print("\n2️⃣ CO2 vs Time Scatter...")
        self.plot_co2_vs_time_scatter()
        
        print("\n3️⃣ Top Tasks Comparison...")
        self.plot_top_tasks_comparison()
        
        print("\n4️⃣ Success Rate Comparison...")
        self.plot_success_rate_comparison()
        
        print("\n5️⃣ CO2 Distribution Boxplot...")
        self.plot_co2_distribution_boxplot()
        
        print(f"\n✅ COMPLETE! All charts saved in: {self.output_dir}")
        print(f"📁 Directory: {self.output_dir.absolute()}")


def main():
    """Main function"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Visualize CLAP results')
    parser.add_argument('--all', action='store_true', help='Generate all charts')
    parser.add_argument('--ranking', action='store_true', help='Energy ranking chart')
    parser.add_argument('--scatter', action='store_true', help='CO2 vs Time scatter chart')
    parser.add_argument('--tasks', action='store_true', help='Tasks comparison chart')
    parser.add_argument('--success', action='store_true', help='Success rate chart')
    parser.add_argument('--boxplot', action='store_true', help='CO2 boxplot chart')
    
    args = parser.parse_args()
    
    viz = CLAPVisualizer()
    
    if args.all or not any([args.ranking, args.scatter, args.tasks, args.success, args.boxplot]):
        viz.generate_all_plots()
    else:
        if args.ranking:
            viz.plot_language_energy_ranking()
        if args.scatter:
            viz.plot_co2_vs_time_scatter()
        if args.tasks:
            viz.plot_top_tasks_comparison()
        if args.success:
            viz.plot_success_rate_comparison()
        if args.boxplot:
            viz.plot_co2_distribution_boxplot()


if __name__ == '__main__':
    main()
