#!/usr/bin/env python3
"""
Unified Task Finder - Sistema unificato per analisi task comuni
Combina AdvancedTaskFinder + PandasAnalyzer in un'unica soluzione ottimale
"""

import pandas as pd
import numpy as np
from pathlib import Path
import json
import os
import re
import random
from collections import defaultdict, Counter
from datetime import datetime

class UnifiedTaskFinder:
    """
    Sistema unificato per analisi task comuni - Il MEGLIO di entrambi i mondi
    
    Combina:
    - AdvancedTaskFinder: analisi qualitativa, pattern, estensioni
    - PandasAnalyzer: query potenti, statistiche, DataFrame
    """
    
    def __init__(self):
        self.results_dir = "results/task_analysis"
        self.code_snippets_dir = "data/generated/code_snippets"
        self.df = None  # DataFrame principale
        
        # Crea directory results se non esiste
        os.makedirs(self.results_dir, exist_ok=True)
        
        # Linguaggi supportati e loro estensioni (da AdvancedTaskFinder)
        self.supported_languages = {
            'c': ['.c'],
            'c++': ['.cpp'],
            'csharp': ['.cs'],
            'go': ['.go'],
            'haskell': ['.hs'],
            'java': ['.java'],
            'javascript': ['.js'],
            'julia': ['.jl'],
            'matlab': ['.m'],
            'ocaml': ['.ml'],
            'php': ['.php'],
            'python': ['.py'],
            'r': ['.r'],
            'ruby': ['.rb'],
            'rust': ['.rs'],
            'typescript': ['.ts']
        }
        
        # Pattern per analisi qualitativa (da AdvancedTaskFinder)
        self.quality_patterns = {
            'comments': [
                r'//.*', r'/\*.*?\*/', r'#.*', r'--.*', r'\(\*.*?\*\)'
            ],
            'functions': [
                r'def\s+\w+\s*\(', r'function\s+\w+\s*\(',
                r'public\s+\w+\s+\w+\s*\(', r'\w+\s+\w+\s*\([^)]*\)\s*{',
                r'fn\s+\w+\s*\('
            ],
            'error_handling': [
                r'try\s*{', r'catch\s*\(', r'except\s*:', r'except\s+\w+',
                r'Result<', r'Option<', r'if\s+err\s*!=\s*nil'
            ]
        }
    
    def create_dataset_dataframe(self):
        """Crea DataFrame Pandas completo del dataset (con info aggiuntive)"""
        print(" Creazione DataFrame unificato...")
        
        data = []
        
        # Scansiona struttura: code_snippets/category/language/files
        for category_dir in Path(self.code_snippets_dir).iterdir():
            if category_dir.is_dir():
                print(f" Categoria: {category_dir.name}")
                
                for language_dir in category_dir.iterdir():
                    if language_dir.is_dir():
                        language = language_dir.name
                        
                        for file_path in language_dir.iterdir():
                            if file_path.is_file():
                                task_name = self.extract_task_name(file_path.name)
                                
                                if task_name:
                                    # Info base
                                    row = {
                                        'task_name': task_name,
                                        'language': language,
                                        'category': category_dir.name,
                                        'file_path': str(file_path),
                                        'file_size': file_path.stat().st_size,
                                        'extension': file_path.suffix
                                    }
                                    
                                    # Analisi qualitativa del codice (se richiesta)
                                    if hasattr(self, '_include_quality_analysis') and self._include_quality_analysis:
                                        try:
                                            with open(file_path, 'r', encoding='utf-8') as f:
                                                code = f.read()
                                            
                                            quality = self.analyze_code_quality(code, language)
                                            row.update(quality)
                                        except:
                                            pass  # Skip se non riesce a leggere
                                    
                                    data.append(row)
        
        self.df = pd.DataFrame(data)
        print(f" DataFrame creato con {len(self.df)} record")
        return self.df
    
    def extract_task_name(self, filename):
        """Estrae task name dal filename formato snippet_N_TaskName.ext"""
        if filename.startswith('snippet_'):
            name_part = filename.rsplit('.', 1)[0]
            parts = name_part.split('_', 2)
            if len(parts) >= 3:
                return parts[2]
        return None
    
    def find_common_tasks(self, min_languages=8, use_pandas=True, available_languages=None):
        """
        Trova task comuni - METODO UNIFICATO
        
        Args:
            min_languages: Minimo linguaggi richiesti
            use_pandas: Se True usa Pandas, altrimenti metodo tradizionale
            available_languages: Lista linguaggi disponibili dal test (opzionale)
        """
        
        if use_pandas:
            return self._find_common_tasks_pandas(min_languages, available_languages)
        else:
            return self._find_common_tasks_traditional(min_languages, available_languages)
    
    def _find_common_tasks_pandas(self, min_languages=8, available_languages=None):
        """Query Pandas per task comuni con linguaggi disponibili (OTTIMIZZATO)"""
        print(f" Query Pandas: task con >= {min_languages} linguaggi")
        
        if self.df is None:
            self.create_dataset_dataframe()
        
        # Filtra DataFrame per linguaggi disponibili se specificato
        df_filtered = self.df.copy()
        if available_languages:
            print(f" Filtrando per linguaggi disponibili: {len(available_languages)}")
            df_filtered = df_filtered[df_filtered['language'].isin(available_languages)]
            print(f" Record dopo filtro: {len(df_filtered)}")
        
        # Query Pandas ottimizzata su dati filtrati
        task_stats = (df_filtered.groupby('task_name')
                     .agg({
                         'language': ['nunique', 'count'],
                         'category': lambda x: list(x.unique()),
                         'file_size': ['mean', 'std']
                     })
                     .round(2))
        
        # Flatten column names
        task_stats.columns = ['language_count', 'total_implementations', 'categories', 'avg_file_size', 'size_std']
        task_stats = task_stats.reset_index()
        
        # Se non abbiamo linguaggi disponibili, usa il filtro normale
        if available_languages is None:
            common_tasks_df = task_stats[task_stats['language_count'] >= min_languages].copy()
        else:
            # NUOVA LOGICA: Trova le TOP 10 task con più linguaggi disponibili
            print(f" Cercando TOP 10 task con più linguaggi tra quelli disponibili...")
            common_tasks_df = task_stats.sort_values('language_count', ascending=False).head(10)
            print(f" TOP 10 task selezionate (da {len(task_stats)} totali)")
        
        # Converti in formato standard per compatibilità
        common_tasks = []
        for _, row in common_tasks_df.iterrows():
            task_name = row['task_name']
            task_data = df_filtered[df_filtered['task_name'] == task_name]
            
            # Conta solo i linguaggi effettivamente disponibili
            actual_languages = task_data['language'].unique().tolist()
            if available_languages:
                actual_languages = [lang for lang in actual_languages if lang in available_languages]
            
            common_tasks.append({
                'name': task_name,
                'language_count': len(actual_languages),  # Count reale per linguaggi disponibili
                'languages': actual_languages,
                'categories': row['categories'],
                'total_implementations': int(row['total_implementations']),
                'avg_file_size': float(row['avg_file_size']),
                'size_variation': float(row['size_std']) if not pd.isna(row['size_std']) else 0.0,
                'available_language_count': len(actual_languages)  # Nuovo campo
            })
        
        return common_tasks
    
    def _find_common_tasks_traditional(self, min_languages=8, available_languages=None):
        """Metodo tradizionale per compatibilità (da AdvancedTaskFinder)"""
        print(f" Scansione tradizionale: task con >= {min_languages} linguaggi")
        
        task_languages = defaultdict(set)
        
        for category_dir in Path(self.code_snippets_dir).iterdir():
            if category_dir.is_dir():
                for language_dir in category_dir.iterdir():
                    if language_dir.is_dir():
                        language_name = language_dir.name
                        
                        # Filtra per linguaggi disponibili se specificato
                        if available_languages and language_name not in available_languages:
                            continue
                            
                        for code_file in language_dir.iterdir():
                            if code_file.is_file():
                                task_name = self.extract_task_name(code_file.name)
                                if task_name:
                                    task_languages[task_name].add(language_name)
        
        # Filtra e ordina
        common_tasks = []
        for task, languages in task_languages.items():
            # Se abbiamo linguaggi disponibili, usa logica TOP 10
            if available_languages is not None:
                # Per ora aggiungi tutto, poi prenderemo i top 10
                common_tasks.append({
                    'name': task,
                    'languages': list(languages),
                    'language_count': len(languages)
                })
            elif len(languages) >= min_languages:
                common_tasks.append({
                    'name': task,
                    'languages': list(languages),
                    'language_count': len(languages)
                })
        
        common_tasks.sort(key=lambda x: x['language_count'], reverse=True)
        
        # Se usiamo linguaggi disponibili, prendi solo top 10
        if available_languages is not None:
            common_tasks = common_tasks[:10]
            print(f" TOP 10 task con più linguaggi disponibili trovate")
        else:
            print(f" Trovate {len(common_tasks)} task comuni")
        
        return common_tasks
    
    def extract_random_implementation_per_language(self, task_name):
        """Estrae implementazione RANDOMICA per linguaggio"""
        if self.df is None:
            self.create_dataset_dataframe()
        
        task_data = self.df[self.df['task_name'] == task_name]
        
        if task_data.empty:
            print(f"  Task '{task_name}' non trovata nel dataset")
            return {}
        
        implementations = {}
        for language in task_data['language'].unique():
            language_files = task_data[task_data['language'] == language]['file_path'].tolist()
            
            # SCELTA RANDOMICA
            selected_file = random.choice(language_files)
            
            try:
                with open(selected_file, 'r', encoding='utf-8') as f:
                    code = f.read()
                
                implementations[language] = {
                    'file_path': selected_file,
                    'code': code,
                    'file_size': os.path.getsize(selected_file)
                }
            except Exception as e:
                print(f"❌ Errore lettura {selected_file}: {e}")
        
        return implementations
    
    def analyze_code_quality(self, code, language):
        """Analisi qualitativa del codice (da AdvancedTaskFinder)"""
        quality_metrics = {
            'has_comments': False,
            'has_functions': False,
            'has_error_handling': False,
            'code_length': len(code),
            'line_count': len(code.split('\n'))
        }
        
        # Verifica pattern di qualità
        for pattern_type, patterns in self.quality_patterns.items():
            metric_name = f"has_{pattern_type.rstrip('s')}"
            
            for pattern in patterns:
                if re.search(pattern, code, re.MULTILINE):
                    quality_metrics[metric_name] = True
                    break
        
        return quality_metrics
    
    def get_comprehensive_statistics(self):
        """Statistiche complete del dataset (UNIFICATO)"""
        if self.df is None:
            self.create_dataset_dataframe()
        
        print("\n STATISTICHE COMPREHENSIVE")
        print("=" * 50)
        
        stats = {
            'dataset_overview': {
                'total_files': len(self.df),
                'unique_tasks': self.df['task_name'].nunique(),
                'languages': self.df['language'].nunique(),
                'categories': self.df['category'].nunique(),
                'total_size_mb': (self.df['file_size'].sum() / 1024 / 1024)
            },
            
            'top_languages': self.df['language'].value_counts().head(10).to_dict(),
            'top_universal_tasks': (self.df.groupby('task_name')['language']
                                   .nunique()
                                   .sort_values(ascending=False)
                                   .head(10)
                                   .to_dict()),
            'category_distribution': self.df['category'].value_counts().to_dict(),
            'language_categories': self.df.groupby(['category', 'language']).size().to_dict()
        }
        
        # Stampa summary
        overview = stats['dataset_overview']
        print(f"Total file: {overview['total_files']:,}")
        print(f"Task uniche: {overview['unique_tasks']:,}")
        print(f"Linguaggi: {overview['languages']}")
        print(f"Categorie: {overview['categories']}")
        print(f"Dimensione totale: {overview['total_size_mb']:.1f} MB")
        
        return stats
    
    def save_analysis_results(self, common_tasks, filename_suffix="unified"):
        """Salva risultati analisi su file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Salva task comuni
        tasks_file = f"{self.results_dir}/common_tasks_{filename_suffix}_{timestamp}.json"
        with open(tasks_file, 'w') as f:
            json.dump({
                'timestamp': timestamp,
                'total_tasks': len(common_tasks),
                'method': 'unified_pandas_analysis',
                'common_tasks': common_tasks
            }, f, indent=2)
        
        # Salva DataFrame se disponibile
        if self.df is not None:
            df_file = f"{self.results_dir}/dataset_dataframe_{filename_suffix}_{timestamp}.csv"
            self.df.to_csv(df_file, index=False)
            print(f" DataFrame salvato: {df_file}")
        
        print(f" Risultati salvati: {tasks_file}")
        return tasks_file


def main():
    """Demo del sistema unificato"""
    print("🚀 UNIFIED TASK FINDER - Il meglio di entrambi i mondi")
    print("=" * 60)
    
    finder = UnifiedTaskFinder()
    
    print("\n METODO PANDAS (Raccomandato)")
    common_tasks_pandas = finder.find_common_tasks(min_languages=8, use_pandas=True)
    
    print("\n METODO TRADIZIONALE (Compatibilità)")
    common_tasks_traditional = finder.find_common_tasks(min_languages=8, use_pandas=False)
    
    # Confronto risultati
    print(f"\n CONFRONTO:")
    print(f"   Pandas: {len(common_tasks_pandas)} task")
    print(f"   Tradizionale: {len(common_tasks_traditional)} task")
    
    # Test estrazione randomica
    if common_tasks_pandas:
        sample_task = common_tasks_pandas[0]['name']
        print(f"\n Test estrazione randomica per: {sample_task}")
        implementations = finder.extract_random_implementation_per_language(sample_task)
        print(f"   Implementazioni estratte: {len(implementations)} linguaggi")
    
    # Statistiche complete
    stats = finder.get_comprehensive_statistics()
    
    # Salva risultati
    finder.save_analysis_results(common_tasks_pandas, "pandas_method")
    
    print(f"\n✅ Analisi unificata completata!")
    
    return finder


if __name__ == "__main__":
    unified_finder = main()
