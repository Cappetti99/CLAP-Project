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
from collections import defaultdict

class UnifiedTaskFinder:
    """
    Sistema ottimizzato per analisi task comuni con Pandas
    
    Funzionalità principali:
    - Analisi dataset con DataFrame Pandas ottimizzato  
    - Ricerca task comuni per linguaggi disponibili
    - Estrazione randomica implementazioni per benchmark
    """
    
    def __init__(self):
        self.results_dir = "results/task_analysis"
        self.code_snippets_dir = "data/generated/code_snippets"
        self.df = None  # DataFrame principale
        
        # Crea directory results se non esiste
        os.makedirs(self.results_dir, exist_ok=True)
        
        # Linguaggi supportati e loro estensioni
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
        
        # Pattern per analisi qualitativa del codice
        self.quality_patterns = {
            'comments': [
                r'//.*',           # C, C++, Java, JavaScript
                r'/\*.*?\*/',      # C, C++, Java (multiline)
                r'#.*',            # Python, Ruby, Bash
                r'--.*',           # Haskell, SQL
                r'\(\*.*?\*\)'     # OCaml
            ],
            'functions': [
                r'def\s+\w+\s*\(',                    # Python, Ruby
                r'function\s+\w+\s*\(',               # JavaScript
                r'public\s+\w+\s+\w+\s*\(',          # Java
                r'\w+\s+\w+\s*\([^)]*\)\s*{',       # C, C++
                r'fn\s+\w+\s*\(',                    # Rust
                r'func\s+\w+\s*\(',                  # Go
                r'\w+\s*::\s*\w+\s*->'              # Haskell
            ],
            'error_handling': [
                r'try\s*{',        # Java, C#
                r'catch\s*\(',     # Java, C#, JavaScript
                r'except\s*:',     # Python
                r'except\s+\w+',   # Python (specific exception)
                r'Result<',        # Rust
                r'Option<',        # Rust
                r'if\s+err\s*!=\s*nil',  # Go
                r'rescue',         # Ruby
                r'Either'          # Haskell
            ],
            'imports': [
                r'import\s+\w+',           # Python, Java
                r'from\s+\w+\s+import',    # Python
                r'#include\s*<',           # C, C++
                r'require\s*[\'"]',        # Ruby, JavaScript
                r'use\s+\w+',              # Rust
                r'package\s+\w+'           # Go
            ]
        }
    
    def create_dataset_dataframe(self, include_quality_analysis=False, verbose=False):
        """Crea DataFrame Pandas completo del dataset (con info aggiuntive)
        
        Args:
            include_quality_analysis: Se True, include metriche qualitative del codice
            verbose: Se True, mostra dettagli durante la creazione
        """
        if verbose:
            quality_text = " con analisi qualitativa" if include_quality_analysis else ""
            print(f" Creazione DataFrame unificato{quality_text}...")
        
        data = []
        categories_found = 0
        
        # Scansiona struttura: code_snippets/category/language/files
        for category_dir in Path(self.code_snippets_dir).iterdir():
            if category_dir.is_dir():
                categories_found += 1
                if verbose:
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
                                    
                                    # Analisi qualitativa del codice
                                    if include_quality_analysis:
                                        try:
                                            with open(file_path, 'r', encoding='utf-8') as f:
                                                code = f.read()
                                            
                                            quality = self.analyze_code_quality(code, language)
                                            row.update(quality)
                                        except Exception as e:
                                            # Valori default se non riesce a leggere
                                            row.update({
                                                'has_comments': False,
                                                'has_functions': False, 
                                                'has_error_handling': False,
                                                'has_imports': False,
                                                'code_length': 0,
                                                'line_count': 0,
                                                'quality_score': 0
                                            })
                                    
                                    data.append(row)
        
        self.df = pd.DataFrame(data)
        
        total_records = len(self.df)
        unique_tasks = self.df['task_name'].nunique() if not self.df.empty else 0
        unique_languages = self.df['language'].nunique() if not self.df.empty else 0
        
        print(f" Dataset: {total_records:,} file | {unique_tasks:,} task | {unique_languages} linguaggi")
        
        if verbose and not self.df.empty:
            print(f" Categorie analizzate: {categories_found}")
            if include_quality_analysis:
                avg_quality = self.df['quality_score'].mean() if 'quality_score' in self.df.columns else 0
                print(f" Quality score medio: {avg_quality:.1f}/100")
        
        return self.df
    
    def extract_task_name(self, filename):
        """Estrae task name dal filename formato snippet_N_TaskName.ext"""
        if filename.startswith('snippet_'):
            name_part = filename.rsplit('.', 1)[0]
            parts = name_part.split('_', 2)
            if len(parts) >= 3:
                return parts[2]
        return None
    
    def find_common_tasks(self, min_languages=8, available_languages=None, include_quality=False, min_quality_score=0, verbose=False):
        """
        Trova task comuni usando analisi Pandas ottimizzata
        
        Args:
            min_languages: Minimo linguaggi richiesti
            available_languages: Lista linguaggi disponibili dal test (opzionale)
            include_quality: Se True, include analisi qualitativa del codice
            min_quality_score: Filtro minimo per quality score (0-100)
            verbose: Se True, mostra dettagli durante l'analisi
        """
        return self._find_common_tasks_pandas(min_languages, available_languages, include_quality, min_quality_score, verbose)
    
    def _find_common_tasks_pandas(self, min_languages=8, available_languages=None, include_quality=False, min_quality_score=0, verbose=False):
        """Query Pandas per task comuni con linguaggi disponibili e quality analysis"""
        
        if self.df is None:
            self.create_dataset_dataframe(include_quality_analysis=include_quality, verbose=verbose)
        elif include_quality and 'quality_score' not in self.df.columns:
            if verbose:
                print(" Ricreo DataFrame con analisi qualitativa...")
            self.create_dataset_dataframe(include_quality_analysis=True, verbose=verbose)
        
        # Filtra DataFrame per linguaggi disponibili se specificato
        df_filtered = self.df.copy()
        initial_count = len(df_filtered)
        
        if available_languages:
            df_filtered = df_filtered[df_filtered['language'].isin(available_languages)]
            
        # Filtra per quality score se specificato
        if include_quality and min_quality_score > 0:
            df_filtered = df_filtered[df_filtered['quality_score'] >= min_quality_score]
        
        # Output conciso dei filtri applicati
        final_count = len(df_filtered)
        if verbose and initial_count != final_count:
            filters_applied = []
            if available_languages:
                filters_applied.append(f"{len(available_languages)} linguaggi")
            if include_quality and min_quality_score > 0:
                filters_applied.append(f"quality≥{min_quality_score}")
            print(f" Filtri: {', '.join(filters_applied)} | {initial_count:,} → {final_count:,} record")
        
        # Query Pandas ottimizzata su dati filtrati
        agg_dict = {
            'language': ['nunique', 'count'],
            'category': lambda x: list(x.unique()),
            'file_size': ['mean', 'std']
        }
        
        # Aggiungi metriche qualitative se disponibili
        if include_quality and 'quality_score' in df_filtered.columns:
            agg_dict['quality_score'] = ['mean', 'std', 'min', 'max']
        
        task_stats = (df_filtered.groupby('task_name').agg(agg_dict).round(2))
        
        # Flatten column names
        if include_quality and 'quality_score' in df_filtered.columns:
            task_stats.columns = ['language_count', 'total_implementations', 'categories', 
                                 'avg_file_size', 'size_std', 'avg_quality', 'quality_std', 
                                 'min_quality', 'max_quality']
        else:
            task_stats.columns = ['language_count', 'total_implementations', 'categories', 
                                 'avg_file_size', 'size_std']
        
        task_stats = task_stats.reset_index()
        
        # Se non abbiamo linguaggi disponibili, usa il filtro normale
        if available_languages is None:
            common_tasks_df = task_stats[task_stats['language_count'] >= min_languages].copy()
        else:
            # Trova le TOP 10 task con più linguaggi disponibili
            common_tasks_df = task_stats.sort_values('language_count', ascending=False).head(10)
        
        # Output risultato conciso
        result_count = len(common_tasks_df)
        if available_languages:
            print(f" TOP 10 task trovate (su {len(task_stats)} totali)")
        else:
            print(f" {result_count} task comuni (≥{min_languages} linguaggi)")
            
        if include_quality and result_count > 0 and 'avg_quality' in common_tasks_df.columns:
            avg_quality = common_tasks_df['avg_quality'].mean()
            print(f"   Quality medio: {avg_quality:.1f}/100")
        
        # Converti in formato standard per compatibilità
        common_tasks = []
        for _, row in common_tasks_df.iterrows():
            task_name = row['task_name']
            task_data = df_filtered[df_filtered['task_name'] == task_name]
            
            # Conta solo i linguaggi effettivamente disponibili
            actual_languages = task_data['language'].unique().tolist()
            if available_languages:
                actual_languages = [lang for lang in actual_languages if lang in available_languages]
            
            task_info = {
                'name': task_name,
                'language_count': len(actual_languages),  # Count reale per linguaggi disponibili
                'languages': actual_languages,
                'categories': row['categories'],
                'total_implementations': int(row['total_implementations']),
                'avg_file_size': float(row['avg_file_size']),
                'size_variation': float(row['size_std']) if not pd.isna(row['size_std']) else 0.0,
                'available_language_count': len(actual_languages) 
            }
            
            # Aggiungi metriche qualitative se disponibili
            if include_quality and 'avg_quality' in row:
                task_info.update({
                    'avg_quality_score': float(row['avg_quality']),
                    'quality_variation': float(row['quality_std']) if not pd.isna(row['quality_std']) else 0.0,
                    'min_quality_score': float(row['min_quality']),
                    'max_quality_score': float(row['max_quality'])
                })
            
            common_tasks.append(task_info)
        
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
        """Analisi qualitativa avanzata del codice con score"""
        import re
        
        quality_metrics = {
            'has_comments': False,
            'has_functions': False,
            'has_error_handling': False,
            'has_imports': False,
            'code_length': len(code),
            'line_count': len(code.split('\n')),
            'non_empty_lines': len([line for line in code.split('\n') if line.strip()])
        }
        
        # Verifica pattern di qualità
        for pattern_type, patterns in self.quality_patterns.items():
            metric_name = f"has_{pattern_type}"
            
            for pattern in patterns:
                if re.search(pattern, code, re.MULTILINE | re.DOTALL):
                    quality_metrics[metric_name] = True
                    break
        
        # Calcola quality score (0-100)
        score = 0
        if quality_metrics['has_comments']: score += 25     # Documentazione
        if quality_metrics['has_functions']: score += 30    # Struttura modulare  
        if quality_metrics['has_error_handling']: score += 25  # Robustezza
        if quality_metrics['has_imports']: score += 10     # Uso librerie
        
        # Bonus per codice ben strutturato
        if quality_metrics['non_empty_lines'] > 10: score += 10  # Non triviale
        
        quality_metrics['quality_score'] = min(score, 100)
        
        return quality_metrics
    
    def get_quality_statistics(self, verbose=False):
        """Statistiche qualitative del dataset (richiede DataFrame con quality analysis)"""
        if self.df is None:
            print("❌ Devi prima creare il DataFrame con create_dataset_dataframe()")
            return None
            
        # Verifica che abbiamo dati di qualità
        if 'quality_score' not in self.df.columns:
            print("❌ DataFrame non contiene analisi qualitative")
            if verbose:
                print(" Usa create_dataset_dataframe(include_quality_analysis=True)")
            return None
        
        # Calcola statistiche senza stampare header se non verbose
        if verbose:
            print("\n STATISTICHE QUALITATIVE")
            print("=" * 50)
        
        stats = {
            'quality_overview': {
                'avg_quality_score': self.df['quality_score'].mean(),
                'median_quality_score': self.df['quality_score'].median(),
                'std_quality_score': self.df['quality_score'].std(),
                'min_quality_score': self.df['quality_score'].min(),
                'max_quality_score': self.df['quality_score'].max()
            },
            'feature_coverage': {
                'has_comments_pct': (self.df['has_comments'].sum() / len(self.df)) * 100,
                'has_functions_pct': (self.df['has_functions'].sum() / len(self.df)) * 100,
                'has_error_handling_pct': (self.df['has_error_handling'].sum() / len(self.df)) * 100,
                'has_imports_pct': (self.df['has_imports'].sum() / len(self.df)) * 100
            },
            'quality_by_language': self.df.groupby('language')['quality_score'].agg(['mean', 'count']).round(1).to_dict(),
            'quality_by_category': self.df.groupby('category')['quality_score'].agg(['mean', 'count']).round(1).to_dict(),
            'top_quality_tasks': (self.df.groupby('task_name')['quality_score']
                                 .mean()
                                 .sort_values(ascending=False)
                                 .head(10)
                                 .round(1)
                                 .to_dict())
        }
        
        # Stampa summary solo se verbose
        if verbose:
            overview = stats['quality_overview']
            features = stats['feature_coverage']
            
            print(f"Quality Score medio: {overview['avg_quality_score']:.1f}/100")
            print(f"Quality Score mediano: {overview['median_quality_score']:.1f}/100")
            print(f"Range: {overview['min_quality_score']:.0f} - {overview['max_quality_score']:.0f}")
            print(f"")
            print(f"Copertura Features:")
            print(f" Commenti: {features['has_comments_pct']:.1f}%")
            print(f" Funzioni: {features['has_functions_pct']:.1f}%")
            print(f" Error Handling: {features['has_error_handling_pct']:.1f}%")
            print(f" Import: {features['has_imports_pct']:.1f}%")
        
        return stats
    

    

    



def main():
    """Demo del sistema unificato con analisi qualitativa"""
    print("🚀 UNIFIED TASK FINDER - Sistema ottimizzato con Quality Analysis")
    print("=" * 65)
    
    finder = UnifiedTaskFinder()
    
    print("\n🔍 ANALISI TASK COMUNI (Base)")
    common_tasks = finder.find_common_tasks(min_languages=8)
    print(f" Trovate {len(common_tasks)} task comuni")
    
    print("\n📊 ANALISI CON QUALITY METRICS")
    quality_tasks = finder.find_common_tasks(min_languages=6, include_quality=True, min_quality_score=50)
    print(f" Trovate {len(quality_tasks)} task di alta qualità")
    
    # Mostra esempio con quality metrics
    if quality_tasks:
        sample = quality_tasks[0]
        print(f"\n🏆 Esempio task di qualità: {sample['name']}")
        print(f"   Linguaggi: {sample['language_count']}")
        if 'avg_quality_score' in sample:
            print(f"   Quality Score: {sample['avg_quality_score']:.1f}/100")
            print(f"   Range Quality: {sample['min_quality_score']:.0f}-{sample['max_quality_score']:.0f}")
    
    # Statistiche qualitative
    print("\n📊 STATISTICHE QUALITATIVE")
    quality_stats = finder.get_quality_statistics()
    
    print(f"\n✅ Analisi completata!")
    
    return finder


if __name__ == "__main__":
    unified_finder = main()
