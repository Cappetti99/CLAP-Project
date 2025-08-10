#!/usr/bin/env python3
"""
Advanced Task Finder - Sistema avanzato per analisi task comuni
Trova task presenti in tutti i linguaggi e analizza la qualità del codice
"""

import sys
import os
import json
import re
from pathlib import Path
from collections import defaultdict, Counter
from datetime import datetime

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

class AdvancedTaskFinder:
    """Finder avanzato per task comuni con analisi qualitativa"""
    
    def __init__(self):
        self.results_dir = "results/task_analysis"
        self.code_snippets_dir = "data/generated/code_snippets"  # Path corretto ai dati
        
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
        
        # Pattern per determinare la qualità del codice
        self.quality_patterns = {
            'comments': [
                r'//.*',  # C-style comments
                r'/\*.*?\*/',  # Multi-line C comments
                r'#.*',  # Shell/Python style
                r'--.*',  # Haskell comments
                r'\(\*.*?\*\)'  # OCaml comments
            ],
            'functions': [
                r'def\s+\w+\s*\(',  # Python
                r'function\s+\w+\s*\(',  # JavaScript
                r'public\s+\w+\s+\w+\s*\(',  # Java
                r'\w+\s+\w+\s*\([^)]*\)\s*{',  # C/C++
                r'fn\s+\w+\s*\(',  # Rust
            ],
            'error_handling': [
                r'try\s*{',
                r'catch\s*\(',
                r'except\s*:',
                r'except\s+\w+',
                r'Result<',
                r'Option<',
                r'if\s+err\s*!=\s*nil'
            ]
        }
    
    def scan_available_tasks(self):
        """Scansiona tutte le task disponibili nella struttura gerarchica"""
        print("🔍 Scansione task disponibili...")
        
        if not os.path.exists(self.code_snippets_dir):
            print(f"❌ Directory non trovata: {self.code_snippets_dir}")
            return {}
        
        task_languages = defaultdict(set)
        
        # Scansiona struttura: code_snippets/category/language/files
        for category_dir in Path(self.code_snippets_dir).iterdir():
            if category_dir.is_dir():
                print(f"  📁 Categoria: {category_dir.name}")
                
                for language_dir in category_dir.iterdir():
                    if language_dir.is_dir():
                        language = language_dir.name
                        print(f"    🔹 Linguaggio: {language}")
                        
                        # Scansiona i file del linguaggio
                        for code_file in language_dir.iterdir():
                            if code_file.is_file():
                                # Estrai il nome della task dal filename
                                task_name = self.extract_task_name(code_file.name)
                                if task_name:
                                    task_languages[task_name].add(language)
        
        print(f"📊 Trovate {len(task_languages)} task totali")
        return dict(task_languages)
    
    def extract_task_name(self, filename):
        """Estrae il nome della task dal filename"""
        # Format: snippet_N_Task_Name.ext
        if filename.startswith('snippet_'):
            # Rimuovi l'estensione
            name_part = filename.rsplit('.', 1)[0]
            # Rimuovi snippet_N_
            parts = name_part.split('_', 2)
            if len(parts) >= 3:
                return parts[2]  # Task_Name
        return None
        
        print(f"📊 Trovate {len(task_languages)} task totali")
        return dict(task_languages)
    
    def detect_language(self, filepath):
        """Determina il linguaggio da un file"""
        extension = filepath.suffix.lower()
        
        for lang, extensions in self.supported_languages.items():
            if extension in extensions:
                return lang
        
        return None
    
    def find_common_tasks(self, min_languages=None):
        """Trova task comuni tra i linguaggi"""
        task_languages = self.scan_available_tasks()
        
        if not task_languages:
            return []
        
        # Se non specificato, usa tutti i linguaggi supportati
        if min_languages is None:
            min_languages = len(self.supported_languages)
        
        # Trova task presenti in almeno min_languages linguaggi
        common_tasks = []
        
        for task, languages in task_languages.items():
            if len(languages) >= min_languages:
                common_tasks.append({
                    'name': task,
                    'languages': list(languages),
                    'language_count': len(languages)
                })
        
        # Ordina per numero di linguaggi (decrescente)
        common_tasks.sort(key=lambda x: x['language_count'], reverse=True)
        
        print(f"✅ Trovate {len(common_tasks)} task comuni (≥{min_languages} linguaggi)")
        
        return common_tasks
    
    def analyze_code_quality(self, task_name):
        """Analizza la qualità del codice per una task"""
        task_dir = os.path.join(self.code_snippets_dir, task_name)
        
        if not os.path.exists(task_dir):
            return {}
        
        quality_analysis = {}
        
        for code_file in Path(task_dir).iterdir():
            if code_file.is_file():
                language = self.detect_language(code_file)
                
                if language:
                    try:
                        with open(code_file, 'r', encoding='utf-8') as f:
                            code = f.read()
                        
                        analysis = self.analyze_single_code(code, language)
                        quality_analysis[language] = analysis
                        
                    except Exception as e:
                        quality_analysis[language] = {
                            'error': str(e),
                            'readable': False
                        }
        
        return quality_analysis
    
    def analyze_single_code(self, code, language):
        """Analizza un singolo snippet di codice"""
        lines = code.split('\n')
        non_empty_lines = [line for line in lines if line.strip()]
        
        analysis = {
            'total_lines': len(lines),
            'code_lines': len(non_empty_lines),
            'empty_lines': len(lines) - len(non_empty_lines),
            'comments': 0,
            'functions': 0,
            'error_handling': 0,
            'complexity_score': 0,
            'readability_score': 0
        }
        
        # Conta pattern di qualità
        for pattern_type, patterns in self.quality_patterns.items():
            count = 0
            for pattern in patterns:
                matches = re.findall(pattern, code, re.MULTILINE | re.DOTALL)
                count += len(matches)
            analysis[pattern_type] = count
        
        # Calcola metriche di qualità
        analysis['comment_ratio'] = analysis['comments'] / max(analysis['code_lines'], 1)
        analysis['function_density'] = analysis['functions'] / max(analysis['code_lines'], 1) * 100
        
        # Score di complessità (più basso = meglio)
        complexity_indicators = ['for', 'while', 'if', 'switch', 'case', 'try', 'catch']
        complexity_count = sum(len(re.findall(rf'\b{indicator}\b', code, re.IGNORECASE)) 
                             for indicator in complexity_indicators)
        analysis['complexity_score'] = complexity_count / max(analysis['code_lines'], 1) * 100
        
        # Score di leggibilità
        readability_factors = [
            analysis['comment_ratio'] * 10,  # Commenti migliorano leggibilità
            min(analysis['function_density'], 20),  # Funzioni moderate sono buone
            max(0, 50 - analysis['complexity_score'])  # Meno complessità = più leggibile
        ]
        analysis['readability_score'] = sum(readability_factors) / len(readability_factors)
        
        return analysis
    
    def generate_comprehensive_report(self, common_tasks, min_languages=None):
        """Genera un report completo delle task comuni"""
        print("📋 Generazione report completo...")
        
        report = {
            'timestamp': datetime.now().isoformat(),
            'analysis_config': {
                'min_languages': min_languages or len(self.supported_languages),
                'supported_languages': list(self.supported_languages.keys())
            },
            'summary': {
                'total_common_tasks': len(common_tasks),
                'languages_analyzed': len(self.supported_languages)
            },
            'tasks': []
        }
        
        for task_info in common_tasks:
            task_name = task_info['name']
            print(f"  🔍 Analizzando {task_name}...")
            
            quality_analysis = self.analyze_code_quality(task_name)
            
            task_report = {
                'name': task_name,
                'languages': task_info['languages'],
                'language_count': task_info['language_count'],
                'quality_analysis': quality_analysis,
                'quality_summary': self.summarize_quality(quality_analysis)
            }
            
            report['tasks'].append(task_report)
        
        return report
    
    def summarize_quality(self, quality_analysis):
        """Riassume l'analisi qualitativa"""
        if not quality_analysis:
            return {'average_readability': 0, 'best_implementation': None, 'worst_implementation': None}
        
        readability_scores = []
        language_scores = {}
        
        for lang, analysis in quality_analysis.items():
            if 'readability_score' in analysis:
                score = analysis['readability_score']
                readability_scores.append(score)
                language_scores[lang] = score
        
        if not readability_scores:
            return {'average_readability': 0, 'best_implementation': None, 'worst_implementation': None}
        
        avg_readability = sum(readability_scores) / len(readability_scores)
        best_lang = max(language_scores, key=language_scores.get)
        worst_lang = min(language_scores, key=language_scores.get)
        
        return {
            'average_readability': round(avg_readability, 2),
            'best_implementation': best_lang,
            'worst_implementation': worst_lang,
            'readability_range': {
                'min': round(min(readability_scores), 2),
                'max': round(max(readability_scores), 2)
            }
        }
    
    def save_results(self, report, common_tasks_simple):
        """Salva i risultati dell'analisi"""
        os.makedirs(self.results_dir, exist_ok=True)
        
        # Salva report completo
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        detailed_file = os.path.join(self.results_dir, f"detailed_analysis_{timestamp}.json")
        
        try:
            with open(detailed_file, 'w') as f:
                json.dump(report, f, indent=2)
            print(f"💾 Report dettagliato salvato: {detailed_file}")
        except Exception as e:
            print(f"⚠️ Errore salvataggio report dettagliato: {e}")
        
        # Salva versione semplice (compatibilità)
        simple_file = os.path.join(self.results_dir, "common_tasks.json")
        
        try:
            simple_data = {
                'total_tasks': len(common_tasks_simple),
                'languages_count': len(self.supported_languages),
                'languages': list(self.supported_languages.keys()),
                'tasks': [task['name'] for task in common_tasks_simple]
            }
            
            with open(simple_file, 'w') as f:
                json.dump(simple_data, f, indent=2)
            print(f"💾 Lista task comuni salvata: {simple_file}")
        except Exception as e:
            print(f"⚠️ Errore salvataggio lista semplice: {e}")
    
    def print_summary(self, report):
        """Stampa un riassunto dei risultati"""
        print(f"\n📊 RIASSUNTO ANALISI:")
        print(f"🎯 Task comuni trovate: {report['summary']['total_common_tasks']}")
        print(f"🔤 Linguaggi analizzati: {report['summary']['languages_analyzed']}")
        
        if report['tasks']:
            print(f"\n🏆 TOP TASK (per numero di linguaggi):")
            
            for i, task in enumerate(report['tasks'][:5], 1):
                quality = task['quality_summary']
                print(f"  {i}. {task['name']}")
                print(f"     💬 Linguaggi: {task['language_count']}")
                print(f"     📈 Leggibilità media: {quality['average_readability']:.1f}")
                if quality['best_implementation']:
                    print(f"     🥇 Migliore: {quality['best_implementation']}")
        
        # Statistiche qualità
        all_scores = []
        for task in report['tasks']:
            if task['quality_summary']['average_readability'] > 0:
                all_scores.append(task['quality_summary']['average_readability'])
        
        if all_scores:
            avg_overall = sum(all_scores) / len(all_scores)
            print(f"\n📊 QUALITÀ MEDIA COMPLESSIVA: {avg_overall:.1f}/100")
    
    def find_and_analyze_all(self, min_languages=None):
        """Funzione principale: trova e analizza tutte le task comuni"""
        print("🚀 ADVANCED TASK FINDER - Analisi Completa")
        print("🔍 Ricerca e analisi task comuni avanzata")
        
        # Trova task comuni
        common_tasks = self.find_common_tasks(min_languages)
        
        if not common_tasks:
            print("❌ Nessuna task comune trovata")
            return
        
        # Genera report completo
        report = self.generate_comprehensive_report(common_tasks, min_languages)
        
        # Salva risultati
        self.save_results(report, common_tasks)
        
        # Stampa riassunto
        self.print_summary(report)
        
        print("✅ Analisi completata")
        return report
    
    def flexible_search(self, min_coverage=0.8):
        """Ricerca flessibile con soglia di copertura"""
        print(f"🔍 Ricerca flessibile (copertura ≥{min_coverage*100:.0f}%)")
        
        total_languages = len(self.supported_languages)
        min_languages = int(total_languages * min_coverage)
        
        return self.find_and_analyze_all(min_languages)


if __name__ == "__main__":
    finder = AdvancedTaskFinder()
    
    # Ricerca standard (tutti i linguaggi)
    print("1️⃣ Ricerca task presenti in TUTTI i linguaggi:")
    finder.find_and_analyze_all()
    
    print("\n" + "="*60)
    
    # Ricerca flessibile (80% dei linguaggi)
    print("2️⃣ Ricerca flessibile (80% dei linguaggi):")
    finder.flexible_search(min_coverage=0.8)
