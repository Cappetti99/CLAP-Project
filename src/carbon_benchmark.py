#!/usr/bin/env python3
"""
Carbon Benchmark - Sistema di benchmarking per misurare emissioni CO2 medie
Esegue ogni codice 30 volte per calcolare statistiche accurate delle emissioni
"""

import os
import sys
import json
import time
import re
import statistics
from datetime import datetime
from pathlib import Path

# Aggiunge il path per importare i moduli SWAM
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

try:
 from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
 from src.enhanced_executor import EnhancedExecutor
 CARBON_TRACKING_AVAILABLE = True
except ImportError:
 try:
 # Fallback per import diretto
 from carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
 from enhanced_executor import EnhancedExecutor
 CARBON_TRACKING_AVAILABLE = True
 except ImportError:
 CARBON_TRACKING_AVAILABLE = False

class CarbonBenchmark:
 """Sistema di benchmarking per misurare emissioni CO2 con ripetizioni multiple"""

 def __init__(self, iterations=30):
 self.iterations = iterations
 self.results_dir = "results/carbon_benchmark"
 self.executor = EnhancedExecutor()
 self.benchmark_results = {}

 # Crea directory per i risultati
 os.makedirs(self.results_dir, exist_ok=True)

 print(f" CARBON BENCHMARK SYSTEM")
 print(f" Configurato per {self.iterations} iterazioni per codice")
 print(f" Risultati salvati in: {self.results_dir}")

 if not CARBON_TRACKING_AVAILABLE:
 print(" Carbon tracking non disponibile")
 return

 def benchmark_single_execution(self, code, language, task_name, iteration):
 """Esegue un singolo benchmark di un codice"""
 session_id = f"{task_name}_{language}_iter{iteration:02d}_{datetime.now().strftime('%H%M%S')}"

 try:
 # Avvia tracking
 if CARBON_TRACKING_AVAILABLE:
 start_carbon_tracking(f"{task_name}_benchmark", f"{language}_iter{iteration}")

 start_time = time.time()

 # Disabilita temporaneamente il tracking nel smart_executor per evitare doppio tracking
 original_tracking = getattr(self.executor, 'disable_carbon_tracking', False)
 self.executor.disable_carbon_tracking = True

 # Esegui il codice
 result = self.executor.execute_code(code, language, f"{task_name}_benchmark_iter{iteration}")

 # Ripristina il tracking del smart_executor
 self.executor.disable_carbon_tracking = original_tracking

 execution_time = time.time() - start_time

 # Ferma tracking e ottieni emissioni
 emissions = 0.0
 if CARBON_TRACKING_AVAILABLE:
 emissions = stop_carbon_tracking()

 return {
 'iteration': iteration,
 'success': result['success'],
 'execution_time': execution_time,
 'emissions': emissions if emissions else 0.0,
 'session_id': session_id,
 'error': result.get('error', '') if not result['success'] else '',
 'timestamp': datetime.now().isoformat()
 }

 except Exception as e:
 if CARBON_TRACKING_AVAILABLE:
 stop_carbon_tracking()

 return {
 'iteration': iteration,
 'success': False,
 'execution_time': 0.0,
 'emissions': 0.0,
 'session_id': session_id,
 'error': str(e),
 'timestamp': datetime.now().isoformat()
 }

 def load_task_code(self, task_name, language):
 """Carica il codice per una specifica task e linguaggio"""
 # Mapping delle categorie di linguaggi
 language_categories = {
 'python': 'scripting',
 'javascript': 'scripting',
 'ruby': 'scripting',
 'typescript': 'scripting',
 'java': 'oop',
 'cpp': 'oop',
 'csharp': 'oop',
 'c': 'imperative',
 'go': 'imperative',
 'rust': 'imperative',
 'php': 'imperative',
 'haskell': 'functional',
 'ocaml': 'functional',
 'r': 'scientific',
 'julia': 'scientific',
 'matlab': 'scientific'
 }

 category = language_categories.get(language)
 if not category:
 return None

 # Costruisci il path del file
 code_file = f"data/generated/code_snippets/{category}/{language}/snippet_*.{task_name}"

 # Cerca il file usando glob
 import glob
 matching_files = glob.glob(code_file)

 if not matching_files:
 # Prova con nome task leggermente diverso
 safe_task_name = task_name.replace('-', '_')
 code_file_alt = f"data/generated/code_snippets/{category}/{language}/snippet_*_{safe_task_name}"
 matching_files = glob.glob(code_file_alt)

 if not matching_files:
 return None

 # Prendi il primo file trovato
 file_path = matching_files[0]

 try:
 with open(file_path, 'r', encoding='utf-8') as f:
 code = f.read()

 # Verifica che il codice sia eseguibile
 if self.is_code_executable(code, language)[0]:
 return code
 else:
 return None

 except UnicodeDecodeError:
 # Prova con encoding diverso
 try:
 with open(file_path, 'r', encoding='latin1') as f:
 code = f.read()
 return code
 except:
 return None
 except Exception as e:
 print(f" Errore lettura file {file_path}: {e}")
 return None
 """Verifica se il codice è potenzialmente eseguibile"""
 if not code or len(code.strip()) < 10:
 return False, "Codice troppo corto"

 # Pattern che indicano codice non eseguibile
 problematic_patterns = {
 'python': [
 r'>>>\s', # Interactive prompt
 r'^\s*\.\.\.\s', # Continuation prompt
 r'Example:|example:|EXAMPLE:', # Documentation
 r'Output:|output:|OUTPUT:', # Expected output
 ],
 'javascript': [
 r'>\s', # Browser console
 r'Example:|example:|EXAMPLE:',
 r'Output:|output:|OUTPUT:',
 ],
 'all': [
 r'^\s*#.*Example', # Comments with examples
 r'^\s*//.*Example',
 r'Sample\s+(output|run)',
 r'Expected\s+(output|result)',
 r'^\s*\*\s.*example', # Commented examples
 ]
 }

 # Verifica pattern problematici
 patterns_to_check = problematic_patterns.get(language, []) + problematic_patterns.get('all', [])

 for pattern in patterns_to_check:
 if re.search(pattern, code, re.MULTILINE | re.IGNORECASE):
 return False, f"Codice non eseguibile: contiene pattern '{pattern[:20]}...'"

 # Verifica se è principalmente commenti
 lines = code.split('\n')
 code_lines = 0
 comment_lines = 0

 for line in lines:
 line = line.strip()
 if not line:
 continue
 if line.startswith('#') or line.startswith('//') or line.startswith('/*'):
 comment_lines += 1
 else:
 code_lines += 1

 if code_lines == 0:
 return False, "Solo commenti, nessun codice eseguibile"

 if comment_lines > code_lines * 2:
 return False, "Troppi commenti rispetto al codice"

 return True, "OK"

 def benchmark_task_language(self, task_name, language, code):
 print(f"\n Benchmark: {task_name} in {language}")
 print(f" Eseguendo {self.iterations} iterazioni...")

 iteration_results = []
 successful_runs = 0

 for i in range(1, self.iterations + 1):
 print(f" Iterazione {i:2d}/{self.iterations}", end=" ")

 result = self.benchmark_single_execution(code, language, task_name, i)
 iteration_results.append(result)

 if result['success']:
 successful_runs += 1
 print(f" {result['execution_time']:.3f}s - {result['emissions']:.8f} kg CO2eq")
 else:
 print(f" Errore: {result['error'][:50]}...")

 # Piccola pausa tra iterazioni per stabilizzare le misurazioni
 time.sleep(0.1)

 # Calcola statistiche
 success_rate = (successful_runs / self.iterations) * 100

 # Filtra solo le esecuzioni riuscite per le statistiche
 successful_results = [r for r in iteration_results if r['success']]

 statistics_data = {
 'total_iterations': self.iterations,
 'successful_runs': successful_runs,
 'success_rate': success_rate,
 'emissions_stats': {},
 'execution_time_stats': {},
 'all_iterations': iteration_results
 }

 if successful_results:
 emissions_values = [r['emissions'] for r in successful_results]
 execution_times = [r['execution_time'] for r in successful_results]

 statistics_data['emissions_stats'] = {
 'mean': statistics.mean(emissions_values),
 'median': statistics.median(emissions_values),
 'min': min(emissions_values),
 'max': max(emissions_values),
 'std_dev': statistics.stdev(emissions_values) if len(emissions_values) > 1 else 0,
 'total': sum(emissions_values),
 'samples': len(emissions_values)
 }

 statistics_data['execution_time_stats'] = {
 'mean': statistics.mean(execution_times),
 'median': statistics.median(execution_times),
 'min': min(execution_times),
 'max': max(execution_times),
 'std_dev': statistics.stdev(execution_times) if len(execution_times) > 1 else 0,
 'total': sum(execution_times)
 }

 # Stampa risultati
 self.print_benchmark_summary(task_name, language, statistics_data)

 return statistics_data

 def print_benchmark_summary(self, task_name, language, stats):
 """Stampa un riassunto del benchmark"""
 print(f"\n RISULTATI BENCHMARK: {task_name} - {language}")
 print("-" * 60)
 print(f" Esecuzioni riuscite: {stats['successful_runs']}/{stats['total_iterations']} ({stats['success_rate']:.1f}%)")

 if stats['emissions_stats']:
 em = stats['emissions_stats']
 print(f"🌍 EMISSIONI CO2:")
 print(f" • Media: {em['mean']:.8f} kg CO2eq")
 print(f" • Mediana: {em['median']:.8f} kg CO2eq")
 print(f" • Min-Max: {em['min']:.8f} - {em['max']:.8f} kg CO2eq")
 print(f" • Deviazione std: {em['std_dev']:.8f} kg CO2eq")
 print(f" • Totale {self.iterations} esecuzioni: {em['total']:.8f} kg CO2eq")

 et = stats['execution_time_stats']
 print(f" TEMPI ESECUZIONE:")
 print(f" • Media: {et['mean']:.3f}s")
 print(f" • Mediana: {et['median']:.3f}s")
 print(f" • Min-Max: {et['min']:.3f} - {et['max']:.3f}s")
 print(f" • Deviazione std: {et['std_dev']:.3f}s")
 else:
 print(" Nessuna esecuzione riuscita - impossibile calcolare statistiche")

 def benchmark_common_tasks(self, max_tasks=5):
 """Esegue benchmark delle task comuni"""
 print(f"\n AVVIO CARBON BENCHMARK SYSTEM")
 print("=" * 60)

 if not CARBON_TRACKING_AVAILABLE:
 print(" Carbon tracking non disponibile - uscita")
 return {}

 # Rileva linguaggi disponibili dall'executor
 available_languages = list(self.executor.available_languages.keys())

 print(f" Linguaggi disponibili: {len(available_languages)}")
 print(f"📝 Linguaggi: {', '.join(available_languages)}")

 if not available_languages:
 print(" Nessun linguaggio disponibile per il benchmark")
 return {}

 # Carica task comuni
 try:
 common_tasks_file = "results/task_analysis/common_tasks.json"
 if os.path.exists(common_tasks_file):
 with open(common_tasks_file, 'r') as f:
 common_tasks_data = json.load(f)
 if isinstance(common_tasks_data, dict) and 'tasks' in common_tasks_data:
 common_tasks = common_tasks_data['tasks']
 if len(common_tasks) > 0:
 # Prendi le prime max_tasks
 selected_tasks = common_tasks[:max_tasks]
 print(f" Task selezionate per benchmark: {len(selected_tasks)}")
 else:
 print(" Nessuna task comune trovata")
 return {}
 else:
 print(" Formato task comuni non valido")
 return {}
 else:
 print(" File task comuni non trovato. Esegui prima 'python main.py analyze'")
 return {}
 except Exception as e:
 print(f" Errore caricamento task: {e}")
 return {}

 # Esegui benchmark per ogni task/linguaggio
 benchmark_data = {}
 total_combinations = len(selected_tasks) * len(available_languages)
 current_combination = 0

 for task_name in selected_tasks:
 benchmark_data[task_name] = {}
 print(f"\n TASK: {task_name}")
 print("=" * 40)

 for language in available_languages:
 current_combination += 1
 print(f"\n Progresso: {current_combination}/{total_combinations} ({(current_combination/total_combinations)*100:.1f}%)")

 # Carica il codice per questa task/linguaggio
 code = self.load_task_code(task_name, language)

 if not code:
 print(f" File non trovato per {task_name} in {language}")
 continue

 print(f"\n Benchmark: {task_name} in {language}")

 # Esegui benchmark per questo linguaggio
 stats = self.benchmark_task_language(task_name, language, code)
 benchmark_data[task_name][language] = stats

 # Salva risultati
 self.save_benchmark_results(benchmark_data)
 self.generate_benchmark_report(benchmark_data)

 return benchmark_data

 def save_benchmark_results(self, benchmark_data):
 """Salva i risultati del benchmark"""
 timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

 # Salva dati completi
 detailed_file = os.path.join(self.results_dir, f"carbon_benchmark_detailed_{timestamp}.json")
 with open(detailed_file, 'w') as f:
 json.dump(benchmark_data, f, indent=2)

 # Salva summary
 summary_data = {}
 for task_name, task_data in benchmark_data.items():
 summary_data[task_name] = {}
 for language, stats in task_data.items():
 if stats.get('emissions_stats'):
 summary_data[task_name][language] = {
 'success_rate': stats['success_rate'],
 'mean_emissions': stats['emissions_stats']['mean'],
 'mean_execution_time': stats['execution_time_stats']['mean'],
 'total_emissions': stats['emissions_stats']['total']
 }

 summary_file = os.path.join(self.results_dir, f"carbon_benchmark_summary_{timestamp}.json")
 with open(summary_file, 'w') as f:
 json.dump(summary_data, f, indent=2)

 print(f"\n💾 Risultati salvati:")
 print(f" Dettagliati: {detailed_file}")
 print(f" Summary: {summary_file}")

 def generate_benchmark_report(self, benchmark_data):
 """Genera un report finale del benchmark"""
 print(f"\n CARBON BENCHMARK REPORT FINALE")
 print("=" * 60)

 total_tasks = len(benchmark_data)
 total_emissions = 0.0
 total_executions = 0
 language_stats = {}

 for task_name, task_data in benchmark_data.items():
 for language, stats in task_data.items():
 if stats.get('emissions_stats'):
 emissions = stats['emissions_stats']['total']
 executions = stats['successful_runs']

 total_emissions += emissions
 total_executions += executions

 if language not in language_stats:
 language_stats[language] = {
 'total_emissions': 0.0,
 'total_executions': 0,
 'avg_emissions_per_execution': 0.0
 }

 language_stats[language]['total_emissions'] += emissions
 language_stats[language]['total_executions'] += executions

 # Calcola medie per linguaggio
 for language in language_stats:
 if language_stats[language]['total_executions'] > 0:
 language_stats[language]['avg_emissions_per_execution'] = \
 language_stats[language]['total_emissions'] / language_stats[language]['total_executions']

 print(f" STATISTICHE GENERALI:")
 print(f" Task benchmarked: {total_tasks}")
 print(f" Linguaggi testati: {len(language_stats)}")
 print(f" Esecuzioni totali riuscite: {total_executions}")
 print(f" 🌍 Emissioni totali: {total_emissions:.8f} kg CO2eq")
 print(f" Media per esecuzione: {total_emissions/total_executions:.8f} kg CO2eq" if total_executions > 0 else " Media per esecuzione: N/A")

 print(f"\n🏅 RANKING LINGUAGGI PER EFFICIENZA ENERGETICA:")
 sorted_languages = sorted(language_stats.items(),
 key=lambda x: x[1]['avg_emissions_per_execution'])

 for i, (language, stats) in enumerate(sorted_languages, 1):
 print(f" {i:2d}. {language:12s}: {stats['avg_emissions_per_execution']:.8f} kg CO2eq/run "
 f"({stats['total_executions']} runs)")

 # Stima impatto annuale
 if total_emissions > 0:
 daily_estimate = total_emissions * (86400 / (self.iterations * len(language_stats))) # 24h estimate
 yearly_estimate = daily_estimate * 365
 print(f"\n🔮 STIME IMPATTO:")
 print(f" Stima giornaliera (uso continuo): {daily_estimate:.6f} kg CO2eq/giorno")
 print(f" Stima annuale (uso continuo): {yearly_estimate:.3f} kg CO2eq/anno")

def benchmark_quick_test(iterations=5):
 """Test rapido del sistema di benchmark"""
 print(" QUICK BENCHMARK TEST")
 benchmark = CarbonBenchmark(iterations=iterations)

 # Test con codice Python semplice
 test_code = '''
import time
import math

# Calcolo matematico semplice
result = 0
for i in range(1000):
 result += math.sqrt(i) * math.sin(i)

print(f"Risultato: {result:.2f}")
'''

 stats = benchmark.benchmark_task_language("test_math", "python", test_code)
 return stats

if __name__ == "__main__":
 if len(sys.argv) > 1 and sys.argv[1] == "test":
 # Test rapido
 benchmark_quick_test(5)
 elif len(sys.argv) > 1 and sys.argv[1] == "debug":
 # Debug veloce (3 iterazioni, 2 task) - per test rapidi
 benchmark = CarbonBenchmark(iterations=3)
 benchmark.benchmark_common_tasks(max_tasks=2)
 elif len(sys.argv) > 1 and sys.argv[1] == "quick":
 # Benchmark veloce (10 iterazioni, 3 task)
 benchmark = CarbonBenchmark(iterations=10)
 benchmark.benchmark_common_tasks(max_tasks=3)
 else:
 # Benchmark completo (30 iterazioni, 5 task)
 benchmark = CarbonBenchmark(iterations=30)
 benchmark.benchmark_common_tasks(max_tasks=5)
