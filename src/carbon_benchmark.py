#!/usr/bin/env python3
"""
Carbon Benchmark - Benchmarking system for measuring average CO2 emissions
Runs each code 30 times to calculate accurate emissions statistics
Refactored to use TaskSearcher for unified task management
"""

import os
import sys
import json
import time
import logging
from pathlib import Path
import statistics
from datetime import datetime
from tqdm import tqdm

# Adds the path to import CLAP modules
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
scripts_path = os.path.join(project_root, 'scripts')
sys.path.insert(0, modules_path)
sys.path.insert(0, scripts_path)

# Import TaskSearcher for unified task management
try:
    from task_searcher import TaskSearcher, CodeQualityAnalyzer
    TASK_SEARCHER_AVAILABLE = True
except ImportError:
    TASK_SEARCHER_AVAILABLE = False
    print("⚠️ TaskSearcher not available - using fallback mode")

# Import export and visualization tools
try:
    from export_to_csv import SWAMCSVExporter
    CSV_EXPORT_AVAILABLE = True
except ImportError:
    CSV_EXPORT_AVAILABLE = False
    
try:
    from visualize_results import CLAPVisualizer
    VISUALIZATION_AVAILABLE = True
except ImportError:
    VISUALIZATION_AVAILABLE = False

try:
    from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
    from src.smart_executor import SmartExecutor
    from src.finder import UnifiedTaskFinder
    CARBON_TRACKING_AVAILABLE = True
except ImportError:
    try:
        # Fallback for direct import
        from carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
        from smart_executor import SmartExecutor
        from finder import UnifiedTaskFinder
        CARBON_TRACKING_AVAILABLE = True
    except ImportError:
        CARBON_TRACKING_AVAILABLE = False


class CarbonBenchmark:
    """Benchmarking system for measuring CO2 emissions with multiple repetitions"""

    def __init__(self, iterations=30, mode='TOP10', auto_export_csv=True, auto_visualize=True, timeout=90):
        """
        Initialize Carbon Benchmark system
        
        Args:
            iterations: Number of iterations per code execution
            mode: Benchmark mode ('FAST', 'TOP10', 'COMPLETE')
            auto_export_csv: Automatically export results to CSV after benchmark
            auto_visualize: Automatically generate visualizations after benchmark
            timeout: Maximum execution time per task in seconds (default: 90)
        """
        self.iterations = iterations
        self.mode = mode.upper()  # Store benchmark mode: FAST, TOP10, COMPLETE
        self.auto_export_csv = auto_export_csv
        self.auto_visualize = auto_visualize
        self.timeout = timeout
        
        # Use absolute paths based on project root for robustness
        base_results_dir = os.path.join(project_root, "results/carbon_benchmark")
        
        # Create mode-specific subdirectory
        mode_dir_map = {
            'FAST': 'fast',
            'TOP10': 'top10',
            'COMPLETE': 'complete'
        }
        mode_subdir = mode_dir_map.get(self.mode, 'other')
        self.results_dir = os.path.join(base_results_dir, mode_subdir)
        
        self.code_base_path = os.path.join(project_root, "data/generated/code_snippets")
        
        self.executor = SmartExecutor()
        
        # Initialize TaskSearcher for unified task management
        if TASK_SEARCHER_AVAILABLE:
            self.task_searcher = TaskSearcher()
            self.logger = logging.getLogger(__name__)
            self.logger.setLevel(logging.DEBUG)
            if not self.logger.handlers:
                handler = logging.StreamHandler()
                handler.setLevel(logging.DEBUG)
                formatter = logging.Formatter('%(levelname)s - %(message)s')
                handler.setFormatter(formatter)
                self.logger.addHandler(handler)
        else:
            # Fallback to original finder
            self.finder = UnifiedTaskFinder()
            self.finder.create_dataset_dataframe(verbose=False)
            self.task_searcher = None
            self.logger = logging.getLogger(__name__)
            
        self.benchmark_results = {}

        # Statistics on failures by language
        self.language_failures = {}

        # Create directories for results
        os.makedirs(self.results_dir, exist_ok=True)

        print(f"🔬 CARBON BENCHMARK SYSTEM - {self.mode} MODE")
        print(f"⚙️  Configured for {self.iterations} iterations per code")
        print(f"📁 Results saved in: {self.results_dir}")

        if not CARBON_TRACKING_AVAILABLE:
            print("⚠️  Carbon tracking not available")
            return
    
    def save_checkpoint(self, benchmark_data, checkpoint_id):
        """Save intermediate checkpoint to resume later if crash occurs"""
        checkpoint_file = os.path.join(self.results_dir, f"checkpoint_{checkpoint_id}.json")
        try:
            with open(checkpoint_file, 'w', encoding='utf-8') as f:
                json.dump({
                    'checkpoint_id': checkpoint_id,
                    'timestamp': datetime.now().isoformat(),
                    'mode': self.mode,
                    'iterations': self.iterations,
                    'data': benchmark_data
                }, f, indent=2)
            print(f"   💾 Checkpoint saved: {os.path.basename(checkpoint_file)}")
            return True
        except Exception as e:
            print(f"   ⚠️ Error saving checkpoint: {e}")
            return False
    
    def load_latest_checkpoint(self):
        """Load the most recent checkpoint if exists"""
        import glob
        checkpoint_files = glob.glob(os.path.join(self.results_dir, "checkpoint_*.json"))
        if not checkpoint_files:
            return None
        
        # Get most recent checkpoint
        latest_checkpoint = max(checkpoint_files, key=os.path.getmtime)
        
        try:
            with open(latest_checkpoint, 'r', encoding='utf-8') as f:
                checkpoint_data = json.load(f)
            print(f"   📂 Found checkpoint: {os.path.basename(latest_checkpoint)}")
            print(f"      Created: {checkpoint_data.get('timestamp', 'unknown')}")
            print(f"      Tasks completed: {len(checkpoint_data.get('data', {}))}")
            return checkpoint_data
        except Exception as e:
            print(f"   ⚠️ Error loading checkpoint: {e}")
            return None
    
    def clear_checkpoints(self):
        """Clear all checkpoint files after successful completion"""
        import glob
        checkpoint_files = glob.glob(os.path.join(self.results_dir, "checkpoint_*.json"))
        for checkpoint_file in checkpoint_files:
            try:
                os.remove(checkpoint_file)
            except Exception as e:
                print(f"   ⚠️ Could not remove checkpoint {checkpoint_file}: {e}")
    
    def free_memory(self):
        """Force garbage collection to free memory"""
        import gc
        gc.collect()
    
    def format_co2_value(self, kg_value, unit='mg'):
        """Converts and formats CO2 values from kg to more readable units."""
        if unit == 'mg':
            # Converts from kg to mg (1 kg = 1,000,000 mg)
            mg_value = kg_value * 1_000_000
            if mg_value < 0.001:
                return f"{mg_value:.6f} mg CO2eq"
            elif mg_value < 1:
                return f"{mg_value:.3f} mg CO2eq"
            else:
                return f"{mg_value:.2f} mg CO2eq"
        elif unit == 'g':
            # Converts from kg to g (1 kg = 1,000 g)
            g_value = kg_value * 1_000
            return f"{g_value:.6f} g CO2eq"
        else:
            # Keeps in kg
            return f"{kg_value:.8f} kg CO2eq"

    def benchmark_single_execution(self, code, language, task_name, iteration):
        """Runs a single benchmark of a code"""
        session_id = f"{task_name}_{language}_iter{iteration:02d}_{datetime.now().strftime('%H%M%S')}"

        try:
            # Start tracking
            if CARBON_TRACKING_AVAILABLE:
                start_carbon_tracking(f"{task_name}_benchmark", f"{language}_iter{iteration}")

            start_time = time.time()

            # Temporarily disable tracking in smart_executor to avoid double tracking
            original_tracking = getattr(self.executor, 'disable_carbon_tracking', False)
            self.executor.disable_carbon_tracking = True

            # Execute the code
            result = self.executor.execute_code(code, language, f"{task_name}_benchmark_iter{iteration}", timeout=self.timeout)

            # Restore smart_executor tracking
            self.executor.disable_carbon_tracking = original_tracking

            execution_time = time.time() - start_time

            # Stop tracking and get emissions
            emissions = 0.0
            if CARBON_TRACKING_AVAILABLE:
                emissions = stop_carbon_tracking()

            # Capture detailed error information if execution failed
            error_info = {}
            if not result['success']:
                error_info = {
                    'error_message': result.get('error', 'Unknown error'),
                    'error_type': result.get('error_type', 'ExecutionError'),
                    'exit_code': result.get('exit_code', None),
                    'stdout': result.get('output', '')[:500] if result.get('output') else '',  # First 500 chars
                    'stderr': result.get('stderr', '')[:500] if result.get('stderr') else ''   # First 500 chars
                }

            return {
                'iteration': iteration,
                'success': result['success'],
                'execution_time': execution_time,
                'emissions': emissions if emissions else 0.0,
                'session_id': session_id,
                'error': result.get('error', '') if not result['success'] else '',
                'error_details': error_info if error_info else None,
                'timestamp': datetime.now().isoformat()
            }

        except Exception as e:
            if CARBON_TRACKING_AVAILABLE:
                stop_carbon_tracking()

            # Capture exception details
            import traceback
            error_traceback = traceback.format_exc()

            return {
                'iteration': iteration,
                'success': False,
                'execution_time': 0.0,
                'emissions': 0.0,
                'session_id': session_id,
                'error': str(e),
                'error_details': {
                    'error_message': str(e),
                    'error_type': type(e).__name__,
                    'traceback': error_traceback[:1000]  # First 1000 chars of traceback
                },
                'timestamp': datetime.now().isoformat()
            }

    def load_task_code(self, task_name, language):
        """
        Loads the code for a specific task using TaskSearcher for unified access.
        Falls back to original method if TaskSearcher is not available.
        """
        try:
            if TASK_SEARCHER_AVAILABLE and self.task_searcher:
                # Use TaskSearcher to find and load the task
                found_tasks = self.task_searcher.search_tasks(task_name, fuzzy=False)
                
                if language in found_tasks and found_tasks[language]:
                    file_path = found_tasks[language][0]
                    
                    try:
                        with open(file_path, 'r', encoding='utf-8') as f:
                            content = f.read()
                            # Apply cleaning using UnifiedTaskFinder
                            content = UnifiedTaskFinder.clean_code_content(content)
                            self.logger.debug(f"✅ Found code for '{task_name}' in {file_path}")
                            return content
                    except Exception as e:
                        self.logger.warning(f"⚠️ Error reading file {file_path}: {e}")
                        return None
                else:
                    self.logger.debug(f"❌ No file found for task '{task_name}' in {language}")
                    return None
            else:
                # Fallback to original method using finder
                return self._load_task_code_fallback(task_name, language)
                
        except Exception as e:
            self.logger.error(f"❌ Error loading code for {task_name}: {e}")
            return None

    def _load_task_code_fallback(self, task_name, language):
        """Fallback method for loading code when TaskSearcher is not available"""
        try:
            df = self.finder.df
            filtered = df[(df['task_name'] == task_name) & (df['language'].str.lower() == language.lower())]
            
            if filtered.empty:
                self.logger.debug(f"No file found for task '{task_name}' in {language}")
                return None
            
            file_path = filtered.iloc[0]['file_path']
            
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                    content = UnifiedTaskFinder.clean_code_content(content)
                    self.logger.debug(f"Found code for '{task_name}' in {file_path}")
                    return content
            except Exception as e:
                self.logger.warning(f"Error reading file {file_path}: {e}")
                return None
                
        except Exception as e:
            self.logger.error(f"Error loading code for {task_name}: {e}")
            return None

    def benchmark_task_language(self, task_name, language, code):
        """Benchmark a task in a specific language with quality analysis"""
        print(f"\n Benchmark: {task_name} in {language}")

        # Perform quality analysis using TaskSearcher's analyzer
        if TASK_SEARCHER_AVAILABLE:
            metrics = CodeQualityAnalyzer.analyze(code, language)
            print(f" Quality Score: {metrics.quality_score}/100 ({metrics.score_label})")

        iteration_results = []
        successful_runs = 0

        # Use tqdm for the progress bar of iterations
        desc = f"{language:>12}"
        with tqdm(total=self.iterations, desc=desc, unit="iter", 
                  bar_format="{desc}: {percentage:3.0f}%|{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}, {rate_fmt}]") as pbar:
            
            for i in range(1, self.iterations + 1):
                result = self.benchmark_single_execution(code, language, task_name, i)
                iteration_results.append(result)

                if result['success']:
                    successful_runs += 1
                    pbar.set_postfix({
                        'Status': '✅',
                        'Time': f"{result['execution_time']:.3f}s",
                        'CO2': self.format_co2_value(result['emissions'])[:8]
                    })
                else:
                    pbar.set_postfix({
                        'Status': '❌', 
                        'Error': result['error'][:20] + "..." if len(result['error']) > 20 else result['error']
                    })

                pbar.update(1)
                # Short pause between iterations to stabilise measurements
                time.sleep(0.1)

        # Calculate statistics
        success_rate = (successful_runs / self.iterations) * 100

        # Track failures by language
        if successful_runs == 0:
            if language not in self.language_failures:
                self.language_failures[language] = {
                    'failed_tasks': [],
                    'total_failures': 0,
                    'sample_errors': set()
                }
            
            self.language_failures[language]['failed_tasks'].append(task_name)
            self.language_failures[language]['total_failures'] += 1

            # Collect error samples (max 3 per language)
            failed_results = [r for r in iteration_results if not r['success']]
            if failed_results and len(self.language_failures[language]['sample_errors']) < 3:
                sample_error = failed_results[0]['error'][:100] + "..." if len(failed_results[0]['error']) > 100 else failed_results[0]['error']
                self.language_failures[language]['sample_errors'].add(sample_error)

        # Collect detailed error information from all failed iterations
        error_details = []
        for r in iteration_results:
            if not r['success'] and r.get('error_details'):
                error_details.append({
                    'iteration': r['iteration'],
                    'error_type': r['error_details'].get('error_type', 'Unknown'),
                    'error_message': r['error_details'].get('error_message', r.get('error', '')),
                    'traceback': r['error_details'].get('traceback', None),
                    'exit_code': r['error_details'].get('exit_code', None),
                    'stdout': r['error_details'].get('stdout', None),
                    'stderr': r['error_details'].get('stderr', None)
                })

        # Filter only successful executions for statistics
        successful_results = [r for r in iteration_results if r['success']]

        statistics_data = {
            'total_iterations': self.iterations,
            'successful_runs': successful_runs,
            'success_rate': success_rate,
            'emissions_stats': {},
            'execution_time_stats': {},
            'all_iterations': iteration_results,
            'error_details': error_details if error_details else None
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

        # Print results
        self.print_benchmark_summary(task_name, language, statistics_data)

        return statistics_data

    def print_benchmark_summary(self, task_name, language, stats):
        """Print a summary of the benchmark"""
        print(f"\n BENCHMARK RESULTS: {task_name} - {language}")
        print("-" * 60)
        print(f"✅ Successful executions: {stats['successful_runs']}/{stats['total_iterations']} ({stats['success_rate']:.1f}%)")

        if stats['emissions_stats']:
            em = stats['emissions_stats']
            print(f" CO2 EMISSIONS:")
            print(f"   • Mean: {self.format_co2_value(em['mean'])}")
            print(f"   • Median: {self.format_co2_value(em['median'])}")
            print(f"   • Min-Max: {self.format_co2_value(em['min'])} - {self.format_co2_value(em['max'])}")
            print(f"   • Std Dev: {self.format_co2_value(em['std_dev'])}")
            print(f"   • Total {self.iterations} executions: {self.format_co2_value(em['total'])}")

            et = stats['execution_time_stats']
            print(f"  EXECUTION TIME:")
            print(f"   • Mean: {et['mean']:.3f}s")
            print(f"   • Median: {et['median']:.3f}s")
            print(f"   • Min-Max: {et['min']:.3f} - {et['max']:.3f}s")
            print(f"   • Std Dev: {et['std_dev']:.3f}s")
        else:
            print("❌ No successful executions - unable to calculate statistics")

    def get_available_languages(self):
        """Get list of available languages using TaskSearcher"""
        if TASK_SEARCHER_AVAILABLE and self.task_searcher:
            return self.task_searcher.available_languages
        else:
            # Fallback to executor languages
            return list(self.executor.available_languages.keys())

    def get_all_unique_tasks(self):
        """Get all unique tasks from the dataset using TaskSearcher"""
        if TASK_SEARCHER_AVAILABLE and self.task_searcher:
            # Use TaskSearcher to scan all tasks
            all_tasks = set()
            
            # Search for common patterns to discover all tasks
            for category in ['algorithms', 'basic', 'io', 'mathematics', 'misc', 'strings']:
                category_path = os.path.join(self.code_base_path, category)
                if os.path.exists(category_path):
                    for lang_dir in os.listdir(category_path):
                        lang_path = os.path.join(category_path, lang_dir)
                        if os.path.isdir(lang_path):
                            for file_name in os.listdir(lang_path):
                                if file_name.startswith('snippet_'):
                                    task_name = self._extract_task_name_from_filename(file_name)
                                    if task_name:
                                        all_tasks.add(task_name)
            
            return sorted(all_tasks)
        else:
            # Fallback to original method
            return self._get_all_unique_tasks_fallback()

    def _extract_task_name_from_filename(self, filename):
        """Extract the task name from the filename"""
        try:
            if filename.startswith('snippet_') and '_' in filename:
                parts = filename.split('_', 2)
                if len(parts) >= 3:
                    task_name = parts[2].rsplit('.', 1)[0]
                    return task_name
            return None
        except:
            return None

    def _get_all_unique_tasks_fallback(self):
        """Fallback method to get all unique tasks"""
        all_tasks = set()
        categories = ['algorithms', 'basic', 'io', 'mathematics', 'misc', 'strings']
        
        for category in categories:
            category_path = os.path.join(self.code_base_path, category)
            if os.path.exists(category_path):
                for lang_dir in os.listdir(category_path):
                    lang_path = os.path.join(category_path, lang_dir)
                    if os.path.isdir(lang_path):
                        for file_name in os.listdir(lang_path):
                            if file_name.startswith('snippet_'):
                                task_name = self._extract_task_name_from_filename(file_name)
                                if task_name:
                                    all_tasks.add(task_name)
        
        return sorted(all_tasks)

    def task_exists_for_language(self, task_name, language):
        """Check if a task exists for a specific language using TaskSearcher"""
        if TASK_SEARCHER_AVAILABLE and self.task_searcher:
            found_tasks = self.task_searcher.search_tasks(task_name, fuzzy=False)
            return language in found_tasks and len(found_tasks[language]) > 0
        else:
            # Fallback
            code = self.load_task_code(task_name, language)
            return code is not None

    def benchmark_all_tasks(self):
        """Run benchmark on ALL tasks available in the dataset (FULL mode)
        With checkpoint support to handle crashes and memory issues"""

        print(f"\n STARTING CARBON BENCHMARK SYSTEM - FULL MODE")
        print("=" * 60)

        if not CARBON_TRACKING_AVAILABLE:
            print("⚠️  Carbon tracking not available - exiting")
            return {}

        # Check for existing checkpoint
        checkpoint_data = self.load_latest_checkpoint()
        resume_from_checkpoint = False
        
        if checkpoint_data:
            try:
                resume = input("   Resume from checkpoint? [S/n]: ").strip().lower()
                resume_from_checkpoint = resume in ['', 's', 'si', 'sì', 'y', 'yes']
            except (EOFError, KeyboardInterrupt):
                print("\n   Starting fresh benchmark...")
                resume_from_checkpoint = False
        
        # Get available languages using TaskSearcher
        available_languages = self.get_available_languages()

        print(f" Available languages: {len(available_languages)}")
        print(f" Languages: {', '.join(available_languages)}")

        if not available_languages:
            print("❌ No languages available for benchmarking")
            return {}

        # Get all unique tasks using TaskSearcher
        print(" Scanning complete dataset to find all tasks...")
        all_tasks = self.get_all_unique_tasks()

        # Filter only tasks that have at least 2 available languages
        valid_tasks = []
        print("✅ Checking task availability for available languages...")

        for task_name in all_tasks:
            available_count = 0
            for language in available_languages:
                if self.task_exists_for_language(task_name, language):
                    available_count += 1

            if available_count >= 2:
                valid_tasks.append({
                    'name': task_name,
                    'available_languages': available_count
                })

        # Sort by number of available languages (descending)
        valid_tasks.sort(key=lambda x: x['available_languages'], reverse=True)
        selected_tasks = [task['name'] for task in valid_tasks]

        print(f" Total tasks found in dataset: {len(all_tasks)}")
        print(f" Valid tasks (≥2 available languages): {len(selected_tasks)}")
        print(f"  Estimated time: ~{len(selected_tasks) * len(available_languages) * self.iterations * 0.5 / 3600:.1f} hours")

        if not selected_tasks:
            print("❌ No valid tasks found")
            return {}

        # Initialize or resume benchmark data
        if resume_from_checkpoint and checkpoint_data:
            benchmark_data = checkpoint_data.get('data', {})
            completed_tasks = set(benchmark_data.keys())
            selected_tasks = [t for t in selected_tasks if t not in completed_tasks]
            print(f"   ✅ Resuming: {len(completed_tasks)} tasks already completed")
            print(f"   🔄 Remaining: {len(selected_tasks)} tasks to process")
        else:
            benchmark_data = {}
            completed_tasks = set()

        # User confirmation for very large benchmarks
        if len(selected_tasks) > 100:
            print(f"\n⚠️  WARNING: Very extensive benchmark!")
            print(f"   • {len(selected_tasks)} tasks")
            print(f"   • {len(available_languages)} languages")
            print(f"   • {self.iterations} iterations per combination")
            print(f"   • {len(selected_tasks) * len(available_languages) * self.iterations:,} total executions")

            try:
                confirm = input("Proceed anyway? [s/N]: ").strip().lower()
                if confirm not in ['s', 'si', 'sì', 'y', 'yes']:
                    print("❌ Benchmark cancelled by user")
                    return {}
            except (EOFError, KeyboardInterrupt):
                print("\n❌ Benchmark cancelled")
                return {}

        # Checkpoint configuration
        CHECKPOINT_INTERVAL = 2  # Save every 2 tasks
        tasks_since_checkpoint = 0

        # Run benchmark for each task/language
        total_combinations = len(selected_tasks) * len(available_languages)

        print(f"\n Starting full benchmark: {len(selected_tasks)} tasks × {len(available_languages)} languages")
        print(f" Total combinations: {total_combinations:,}")
        print(f" 💾 Checkpoint every {CHECKPOINT_INTERVAL} tasks")
        print("=" * 60)

        # Use tqdm for overall progress
        with tqdm(total=total_combinations, desc=" Benchmark Progress", unit="combo",
                  bar_format="{desc}: {percentage:3.0f}%|{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}, {rate_fmt}]") as main_pbar:
            
            for task_idx, task_name in enumerate(selected_tasks):
                benchmark_data[task_name] = {}
                main_pbar.set_description(f"🔬 Task: {task_name[:25]}")

                for language in available_languages:
                    # Check if the task exists for this language
                    if not self.task_exists_for_language(task_name, language):
                        benchmark_data[task_name][language] = {
                            'total_iterations': self.iterations,
                            'successful_runs': 0,
                            'success_rate': 0.0,
                            'error': 'Task not available for this language'
                        }
                        main_pbar.set_postfix({'Lang': language, 'Status': ' Skip'})
                        main_pbar.update(1)
                        continue

                    # Load code for this task/language using TaskSearcher
                    code = self.load_task_code(task_name, language)

                    if not code:
                        benchmark_data[task_name][language] = {
                            'total_iterations': self.iterations,
                            'successful_runs': 0,
                            'success_rate': 0.0,
                            'error': 'Code not found in dataset'
                        }
                        main_pbar.set_postfix({'Lang': language, 'Status': '❌ No Code'})
                        main_pbar.update(1)
                        continue

                    # Update postfix with current info
                    main_pbar.set_postfix({'Lang': language, 'Status': '🔄 Running'})

                    # Run benchmark for this language
                    stats = self.benchmark_task_language(task_name, language, code)
                    benchmark_data[task_name][language] = stats

                    # Update with result
                    success_rate = stats.get('success_rate', 0)
                    status = '✅' if success_rate > 50 else '❌' if success_rate == 0 else '⚠️'
                    main_pbar.set_postfix({'Lang': language, 'Status': f'{status} {success_rate:.0f}%'})
                    main_pbar.update(1)
                
                # Checkpoint management after each task
                tasks_since_checkpoint += 1
                
                # Save checkpoint every N tasks
                if tasks_since_checkpoint >= CHECKPOINT_INTERVAL:
                    checkpoint_id = datetime.now().strftime("%Y%m%d_%H%M%S")
                    self.save_checkpoint(benchmark_data, checkpoint_id)
                    tasks_since_checkpoint = 0
                    
                    # Free memory
                    self.free_memory()
                    main_pbar.set_description(f"🔬 Task: {task_name[:25]} [💾 Saved]")

        # Save final results
        print("\n💾 Saving final results...")
        self.save_benchmark_results(benchmark_data)
        self.generate_benchmark_report(benchmark_data)
        
        # Clear checkpoints after successful completion
        print("🧹 Cleaning up checkpoints...")
        self.clear_checkpoints()
        
        # Generate rankings
        self.generate_rankings(benchmark_data)
        
        # Auto-export and visualize if enabled
        if self.auto_export_csv:
            self.export_to_csv()
        if self.auto_visualize:
            self.generate_visualizations()

        return benchmark_data

    def benchmark_common_tasks(self, max_tasks=10):
        """Run benchmark on common tasks across languages"""

        # Check if common tasks analysis file exists
        common_tasks_path = os.path.join(project_root, "results/task_analysis/common_tasks.json")
        
        if not os.path.exists(common_tasks_path):
            print("❌ Common tasks file not found.")
            print(" To generate the common_tasks.json file, please run:")
            print("   python main.py analyze")
            return {}

        print(f"\n START CARBON BENCHMARK SYSTEM")
        print("=" * 60)

        if not CARBON_TRACKING_AVAILABLE:
            print("⚠️  Carbon tracking not available - exiting")
            return {}

        # Get available languages using TaskSearcher
        available_languages = self.get_available_languages()

        print(f" Available languages: {len(available_languages)}")
        print(f" Languages: {', '.join(available_languages)}")

        if not available_languages:
            print("❌ No languages available for benchmarking")
            return {}

        # Load common tasks from file
        try:
            with open(common_tasks_path, 'r', encoding='utf-8') as f:
                data = json.load(f)
                
                if 'common_tasks' in data and isinstance(data['common_tasks'], list):
                    available_tasks = [task['name'] for task in data['common_tasks'] if 'name' in task]

                    # Select tasks according to max_tasks
                    if max_tasks and len(available_tasks) > max_tasks:
                        selected_tasks = available_tasks[:max_tasks]
                        print(f" Selected tasks for benchmark: {len(selected_tasks)} (first {max_tasks} tasks)")
                    else:
                        selected_tasks = available_tasks
                        print(f" Selected tasks for benchmark: {len(selected_tasks)}")

                    if not selected_tasks:
                        print("❌ No common tasks found")
                        return {}
                else:
                    print("❌ Invalid common tasks format")
                    return {}
        except Exception as e:
            print(f"❌ Error loading tasks: {e}")
            return {}

        # Run benchmark for each task/language combination
        benchmark_data = {}
        total_combinations = len(selected_tasks) * len(available_languages)
        current_combination = 0
        
        # Checkpoint configuration (for larger benchmarks)
        CHECKPOINT_INTERVAL = 2  # Save every 2 tasks for common tasks
        tasks_since_checkpoint = 0

        for task_name in selected_tasks:
            benchmark_data[task_name] = {}
            print(f"\n TASK: {task_name}")
            print("=" * 40)

            for language in available_languages:
                current_combination += 1
                print(f"\n Progress: {current_combination}/{total_combinations} ({(current_combination/total_combinations)*100:.1f}%)")

                # Load code for this task/language using TaskSearcher
                code = self.load_task_code(task_name, language)

                if not code:
                    print(f"⚠️  File not found for {task_name} in {language} - skipping")
                    benchmark_data[task_name][language] = {
                        'total_iterations': self.iterations,
                        'successful_runs': 0,
                        'success_rate': 0.0,
                        'error': 'File not available in dataset'
                    }
                    continue

                print(f"\n Benchmark: {task_name} in {language}")

                # Run benchmark for this language
                stats = self.benchmark_task_language(task_name, language, code)
                benchmark_data[task_name][language] = stats
            
            # Checkpoint management after each task
            tasks_since_checkpoint += 1
            
            # Save checkpoint every N tasks (only for larger benchmarks)
            if len(selected_tasks) > 2 and tasks_since_checkpoint >= CHECKPOINT_INTERVAL:
                checkpoint_id = datetime.now().strftime("%Y%m%d_%H%M%S")
                self.save_checkpoint(benchmark_data, checkpoint_id)
                tasks_since_checkpoint = 0
                print(f"   💾 Checkpoint saved")
                
                # Free memory
                self.free_memory()

        # Save final results
        print("\n💾 Saving final results...")
        self.save_benchmark_results(benchmark_data)
        self.generate_benchmark_report(benchmark_data)
        
        # Clear checkpoints after successful completion
        if len(selected_tasks) > 2:
            print("🧹 Cleaning up checkpoints...")
            self.clear_checkpoints()
        
        # Generate rankings
        self.generate_rankings(benchmark_data)
        
        # Auto-export and visualize if enabled
        if self.auto_export_csv:
            self.export_to_csv()
        if self.auto_visualize:
            self.generate_visualizations()

        return benchmark_data

    def save_benchmark_results(self, benchmark_data):
        """Save benchmark results in mode-specific subdirectory
        
        Results are saved in:
        - results/carbon_benchmark/fast/      (FAST mode)
        - results/carbon_benchmark/top10/     (TOP10 mode)
        - results/carbon_benchmark/complete/  (COMPLETE mode)
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Save detailed results
        detailed_file = os.path.join(self.results_dir, f"carbon_benchmark_detailed_{timestamp}.json")
        with open(detailed_file, 'w') as f:
            json.dump(benchmark_data, f, indent=2)

        # Save summary
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

        print(f"\n💾 Results saved:")
        print(f"   📄 Detailed: {detailed_file}")
        print(f"   📄 Summary: {summary_file}")
        
        return detailed_file, summary_file
    
    def export_to_csv(self, auto_export=True):
        """Export benchmark results to CSV format
        
        Args:
            auto_export: If True, automatically exports latest results
        
        Returns:
            List of exported CSV file paths, or None if export fails
        """
        if not CSV_EXPORT_AVAILABLE:
            print("\n⚠️  CSV export not available (missing export_to_csv module)")
            return None
        
        try:
            print(f"\n📊 Exporting results to CSV (mode: {self.mode})...")
            exporter = SWAMCSVExporter(mode=self.mode)
            
            if auto_export:
                exported_files = exporter.export_all_latest()
                return exported_files
            else:
                # Manual export with specific file
                latest = exporter.find_latest_results()
                if 'carbon_detailed' in latest:
                    files = []
                    files.append(exporter.export_carbon_detailed(latest['carbon_detailed']))
                    files.append(exporter.export_carbon_individual_runs(latest['carbon_detailed']))
                    files.append(exporter.export_language_rankings(latest['carbon_detailed']))
                    return files
                    
        except Exception as e:
            print(f"\n❌ CSV export failed: {e}")
            return None
    
    def generate_visualizations(self, auto_visualize=True):
        """Generate visualization charts from benchmark results
        
        Args:
            auto_visualize: If True, automatically generates all charts
        
        Returns:
            True if successful, False otherwise
        """
        if not VISUALIZATION_AVAILABLE:
            print("\n⚠️  Visualization not available - missing dependencies")
            print("   Install with: conda install matplotlib seaborn pandas numpy")
            print("   Or: pip install matplotlib seaborn pandas numpy")
            print("   CSV files are still available in results/csv/")
            return False
        
        try:
            print(f"\n📈 Generating visualizations (mode: {self.mode})...")
            visualizer = CLAPVisualizer(mode=self.mode)
            
            if auto_visualize:
                visualizer.generate_all_plots()
                return True
            else:
                # Could add selective visualization here
                return False
                
        except Exception as e:
            print(f"\n❌ Visualization failed: {e}")
            print("   This might be due to:")
            print("   - Missing data files")
            print("   - Incompatible data format")
            print("   - Display/backend issues")
            print("\n   CSV files are still available for manual analysis")
            import traceback
            traceback.print_exc()
            return False
    
    def generate_rankings(self, benchmark_data=None):
        """Generate multi-tier rankings from benchmark results
        
        Args:
            benchmark_data: Benchmark results dictionary (uses self.benchmark_results if None)
        
        Returns:
            Tuple of (category_rankings, overall_rankings)
        """
        try:
            from ranking_analyzer import RankingAnalyzer
            
            print(f"\n📊 Generating Multi-Tier Rankings...")
            
            # Use provided data or load from results
            data_to_analyze = benchmark_data or self.benchmark_results
            
            if not data_to_analyze:
                print("⚠️  No benchmark data available for ranking analysis")
                return None, None
            
            analyzer = RankingAnalyzer(benchmark_results=data_to_analyze)
            category_rankings, overall_rankings = analyzer.generate_all_rankings(save=True)
            
            return category_rankings, overall_rankings
            
        except ImportError:
            print("\n⚠️  Ranking analyzer not available")
            return None, None
        except Exception as e:
            print(f"\n❌ Ranking analysis failed: {e}")
            import traceback
            traceback.print_exc()
            return None, None

    def generate_benchmark_report(self, benchmark_data):
        """Generate a final benchmark report"""
        print(f"\n FINAL CARBON BENCHMARK REPORT")
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

        # Calculate averages per language
        for language in language_stats:
            if language_stats[language]['total_executions'] > 0:
                language_stats[language]['avg_emissions_per_execution'] = \
                    language_stats[language]['total_emissions'] / language_stats[language]['total_executions']

        # Get complete list of supported languages
        all_languages = self.get_available_languages()

        # Calculate language statistics
        working_languages = set(language_stats.keys())
        failed_languages = set(all_languages) - working_languages

        print(f" GENERAL STATISTICS:")
        print(f"  Tasks benchmarked: {total_tasks}")
        print(f"   Supported languages: {len(all_languages)}")
        print(f"  Working languages: {len(working_languages)}/{len(all_languages)} ({len(working_languages)/len(all_languages)*100:.1f}%)")
        print(f"   Successful total executions: {total_executions}")
        print(f"   Total emissions: {self.format_co2_value(total_emissions)}")
        print(f"   Mean per execution: {self.format_co2_value(total_emissions/total_executions)}" if total_executions > 0 else "   Mean per execution: N/A")

        print(f"\n✅ WORKING LANGUAGES ({len(working_languages)}):")
        if working_languages:
            sorted_languages = sorted(language_stats.items(),
                                      key=lambda x: x[1]['avg_emissions_per_execution'])
            for i, (language, stats) in enumerate(sorted_languages, 1):
                print(f"   {i:2d}. {language:12s}: {self.format_co2_value(stats['avg_emissions_per_execution'])}/run "
                      f"({stats['total_executions']} runs)")
        else:
            print("   ❌ No language successfully completed the benchmark")

        print(f"\n❌ LANGUAGES WITH ISSUES ({len(failed_languages)}):")
        if failed_languages:
            for language in sorted(failed_languages):
                if language in self.language_failures:
                    failure_info = self.language_failures[language]
                    failed_tasks = len(failure_info['failed_tasks'])
                    print(f"   • {language:12s}: {failed_tasks} failed tasks")

                    # Show some sample errors
                    if failure_info['sample_errors']:
                        sample_error = list(failure_info['sample_errors'])[0]
                        if 'Compilation error:' in sample_error:
                            error_type = "Compilation Errors"
                        elif 'libtinfo' in sample_error.lower():
                            error_type = "Library Issues"
                        elif 'no input files' in sample_error.lower():
                            error_type = "Compiler Configuration Issues"
                        elif 'unknown start of token' in sample_error.lower():
                            error_type = "Syntax Errors"
                        elif 'traceback' in sample_error.lower():
                            error_type = "Python Runtime Errors"
                        elif 'error:' in sample_error.lower():
                            error_type = "Compilation Errors"
                        else:
                            error_type = "Other Errors"
                        print(f"     → {error_type}")
                else:
                    print(f"   • {language:12s}: Not Tested")
        else:
            print("   ✅ All languages are working correctly!")

        # Summary statistics of issues
        if self.language_failures:
            print(f"\n⚠️  ISSUE DETAILS:")
            total_failed_tasks = sum(len(info['failed_tasks']) for info in self.language_failures.values())
            print(f"   ❌ Total failed tasks: {total_failed_tasks}")

            # Categorize error types by language
            error_categories = {
                'Compilation': set(),
                'Library Issues': set(),
                'Syntax Errors': set(),
                'Runtime Errors': set(),
                'Configuration Issues': set(),
                'Other Errors': set()
            }
            
            for language, failure_info in self.language_failures.items():
                for error in failure_info['sample_errors']:
                    error_lower = error.lower()
                    if 'compilation' in error_lower or 'compile' in error_lower:
                        error_categories['Compilation'].add(language)
                    elif 'libtinfo' in error_lower or 'library' in error_lower:
                        error_categories['Library Issues'].add(language)
                    elif 'syntax' in error_lower or 'token' in error_lower:
                        error_categories['Syntax Errors'].add(language)
                    elif 'traceback' in error_lower or 'runtime' in error_lower:
                        error_categories['Runtime Errors'].add(language)
                    elif 'no input files' in error_lower or 'command not found' in error_lower:
                        error_categories['Configuration Issues'].add(language)
                    else:
                        error_categories['Other Errors'].add(language)
            
            for category, language_set in error_categories.items():
                if len(language_set) > 0:
                    print(f"   • {category}: {len(language_set)} languages")

        # Annual impact estimate
        if total_emissions > 0:
            daily_estimate = total_emissions * (86400 / (self.iterations * len(language_stats)))
            yearly_estimate = daily_estimate * 365
            print(f"\n IMPACT ESTIMATES:")
            print(f"   Daily estimate (continuous usage): {self.format_co2_value(daily_estimate, 'g')}/day")
            print(f"   Yearly estimate (continuous usage): {self.format_co2_value(yearly_estimate, 'g')}/year")


def benchmark_quick_test(iterations=5):
    """Quick test of the benchmarking system"""
    print("🧪 QUICK BENCHMARK TEST")
    benchmark = CarbonBenchmark(iterations=iterations, mode='FAST')

    # Test with simple Python code
    test_code = '''
import time
import math

# Simple mathematical computation
result = 0
for i in range(1000):
    result += math.sqrt(i) * math.sin(i)

print(f"Result: {result:.2f}")
'''

    stats = benchmark.benchmark_task_language("test_math", "python", test_code)
    return stats


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "test":
        # Quick test
        benchmark_quick_test(5)
    elif len(sys.argv) > 1 and sys.argv[1] == "debug":
        # Fast debug (3 iterations, 2 tasks) - for quick tests
        benchmark = CarbonBenchmark(iterations=3, mode='FAST')
        benchmark.benchmark_common_tasks(max_tasks=2)
    elif len(sys.argv) > 1 and sys.argv[1] == "quick":
        # Quick benchmark (10 iterations, 3 tasks)
        benchmark = CarbonBenchmark(iterations=10, mode='FAST')
        benchmark.benchmark_common_tasks(max_tasks=3)
    else:
        # Default: Top10 (30 iterations, 10 tasks)
        benchmark = CarbonBenchmark(iterations=30, mode='TOP10')
        benchmark.benchmark_common_tasks(max_tasks=10)