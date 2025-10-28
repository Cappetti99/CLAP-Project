#!/usr/bin/env python3
"""
CLAP Project - Main Entry Point
Comprehensive system for analyzing and executing multi-language code with CO2 monitoring

This module serves as the main entry point for the CLAP project. It provides:
- Language detection and testing capabilities
- Task analysis and discovery algorithms
- Adaptive code execution across multiple programming languages
- CO2 emission monitoring and benchmarking
- Multi-tier ranking analysis for language performance
- Cleanup and project management utilities
"""

import sys
import os
import argparse
import glob
import shutil
from pathlib import Path

# ===== PATH CONFIGURATION =====
# Add the modules and src directories to the Python path for proper imports
script_dir = os.path.dirname(os.path.abspath(__file__))
modules_path = os.path.join(script_dir, 'modules')
src_path = os.path.join(script_dir, 'src')
sys.path.insert(0, modules_path)
sys.path.insert(0, src_path)

def print_banner():
    """Prints the project banner with version information and ASCII art"""
    # Display the main header with project name and author information
    print("=" * 40)
    print("CLAP PROJECT")
    print("Author: Lorenzo Cappetti")
    print("=" * 40)

def print_help():
    """Prints concise help information showing all available commands and usage patterns.
    
    Displays:
    - Main commands (test, analyze, smart, benchmark, carbon, rankings)
    - Additional commands (execute, find, quality, clean, status)
    - Quick start workflows
    - Benchmark usage examples
    """
    # Display main command group
    print("\nMAIN COMMANDS:")
    print("  test       - Check available languages")
    print("  analyze    - Find common tasks")
    print("  smart      - Smart execution (RECOMMENDED)")
    print("  benchmark  - CO2 measurement")
    print("  carbon     - Emissions report")
    print("  rankings   - Multi-tier rankings analysis")
    
    # Display additional/advanced command group
    print("\nADDITIONAL COMMANDS:")
    print("  execute    - Full execution (all languages)")
    print("  find       - Search task + quality analysis + CO2")
    print("  quality    - Qualitative analysis (experimental)")
    print("  clean      - Cache cleanup")
    print("  status     - Project status")
    
    # Show quick start example workflow
    print("\n QUICK START:")
    print("  python main.py test && python main.py analyze && python main.py smart")
    
def analyze_tasks():
    """Performs task analysis to find the most common programming challenges across available languages.
    
    This function:
    1. Loads language test results to determine available languages
    2. Searches for common tasks that can be executed in multiple languages
    3. Returns the TOP 10 most widely supported tasks
    4. Saves results for use by the smart executor
    
    Returns:
        bool: True if analysis completed successfully, False otherwise
    """
    print("\nANALYSIS OF COMMON TASKS")
    print("-" * 40)
    
    # ===== STEP 1: LOAD AVAILABLE LANGUAGES FROM TEST RESULTS =====
    # Check for recent language test results to determine which languages are available
    available_languages = []
    test_results_dir = Path("results/execution")
    
    if test_results_dir.exists():
        # Look for the most recent test file
        test_files = list(test_results_dir.glob("language_test_results_*.json"))
        if test_files:
            # Get the latest test results file
            latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
            try:
                import json
                with open(latest_test, 'r') as f:
                    test_data = json.load(f)
                    # Extract available languages from test results
                    for lang, result in test_data['results'].items():
                        if result['available']:
                            available_languages.append(lang)
                
                print(f"Using test results from: {latest_test.name}")
                print(f"Detected available languages: {len(available_languages)}")
                print(f"Languages: {', '.join(sorted(available_languages))}")
                print()
            except Exception as e:
                print(f"Error reading test results: {e}")
                available_languages = []
    
    # ===== STEP 2: VERIFY AVAILABLE LANGUAGES =====
    # Ensure that language test has been run and languages are available
    if not available_languages:
        print("  No test results found.")
        print("  REQUIRED ACTION: Run 'python main.py test' first to detect available languages.")
        print("  The 'analyze' command requires test results to work properly.")
        return False
    
    # ===== STEP 3: FIND TOP 10 COMMON TASKS =====
    try:
        from src.finder import UnifiedTaskFinder
        finder = UnifiedTaskFinder()
        
        # Use the TOP 10 logic with available languages
        print(f"Searching for the TOP 10 tasks with the most languages among those available...")
        print(f"Available languages: {len(available_languages)}")
        
        # Use the new logic for TOP 10 tasks with optimized output
        common_tasks = finder.find_common_tasks(
            min_languages=1, 
            available_languages=available_languages,
            verbose=False  # Concise output for normal workflow
        )
        
        if common_tasks:
            print(f"TOP 10 tasks with the most available languages:")
                
            # Display the top 10 tasks in a formatted list
            for i, task in enumerate(common_tasks[:10], 1):  # Show top 10
                lang_info = f"{task['language_count']} languages"
                print(f"  {i:2d}. {task['name']} - {lang_info}")
            
            # ===== STEP 4: SAVE ANALYSIS RESULTS =====
            # Save the results to a JSON file for later use by smart executor
            import json
            import os
            os.makedirs("results/task_analysis", exist_ok=True)
            
            # Structure the result data with metadata
            result_data = {
                "analysis_type": "top_10_available_languages",
                "total_tasks": len(common_tasks),
                "available_languages_count": len(available_languages),
                "all_languages_count": len(finder.supported_languages),
                "languages": list(finder.supported_languages),
                "available_languages": available_languages,
                "common_tasks": common_tasks,
                "min_languages_filter": 1,
                "search_strategy": "top_10_by_coverage"
            }
            
            output_file = "results/task_analysis/common_tasks.json"
            with open(output_file, 'w') as f:
                json.dump(result_data, f, indent=2)
            
            print(f"\n Results saved to: {output_file}")
            print(f" Next step: Run 'python main.py smart' to execute these {len(common_tasks[:10])} tasks")
                
        else:
            print("No common tasks found")
            
    except ImportError as e:
        print(f"Error importing analysis module: {e}")
        return False
    except Exception as e:
        print(f"Error: {e}")
        return False
    
    return True

def execute_codes():
    """Executes the code of common tasks found during analysis.
    
    This function:
    1. Loads the common tasks from the analysis results
    2. Executes each task across all available languages
    3. Records execution results and performance metrics
    
    Returns:
        bool: True if execution completed successfully, False otherwise
    """
    print("\nEXECUTING CODES")
    print("-" * 40)
    
    try:
        from src.smart_executor import SmartExecutor
        executor = SmartExecutor()
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"Error importing executor module: {e}")
        return False
    except Exception as e:
        print(f"Error: {e}")
        return False
    
    return True

def test_languages():
    """Tests the availability of all supported programming languages.
    
    This function:
    1. Attempts to compile/run a simple test program in each language
    2. Detects which compilers or interpreters are installed
    3. Generates a test results report
    4. Provides installation suggestions for missing languages
    
    The test results are saved to JSON for later use by the analyzer.
    
    Returns:
        bool: True if tests completed successfully, False otherwise
    """
    print("\nTEST LANGUAGES")
    print("-" * 40)
    
    try:
        # Import the language tester module
        from language_tester import LanguageTester
        tester = LanguageTester()
        # Run tests for all supported languages
        tester.test_all_languages()
        
        # ===== DISPLAY DETAILED TEST RESULTS =====
        print("\n" + "="*60)
        print(" DETAILED TEST RESULTS")
        print("="*60)
        
        # Separate working and non-working languages
        working = []
        not_working = []
        
        for language, result in tester.test_results.items():
            if result['available']:
                working.append(language)
            else:
                not_working.append(language)
        
        # ===== SECTION: WORKING LANGUAGES =====
        # Display all languages that passed the test
        if working:
            print(f"\n[OK] LANGUAGES WORKING ({len(working)}/{len(tester.test_results)}):")
            for lang in sorted(working):
                print(f"   [OK] {lang.upper()}")
        
        # ===== SECTION: NON-WORKING LANGUAGES =====
        # Display languages that failed and their error messages
        if not_working:
            print(f"\n[FAIL] LANGUAGES NOT WORKING ({len(not_working)}/{len(tester.test_results)}):")
            for lang in sorted(not_working):
                result = tester.test_results[lang]
                error_msg = result.get('error', 'Unknown error')[:60]
                print(f"   [FAIL] {lang.upper()}: {error_msg}")
        
        # ===== STATISTICS SECTION =====
        # Calculate and display overall statistics
        success_rate = len(working) / len(tester.test_results) * 100
        print(f"\n STATISTICS:")
        print(f"   - Total tested: {len(tester.test_results)}")
        print(f"   - Working: {len(working)}")
        print(f"   - Not working: {len(not_working)}")
        print(f"   - Success rate: {success_rate:.1f}%")
        
        # ===== INSTALLATION SUGGESTIONS =====
        # Provide installation commands for missing languages
        if not_working:
            print(f"\n SUGGESTIONS FOR INSTALLATION:")
            print(f"   Commands to install missing languages on Linux:")
            
            for lang in sorted(not_working):
                print(f"   - {lang.upper()}:", end=" ")
                
                # Provide platform-specific installation command for each language
                if lang == 'c':
                    print("sudo apt install gcc build-essential")
                elif lang == 'cpp':
                    print("sudo apt install g++ build-essential")
                elif lang == 'java':
                    print("sudo apt install openjdk-11-jdk")
                elif lang == 'javascript':
                    print("sudo apt install nodejs")
                elif lang == 'python':
                    print("sudo apt install python3 python3-pip")
                elif lang == 'go':
                    print("sudo apt install golang-go")
                elif lang == 'rust':
                    print("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
                elif lang == 'csharp':
                    print("sudo apt install mono-complete")
                elif lang == 'ruby':
                    print("sudo apt install ruby-full")
                elif lang == 'php':
                    print("sudo apt install php-cli")
                elif lang == 'perl':
                    print("sudo apt install perl")
                elif lang == 'lua':
                    print("sudo apt install lua5.3")
                elif lang == 'swift':
                    print("Installazione complessa - vedi swift.org/download")
                elif lang == 'kotlin':
                    print("sudo snap install kotlin --classic")
                elif lang == 'dart':
                    print("sudo apt install dart")
                elif lang == 'scala':
                    print("sudo apt install scala")
                elif lang == 'haskell':
                    print("sudo apt install ghc")
                elif lang == 'ocaml':
                    print("sudo apt install ocaml")
                elif lang == 'fortran':
                    print("sudo apt install gfortran")
                elif lang == 'r':
                    print("sudo apt install r-base")
                elif lang == 'julia':
                    print("wget https://julialang.org/downloads/ (manual installation)")
                else:
                    print(f"sudo apt install {lang} (verify package name)")
            
            # Display installation tips for other operating systems
            print(f"\n   Other OS:")
            print(f"   • macOS: use 'brew install <language>'")
            print(f"   • Windows: use Windows Package Manager 'winget install'")
            print(f"   • Arch Linux: use 'pacman -S <language>'")
            print(f"   • Fedora/RHEL: use 'dnf install <language>'")
            
            print(f"\n  Note: Some languages may require restarting the terminal")
        
        print("\n" + "="*60)
        
    except ImportError as e:
        print(f"Error importing module test: {e}")
        return False
    except Exception as e:
        print(f"Error during test: {e}")
        return False
    
    return True

def smart_execute():
    """Executes the TOP 10 common tasks only in tested and available languages.
    
    This is the recommended execution mode because:
    1. It only executes on languages that passed the test phase
    2. It focuses on the most common programming tasks
    3. Results are optimized for multi-language comparison
    4. Automatically handles language-specific timeouts
    
    Workflow: test -> analyze -> smart -> (optional) benchmark
    
    Returns:
        bool: True if execution completed successfully, False otherwise
    """
    print("\nADAPTIVE EXECUTION")
    print("-" * 40)
    
    # ===== VALIDATION: CHECK IF ANALYSIS HAS BEEN COMPLETED =====
    # Verify that analyze has been run first to generate common tasks
    import os
    from pathlib import Path
    common_tasks_file = Path("results/task_analysis/common_tasks.json")
    
    if not common_tasks_file.exists():
        print("  No task analysis found.")
        print("  REQUIRED ACTION: Run 'python main.py analyze' first to find common tasks.")
        print("  The 'smart' command executes the tasks found by 'analyze'.")
        print()
        print("  Workflow:")
        print("    1. python main.py test     # Detect available languages")
        print("    2. python main.py analyze  # Find TOP 10 common tasks")
        print("    3. python main.py smart    # Execute those tasks")
        return False
    
    # ===== EXECUTION PHASE =====
    try:
        from src.smart_executor import SmartExecutor
        executor = SmartExecutor()
        # Execute all common tasks that were found during analysis
        executor.execute_all_common_tasks()
    except ImportError as e:
        print(f"Error importing smart executor module: {e}")
        return False
    except Exception as e:
        print(f"Error during adaptive execution: {e}")
        return False
    
    return True

def benchmark_carbon(mode=None, auto_export_csv=True, auto_visualize=True, timeout=90):
    """Executes a carbon benchmark with multiple repetitions.
    
    Args:
        mode: Benchmark mode ('fast', 'top10', 'complete')
        auto_export_csv: Auto-export results to CSV
        auto_visualize: Auto-generate visualizations
        timeout: Timeout for task execution in seconds (default: 90)
    """
    print("\n CARBON BENCHMARK - CO2 Measurement System")
    print("=" * 55)
    
    try:
        from src.carbon_benchmark import CarbonBenchmark
        
        # Display available options
        print("Available benchmark modes:")
        print()
        print("  TOP10 - Main Task Analysis")
        print("   • Only the top 10 most frequent tasks")
        print("   • 30 repetitions per task to calculate the average")
        print("   • Estimated time: ~45-60 minutes")
        print("   • Ideal for regular analysis")
        print()
        print("  FAST - Functionality Test")
        print("   • Only 3 sample tasks")
        print("   • 3 repetitions per task")
        print("   • Estimated time: ~3-5 minutes")
        print("   • Perfect for verifying everything works")
        print()
        print("  COMPLETE - Exhaustive Analysis")
        print("   • ALL tasks in the dataset (1000+ tasks)")
        print("   • 3 repetitions per task")
        print("   • Estimated time: ~6-8 hours")
        print("   • Full coverage for scientific research")
        print()
        
        # Handle input
        if mode is None:
            print("Select mode:")
            choice = input("Enter [1/top10] [2/fast] [3/complete] (default: top10): ").strip().lower()
        else:
            choice = mode.lower()
            print(f"Mode selected via parameter: {choice}")
        
        # Configure mode
        if choice in ["2", "fast", "speed"]:
            iterations = 1
            max_tasks = 10
            mode_name = "FAST"
            description = f"Functionality test - {max_tasks} tasks, {iterations} repetitions"
        elif choice in ["3", "complete", "full", "all"]:
            iterations = 1
            max_tasks = None  # All available tasks
            mode_name = "COMPLETE"
            description = f"Exhaustive analysis - all tasks, {iterations} repetitions"
        else:
            # Default: top10
            iterations = 30
            max_tasks = 10
            mode_name = "TOP10"
            description = f"Main task analysis - {max_tasks} tasks, {iterations} repetitions"
        
        # Confirm configuration
        print(f"\n SELECTED MODE: {mode_name}")
        print(f" {description}")
        print(f" Configuration: {iterations} iterations per task")
        print(f" Tasks to test: {'ALL available' if max_tasks is None else f'Top {max_tasks} tasks'}")
        
        # User confirmation
        if mode is None:  # Only if interactive
            print()
            try:
                confirm = input("Proceed with the benchmark? [y/N]: ").strip().lower()
                if confirm not in ['y', 'yes']:
                    print("❌ Benchmark canceled by user")
                    return False
            except EOFError:
                # Non-interactive input, proceed automatically
                print(" Non-interactive mode detected, proceeding automatically...")
                pass
        
        print(f"\n✅ Starting benchmark in {mode_name} mode...")
        print(" Use Ctrl+C to interrupt at any time")
        
        # Start benchmark with mode parameter and export/viz options
        benchmark = CarbonBenchmark(
            iterations=iterations, 
            mode=mode_name,
            auto_export_csv=auto_export_csv,
            auto_visualize=auto_visualize,
            timeout=timeout
        )
        
        # Select benchmark method based on mode
        if max_tasks is None:  # COMPLETE mode
            print(" COMPLETE mode: using ALL tasks in the dataset")
            benchmark.benchmark_all_tasks()
        else:  # TOP10 or FAST mode
            print(f" {mode_name} mode: using common tasks")
            benchmark.benchmark_common_tasks(max_tasks=max_tasks)
        
    except ImportError as e:
        print(f"Error importing benchmark module: {e}")
        return False
    except KeyboardInterrupt:
        print("\nBenchmark interrupted by user")
        return False
    except Exception as e:
        print(f"Error during benchmark: {e}")
        return False
    
    return True

def clean_project(args=None):
    """Enhanced cleanup with options from new cleaner.py"""
    print("\n PROJECT CLEANUP")
    print("-" * 40)
    
    try:
        from src.cleaner import (
            cleanup_carbon_sessions,
            cleanup_temp_files,
            show_session_statistics,
            get_directory_stats,
            deep_clean
        )
        
        # Parse options
        dry_run = True if args and hasattr(args, 'dry_run') and args.dry_run else not (args and hasattr(args, 'execute') and args.execute)
        days = args.days if args and hasattr(args, 'days') else 2
        
        # Show statistics mode
        if args and hasattr(args, 'stats') and args.stats:
            show_session_statistics()
            get_directory_stats()
            return True
        
        # Deep clean mode (comprehensive)
        if args and hasattr(args, 'deep') and args.deep:
            deep_clean(days_to_keep=days, keep_benchmarks=3, dry_run=dry_run)
            return True
        
        # Default: carbon sessions + temp files
        cleanup_carbon_sessions(days_to_keep=days, dry_run=dry_run)
        cleanup_temp_files(dry_run=dry_run)
        
        print("\n✅ Cleanup completed!")
        
        if dry_run:
            print("\n To actually delete files, use: python main.py clean --execute")
        
        return True
        
    except Exception as e:
        print(f"❌ Error during cleanup: {e}")
        import traceback
        traceback.print_exc()
        return False


def show_status():
    """Displays the project status"""
    print("\nPROJECT STATUS")
    print("-" * 40)
    
    # Check main directories
    directories = [
        "data/generated/code_snippets",
        "results/task_analysis", 
        "results/execution",
        "modules",
        "src"
    ]
    
    print("Directories:")
    for directory in directories:
        path = Path(directory)
        if path.exists():
            if path.is_dir():
                file_count = len(list(path.rglob("*")))
                print(f"  {directory} ({file_count} files)")
            else:
                print(f"   {directory} (not a directory)")
        else:
            print(f"  {directory} (not found)")
    
    # Check analysis files
    analysis_files = [
        "results/task_analysis/common_tasks.json",
        "results/task_analysis/dependency_analysis.json"
    ]
    
    print("\nAnalysis Files:")
    for file_path in analysis_files:
        path = Path(file_path)
        if path.exists():
            size = path.stat().st_size
            print(f"  {file_path} ({size} bytes)")
        else:
            print(f"  {file_path} (not found)")
    
    # Count available tasks
    task_count = 0
    common_tasks_file = Path("results/task_analysis/common_tasks.json")
    if common_tasks_file.exists():
        try:
            import json
            with open(common_tasks_file, 'r') as f:
                data = json.load(f)
                task_count = data.get('total_tasks', 0)
        except:
            pass
    
    print(f"\nAnalyzed Tasks: {task_count} (run 'analyze' first)")
    
    # Check execution results
    exec_dir = Path("results/execution")
    if exec_dir.exists():
        exec_files = list(exec_dir.glob("execution_results_*.json"))
        print(f"\nCompleted Executions: {len(exec_files)}")
        if exec_files:
            latest = max(exec_files, key=lambda x: x.stat().st_mtime)
            print(f"  Latest execution: {latest.name}")
    else:
        print("\nCompleted Executions: 0 (run 'execute' first)")

def rankings_analysis(mode='COMPLETE', no_save=False, top=10):
    """Generate multi-tier rankings from benchmark results"""
    print("\nMULTI-TIER RANKINGS ANALYSIS")
    print("-" * 40)
    
    try:
        from src.ranking_analyzer import RankingAnalyzer
        
        # Load latest benchmark results for the specified mode
        benchmark_dir = Path(f"results/carbon_benchmark/{mode.lower()}")
        if not benchmark_dir.exists():
            print(f"❌ No benchmark results found for mode '{mode}'")
            print(f"   Expected directory: {benchmark_dir}")
            print("\nRun a benchmark first with:")
            print(f"  python main.py benchmark --mode {mode}")
            return False
        
        json_files = list(benchmark_dir.glob("carbon_benchmark_detailed_*.json"))
        if not json_files:
            print(f"❌ No detailed benchmark JSON files found in {benchmark_dir}")
            return False
        
        latest_file = max(json_files, key=lambda x: x.stat().st_mtime)
        print(f"📊 Loading benchmark results from: {latest_file.name}")
        
        import json
        with open(latest_file, 'r', encoding='utf-8') as f:
            benchmark_results = json.load(f)
        
        # Create analyzer and generate rankings
        analyzer = RankingAnalyzer(benchmark_results)
        
        # Generate all rankings
        category_rankings, overall_rankings = analyzer.generate_all_rankings(
            save=not no_save
        )
        
        if category_rankings and overall_rankings:
            print("\n✅ Rankings generated successfully!")
            if not no_save:
                print(f"   Rankings saved to: results/rankings/{mode.lower()}/")
            return True
        else:
            print("\n❌ Failed to generate rankings")
            return False
            
    except ImportError as e:
        print(f"❌ Error importing ranking analyzer: {e}")
        return False
    except Exception as e:
        print(f"❌ Error during ranking analysis: {e}")
        import traceback
        traceback.print_exc()
        return False

def quality_analysis():
    """Performs advanced qualitative analysis of the dataset"""
    print("\nADVANCED QUALITATIVE ANALYSIS")
    print("-" * 40)
    
    try:
        from src.finder import UnifiedTaskFinder
        finder = UnifiedTaskFinder()
        
        print(" Creating dataset with qualitative analysis...")
        
        # Create DataFrame with quality analysis (concise output)
        finder.create_dataset_dataframe(include_quality_analysis=True, verbose=False)
        
        if finder.df is None or len(finder.df) == 0:
            print("❌ No data found for analysis")
            return False
        
        # Analyze high-quality tasks
        print("\n HIGH-QUALITY TASKS")
        quality_tasks = finder.find_common_tasks(
            min_languages=5,
            include_quality=True,
            min_quality_score=70,
            verbose=False
        )
        
        if quality_tasks:
            print(f"The top {min(5, len(quality_tasks))} tasks by quality:")
            for i, task in enumerate(quality_tasks[:5], 1):
                quality_score = task.get('avg_quality_score', 0)
                print(f"  {i}. {task['name']} - {quality_score:.1f}/100 ({task['language_count']} languages)")
        else:
            print("  No high-quality tasks found")
        
        # General statistics (verbose for details)
        print("\n QUALITATIVE STATISTICS")
        stats = finder.get_quality_statistics(verbose=True)
        
        # Suggestions for improvements
        if stats:
            features = stats['feature_coverage']
            print("\n SUGGESTIONS FOR IMPROVEMENT:")
            
            if features['has_error_handling_pct'] < 10:
                print(f"  Very low error handling ({features['has_error_handling_pct']:.1f}%) - Consider more robust tasks")
            
            if features['has_comments_pct'] < 30:
                print(f"  Poor documentation ({features['has_comments_pct']:.1f}%) - Improve comments")
            
            if features['has_functions_pct'] < 50:
                print(f"  Poorly structured code ({features['has_functions_pct']:.1f}%) - Add more modular functions")
        
        print("\n✅ Qualitative analysis completed!")
        
    except ImportError as e:
        print(f"Error importing finder module: {e}")
        return False
    except Exception as e:
        print(f"Error during qualitative analysis: {e}")
        return False
    
    return True

def main():
    """Main function"""
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument(
        'command', 
        nargs='?',
        choices=['analyze', 'execute', 'smart', 'test', 'clean', 'status', 'carbon', 'benchmark', 'quality', 'find', 'rankings', 'help'],
        default='help',
        help='Command to execute'
    )
    
    parser.add_argument(
        '--mode',
        choices=['fast', 'top10', 'complete'],
        help='Mode for the benchmark command (fast/top10/complete)'
    )
    
    parser.add_argument(
        '--no-csv',
        action='store_true',
        help='Disable automatic CSV export after benchmark'
    )
    
    parser.add_argument(
        '--no-viz',
        action='store_true',
        help='Disable automatic visualization generation after benchmark'
    )
    
    parser.add_argument(
        '--timeout',
        type=int,
        default=90,
        help='Timeout for task execution in seconds (default: 90)'
    )
    
    parser.add_argument(
        '--task',
        help='Name of the task to search for (for the find command)'
    )
    
    # Clean command options
    parser.add_argument(
        '--execute',
        action='store_true',
        help='Actually delete files (default is dry-run)'
    )
    
    parser.add_argument(
        '--days',
        type=int,
        default=2,
        help='Number of days to keep for session files (default: 2)'
    )
    
    parser.add_argument(
        '--stats',
        action='store_true',
        help='Show statistics about files without deleting'
    )
    
    parser.add_argument(
        '--top',
        type=int,
        default=10,
        help='Number of top languages to show in rankings (default: 10)'
    )
    
    parser.add_argument(
        '--no-save',
        action='store_true',
        help='Disable saving rankings to JSON files (only print to console)'
    )
    
    parser.add_argument(
        '--deep',
        action='store_true',
        help='Deep clean: sessions + temp files + old benchmarks'
    )
    
    args = parser.parse_args()
    
    print_banner()
    
    if args.command == 'help':
        print_help()
    elif args.command == 'analyze':
        success = analyze_tasks()
        if success:
            print("\nAnalysis completed successfully!")
            print("You can now run 'python main.py execute' to test the codes")
        else:
            print("\nAnalysis failed")
            sys.exit(1)
    elif args.command == 'execute':
        success = execute_codes()
        if success:
            print("\nExecution completed!")
            print("Check the results in results/execution/")
        else:
            print("\nExecution failed")
            sys.exit(1)
    elif args.command == 'smart':
        success = smart_execute()
        if success:
            print("\nAdaptive execution completed!")
            print("Check the results in results/execution/")
        else:
            print("\nAdaptive execution failed")
            sys.exit(1)
    elif args.command == 'find':
        # Command for searching and targeted execution of specific tasks
        try:
            from src.task_searcher import search_and_execute_task
            success = search_and_execute_task(args.task)
            if not success:
                print("\nTask search failed or canceled")
        except ImportError:
            print("Task Searcher not available")
            print("Ensure all modules are correctly installed")
    elif args.command == 'test':
        success = test_languages()
        if success:
            print("\nLanguage test completed!")
            print("Check the results to see which languages are available")
        else:
            print("\nLanguage test failed")
            sys.exit(1)
    elif args.command == 'clean':
        success = clean_project(args)
        if success:
            print("\n✅ Cleanup completed!")
        else:
            print("\nCleanup failed")
            sys.exit(1)
    elif args.command == 'status':
        show_status()
    elif args.command == 'carbon':
        # Command to display the environmental impact report
        try:
            from src.carbon_tracker import print_carbon_report
            print_carbon_report()
        except ImportError:
            print("Carbon tracker not available")
            print("Install CodeCarbon with: pip install codecarbon")
    elif args.command == 'benchmark':
        # Command to perform CO2 benchmark with multiple repetitions
        success = benchmark_carbon(
            mode=args.mode,
            auto_export_csv=not args.no_csv,
            auto_visualize=not args.no_viz,
            timeout=args.timeout
        )
        if success:
            print("\nCO2 benchmark completed!")
            print("Check the results in results/carbon_benchmark/")
        else:
            print("\nCO2 benchmark failed")
            sys.exit(1)
    elif args.command == 'quality':
        # Command for advanced qualitative analysis
        success = quality_analysis()
        if success:
            print("\nQualitative analysis completed!")
            print("The results show the code quality in the dataset")
        else:
            print("\nQualitative analysis failed")
            sys.exit(1)
    elif args.command == 'rankings':
        # Command for multi-tier rankings analysis
        success = rankings_analysis(
            mode=args.mode,
            no_save=args.no_save,
            top=getattr(args, 'top', 10)
        )
        if success:
            print("\nRankings analysis completed!")
            print("The results show language performance rankings by category")
        else:
            print("\nRankings analysis failed")
            sys.exit(1)
    else:
        print(f"Unrecognized command: {args.command}")
        print("Use 'python main.py help' to see the available commands")

if __name__ == "__main__":
    main()
