#!/usr/bin/env python3
"""
Task Searcher - Targeted search and execution system for specific tasks
Allows you to search for tasks by name, display available languages and measure CO2
"""

import os
import sys
import json
import glob
import re
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from collections import defaultdict

# Adds the path to import CLAP modules
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

try:
    from carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
    from smart_executor import SmartExecutor
    DEPENDENCIES_AVAILABLE = True
except ImportError:
    DEPENDENCIES_AVAILABLE = False
    print(" Some dependencies not available - limited functionality")


class TaskSearcher:
    """
    Single Responsibility: Targeted search and execution of specific tasks
    Allows you to search for tasks by name, display available languages and measure CO2
    """
    
    def __init__(self):
        self.code_base_path = "data/generated/code_snippets"
        self.results_dir = "results/task_search"

        # Load tested and available languages
        self.available_languages = self._load_tested_languages()

        # Initialize components if available
        if DEPENDENCIES_AVAILABLE:
            self.executor = SmartExecutor()
        else:
            self.executor = None

        # Create results directory
        os.makedirs(self.results_dir, exist_ok=True)

        # Mapping common languages for search (standardized names)
        all_language_mapping = {
            'python': ['python'],
            'java': ['java'],
            'javascript': ['javascript'],
            'c': ['c'],
            'cpp': ['cpp'],                    # C++ standardized to 'cpp'
            'c++': ['cpp'],
            'go': ['go'],
            'rust': ['rust'],
            'ruby': ['ruby'],
            'php': ['php'],
            'csharp': ['csharp'],             # C# standardized to 'csharp'
            'c#': ['csharp'],
            'haskell': ['haskell'],
            'ocaml': ['ocaml'],
            'r': ['r'],
            'julia': ['julia'],
            'matlab': ['matlab'],
            'typescript': ['typescript']
        }

        # Filter only available languages
        self.language_mapping = {k: v for k, v in all_language_mapping.items()
                               if k.lower() in [lang.lower() for lang in self.available_languages]}
    
    def _load_tested_languages(self) -> List[str]:
        """Load the list of tested and available languages"""
        import json
        from pathlib import Path
        
        test_results_dir = Path("results/execution")
        available_languages = []
        
        if test_results_dir.exists():
            # Look for the most recent test file
            test_files = list(test_results_dir.glob("language_test_results_*.json"))
            if test_files:
                latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
                try:
                    with open(latest_test, 'r') as f:
                        test_data = json.load(f)

                    # Load silently (detailed output is already shown by SmartExecutor)

                    # Map test names to dataset names
                    test_to_dataset_mapping = {
                        'cpp': 'cpp',             # C++ standardized to 'cpp'
                        'c++': 'cpp',             # C++ alternative
                        'cplusplus': 'cpp',       # C++ full form
                        'java': 'java',
                        'csharp': 'csharp',       # C# standardized to 'csharp'
                        'c#': 'csharp',           # C# direct symbol
                        'cs': 'csharp',           # C# abbreviation
                        'python': 'python',
                        'python3': 'python',      # Python 3 specific
                        'py': 'python',           # Python abbreviation
                        'ruby': 'ruby',
                        'rb': 'ruby',             # Ruby abbreviation
                        'javascript': 'javascript',
                        'js': 'javascript',       # JavaScript abbreviation
                        'node': 'javascript',     # Node.js
                        'nodejs': 'javascript',   # Node.js full form
                        'typescript': 'typescript',
                        'ts': 'typescript',       # TypeScript abbreviation
                        'c': 'c',
                        'go': 'go',
                        'golang': 'go',           # Go full form
                        'rust': 'rust',
                        'rs': 'rust',             # Rust abbreviation
                        'php': 'php',
                        'haskell': 'haskell',
                        'hs': 'haskell',          # Haskell abbreviation
                        'ocaml': 'ocaml',
                        'ml': 'ocaml',            # OCaml/ML abbreviation
                        'r': 'r',
                        'rlang': 'r',             # R full form
                        'julia': 'julia',
                        'jl': 'julia',            # Julia abbreviation
                        'matlab': 'matlab',
                        'octave': 'matlab'        # MATLAB/Octave compatibility
                    }
                    
                    # Only load languages that have passed the test
                    for lang, result in test_data['results'].items():
                        if result['available'] and lang in test_to_dataset_mapping:
                            dataset_lang = test_to_dataset_mapping[lang]
                            available_languages.append(dataset_lang)
                    
                    if available_languages:
                        # Reduced output - details already shown by SmartExecutor
                        return available_languages
                        
                except Exception as e:
                    print(f" Error loading test results: {e}")

        # Fallback: use all main languages if no test results are found
        print(" No test results found, using fallback main languages")
        fallback_languages = ['python', 'java', 'javascript', 'c', 'cplusplus', 'go', 'rust']
        print(f"Fallback languages: {', '.join(fallback_languages)}")
        return fallback_languages
    
    def normalize_task_name(self, task_name: str) -> str:
        """Normalizes the task name for search"""
        # Removes special characters and converts to lowercase
        normalized = re.sub(r'[^a-zA-Z0-9\s]', '', task_name.lower())
        # Replaces spaces with underscores
        normalized = re.sub(r'\s+', '_', normalized.strip())
        return normalized
    
    def _get_file_extensions(self, language: str) -> List[str]:
        """Gets the supported file extensions for a language"""
        # Maps languages to their extensions (standardized names)
        extension_mapping = {
            'python': ['.py', '.txt'],
            'java': ['.java', '.txt'],
            'javascript': ['.js', '.txt'],
            'typescript': ['.ts', '.txt'],
            'c': ['.c', '.txt'],
            'cpp': ['.cpp', '.cxx', '.cc', '.c++', '.txt'],  # C++ standardized to 'cpp'
            'csharp': ['.cs', '.txt'],                       # C# standardized to 'csharp'
            'go': ['.go', '.txt'],
            'rust': ['.rs', '.txt'],
            'ruby': ['.rb', '.txt'],
            'php': ['.php', '.txt'],
            'haskell': ['.hs', '.lhs', '.txt'],  # .lhs for Literate Haskell
            'ocaml': ['.ml', '.mli', '.txt'],   # .mli for interface files
            'julia': ['.jl', '.txt'],
            'r': ['.r', '.R', '.txt']
        }
        
        return extension_mapping.get(language.lower(), ['.txt'])  # Default to .txt
    
    def search_tasks(self, task_name: str, fuzzy: bool = True) -> Dict[str, List[str]]:
        """
        Searches for tasks by name in the dataset (only in tested and available languages)

        Args:
            task_name: Name of the task to search for
            fuzzy: If True, use fuzzy search (containment), otherwise exact match

        Returns:
            Dict with languages as keys and list of files as values
        """
        print(f"\nSearching for task: '{task_name}' (only tested languages)")
        print("=" * 50)
        
        normalized_name = self.normalize_task_name(task_name)
        found_tasks = defaultdict(list)

        # Filters only available languages
        if not self.available_languages:
            print(" No available languages - please run 'python main.py test' first")
            return dict(found_tasks)

        print(f"Searching in {len(self.available_languages)} tested languages...")

        # Searches in all language directories
        for category_dir in glob.glob(os.path.join(self.code_base_path, "*")):
            if not os.path.isdir(category_dir):
                continue

            # Searches in each language within the category
            for lang_dir in glob.glob(os.path.join(category_dir, "*")):
                if not os.path.isdir(lang_dir):
                    continue
                    
                lang_name = os.path.basename(lang_dir).lower()

                # FILTER: Consider only tested and available languages
                if lang_name not in [lang.lower() for lang in self.available_languages]:
                    continue

                # Searches for files matching the task name
                # Supports multiple extensions based on the language
                extensions = self._get_file_extensions(lang_name)
                
                for ext in extensions:
                    pattern = os.path.join(lang_dir, f"*{ext}")
                    for file_path in glob.glob(pattern):
                        file_name = os.path.basename(file_path)
                        file_name_normalized = self.normalize_task_name(file_name)

                        # Check for match (exact or fuzzy)
                        if fuzzy:
                            if normalized_name in file_name_normalized:
                                found_tasks[lang_name].append(file_path)
                        else:
                            # Removes the extension for exact match
                            name_without_ext = file_name_normalized
                            for extension in extensions:
                                name_without_ext = name_without_ext.replace(extension.lower(), '')
                            if normalized_name == name_without_ext:
                                found_tasks[lang_name].append(file_path)
        
        return dict(found_tasks)
    
    def display_search_results(self, found_tasks: Dict[str, List[str]], task_name: str):
        """Displays the search results in a user-friendly format"""
        if not found_tasks:
            print(f"No tasks found for '{task_name}' in tested languages")
            print("\n Suggestions:")
            print("   • Try a more generic search (e.g., 'sort' instead of 'quicksort')")
            print("   • Use keywords in English")
            print("   • Verify that the languages have been tested with 'python main.py test'")
            return
        
        total_tasks = sum(len(files) for files in found_tasks.values())
        print(f"Found {total_tasks} tasks in {len(found_tasks)} tested languages:")
        print(f"Available in {len(self.available_languages)} languages from the test")
        print()
        
        # Group by language
        for lang, files in sorted(found_tasks.items()):
            print(f"{lang.upper()} ({len(files)} files):")
            for file_path in files[:3]:  # Show max 3 files per language
                file_name = os.path.basename(file_path)
                print(f"   • {file_name}")
            if len(files) > 3:
                print(f"   • ... and {len(files) - 3} more files")
            print()
    
    def select_task_for_execution(self, found_tasks: Dict[str, List[str]]) -> Optional[Tuple[str, str]]:
        """
        Allows the user to select a specific task for execution
        
        Returns:
            Tuple (language, file_path) or None if canceled
        """
        if not found_tasks:
            return None

        print("Select task to execute:")
        print()

        # Create sorted list of options
        options = []
        for lang, files in sorted(found_tasks.items()):
            for file_path in files:
                file_name = os.path.basename(file_path)
                options.append((lang, file_path, file_name))

        # Show options
        for i, (lang, file_path, file_name) in enumerate(options, 1):
            print(f"  {i}. [{lang.upper()}] {file_name}")

        print(f"  0. Cancel operation")
        print()

        # User input
        try:
            choice = input(f"Select option (1-{len(options)}) [0]: ").strip()
            if choice == '' or choice == '0':
                return None
            
            choice_num = int(choice)
            if 1 <= choice_num <= len(options):
                lang, file_path, _ = options[choice_num - 1]
                return lang, file_path
            else:
                print("Invalid choice")
                return None
                
        except (ValueError, KeyboardInterrupt):
            print("Operation canceled")
            return None
    
    def execute_task_with_carbon_tracking(self, language: str, file_path: str) -> bool:
        """
        Executes a specific task with CO2 monitoring
        
        Args:
            language: Name of the programming language
            file_path: Path to the task file

        Returns:
            True if the execution was successful, False otherwise
        """
        if not DEPENDENCIES_AVAILABLE or not self.executor:
            print("Dependencies not available for execution")
            return False
        
        task_name = os.path.basename(file_path)
        print(f"\nExecuting task: '{task_name}' in {language.upper()}")
        print("=" * 50)
        
        try:
            # Read the code
            with open(file_path, 'r', encoding='utf-8') as f:
                code = f.read()

            print(f"File: {os.path.basename(file_path)}")
            print(f"Language: {language}")
            print(f"Size: {len(code)} characters")

            # Qualitative analysis of the code before execution
            print("\nQUALITATIVE ANALYSIS")
            print("-" * 30)
            quality_analysis = self._analyze_code_quality(code, language)
            self._display_quality_results(quality_analysis)
            print()

            # Start CO2 tracking
            if CODECARBON_AVAILABLE:
                carbon_session = start_carbon_tracking(task_name, language)
                print("CO2 monitoring started")
            else:
                carbon_session = None
                print("CO2 monitoring not available")

            print("Execution in progress...")
            print("-" * 40)

            # Execute the code
            result = self.executor.execute_code(
                code=code,
                language=language,
                task_name=task_name
            )
            
            print("-" * 40)

            # Stop CO2 tracking
            emissions = None
            if carbon_session and CODECARBON_AVAILABLE:
                emissions = stop_carbon_tracking()
                if emissions:
                    print(f"CO2 emissions: {emissions:.6f} kg ({emissions * 1000:.3f} g)")
                else:
                    print("CO2 data not available")

            # Save results
            self._save_execution_results(task_name, language, file_path, result.get('success', False), emissions)
            
            if result.get('success', False):
                print("Execution completed successfully")
                if result.get('output'):
                    print(f"Output: {result['output'][:100]}...")
            else:
                print("Execution failed")
                if result.get('error'):
                    print(f"Error: {result['error'][:100]}...")

            return result.get('success', False)
            
        except Exception as e:
            print(f"Error during execution: {e}")
            return False
    
    def _save_execution_results(self, task_name: str, language: str, file_path: str, 
                              success: bool, emissions: Optional[float]):
        """Save execution results"""
        try:
            from datetime import datetime
            
            result = {
                'task_name': task_name,
                'language': language,
                'file_path': file_path,
                'success': success,
                'co2_emissions_kg': emissions,
                'timestamp': datetime.now().isoformat(),
                'execution_type': 'task_search'
            }

            # Save to JSON file
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            result_file = os.path.join(self.results_dir, f"task_execution_{timestamp}.json")
            
            with open(result_file, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2, ensure_ascii=False)

            print(f" Results saved to: {result_file}")

        except Exception as e:
            print(f" Error saving results: {e}")

    def interactive_task_search(self, initial_query: Optional[str] = None):
        """
        Interactive interface for searching and executing tasks
        
        Args:
            initial_query: Optional initial query
        """
        print("\nTASK SEARCHER - Targeted Search and Execution")
        print("=" * 50)
        print(f"Limited to {len(self.available_languages)} tested and available languages")
        print()

        # Initial query
        if initial_query:
            query = initial_query
            print(f"Initial query: '{query}'")
        else:
            query = input(" Enter task name to search: ").strip()
        
        if not query:
            print("Empty query, operation cancelled")
            return False

        # Search tasks (only in tested languages)
        found_tasks = self.search_tasks(query, fuzzy=True)
        self.display_search_results(found_tasks, query)
        
        if not found_tasks:
            return False

        # Selection for execution
        print("Do you want to execute one of these tasks?")
        execute_choice = input("Confirm [y/N]: ").strip().lower()

        if execute_choice not in ['y', 'yes']:
            print("Operation cancelled")
            print("\nTask search and execution completed!")
            print("Check the results in results/task_search/")
            print("=" * 60)
            return True

        # Specific task selection
        selection = self.select_task_for_execution(found_tasks)
        if not selection:
            print("No task selected")
            print("\nTask search and execution completed!")
            print("Check the results in results/task_search/")
            print("=" * 60)
            return True
        
        language, file_path = selection
        
        # Execution with CO2 tracking
        success = self.execute_task_with_carbon_tracking(language, file_path)

        print("\nTask search and execution completed!")
        print("Check the results in results/task_search/")
        print("=" * 60)
        
        return success
    
    def _analyze_code_quality(self, code: str, language: str) -> Dict:
        """
        Analyze code quality
        
        Args:
            code: Source code to analyze
            language: Programming language

        Returns:
            Dictionary with quality metrics
        """
        quality_patterns = {
            'comments': [
                r'//.*',           # C, C++, Java, JavaScript, Go, Rust
                r'/\*.*?\*/',      # C, C++, Java, JavaScript
                r'#.*',            # Python, Ruby, Perl, Bash
                r'<!--.*?-->',     # HTML, XML
                r'--.*',           # SQL, Haskell
                r';.*',            # Scheme, Lisp
                r'\(\*.*?\*\)'     # OCaml, F#
            ],
            'functions': [
                r'def\s+\w+',      # Python
                r'function\s+\w+', # JavaScript
                r'func\s+\w+',     # Go
                r'fn\s+\w+',       # Rust
                r'\w+\s+\w+\s*\([^)]*\)\s*{',  # C, C++, Java
                r'sub\s+\w+',      # Perl
                r'class\s+\w+',    # OOP languages
                r'module\s+\w+'    # F#, Haskell
            ],
            'error_handling': [
                r'try\s*{',        # Java, C#
                r'try:',           # Python
                r'catch',          # Java, C#, JavaScript
                r'except',         # Python
                r'finally',        # Java, Python
                r'panic',          # Go, Rust
                r'Result<',        # Rust
                r'Option<',        # Rust
                r'if.*error',      # Go
                r'rescue',         # Ruby
                r'ensure'          # Ruby
            ],
            'imports': [
                r'import\s+',      # Python, Java, JavaScript
                r'#include\s*<',   # C, C++
                r'using\s+',       # C#
                r'require\s+',     # Ruby, JavaScript
                r'use\s+',         # Rust
                r'from\s+\w+\s+import', # Python
                r'package\s+',     # Go, Java
                r'extern\s+crate'  # Rust
            ]
        }
        
        quality_metrics = {
            'has_comments': False,
            'has_functions': False,
            'has_error_handling': False,
            'has_imports': False,
            'code_length': len(code),
            'line_count': len(code.split('\n')),
            'non_empty_lines': len([line for line in code.split('\n') if line.strip()])
        }
        
        # Quality pattern verification
        import re
        for pattern_type, patterns in quality_patterns.items():
            metric_name = f"has_{pattern_type}"
            
            for pattern in patterns:
                if re.search(pattern, code, re.MULTILINE | re.DOTALL):
                    quality_metrics[metric_name] = True
                    break

        # Calculate quality score (0-100)
        score = 0
        if quality_metrics['has_comments']: score += 25     # Documentation
        if quality_metrics['has_functions']: score += 30    # Modular structure
        if quality_metrics['has_error_handling']: score += 25  # Robustness
        if quality_metrics['has_imports']: score += 10     # Library usage

        # Bonus for well-structured code
        if quality_metrics['non_empty_lines'] > 10: score += 10  # Non-trivial

        quality_metrics['quality_score'] = min(score, 100)
        quality_metrics['language'] = language
        
        return quality_metrics
    
    def _display_quality_results(self, quality_analysis: Dict):
        """
        Show the results of the quality analysis
        
        Args:
            quality_analysis: Results of the quality analysis
        """
        score = quality_analysis.get('quality_score', 0)

        # Text for the score
        if score >= 80:
            score_text = "EXCELLENT"
        elif score >= 60:
            score_text = "GOOD"
        elif score >= 40:
            score_text = "FAIR"
        else:
            score_text = "NEEDS IMPROVEMENT"

        print(f"Quality Score: {score}/100 ({score_text})")
        print(f"Lines: {quality_analysis['line_count']} (effective: {quality_analysis['non_empty_lines']})")

        # Feature details
        features = []
        if quality_analysis.get('has_comments'): features.append("Comments")
        if quality_analysis.get('has_functions'): features.append("Functions")
        if quality_analysis.get('has_error_handling'): features.append("Error handling")
        if quality_analysis.get('has_imports'): features.append("Import/Include")
        
        if features:
            print(f"Features: {', '.join(features)}")
        else:
            print("Basic features: none detected")


def search_and_execute_task(task_name: Optional[str] = None) -> bool:
    """
    Main function for searching and executing tasks

    Args:
        task_name: Name of the task to search for (optional, if None asks for input)

    Returns:
        True if the operation was successful, False otherwise
    """
    try:
        searcher = TaskSearcher()
        return searcher.interactive_task_search(task_name)
    except KeyboardInterrupt:
        print("\n Operation interrupted by user")
        return False
    except Exception as e:
        print(f"Error in task searcher: {e}")
        return False


if __name__ == "__main__":
    # Test the module
    if len(sys.argv) > 1:
        task_name = " ".join(sys.argv[1:])
        search_and_execute_task(task_name)
    else:
        search_and_execute_task()