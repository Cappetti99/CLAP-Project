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
from dataclasses import dataclass
from datetime import datetime

# Adds the path to import CLAP modules
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

try:
    from carbon_tracker import start_carbon_tracking, stop_carbon_tracking, CODECARBON_AVAILABLE
    from smart_executor import SmartExecutor
    from modules.language_config import LanguageConfigManager
    DEPENDENCIES_AVAILABLE = True
except ImportError:
    DEPENDENCIES_AVAILABLE = False
    print("WARNING: Some dependencies not available - limited functionality")


# Constants
MAX_FILES_DISPLAY = 3
QUALITY_WEIGHTS = {
    'comments': 25,
    'functions': 30,
    'error_handling': 25,
    'imports': 10,
    'non_trivial': 10
}
MIN_NON_TRIVIAL_LINES = 10


@dataclass
class QualityMetrics:
    """Code quality metrics"""
    has_comments: bool = False
    has_functions: bool = False
    has_error_handling: bool = False
    has_imports: bool = False
    code_length: int = 0
    line_count: int = 0
    non_empty_lines: int = 0
    language: str = ""
    
    @property
    def quality_score(self) -> int:
        """Calculate quality score (0-100)"""
        score = 0
        if self.has_comments:
            score += QUALITY_WEIGHTS['comments']
        if self.has_functions:
            score += QUALITY_WEIGHTS['functions']
        if self.has_error_handling:
            score += QUALITY_WEIGHTS['error_handling']
        if self.has_imports:
            score += QUALITY_WEIGHTS['imports']
        if self.non_empty_lines > MIN_NON_TRIVIAL_LINES:
            score += QUALITY_WEIGHTS['non_trivial']
        return min(score, 100)
    
    @property
    def score_label(self) -> str:
        """Get quality label based on score"""
        score = self.quality_score
        if score >= 80:
            return "EXCELLENT"
        elif score >= 60:
            return "GOOD"
        elif score >= 40:
            return "FAIR"
        return "NEEDS IMPROVEMENT"


class LanguageMapper:
    """Handles language name normalization and mapping"""
    
    # Single source of truth for language mappings
    LANGUAGE_STANDARDS = {
        'c': ['c'],
        'cpp': ['cpp', 'c++', 'cplusplus', 'c plus plus', 'cc', 'cxx'],
        'csharp': ['csharp', 'c#', 'c sharp', 'cs'],
        'go': ['go', 'golang'],
        'haskell': ['haskell', 'hs'],
        'java': ['java'],
        'javascript': ['javascript', 'js', 'ecmascript', 'node', 'nodejs'],
        'julia': ['julia', 'jl'],
        'matlab': ['matlab', 'm', 'octave'],
        'ocaml': ['ocaml', 'ml', 'mli', 'objective caml', 'o caml'],
        'php': ['php'],
        'python': ['python', 'python3', 'py'],
        'r': ['r', 'rstats', 'rlang'],
        'ruby': ['ruby', 'rb'],
        'rust': ['rust', 'rs'],
        'typescript': ['typescript', 'ts']
    }
    
    def __init__(self):
        # Build reverse mapping for O(1) lookups
        self._variant_to_standard = {}
        for standard, variants in self.LANGUAGE_STANDARDS.items():
            for variant in variants:
                self._variant_to_standard[variant.lower()] = standard
    
    def normalize(self, lang_name: str) -> str:
        """Normalize language name to standard form"""
        return self._variant_to_standard.get(lang_name.lower(), lang_name.lower())
    
    def get_variants(self, standard_name: str) -> List[str]:
        """Get all variants for a standard language name"""
        return self.LANGUAGE_STANDARDS.get(standard_name.lower(), [standard_name])


class CodeQualityAnalyzer:
    """Analyzes code quality using pattern matching"""
    
    # Compiled regex patterns (initialized once)
    QUALITY_PATTERNS = {
        'comments': [
            re.compile(r'//.*'),
            re.compile(r'/\*.*?\*/', re.DOTALL),
            re.compile(r'#.*'),
            re.compile(r'<!--.*?-->', re.DOTALL),
            re.compile(r'--.*'),
            re.compile(r';.*'),
            re.compile(r'\(\*.*?\*\)', re.DOTALL)
        ],
        'functions': [
            re.compile(r'def\s+\w+'),
            re.compile(r'function\s+\w+'),
            re.compile(r'func\s+\w+'),
            re.compile(r'fn\s+\w+'),
            re.compile(r'\w+\s+\w+\s*\([^)]*\)\s*{'),
            re.compile(r'sub\s+\w+'),
            re.compile(r'class\s+\w+'),
            re.compile(r'module\s+\w+')
        ],
        'error_handling': [
            re.compile(r'try\s*[{:]'),
            re.compile(r'catch'),
            re.compile(r'except'),
            re.compile(r'finally'),
            re.compile(r'panic'),
            re.compile(r'Result<'),
            re.compile(r'Option<'),
            re.compile(r'if.*error'),
            re.compile(r'rescue'),
            re.compile(r'ensure')
        ],
        'imports': [
            re.compile(r'import\s+'),
            re.compile(r'#include\s*<'),
            re.compile(r'using\s+'),
            re.compile(r'require\s+'),
            re.compile(r'use\s+'),
            re.compile(r'from\s+\w+\s+import'),
            re.compile(r'package\s+'),
            re.compile(r'extern\s+crate')
        ]
    }
    
    @classmethod
    def analyze(cls, code: str, language: str) -> QualityMetrics:
        """Analyze code quality and return metrics"""
        metrics = QualityMetrics(
            code_length=len(code),
            line_count=len(code.split('\n')),
            non_empty_lines=len([line for line in code.split('\n') if line.strip()]),
            language=language
        )
        
        # Check each pattern category
        for category, patterns in cls.QUALITY_PATTERNS.items():
            if any(pattern.search(code) for pattern in patterns):
                setattr(metrics, f'has_{category}', True)
        
        return metrics


class TaskSearcher:
    """
    Single Responsibility: Targeted search and execution of specific tasks
    Allows you to search for tasks by name, display available languages and measure CO2
    """
    
    def __init__(self):
        self.code_base_path = "data/generated/code_snippets"
        self.results_dir = "results/task_search"
        
        # Initialize helpers
        self.lang_mapper = LanguageMapper()
        self.available_languages = self._load_tested_languages()
        
        # Initialize components if available
        if DEPENDENCIES_AVAILABLE:
            self.executor = SmartExecutor()
            self.lang_config = LanguageConfigManager()
        else:
            self.executor = None
            self.lang_config = None
        
        # Create results directory
        os.makedirs(self.results_dir, exist_ok=True)
    
    def _load_tested_languages(self) -> List[str]:
        """Load the list of tested and available languages"""
        test_results_dir = Path("results/execution")
        
        if not test_results_dir.exists():
            return self._get_fallback_languages()
        
        test_files = list(test_results_dir.glob("language_test_results_*.json"))
        if not test_files:
            return self._get_fallback_languages()
        
        latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
        
        try:
            with open(latest_test, 'r') as f:
                test_data = json.load(f)
            
            available = []
            for lang, result in test_data['results'].items():
                if result['available']:
                    normalized = self.lang_mapper.normalize(lang)
                    if normalized not in available:
                        available.append(normalized)
            
            return available if available else self._get_fallback_languages()
            
        except Exception as e:
            print(f"⚠ Error loading test results: {e}")
            return self._get_fallback_languages()
    
    def _get_fallback_languages(self) -> List[str]:
        """Get fallback languages when test results are not available"""
        print("ℹ No test results found, using fallback main languages")
        fallback = ['python', 'java', 'javascript', 'c', 'cpp', 'go', 'rust']
        print(f"Fallback languages: {', '.join(fallback)}")
        return fallback
    
    @staticmethod
    def normalize_task_name(task_name: str) -> str:
        """Normalizes the task name for search, preserving underscores"""
        # Keep underscores, drop other punctuation, collapse whitespace to underscores
        normalized = re.sub(r'[^a-zA-Z0-9_\s]', '', task_name.lower())
        return re.sub(r'\s+', '_', normalized.strip())

    @staticmethod
    def extract_task_name_from_filename(filename: str) -> Optional[str]:
        """Extract task name from filenames like snippet_<id>_<TaskName>.<ext>"""
        try:
            base = os.path.basename(filename)
            if base.startswith('snippet_'):
                name_part = base.rsplit('.', 1)[0]
                parts = name_part.split('_', 2)
                if len(parts) >= 3:
                    return parts[2]
        except Exception:
            pass
        return None
    
    def _get_file_extensions(self, language: str) -> List[str]:
        """Gets the supported file extensions for a language"""
        if self.lang_config:
            config = self.lang_config.get_language_config(language)
            if config and 'extension' in config:
                extensions = [config['extension'], '.txt']
                if language.lower() == 'cpp':
                    extensions = ['.cpp', '.cxx', '.cc', '.c++', '.txt']
                return extensions
        # Fallback mapping when LanguageConfigManager isn't available
        fallback_ext = {
            'python': ['.py', '.txt'],
            'javascript': ['.js', '.txt'],
            'java': ['.java', '.txt'],
            'c': ['.c', '.txt'],
            'cpp': ['.cpp', '.cxx', '.cc', '.c++', '.txt'],
            'go': ['.go', '.txt'],
            'rust': ['.rs', '.txt'],
            'csharp': ['.cs', '.txt'],
            'haskell': ['.hs', '.txt'],
            'julia': ['.jl', '.txt'],
            'matlab': ['.m', '.txt'],
            'ocaml': ['.ml', '.mli', '.txt'],
            'php': ['.php', '.txt'],
            'r': ['.r', '.txt'],
            'ruby': ['.rb', '.txt'],
            'typescript': ['.ts', '.txt'],
        }
        return fallback_ext.get(language.lower(), ['.txt'])
    
    def search_tasks(self, task_name: str, fuzzy: bool = True) -> Dict[str, List[str]]:
        """
        Searches for tasks by name in the dataset (only in tested and available languages)
        """
        print(f"\nSearching for task: '{task_name}' (only tested languages)")
        print("=" * 50)
        
        if not self.available_languages:
            print("WARNING: No available languages - please run 'python main.py test' first")
            return {}
        
        print(f"Searching in {len(self.available_languages)} tested languages...")
        
        normalized_name = self.normalize_task_name(task_name)
        found_tasks = defaultdict(list)
        
        for category_dir in glob.glob(os.path.join(self.code_base_path, "*")):
            if not os.path.isdir(category_dir):
                continue
            
            for lang_dir in glob.glob(os.path.join(category_dir, "*")):
                if not os.path.isdir(lang_dir):
                    continue
                
                lang_name = self.lang_mapper.normalize(os.path.basename(lang_dir))
                
                if lang_name not in self.available_languages:
                    continue
                
                extensions = self._get_file_extensions(lang_name)
                found_tasks[lang_name].extend(
                    self._search_in_directory(lang_dir, normalized_name, extensions, fuzzy)
                )
        
        return dict(found_tasks)
    
    def _search_in_directory(self, lang_dir: str, normalized_name: str, 
                            extensions: List[str], fuzzy: bool) -> List[str]:
        """Search for matching files in a directory"""
        matches = []
        for ext in extensions:
            pattern = os.path.join(lang_dir, f"*{ext}")
            for file_path in glob.glob(pattern):
                # Extract task name segment reliably and normalize both sides
                extracted = self.extract_task_name_from_filename(os.path.basename(file_path))
                if extracted is None:
                    # Fallback to filename normalization
                    file_name_normalized = self.normalize_task_name(os.path.basename(file_path))
                    candidate = file_name_normalized
                else:
                    candidate = self.normalize_task_name(extracted)

                if fuzzy:
                    if normalized_name in candidate:
                        matches.append(file_path)
                else:
                    if normalized_name == candidate:
                        matches.append(file_path)
        return matches
    
    def display_search_results(self, found_tasks: Dict[str, List[str]], task_name: str):
        """Displays the search results in a user-friendly format"""
        if not found_tasks:
            self._display_no_results(task_name)
            return
        
        total_tasks = sum(len(files) for files in found_tasks.values())
        print(f"\nFound {total_tasks} tasks in {len(found_tasks)} tested languages")
        print(f"Available in {len(self.available_languages)} languages from the test\n")
        
        for lang, files in sorted(found_tasks.items()):
            print(f"{lang.upper()} ({len(files)} files):")
            for file_path in files[:MAX_FILES_DISPLAY]:
                print(f"   • {os.path.basename(file_path)}")
            if len(files) > MAX_FILES_DISPLAY:
                print(f"   • ... and {len(files) - MAX_FILES_DISPLAY} more files")
            print()
    
    def _display_no_results(self, task_name: str):
        """Display message when no results are found"""
        print(f"\nNo tasks found for '{task_name}' in tested languages")
        print("\nSuggestions:")
        print("   • Try a more generic search (e.g., 'sort' instead of 'quicksort')")
        print("   • Use keywords in English")
        print("   • Verify that the languages have been tested with 'python main.py test'")
    
    def select_task_for_execution(self, found_tasks: Dict[str, List[str]]) -> Optional[Tuple[str, str]]:
        """Allows the user to select a specific task for execution"""
        if not found_tasks:
            return None
        
        options = [
            (lang, file_path, os.path.basename(file_path))
            for lang, files in sorted(found_tasks.items())
            for file_path in files
        ]
        
        print("\nSelect task to execute:\n")
        for i, (lang, _, file_name) in enumerate(options, 1):
            print(f"  {i}. [{lang.upper()}] {file_name}")
        print(f"  0. Cancel operation\n")
        
        try:
            choice = input(f"Select option (1-{len(options)}) [0]: ").strip()
            if not choice or choice == '0':
                return None
            
            choice_num = int(choice)
            if 1 <= choice_num <= len(options):
                return options[choice_num - 1][0], options[choice_num - 1][1]
            
            print("Invalid choice")
            return None
            
        except (ValueError, KeyboardInterrupt):
            print("\nOperation canceled")
            return None
    
    def execute_task_with_carbon_tracking(self, language: str, file_path: str) -> bool:
        """Executes a specific task with CO2 monitoring"""
        if not DEPENDENCIES_AVAILABLE or not self.executor:
            print("Dependencies not available for execution")
            return False
        
        task_name = os.path.basename(file_path)
        print(f"\nExecuting task: '{task_name}' in {language.upper()}")
        print("=" * 50)
        
        try:
            code = self._read_and_clean_code(file_path)
            print(f"File: {os.path.basename(file_path)}")
            print(f"Language: {language}")
            print(f"Size: {len(code)} characters")
            
            # Qualitative analysis
            self._perform_quality_analysis(code, language)
            
            # Execute with carbon tracking
            result, emissions = self._execute_with_tracking(code, language, task_name)
            
            # Save and display results
            self._save_execution_results(task_name, language, file_path, 
                                        result.get('success', False), emissions)
            self._display_execution_result(result, emissions)
            
            return result.get('success', False)
            
        except Exception as e:
            print(f"Error during execution: {e}")
            return False
    
    def _read_and_clean_code(self, file_path: str) -> str:
        """Read and clean code from file"""
        with open(file_path, 'r', encoding='utf-8') as f:
            code = f.read()
        
        from src.finder import UnifiedTaskFinder
        code_before = code
        code = UnifiedTaskFinder.clean_code_content(code)
        
        if '\u00a0' in code_before and '\u00a0' not in code:
            print("✅ Cleaned U+00A0 characters from code")
        
        return code
    
    def _perform_quality_analysis(self, code: str, language: str):
        """Perform and display quality analysis"""
        print("\n📊 QUALITATIVE ANALYSIS")
        print("-" * 30)
        metrics = CodeQualityAnalyzer.analyze(code, language)
        self._display_quality_metrics(metrics)
        print()
    
    def _execute_with_tracking(self, code: str, language: str, 
                               task_name: str) -> Tuple[Dict, Optional[float]]:
        """Execute code with carbon tracking"""
        carbon_session = None
        if CODECARBON_AVAILABLE:
            carbon_session = start_carbon_tracking(task_name, language)
            print("🌱 CO2 monitoring started")
        else:
            print("⚠ CO2 monitoring not available")
        
        print("\n⏳ Execution in progress...")
        print("-" * 40)
        
        result = self.executor.execute_code(
            code=code,
            language=language,
            task_name=task_name
        )
        
        print("-" * 40)
        
        emissions = None
        if carbon_session and CODECARBON_AVAILABLE:
            emissions = stop_carbon_tracking()
        
        return result, emissions
    
    def _display_execution_result(self, result: Dict, emissions: Optional[float]):
        """Display execution results"""
        if emissions:
            print(f"🌍 CO2 emissions: {emissions:.6f} kg ({emissions * 1000:.3f} g)")
        
        if result.get('success', False):
            print("✅ Execution completed successfully")
            if result.get('output'):
                print(f"📤 Output: {result['output'][:100]}...")
        else:
            print("❌ Execution failed")
            if result.get('error'):
                print(f"⚠ Error: {result['error'][:100]}...")
    
    def _display_quality_metrics(self, metrics: QualityMetrics):
        """Display quality analysis results"""
        print(f"Quality Score: {metrics.quality_score}/100 ({metrics.score_label})")
        print(f"Lines: {metrics.line_count} (effective: {metrics.non_empty_lines})")
        
        features = []
        if metrics.has_comments:
            features.append("Comments")
        if metrics.has_functions:
            features.append("Functions")
        if metrics.has_error_handling:
            features.append("Error handling")
        if metrics.has_imports:
            features.append("Import/Include")
        
        if features:
            print(f"Features: {', '.join(features)}")
        else:
            print("Basic features: none detected")
    
    def _save_execution_results(self, task_name: str, language: str, file_path: str,
                               success: bool, emissions: Optional[float]):
        """Save execution results to JSON"""
        try:
            result = {
                'task_name': task_name,
                'language': language,
                'file_path': file_path,
                'success': success,
                'co2_emissions_kg': emissions,
                'timestamp': datetime.now().isoformat(),
                'execution_type': 'task_search'
            }
            
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            result_file = os.path.join(self.results_dir, f"task_execution_{timestamp}.json")
            
            with open(result_file, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2, ensure_ascii=False)
            
            print(f"💾 Results saved to: {result_file}")
            
        except Exception as e:
            print(f"⚠ Error saving results: {e}")
    
    def interactive_task_search(self, initial_query: Optional[str] = None) -> bool:
        """Interactive interface for searching and executing tasks"""
        print("\n🔎 TASK SEARCHER - Targeted Search and Execution")
        print("=" * 50)
        print(f"Limited to {len(self.available_languages)} tested and available languages\n")
        
        query = initial_query or input("🔍 Enter task name to search: ").strip()
        
        if not query:
            print("❌ Empty query, operation cancelled")
            return False
        
        found_tasks = self.search_tasks(query, fuzzy=True)
        self.display_search_results(found_tasks, query)
        
        if not found_tasks:
            return False
        
        if not self._confirm_execution():
            return True
        
        selection = self.select_task_for_execution(found_tasks)
        if not selection:
            self._print_completion_message()
            return True
        
        language, file_path = selection
        success = self.execute_task_with_carbon_tracking(language, file_path)
        
        self._print_completion_message()
        return success
    
    def _confirm_execution(self) -> bool:
        """Ask user confirmation for execution"""
        print("\n⚡ Do you want to execute one of these tasks?")
        execute_choice = input("Confirm [y/N]: ").strip().lower()
        return execute_choice in ['y', 'yes']
    
    def _print_completion_message(self):
        """Print completion message"""
        print("\n✅ Task search and execution completed!")
        print(f"📁 Check the results in {self.results_dir}/")
        print("=" * 60)


def search_and_execute_task(task_name: Optional[str] = None) -> bool:
    """
    Main function for searching and executing tasks

    Args:
        task_name: Name of the task to search for (optional)

    Returns:
        True if the operation was successful, False otherwise
    """
    try:
        searcher = TaskSearcher()
        return searcher.interactive_task_search(task_name)
    except KeyboardInterrupt:
        print("\n❌ Operation interrupted by user")
        return False
    except Exception as e:
        print(f"❌ Error in task searcher: {e}")
        return False


if __name__ == "__main__":
    if len(sys.argv) > 1:
        task_name = " ".join(sys.argv[1:])
        search_and_execute_task(task_name)
    else:
        search_and_execute_task()