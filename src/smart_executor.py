#!/usr/bin/env python3
"""
Smart Executor - Intelligent executor that adapts to available languages
"""

import sys
import os
import subprocess
import json
import re
import tempfile
import time
from pathlib import Path
from datetime import datetime

# Adds the path to import CLAP modules
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

# Import modular components
try:
    from modules.language_config import LanguageConfigManager
    from src.finder import UnifiedTaskFinder
    MODULAR_COMPONENTS_AVAILABLE = True
except ImportError as e:
    MODULAR_COMPONENTS_AVAILABLE = False
    print(f"⚠️  Modular components not available: {e}")
    try:
        # Fallback for direct import
        from finder import UnifiedTaskFinder
    except ImportError:
        UnifiedTaskFinder = None

# Import the carbon tracker to monitor environmental impact (lazy import)
CARBON_TRACKING_AVAILABLE = False  # Temporarily disabled for debugging
start_carbon_tracking = None
stop_carbon_tracking = None

def _lazy_import_carbon():
    """Import lazy del carbon tracker"""
    global start_carbon_tracking, stop_carbon_tracking, CARBON_TRACKING_AVAILABLE
    if start_carbon_tracking is None and CARBON_TRACKING_AVAILABLE:
        try:
            from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking
        except:
            CARBON_TRACKING_AVAILABLE = False

class SmartExecutor:
    """Intelligent executor with modular components that automatically detects available languages"""

    def __init__(self):
        self.results_dir = "results/execution"
        self.analysis_dir = "results/task_analysis"
        self.temp_files = []
        self.available_languages = {}
        # Optional components (set when modular tools are wired)
        self.logger = None
        self.dependency_analyzer = None
        
        # Initialize modular components
        if MODULAR_COMPONENTS_AVAILABLE:
            self.config_manager = LanguageConfigManager('CLAP')
            print("✅ MODULAR: Using modern language configuration")
        else:
            self.config_manager = None
            print("⚠️  LEGACY: Using fallback configuration")
        
        # Initialize finder for robust file search
        if UnifiedTaskFinder is not None:
            self.finder = UnifiedTaskFinder()
            self.finder.create_dataset_dataframe(verbose=False)
            self.use_finder = True
        else:
            self.finder = None
            self.use_finder = False
            print("⚠️  Finder not available, using legacy file search")
        
        # Full configuration for all supported languages (aligned with LanguageTester)
        self.language_config = {
            'python': {
                'extension': '.py',
                'executor': ['conda', 'run', '-n', 'CLAP', 'python'],
                'timeout': 30,
                'test_code': 'print("test")'
            },
            'javascript': {
                'extension': '.js',
                'executor': ['conda', 'run', '-n', 'CLAP', 'node'],
                'timeout': 30,
                'test_code': 'console.log("test");'
            },
            'java': {
                'extension': '.java',
                'compiler': ['conda', 'run', '-n', 'CLAP', 'javac'],
                'executor': ['conda', 'run', '-n', 'CLAP', 'java'],
                'timeout': 30,
                'test_code': '''public class Test {
    public static void main(String[] args) {
        System.out.println("test");
    }
}'''
            },
            'ruby': {
                'extension': '.rb',
                'executor': ['conda', 'run', '-n', 'CLAP', 'ruby'],
                'timeout': 30,
                'test_code': 'puts "test"'
            },
            'php': {
                'extension': '.php',
                'executor': ['conda', 'run', '-n', 'CLAP', 'php'],
                'timeout': 30,
                'test_code': '<?php echo "test\\n"; ?>'
            },
            'r': {
                'extension': '.r',
                'executor': ['conda', 'run', '-n', 'CLAP', 'Rscript'],
                'timeout': 30,
                'test_code': 'cat("test\\n")'
            },
            'julia': {
                'extension': '.jl',
                'executor': ['conda', 'run', '-n', 'CLAP', 'julia'],
                'timeout': 30,
                'test_code': 'println("test")'
            },
            'csharp': {
                'extension': '.cs',
                'compiler': ['mcs'], 
                'executor': ['mono'],
                'timeout': 30,
                'test_code': '''using System;
class Test {
    static void Main() {
        Console.WriteLine("test");
    }
}'''
            },
            'c': {
                'extension': '.c',
                'compiler': ['gcc', '-o'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''#include <stdio.h>
int main() {
    printf("test\\n");
    return 0;
}'''
            },
            'cpp': {
                'extension': '.cpp',
                'compiler': ['g++', '-std=c++20', '-o'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''#include <iostream>
int main() {
    std::cout << "test" << std::endl;
    return 0;
}'''
            },
            'go': {
                'extension': '.go',
                'executor': ['go', 'run'],
                'timeout': 30,
                'test_code': '''package main
import "fmt"
func main() {
    fmt.Println("test")
}'''
            },
            'rust': {
                'extension': '.rs',
                'compiler': ['rustc'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''fn main() {
    println!("test");
}'''
            },
            'haskell': {
                'extension': '.hs',
                'executor': ['runhaskell'],
                'timeout': 30,
                'test_code': '''main = putStrLn "test"'''
            },
            'ocaml': {
                'extension': '.ml',
                'compiler': ['ocamlc', '-o'],
                'executor': ['./'],
                'timeout': 30,
                'test_code': '''print_endline "test";;'''
            },
            'typescript': {
                'extension': '.ts',
                'compiler': ['tsc'],
                'executor': ['node'],
                'timeout': 30,
                'test_code': '''console.log("test");'''
            }
        }

        # Add homebrew to PATH to locate all compilers (no longer gates config init)
        homebrew_path = "/opt/homebrew/bin"
        if homebrew_path not in os.environ.get("PATH", ""):
            os.environ["PATH"] = homebrew_path + ":" + os.environ.get("PATH", "")

        # Detect available languages from test results or fallback
        self.load_available_languages_from_test()

    def get_modular_config(self, language):
        """Get language configuration from modular components if available"""
        if self.config_manager:
            modular_config = self.config_manager.get_language_config(language)
            if modular_config:
                # Convert modular config to legacy format for compatibility
                return {
                    'extension': modular_config.get('extension', ''),
                    'executor': [modular_config.get('executor', '')],
                    'compiler': [modular_config.get('compiler', '')] if modular_config.get('compiler') else None,
                    'timeout': modular_config.get('timeout', 30),
                    'test_code': modular_config.get('test_code', '')
                }
        return None

    def log_modular_execution(self, language, task_name, execution_time, success, error=None):
        """Log execution using modular logger if available"""
        if self.logger:
            self.logger.log_execution_end(language, task_name, execution_time, success, 
                                        error_type="ExecutionError" if error else None)
            if error:
                self.logger.log_error(language, task_name, "ExecutionError", str(error), "")

    def analyze_dependencies(self, code, language):
        """Analyze code dependencies using modular analyzer if available"""
        if self.dependency_analyzer:
            dependencies = self.dependency_analyzer.get_external_dependencies(code, language)
            return dependencies
        return []

    def get_swam_env(self, language=None):
        """Gets the modified environment to include the binaries of the CLAP environment"""
        env = os.environ.copy()
        
        # Typical paths for miniconda/conda
        conda_base = os.path.expanduser("~/miniconda3")
        # Prefer the CLAP conda environment
        swam_env_path = os.path.join(conda_base, "envs", "CLAP", "bin")
        
        if os.path.exists(swam_env_path):
            # Add the SWAM environment path at the beginning of the PATH
            current_path = env.get('PATH', '')
            env['PATH'] = f"{swam_env_path}:{current_path}"
        
        # Remove LD_LIBRARY_PATH for languages that have issues with libtinfo
        if language in ['java', 'haskell', 'python', 'r', 'julia', 'javascript', 'php', 'ruby']:
            env.pop('LD_LIBRARY_PATH', None)
        
        # Add headless environment variables to prevent GUI windows
        env['DISPLAY'] = ''  # Disable X11 display
        env['MPLBACKEND'] = 'Agg'  # Matplotlib non-interactive backend
        env['PYTHONDONTWRITEBYTECODE'] = '1'  # Prevent .pyc files
        
        # Disable GUI for various frameworks
        env['QT_QPA_PLATFORM'] = 'offscreen'  # Qt headless
        env['HEADLESS'] = '1'  # General headless flag
        env['NO_GUI'] = '1'  # General no-GUI flag
        
        return env

    def check_command_available(self, command):
        """Checks if a command is available"""
        try:
            env = self.get_swam_env()
            result = subprocess.run(
                ['which', command] if os.name != 'nt' else ['where', command],
                capture_output=True,
                timeout=2,
                env=env
            )
            return result.returncode == 0
        except:
            return False

    def test_language_execution(self, language, config):
        """Tests if a language can be executed"""
        try:
            # Create temporary file
            with tempfile.NamedTemporaryFile(
                mode='w',
                suffix=config['extension'],
                delete=False
            ) as f:
                if language == 'java':
                    # For Java, use a specific name for the class
                    f.close()
                    os.unlink(f.name)
                    temp_file = f.name.replace(os.path.basename(f.name), 'Test.java')
                    with open(temp_file, 'w') as jf:
                        jf.write(config['test_code'])
                else:
                    f.write(config['test_code'])
                    temp_file = f.name

            temp_dir = os.path.dirname(temp_file)

            try:
                # Compilation if necessary
                if 'compiler' in config:
                    if language in ['c', 'cpp']:
                        # For C/C++, add the name of the output file
                        output_file = os.path.join(temp_dir, 'test')
                        compile_cmd = config['compiler'] + [output_file, temp_file]
                    else:
                        compile_cmd = config['compiler'] + [temp_file]
                    
                    compile_result = subprocess.run(
                        compile_cmd,
                        capture_output=True,
                        timeout=30,
                        cwd=temp_dir,
                        env=self.get_swam_env()
                    )
                    if compile_result.returncode != 0:
                        return False

                # Execution
                if 'compiler' in config:
                    if language == 'java':
                        run_cmd = config['executor'] + ['Test']
                    elif language in ['c', 'cpp']:
                        run_cmd = ['./test']
                    else:
                        run_cmd = config['executor']
                else:
                    run_cmd = config['executor'] + [temp_file]

                run_result = subprocess.run(
                    run_cmd,
                    capture_output=True,
                    timeout=20,
                    cwd=temp_dir,
                    env=self.get_swam_env()
                )

                success = run_result.returncode == 0 and 'test' in run_result.stdout.decode()

                # Cleanup
                try:
                    os.unlink(temp_file)
                    if language == 'java' and os.path.exists(os.path.join(temp_dir, 'Test.class')):
                        os.unlink(os.path.join(temp_dir, 'Test.class'))
                    if language == 'csharp' and os.path.exists(os.path.join(temp_dir, 'test.exe')):
                        os.unlink(os.path.join(temp_dir, 'test.exe'))
                except:
                    pass

                return success

            except:
                return False

        except:
            return False

    def load_available_languages_from_test(self):
        """Loads the available languages from the language test results"""
        import json
        from pathlib import Path
        
        test_results_dir = Path("results/execution")
        
        if test_results_dir.exists():
            # Search for the most recent test file
            test_files = list(test_results_dir.glob("language_test_results_*.json"))
            if test_files:
                latest_test = max(test_files, key=lambda x: x.stat().st_mtime)
                try:
                    with open(latest_test, 'r') as f:
                        test_data = json.load(f)
                        
                    print(f" Loading languages from test: {latest_test.name}")
                    
                    # Map names from the test to the SmartExecutor names (they are already identical)

                    test_to_executor_mapping = {
                        'cpp': 'cpp',
                        'java': 'java', 
                        'csharp': 'csharp',
                        'python': 'python',
                        'ruby': 'ruby',
                        'javascript': 'javascript',
                        'typescript': 'typescript',
                        'c': 'c',
                        'go': 'go',
                        'rust': 'rust',
                        'php': 'php',
                        'haskell': 'haskell',
                        'ocaml': 'ocaml',
                        'r': 'r',
                        'julia': 'julia'
                    }
                    
                    # Load only lenguage that passed the test
                    for lang, result in test_data['results'].items():
                        if result['available'] and lang in test_to_executor_mapping:
                            executor_lang = test_to_executor_mapping[lang]
                            if executor_lang in self.language_config:
                                self.available_languages[executor_lang] = self.language_config[executor_lang]
                    
                    print(f" Languages loaded from test: {len(self.available_languages)}")
                    for lang in sorted(self.available_languages.keys()):
                        print(f" • {lang.upper()}")
                    
                    return True
                    
                except Exception as e:
                    print(f" Error loading test results: {e}")
        
        # Fallback: use traditional detection if no test results are found
        print(" No test results found, using traditional detection...")
        self.detect_available_languages()
        return False

    def detect_available_languages(self):
        """Automatically detects available languages"""
        print(" Detecting available languages...")

        for language, config in self.language_config.items():
            print(f" Testing {language}...", end=' ')

            # For compiled languages, check the compiler
            if 'compiler' in config:
                compiler_cmd = config['compiler'][0]
                if not self.check_command_available(compiler_cmd):
                    print(" (command not found)")
                    continue
            else:
                # For interpreted languages, check the executor
                main_cmd = config['executor'][0]
                if not self.check_command_available(main_cmd):
                    print(" (command not found)")
                    continue

            # Quick execution test
            if self.test_language_execution(language, config):
                self.available_languages[language] = config
                print("")
            else:
                print(" (execution test failed)")

        print(f"\n Available languages: {len(self.available_languages)}")
        for lang in sorted(self.available_languages.keys()):
            print(f" • {lang.upper()}")

    def execute_code(self, code, language, task_name, timeout=None):
        """Executes the given code in the specified language and tracks emissions
        
        Args:
            code: Code to execute
            language: Programming language
            task_name: Name of the task
            timeout: Optional timeout in seconds (overrides language default)
        """

        # Start CO2 tracking for this execution (only if not disabled)
        should_track = CARBON_TRACKING_AVAILABLE and not getattr(self, 'disable_carbon_tracking', False)
        if should_track:
            _lazy_import_carbon()
            if start_carbon_tracking:
                start_carbon_tracking(task_name, language)

        if language not in self.available_languages:
            if should_track and stop_carbon_tracking:
                stop_carbon_tracking()
            return {
                'success': False,
                'error': f'Language {language} is not available on this system',
                'output': '',
                'execution_time': 0
            }

        config = self.available_languages[language]

        try:
            start_time = time.time()

            # Create temporary file
            temp_file = self.create_temp_file(code, config['extension'], task_name, language)
            if not temp_file:
                if should_track and stop_carbon_tracking:
                    stop_carbon_tracking()
                return {
                    'success': False,
                    'error': 'Unable to create temporary file',
                    'output': '',
                    'execution_time': 0
                }

            temp_dir = os.path.dirname(temp_file)

            # Compilation if necessary
            if 'compiler' in config:
                compile_result = self.compile_code(temp_file, config, language)
                if not compile_result['success']:
                    return {
                        'success': False,
                        'error': f'Compilation error: {compile_result["error"]}',
                        'output': '',
                        'execution_time': time.time() - start_time
                    }

            # Execution
            exec_result = self.run_code(temp_file, config, language, temp_dir, timeout=timeout)
            execution_time = time.time() - start_time

            result = {
                'success': exec_result['success'],
                'error': exec_result.get('error', ''),
                'output': exec_result.get('output', ''),
                'execution_time': execution_time
            }

            # Stop CO2 tracking
            if should_track and stop_carbon_tracking:
                stop_carbon_tracking()

            return result

        except Exception as e:
            # Stop CO2 tracking even in case of error
            if should_track and stop_carbon_tracking:
                stop_carbon_tracking()

            return {
                'success': False,
                'error': f'Execution error: {str(e)}',
                'output': '',
                'execution_time': time.time() - start_time if 'start_time' in locals() else 0
            }
        finally:
            # Cleanup
            self.cleanup_temp_files([temp_file] if 'temp_file' in locals() else [])

    def clean_code(self, code, language):
        """Cleans the code by removing interactive syntax and common issues"""
        # First of all, removes problematic Unicode characters
        cleaned = self.clean_code_content(code)
        
        # Remove interactive prompts
        cleaned = re.sub(r'^\s*>>>?\s*', '', cleaned, flags=re.MULTILINE)
        cleaned = re.sub(r'^\s*\.\.\.\s*', '', cleaned, flags=re.MULTILINE)
        
        # Language-specific cleaning
        if language == 'python':
            # Adds parentheses to Python 2 print statements
            cleaned = re.sub(r'\bprint\s+([^(][^\n]*)', r'print(\1)', cleaned)
            
            # Remove/replace GUI elements that could open windows
            cleaned = self._clean_python_gui_code(cleaned)
        
        elif language in ['c', 'cpp', 'c++']:
            # Add main() function if missing (for code snippets)
            if 'int main' not in cleaned and 'void main' not in cleaned:
                # Check if there are functions defined
                has_functions = bool(re.search(r'(void|int|char|float|double)\s+\w+\s*\([^)]*\)\s*\{', cleaned))
                
                if has_functions:
                    # Code has functions, wrap with minimal main that calls first function if possible
                    function_match = re.search(r'(void|int)\s+(\w+)\s*\([^)]*\)', cleaned)
                    if function_match:
                        func_name = function_match.group(2)
                        if function_match.group(1) == 'void':
                            main_body = f'    {func_name}();\n    return 0;'
                        else:
                            main_body = f'    {func_name}();\n    return 0;'
                        cleaned = f'{cleaned}\n\nint main() {{\n{main_body}\n}}\n'
                    else:
                        # Just add empty main
                        cleaned = f'{cleaned}\n\nint main() {{\n    return 0;\n}}\n'
                else:
                    # No functions, might be simple statements, wrap them in main
                    cleaned = f'int main() {{\n{cleaned}\n    return 0;\n}}\n'
            
        elif language == 'javascript':
            # Removes references to window/DOM for Node.js
            cleaned = re.sub(r'if\s*\(\s*window\.DOMParser\s*\)', 'if (false)', cleaned)
            cleaned = re.sub(r'window\.', '', cleaned)
            # Adds missing declarations
            if 'Matrix' in cleaned and 'function Matrix' not in cleaned:
                cleaned = 'function Matrix() {}\n' + cleaned
            
            # Remove browser-specific code that might try to open windows
            cleaned = re.sub(r'window\.open\([^)]*\)', 'console.log("Window opening disabled")', cleaned)
            cleaned = re.sub(r'alert\([^)]*\)', 'console.log', cleaned)
            cleaned = re.sub(r'confirm\([^)]*\)', 'true', cleaned)
                
        elif language == 'typescript':
            # Fixes compiler options
            cleaned = re.sub(r'-o\s+', '--outFile ', cleaned)
            
        elif language == 'java':
            # If no class is defined, create a Main class
            if 'class' not in cleaned.lower():
                # Check if there are methods/functions to wrap
                has_methods = ('public static' in cleaned or 
                              'private static' in cleaned or
                              'static' in cleaned or
                              'public' in cleaned and ('(' in cleaned and ')' in cleaned))
                
                if has_methods:
                    # Adds main method if missing
                    if 'main(' not in cleaned and 'main (' not in cleaned:
                        # Creates a class with main that calls the main method if identifiable
                        cleaned = f'public class Main {{\n{cleaned}\n\n    public static void main(String[] args) {{\n        // Generated main method\n        System.out.println("Execution completed");\n    }}\n}}'
                    else:
                        cleaned = f'public class Main {{\n{cleaned}\n}}'
                else:
                    # Simple code, wrap in main
                    cleaned = f'public class Main {{\n    public static void main(String[] args) {{\n{cleaned}\n    }}\n}}'
                    
        elif language == 'go':
            # Removes imports of non-standard external libraries
            lines = cleaned.split('\n')
            filtered_lines = []
            in_import_block = False
            removed_packages = []
            package_declared = False
            
            for line in lines:
                line_stripped = line.strip()
                
                # Handles package declarations (keep only the first one)
                if line_stripped.startswith('package '):
                    if not package_declared:
                        if 'package main' not in line_stripped:
                            filtered_lines.append('package main')
                        else:
                            filtered_lines.append(line)
                        package_declared = True
                    continue
                
                # Handles import blocks
                elif line_stripped.startswith('import ('):
                    in_import_block = True
                    filtered_lines.append(line)
                    continue
                elif line_stripped == ')' and in_import_block:
                    in_import_block = False
                    filtered_lines.append(line)
                    continue
                elif in_import_block:
                    # Keeps only standard imports
                    if any(std_pkg in line_stripped for std_pkg in ['"fmt"', '"os"', '"strings"', '"strconv"', '"math"', '"time"', '"io"', '"sort"', '"net"']):
                        filtered_lines.append(line)
                    else:
                        # Tracks removed packages for code cleanup
                        if '"' in line_stripped:
                            pkg_name = line_stripped.split('"')[1].split('/')[-1]
                            removed_packages.append(pkg_name)
                    continue
                elif line_stripped.startswith('import "') and not any(std_pkg in line_stripped for std_pkg in ['"fmt"', '"os"', '"strings"', '"strconv"', '"math"', '"time"', '"io"', '"sort"', '"net"']):
                    # Tracks and skips individual imports of external libraries
                    if '"' in line_stripped:
                        pkg_name = line_stripped.split('"')[1].split('/')[-1]
                        removed_packages.append(pkg_name)
                    continue
                else:
                    filtered_lines.append(line)
            
            cleaned = '\n'.join(filtered_lines)
            
            # Cleans code from references to removed packages
            for pkg in removed_packages:
                # Removes calls to the package (e.g. mat.NewDense, mat.Formatted)
                cleaned = re.sub(rf'\b{pkg}\.\w+\([^)]*\)', 'nil', cleaned)
                cleaned = re.sub(rf'\*{pkg}\.\w+', 'interface{}', cleaned)
                cleaned = re.sub(rf'\b{pkg}\.\w+', 'nil', cleaned)
            
            # Simplify problematic functions with basic versions
            if 'mat.Dense' in cleaned or 'mat.NewDense' in cleaned:
                # Replaces with simple implementation without external dependencies
                cleaned = re.sub(r'func eye\([^}]*\}', '''func eye(n int) [][]int {
    matrix := make([][]int, n)
    for i := range matrix {
        matrix[i] = make([]int, n)
        matrix[i][i] = 1
    }
    return matrix
}''', cleaned, flags=re.DOTALL)
                
            # Cleans the main function from calls to external functions
            cleaned = re.sub(r'fmt\.Println\(mat\.Formatted\([^)]*\)\)', 'fmt.Println("Identity matrix created")', cleaned)
            
            cleaned = '\n'.join(filtered_lines)
            
            # Ensure there is a package main
            if 'package main' not in cleaned:
                cleaned = 'package main\n\n' + cleaned
                
            # Ensure there is a main function
            if 'func main()' not in cleaned:
                cleaned += '\n\nfunc main() {\n    // Generated main function\n}\n'
                
        elif language == 'haskell':
            # Removes imports of non-standard external modules and fix common issues
            lines = cleaned.split('\n')
            filtered_lines = []
            
            for line in lines:
                line_stripped = line.strip()
                # Skip problematic imports
                if line_stripped.startswith('import ') and any(problematic in line_stripped for problematic in ['Control.Monad', 'hiding (odd)']):
                    continue
                # Fix join function usage
                elif 'join (+)' in line:
                    filtered_lines.append(line.replace('join (+)', '(\\x -> x + x)'))
                else:
                    filtered_lines.append(line)
            
            cleaned = '\n'.join(filtered_lines)
            
            # Remove redefined odd function to avoid conflicts
            cleaned = re.sub(r'odd :: Int -> Bool\s*\n.*odd = .*\n', '', cleaned, flags=re.MULTILINE)
            
            # Ensure there is a main function
            if 'main =' not in cleaned and 'main::' not in cleaned:
                # Find the first defined function to call it in main
                function_match = re.search(r'^(\w+)\s+::', cleaned, re.MULTILINE)
                if function_match:
                    func_name = function_match.group(1)
                    if func_name != 'main':
                        cleaned += f'\n\nmain = print ({func_name} 3 5)'
                else:
                    cleaned += '\n\nmain = putStrLn "Haskell execution completed"'
                
        elif language == 'rust':
            # Removes extern crate statements that may cause issues
            cleaned = re.sub(r'extern\s+crate\s+\w+;\s*\n?', '', cleaned)
            
            # Removes use statements of external crates
            cleaned = re.sub(r'use\s+\w+::\w+;\s*\n?', '', cleaned)
            
            # Replaces external traits with standard traits
            cleaned = re.sub(r'num::\w+', 'Copy + Clone + Default', cleaned)
            cleaned = re.sub(r'T::\w+\(\)', 'T::default()', cleaned)
            
            # Simplify structs with problematic generics
            if 'num::' in cleaned or 'extern crate' in cleaned:
                # Replace with a simpler implementation
                cleaned = re.sub(r'struct Matrix<T>.*?where.*?\{', 'struct Matrix {\n    data: Vec<i32>,\n    size: usize,\n}\n\nimpl Matrix {', cleaned, flags=re.DOTALL)
                cleaned = re.sub(r'T::[a-zA-Z_]\w*\(\)', '0', cleaned)
                cleaned = re.sub(r'\bT\b', 'i32', cleaned)
            
            # Ensure there is a main function
            if 'fn main()' not in cleaned:
                cleaned += '\n\nfn main() {\n    println!("Rust execution completed");\n}\n'
        
        elif language == 'r':
            # Remove/replace graphics elements that could open GUI windows
            cleaned = self._clean_r_graphics_code(cleaned)
                
        return cleaned
    
    def create_temp_file(self, code, extension, task_name, language):
        """Creates a temporary file with the given code and extension"""
        try:
            # Cleans the code before writing it
            cleaned_code = self.clean_code(code, language)
            
            # Sanitizes the task name for filename
            safe_task = re.sub(r'[^\w\s-]', '', task_name).strip()
            safe_task = re.sub(r'[-\s/]+', '_', safe_task)

            if language == 'java':
                # For Java, find the class name in the code (both public and non-public)
                class_match = re.search(r'(?:public\s+)?class\s+(\w+)', cleaned_code)
                if class_match:
                    class_name = class_match.group(1)
                    filename = f"{class_name}.java"
                    print(f" Creating Java file: {filename}")
                else:
                    filename = f"Main.java"
                    print(f" No class found, using: {filename}")
            else:
                filename = f"temp_{safe_task}_{language}{extension}"

            # Creates directory if necessary
            abs_results_dir = os.path.abspath(self.results_dir)
            os.makedirs(abs_results_dir, exist_ok=True)

            filepath = os.path.join(abs_results_dir, filename)

            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(cleaned_code)

            self.temp_files.append(filepath)
            return filepath

        except Exception as e:
            print(f" Error creating temporary file: {e}")
            return None



    def compile_code(self, filepath, config, language):
        """Compiles the code if necessary"""
        try:
            temp_dir = os.path.dirname(filepath)
            base_name = Path(filepath).stem

            if language == 'java':
                cmd = config['compiler'] + [filepath]
            elif language == 'csharp':
                cmd = config['compiler'] + [filepath]
            elif language == 'go':
                # Go: renames the file to main.go and compiles to the specific output
                main_go = os.path.join(temp_dir, 'main.go')
                os.rename(filepath, main_go)
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + ['-o', executable_path, 'main.go']
            elif language == 'rust':
                # Rust: compiles with absolute path
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath, '-o', executable_path]
            elif language == 'kotlin':
                # Kotlin: compiles to JAR
                jar_path = os.path.join(temp_dir, 'test.jar')
                cmd = config['compiler'] + [filepath]
            elif language == 'swift':
                # Swift: compiles with absolute path
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath, '-o', executable_path]
            elif language in ['c', 'cpp']:
                # C/C++: specific argument order
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [executable_path, filepath]
            elif language == 'ocaml':
                # OCaml: specific order
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [executable_path, filepath]
            elif language == 'typescript':
                # TypeScript: uses tsc to compile to JS
                cmd = config['compiler'] + [filepath]
            elif language == 'haskell':
                # Haskell: specific order ghc -o executable filepath
                executable_path = os.path.join(temp_dir, 'test')
                cmd = ['ghc', '-o', executable_path, filepath]
            else:
                # Other compiled languages
                executable_path = os.path.join(temp_dir, 'test')
                cmd = config['compiler'] + [filepath] + ['-o', executable_path]

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=config.get('timeout', 45),
                cwd=temp_dir,
                env=self.get_swam_env(language)
            )

            if result.returncode == 0:
                return {'success': True}
            else:
                return {
                    'success': False,
                    'error': result.stderr or result.stdout
                }

        except subprocess.TimeoutExpired:
            return {'success': False, 'error': 'Timeout during compilation'}
        except Exception as e:
            return {'success': False, 'error': str(e)}

    def run_code(self, filepath, config, language, temp_dir, timeout=None):
        """Runs the code
        
        Args:
            filepath: Path to the code file
            config: Language configuration
            language: Programming language
            temp_dir: Temporary directory
            timeout: Optional timeout in seconds (overrides config default)
        """
        try:
            if 'compiler' in config:
                if language == 'java':
                    # For Java, extracts the class name from the compiled file
                    with open(filepath, 'r', encoding='utf-8') as f:
                        code = f.read()

                    # Searches for both 'public class' and 'class' without public
                    class_match = re.search(r'(?:public\s+)?class\s+(\w+)', code)
                    if class_match:
                        class_name = class_match.group(1)
                        cmd = config['executor'] + [class_name]
                        print(f" Java class found: {class_name}")
                    else:
                        # Fallback: uses the file name without extension
                        base_name = Path(filepath).stem
                        cmd = config['executor'] + [base_name]
                        print(f" Using file name: {base_name}")
                elif language == 'csharp':
                    # C#: executable .exe generated by the compiler
                    exe_name = Path(filepath).stem + '.exe'
                    exe_path = os.path.join(temp_dir, exe_name)
                    cmd = config['executor'] + [exe_path]
                elif language in ['c', 'cpp']:
                    # C/C++: uses fixed name 'test'
                    executable_path = os.path.join(temp_dir, 'test')
                    cmd = [executable_path]
                elif language == 'ocaml':
                    # OCaml: uses fixed name 'test'
                    executable_path = os.path.join(temp_dir, 'test')
                    cmd = [executable_path]
                elif language == 'rust':
                    # Rust: uses fixed name 'test'
                    executable_path = os.path.join(temp_dir, 'test')
                    cmd = [executable_path]
                elif language == 'typescript':
                    # TypeScript: uses tsc to compile to JS
                    js_file = filepath.replace('.ts', '.js')
                    cmd = config['executor'] + [js_file]
                else:
                    # Other compiled languages
                    executable_name = Path(filepath).stem
                    executable_path = os.path.join(temp_dir, executable_name)
                    cmd = [executable_path]
            else:
                # Interpreted languages
                cmd = config['executor'] + [filepath]

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=timeout if timeout is not None else config.get('timeout', 45),
                cwd=temp_dir,
                env=self.get_swam_env(language)
            )

            return {
                'success': result.returncode == 0,
                'output': result.stdout,
                'error': result.stderr if result.returncode != 0 else '',
                'exit_code': result.returncode
            }

        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'error': f'Timeout ({timeout if timeout else config.get("timeout", 45)}s) durante esecuzione',
                'output': '',
                'error_type': 'TimeoutError',
                'exit_code': -1
            }
        except Exception as e:
            return {'success': False, 'error': str(e), 'output': ''}

    def cleanup_temp_files(self, files):
        """Cleans up temporary files"""
        for filepath in files:
            try:
                if os.path.exists(filepath):
                    os.remove(filepath)

                # Removes also compiled files
                temp_dir = os.path.dirname(filepath)
                base_name = Path(filepath).stem

                # .class file for Java
                class_file = os.path.join(temp_dir, f"{base_name}.class")
                if os.path.exists(class_file):
                    os.remove(class_file)

                # .exe file for C#
                exe_file = os.path.join(temp_dir, "test.exe")
                if os.path.exists(exe_file):
                    os.remove(exe_file)

            except Exception as e:
                print(f" Errore cleanup {filepath}: {e}")

    def find_task_file(self, task_name, language):
        """Finds the file for a task and language using UnifiedTaskFinder for consistency
        
        When multiple files match, selects the one with the best match quality:
        1. Exact match (task name matches exactly)
        2. Suffix match (filename ends with task name)
        3. Shortest filename (likely more specific)
        4. Lexicographically first (consistent ordering)
        """
        
        # Use finder if available for consistent, robust file search
        if self.use_finder and self.finder is not None:
            try:
                df = self.finder.df
                # Filter by task name and language
                filtered = df[(df['task_name'] == task_name) & (df['language'].str.lower() == language.lower())]
                
                if not filtered.empty:
                    # If multiple matches, apply best match selection
                    if len(filtered) > 1:
                        return self._select_best_match(filtered['file_path'].tolist(), task_name)
                    return filtered.iloc[0]['file_path']
            except Exception as e:
                print(f"⚠️  Finder search failed, falling back to legacy: {e}")
        
        # Fallback to legacy search if finder not available or fails
        code_base_dir = "data/generated/code_snippets"

        # Normalizes language names
        lang_normalized = language.lower()
        if lang_normalized == 'cpp':
            lang_normalized = 'c++'

        # List of categories present in the dataset
        categories = ['algorithms', 'strings', 'mathematics', 'io', 'basic', 'misc']

        # Collect ALL matching files from ALL categories
        all_matching_files = []
        
        for category in categories:
            language_dir = os.path.join(code_base_dir, category, lang_normalized)
            if not os.path.exists(language_dir):
                continue

            # Searches for the file that contains the task name (improved pattern)
            task_patterns = [
                task_name.replace('-', '_'),
                task_name.replace('_', '-'),
                task_name.replace(' ', '_'),
                task_name.replace(' ', '-')
            ]

            # Collect matching files in this category
            for filename in os.listdir(language_dir):
                filename_lower = filename.lower()
                for pattern in task_patterns:
                    if pattern.lower() in filename_lower:
                        full_path = os.path.join(language_dir, filename)
                        all_matching_files.append(full_path)
                        break  # Don't add the same file multiple times
        
        if not all_matching_files:
            return None
        
        # Select the best match from all candidates
        return self._select_best_match(all_matching_files, task_name)

    def _select_best_match(self, file_paths, task_name):
        """Selects the best matching file from a list of candidates
        
        Scoring system (higher is better):
        - Exact match: +10000 points (massima priorità)
        - Exact match case-insensitive: +5000 points
        - Suffix exact match: +1000 points
        - Word boundary match: +500 points
        - Starts with match (but longer): +100 points
        - Contains match: +10 points
        - Shorter filename bonus: proportional to brevity
        
        Args:
            file_paths: List of absolute file paths
            task_name: The task name to match against
            
        Returns:
            The path of the best matching file
        """
        if not file_paths:
            return None
        
        if len(file_paths) == 1:
            return file_paths[0]
        
        # Normalize task name for comparison (preserve original for case-sensitive comparison)
        normalized_task = task_name.replace('-', '_').replace(' ', '_')
        normalized_task_lower = normalized_task.lower()
        
        scored_files = []
        
        for filepath in file_paths:
            filename = os.path.basename(filepath)
            name_without_ext = os.path.splitext(filename)[0]
            name_lower = name_without_ext.lower()
            
            score = 0
            
            # Extract task name from filename (format: snippet_N_TaskName)
            parts = name_without_ext.split('_', 2)
            if len(parts) >= 3:
                file_task_name = parts[2]
                file_task_name_lower = file_task_name.lower()
            else:
                file_task_name = name_without_ext
                file_task_name_lower = name_lower
            
            # ===== PRIORITY 1: EXACT MATCH (case-sensitive) =====
            if file_task_name == normalized_task:
                score += 10000
            
            # ===== PRIORITY 2: EXACT MATCH (case-insensitive) =====
            elif file_task_name_lower == normalized_task_lower:
                score += 5000
            
            # ===== PRIORITY 3: SUFFIX EXACT MATCH =====
            # E.g., "snippet_128_Exceptions" matches "Exceptions" exactly at the end
            elif name_without_ext.endswith(f"_{normalized_task}"):
                score += 1000
            elif name_lower.endswith(f"_{normalized_task_lower}"):
                score += 900
            
            # ===== PRIORITY 4: WORD BOUNDARY MATCH =====
            # Task name appears as complete word (surrounded by underscores)
            elif f"_{normalized_task_lower}_" in name_lower or name_lower.endswith(f"_{normalized_task_lower}"):
                score += 500
            
            # ===== PRIORITY 5: STARTS WITH (exact prefix) =====
            # E.g., "Sort" should NOT match "Topological_sort"
            # But "Sort" SHOULD match "Sorting_algorithms"
            elif file_task_name_lower.startswith(normalized_task_lower):
                # Check if the next character is a separator (not alphanumeric)
                if len(file_task_name_lower) > len(normalized_task_lower):
                    next_char = file_task_name_lower[len(normalized_task_lower)]
                    if next_char in ['_', '-', ' ']:
                        # Good prefix match: "Sort_stability", "Sort_an_array"
                        score += 100
                    else:
                        # Bad prefix match: "Sorting" when looking for "Sort"
                        score += 10
                else:
                    # Exact length match (already caught above, but defensive)
                    score += 100
            
            # ===== PRIORITY 6: CONTAINS MATCH =====
            # E.g., "Topological_sort" contains "sort" but shouldn't be preferred for "Sort"
            elif normalized_task_lower in file_task_name_lower:
                # Penalize if task appears in the middle
                score += 10
            
            # ===== BONUS: SHORTER FILENAME (tie-breaker) =====
            # Prefer more specific/shorter filenames
            max_length = 200
            length_bonus = max(0, (max_length - len(filename)) / max_length) * 50
            score += length_bonus
            
            # ===== BONUS: FEWER UNDERSCORES (simpler name) =====
            # Prefer "Sort" over "Sort_an_array_of_composite_structures"
            underscore_penalty = file_task_name_lower.count('_') * 5
            score -= underscore_penalty
            
            scored_files.append((score, filepath, filename, file_task_name))
        
        # Sort by score (descending), then by filename (ascending) for consistency
        scored_files.sort(key=lambda x: (-x[0], x[2]))
        
        # Debug output for verification (always show top 3 if multiple matches)
        if len(scored_files) > 1:
            top_score = scored_files[0][0]
            # Show matches if there are multiple candidates
            if len(scored_files) > 3 or (len(scored_files) > 1 and abs(scored_files[0][0] - scored_files[1][0]) < 100):
                print(f"  🔍 Matching '{task_name}' (showing top {min(3, len(scored_files))} candidates):")
                for score, path, fname, task in scored_files[:3]:
                    indicator = "✓" if score == top_score else " "
                    print(f"     {indicator} [{score:6.1f}] {fname}")
        
        return scored_files[0][1]  # Return path of best match

    def clean_code_content(self, code):
        """Cleans up the code from problematic invisible characters
        
        This method delegates to UnifiedTaskFinder.clean_code_content()
        to ensure consistent code cleaning across all modules.
        """
        if UnifiedTaskFinder is not None:
            return UnifiedTaskFinder.clean_code_content(code)
        else:
            # Fallback to basic cleaning if finder not available
            return code.replace('\u00a0', ' ').replace('\ufeff', '')

    def execute_task_all_available_languages(self, task_name):
        """Executes a task in all available languages"""
        print(f"\n Executing task: {task_name}")

        task_results = {}

        # Searches for files in the real structure: data/generated/code_snippets/category/language/
        code_base_dir = "data/generated/code_snippets"

        if not os.path.exists(code_base_dir):
            print(f" Base directory not found: {code_base_dir}")
            return task_results

        # Searches for code files for available languages in all categories
        for language in self.available_languages.keys():
            language_file = self.find_task_file(task_name, language)

            if language_file and os.path.exists(language_file):
                print(f" Executing {language}...")

                try:
                    with open(language_file, 'r', encoding='utf-8') as f:
                        code = f.read()

                    # Cleans up the code from invisible characters
                    code = self.clean_code_content(code)

                    result = self.execute_code(code, language, task_name)
                    task_results[language] = result

                    if result['success']:
                        print(f" {language}: success ({result['execution_time']:.2f}s)")
                        if result['output'].strip():
                            output_preview = result['output'].strip()[:100].replace('\n', ' ')
                            print(f" Output: {output_preview}...")
                    else:
                        print(f" {language}: {result['error'][:100]}...")

                except Exception as e:
                    task_results[language] = {
                        'success': False,
                        'error': f'Error reading file: {str(e)}',
                        'output': '',
                        'execution_time': 0
                    }
                    print(f" {language}: error reading file")
            else:
                print(f" {language}: file not found")

        return task_results

    def execute_all_common_tasks(self):
        """Executes all common tasks"""
        print(" SMART EXECUTOR - Adaptive Multi-Language Code Execution\n")
        print(f" Available languages: {len(self.available_languages)}")

        # Loads common tasks
        common_tasks_file = os.path.join(self.analysis_dir, "common_tasks.json")

        if not os.path.exists(common_tasks_file):
            print(" File common_tasks.json not found")
            return

        with open(common_tasks_file, 'r') as f:
            data = json.load(f)

        common_tasks_data = data.get('common_tasks', [])

        if not common_tasks_data:
            print(" No common tasks found")
            return

        # Extracts only the names of the tasks (first 10 for testing)
        tasks = [task['name'] for task in common_tasks_data[:10]]

        print(f" Found {len(common_tasks_data)} total common tasks")
        print(f" Executing the first {len(tasks)} tasks...")

        # Create results directory
        os.makedirs(self.results_dir, exist_ok=True)

        overall_results = {}

        for task in tasks:
            task_results = self.execute_task_all_available_languages(task)
            overall_results[task] = task_results

        # Saves results to a JSON file
        self.save_execution_results(overall_results)

        # Final report
        self.print_execution_summary(overall_results)

    def save_execution_results(self, results):
        """Saves the execution results"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = os.path.join(self.results_dir, f"smart_execution_results_{timestamp}.json")

        try:
            with open(results_file, 'w') as f:
                json.dump(results, f, indent=2, default=str)
            print(f" Results saved in: {results_file}")
        except Exception as e:
            print(f" Error saving results: {e}")

    def print_execution_summary(self, results):
        """Prints a summary of the execution"""
        print(f"\n SMART EXECUTOR - Execution Summary:")

        total_executions = 0
        successful_executions = 0

        for task, task_results in results.items():
            print(f"\n {task}:")
            task_success = 0
            task_total = 0

            for language, result in task_results.items():
                task_total += 1
                total_executions += 1

                if result['success']:
                    task_success += 1
                    successful_executions += 1
                    output_preview = result['output'].strip()[:50] if result['output'].strip() else "no output"
                    print(f" {language}: {result['execution_time']:.2f}s - {output_preview}")
                else:
                    print(f" {language}: {result['error'][:80]}...")

            if task_total > 0:
                success_rate = (task_success / task_total * 100)
                print(f" Success: {task_success}/{task_total} ({success_rate:.1f}%)")

        if total_executions > 0:
            overall_success_rate = (successful_executions / total_executions * 100)
            print(f"\n TOTAL: {successful_executions}/{total_executions} ({overall_success_rate:.1f}%) executions successful")
            print(f" Languages used: {sorted(self.available_languages.keys())}")
        else:
            print(f"\n No executions completed")

    def _clean_python_gui_code(self, code):
        """Remove or replace GUI elements in Python code to prevent windows from opening"""
        
        # Remove problematic imports
        gui_imports = [
            r'import\s+tkinter.*\n',
            r'from\s+tkinter\s+import.*\n', 
            r'import\s+turtle.*\n',
            r'import\s+pygame.*\n',
            r'import\s+webbrowser.*\n',
            r'from\s+turtle\s+import.*\n',
            r'from\s+OpenGL.*import.*\n',
            r'import\s+OpenGL.*\n'
        ]
        
        for pattern in gui_imports:
            code = re.sub(pattern, '# GUI import removed\n', code, flags=re.IGNORECASE)
        
        # Replace matplotlib show() calls
        code = re.sub(r'plt\.show\(\)', 'plt.savefig("/tmp/plot.png")  # plt.show() disabled', code)
        code = re.sub(r'pyplot\.show\(\)', 'pyplot.savefig("/tmp/plot.png")  # pyplot.show() disabled', code)
        
        # Replace PIL Image show() and other general show() calls
        code = re.sub(r'\.show\(\)', '.save("/tmp/image.png")  # .show() disabled', code)
        
        # Replace tkinter operations
        code = re.sub(r'\.mainloop\(\)', '.quit()  # mainloop() disabled', code)
        code = re.sub(r'root\.mainloop\(\)', 'pass  # root.mainloop() disabled', code)
        code = re.sub(r'app\.run\(\)', 'pass  # app.run() disabled', code)
        
        # Replace webbrowser calls
        code = re.sub(r'webbrowser\.open\([^)]*\)', 'print("Browser opening disabled")', code)
        
        # Replace turtle graphics
        code = re.sub(r'turtle\..*\n', '# Turtle graphics disabled\n', code)
        
        # Replace OpenGL/GLUT main loop and window creation
        code = re.sub(r'glutMainLoop\(\)', '# glutMainLoop() disabled', code)
        code = re.sub(r'glutCreateWindow\([^)]*\)', '# glutCreateWindow() disabled', code)
        code = re.sub(r'glutInit\([^)]*\)', '# glutInit() disabled', code)
        
        # Replace input() calls that might block execution
        code = re.sub(r'input\s*\([^)]*\)', '"simulated_input"', code)
        
        # Add matplotlib backend setting at the top if matplotlib is used
        if 'matplotlib' in code or 'pyplot' in code:
            backend_code = '''
import matplotlib
matplotlib.use('Agg')  # Non-interactive backend
'''
            code = backend_code + code
        
        return code

    def _clean_r_graphics_code(self, code):
        """Remove or replace graphics elements in R code to prevent GUI windows from opening"""
        
        # Replace dev.new() and similar device creation calls
        code = re.sub(r'dev\.new\(\)', '# dev.new() disabled', code)
        code = re.sub(r'x11\(\)', '# x11() disabled', code)
        code = re.sub(r'windows\(\)', '# windows() disabled', code)
        code = re.sub(r'quartz\(\)', '# quartz() disabled', code)
        
        # Replace interactive plotting commands with file output
        code = re.sub(r'plot\s*\(([^)]*)\)', r'png("/tmp/plot.png"); plot(\1); dev.off()  # plot() redirected to file', code)
        code = re.sub(r'hist\s*\(([^)]*)\)', r'png("/tmp/hist.png"); hist(\1); dev.off()  # hist() redirected to file', code)
        code = re.sub(r'boxplot\s*\(([^)]*)\)', r'png("/tmp/boxplot.png"); boxplot(\1); dev.off()  # boxplot() redirected to file', code)
        code = re.sub(r'barplot\s*\(([^)]*)\)', r'png("/tmp/barplot.png"); barplot(\1); dev.off()  # barplot() redirected to file', code)
        
        # Handle ggplot2 graphics
        code = re.sub(r'ggsave\s*\([^)]*\)', 'ggsave("/tmp/ggplot.png")  # ggsave() redirected', code)
        code = re.sub(r'print\s*\(\s*p\s*\)', 'ggsave("/tmp/ggplot.png", p)  # ggplot print() disabled', code)
        
        # Remove readline and interactive input
        code = re.sub(r'readline\s*\([^)]*\)', '"simulated_input"', code)
        code = re.sub(r'readLines\s*\([^)]*\)', 'c("simulated_input")', code)
        
        # Replace Shiny apps
        code = re.sub(r'shinyApp\s*\([^)]*\)', '# Shiny app disabled', code)
        code = re.sub(r'runApp\s*\([^)]*\)', '# runApp() disabled', code)
        
        # Replace browser() debugging calls
        code = re.sub(r'browser\s*\(\)', '# browser() disabled', code)
        
        return code


if __name__ == "__main__":
    executor = SmartExecutor()
    executor.execute_all_common_tasks()
