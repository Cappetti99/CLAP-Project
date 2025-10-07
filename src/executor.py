#!/usr/bin/env python3
"""
Enhanced Executor - Improved executor for the CO2 benchmark
Resolves path, compilation, and file management issues for all languages
"""

import sys
import os
import subprocess
import json
import re
import tempfile
import time
import shutil
from pathlib import Path
from datetime import datetime

# Adds the path to import SWAM modules
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

# Import modular components for enhanced functionality
try:
    from modules.language_config import LanguageConfigManager
    from modules.modern_logger import get_logger
    MODULAR_COMPONENTS_AVAILABLE = True
except ImportError:
    MODULAR_COMPONENTS_AVAILABLE = False

try:
    from src.carbon_tracker import start_carbon_tracking, stop_carbon_tracking
    CARBON_TRACKING_AVAILABLE = True
except ImportError:
    CARBON_TRACKING_AVAILABLE = False


class EnhancedExecutor:
    """Improved executor that resolves compilation and execution issues"""

    def __init__(self, session_id=None):
        self.results_dir = os.path.abspath("results/execution")
        self.temp_dir = os.path.join(self.results_dir, "temp")
        os.makedirs(self.temp_dir, exist_ok=True)

        # Use system commands directly
        print(" Using system commands directly")

        # Initialize modular components
        if MODULAR_COMPONENTS_AVAILABLE:
            self.logger = get_logger(f"enhanced_executor_{session_id or int(time.time())}")
            self.logger.info(f" Enhanced executor initialized with system commands")
        else:
            self.config_manager = None
            self.logger = None

        # Language configuration with direct commands
        self.language_config = {
            'python': {
                'extension': '.py',
                'executor': ['python'],
                'timeout': 10,
                'test_code': 'print("Hello from Python!")',
                'type': 'interpreted'
            },
            'javascript': {
                'extension': '.js',
                'executor': ['node'],
                'timeout': 10,
                'test_code': 'console.log("Hello from JavaScript!");',
                'type': 'interpreted'
            },
            'java': {
                'extension': '.java',
                'compiler': ['javac'],
                'executor': ['java'],
                'timeout': 30,
                'test_code': '''public class Test {
 public static void main(String[] args) {
 System.out.println("Hello from Java!");
 }
}''',
                'type': 'compiled',
                'needs_class_name': True
            },
            'ruby': {
                'extension': '.rb',
                'executor': ['ruby'],
                'timeout': 15,
                'test_code': 'puts "Hello from Ruby!"',
                'type': 'interpreted'
            },
            'php': {
                'extension': '.php',
                'executor': ['php'],
                'timeout': 15,
                'test_code': '<?php echo "Hello from PHP!\\n"; ?>',
                'type': 'interpreted'
            },
            'r': {
                'extension': '.r',
                'executor': ['Rscript'],
                'timeout': 15,
                'test_code': 'cat("Hello from R!\\n")',
                'type': 'interpreted'
            },
            'julia': {
                'extension': '.jl',
                'executor': ['julia'],
                'timeout': 20,
                'test_code': 'println("Hello from Julia!")',
                'type': 'interpreted'
            },
            'csharp': {
                'extension': '.cs',
                'compiler': ['mcs'],  # Mono C# compiler
                'executor': ['mono'],
                'executable_ext': '.exe',
                'timeout': 30,
                'test_code': '''using System;
class Test {
 static void Main() {
 Console.WriteLine("Hello from C#!");
 }
}''',
                'type': 'compiled'
            },
            'c': {
                'extension': '.c',
                'compiler': ['gcc'],
                'compiler_args': ['-o'],
                'timeout': 25,
                'test_code': '''#include <stdio.h>
int main() {
 printf("Hello from C!\\n");
 return 0;
}''',
                'type': 'compiled'
            },
            'cpp': {
                'extension': '.cpp',
                'compiler': ['g++'],
                'compiler_args': ['-o'],
                'timeout': 25,
                'test_code': '''#include <iostream>
int main() {
 std::cout << "Hello from C++!" << std::endl;
 return 0;
}''',
                'type': 'compiled'
            },
            'go': {
                'extension': '.go',
                'compiler': ['go', 'build'],
                'compiler_args': ['-o'],
                'timeout': 25,
                'test_code': '''package main
import "fmt"
func main() {
 fmt.Println("Hello from Go!")
}''',
                'type': 'compiled'
            },
            'rust': {
                'extension': '.rs',
                'compiler': ['rustc'],
                'compiler_args': ['-o'],
                'timeout': 30,
                'test_code': '''fn main() {
 println!("Hello from Rust!");
}''',
                'type': 'compiled'
            },
            'haskell': {
                'extension': '.hs',
                'compiler': ['ghc'],
                'compiler_args': ['-o'],
                'timeout': 30,
                'test_code': '''main = putStrLn "Hello from Haskell!"''',
                'type': 'compiled'
            },
            'ocaml': {
                'extension': '.ml',
                'compiler': ['ocamlc'],
                'compiler_args': ['-o'],
                'timeout': 25,
                'test_code': '''print_endline "Hello from OCaml!";;''',
                'type': 'compiled'
            },
            'typescript': {
                'extension': '.ts',
                'compiler': ['tsc'],
                'executor': ['node'],
                'timeout': 25,
                'test_code': '''console.log("Hello from TypeScript!");''',
                'type': 'transpiled'
            }
        }

        self.available_languages = {}
        self.detect_available_languages()

    def check_command_available(self, command):
        """Check whether a command is available in the system or environment conda"""
        try:
            # First try in the conda environment
            if self.conda_env and self.conda_env != 'base':
                result = subprocess.run(
                    ['conda', 'run', '-n', self.conda_env, 'which', command],
                    capture_output=True,
                    timeout=10,
                    text=True
                )
                if result.returncode == 0:
                    return True

            # Fallback: check in the standard PATH
            result = subprocess.run(
                ['which', command] if os.name != 'nt' else ['where', command],
                capture_output=True,
                timeout=5,
                text=True
            )
            return result.returncode == 0
        except:
            return False

    def get_safe_filename(self, task_name, language):
        """Generates a safe file name to avoid conflicts"""
        timestamp = int(time.time() * 1000)
        safe_task = re.sub(r'[^a-zA-Z0-9_]', '_', task_name)
        return f"{safe_task}_{language}_{timestamp}"

    def extract_java_class_name(self, code):
        """Extracts the Java class name from the code"""
        pattern = r'public\s+class\s+(\w+)'
        match = re.search(pattern, code)
        return match.group(1) if match else None

    def create_temp_file(self, code, extension, task_name, language):
        """Creates a temporary file with safe handling"""
        try:
            base_name = self.get_safe_filename(task_name, language)

            # Special handling for Java
            if language == 'java':
                class_name = self.extract_java_class_name(code)
                if class_name:
                    filename = f"{class_name}.java"
                else:
                    filename = f"{base_name}.java"
            else:
                filename = f"{base_name}{extension}"

            file_path = os.path.join(self.temp_dir, filename)

            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(code)

            return file_path

        except Exception as e:
            print(f" Error creating temporary file: {e}")
            return None

    def compile_code(self, file_path, config, language):
        """
        Compiles a code file if necessary
        """
        try:
            if 'compiler' not in config:
                return {'success': True, 'executable': None, 'error': ''}

            compiler = config['compiler']
            base_name = os.path.splitext(file_path)[0]

            # Build the compilation command
            if language == 'java':
                cmd_parts = compiler + [file_path]
                executable_path = None  # Java does not create a direct executable
            elif language == 'typescript':
                cmd_parts = compiler + [file_path]
                executable_path = base_name + '.js'
            elif language == 'go':
                if len(compiler) > 1 and compiler[1] == 'build':
                    # go build -o executable source.go
                    executable_path = base_name
                    cmd_parts = compiler + config.get('compiler_args', []) + [executable_path, file_path]
                else:
                    executable_path = base_name
                    cmd_parts = compiler + config.get('compiler_args', []) + [executable_path, file_path]
            else:
                # Other compiled languages
                executable_path = base_name + config.get('executable_ext', '')
                cmd_parts = compiler + config.get('compiler_args', []) + [executable_path, file_path]

            print(f" Compiling: {' '.join(str(x) for x in cmd_parts)}")

            result = subprocess.run(
                cmd_parts,
                capture_output=True,
                text=True,
                timeout=config['timeout'],
                cwd=os.path.dirname(file_path)
            )

            if result.returncode == 0:
                return {'success': True, 'executable': executable_path, 'error': ''}
            else:
                return {'success': False, 'executable': None, 'error': result.stderr}

        except subprocess.TimeoutExpired:
            return {'success': False, 'executable': None, 'error': f"Timeout during compilation ({config['timeout']}s)"}
        except Exception as e:
            return {'success': False, 'executable': None, 'error': f"Error during compilation: {str(e)}"}

    def run_code(self, file_path, config, language, executable_path=None):
        """
        Runs a code file in the specified language
        """
        try:
            if config['type'] == 'interpreted' or config['type'] == 'transpiled':
                # For interpreted languages, use the configured executor
                if 'run' in config:
                    cmd_parts = config['run'] + [file_path]
                else:
                    cmd_parts = config['executor'] + [file_path]
            elif config['type'] == 'compiled':
                # For compiled languages, run the executable
                if executable_path:
                    if language == 'java':
                        # Java needs the classpath
                        class_name = os.path.splitext(os.path.basename(file_path))[0]
                        cmd_parts = config['executor'] + ['-cp', os.path.dirname(file_path), class_name]
                    else:
                        cmd_parts = [executable_path]
                else:
                    return {'success': False, 'output': '', 'error': 'Executable not found'}

            print(f" Running: {' '.join(str(x) for x in cmd_parts)}")

            result = subprocess.run(
                cmd_parts,
                capture_output=True,
                text=True,
                timeout=config['timeout'],
                cwd=os.path.dirname(file_path)
            )

            success = result.returncode == 0
            output = result.stdout
            error = result.stderr

            return {'success': success, 'output': output, 'error': error}

        except subprocess.TimeoutExpired:
            return {'success': False, 'output': '', 'error': f"Timeout during execution ({config['timeout']}s)"}
        except Exception as e:
            return {'success': False, 'output': '', 'error': f"Error during execution: {str(e)}"}

    def test_language_execution(self, language, config):
        """Tests the execution of a language"""
        try:
            temp_file = self.create_temp_file(config['test_code'], config['extension'], 'test', language)
            if not temp_file:
                return False

            executable_path = None

            # Compilation if necessary
            if config['type'] in ['compiled', 'transpiled']:
                compile_result = self.compile_code(temp_file, config, language)
                if not compile_result['success']:
                    self.cleanup_temp_files([temp_file])
                    return False
                executable_path = compile_result['executable']

            # Execution
            run_result = self.run_code(temp_file, config, language, executable_path)
            success = run_result['success'] and "Hello from" in run_result.get('output', '')

            if not success:
                print(f" Test output: {run_result.get('output', '')[:50]}...")
                print(f" Test error: {run_result.get('error', '')[:50]}...")

            # Cleanup
            self.cleanup_temp_files([temp_file])
            if executable_path and os.path.exists(executable_path):
                try:
                    os.remove(executable_path)
                except:
                    pass

            return success

        except Exception as e:
            print(f" Errore test {language}: {e}")
            return False

    def detect_available_languages(self):
        """Detects available languages"""
        print(" Detecting available languages...")

        for language, config in self.language_config.items():
            print(f" Testing {language.upper()}... ", end="")

            # Extract base commands from configurations
            commands_to_check = []

            if config['type'] in ['compiled', 'transpiled']:
                # For compiled languages, extract the base command from the compiler
                compiler_cmd = config['compiler']
                if isinstance(compiler_cmd, list):
                    base_cmd = compiler_cmd[0] if len(compiler_cmd) > 0 else None
                    # If it's a conda command, extract the real command
                    if base_cmd == 'conda' and len(compiler_cmd) > 4:
                        base_cmd = compiler_cmd[4]
                    elif isinstance(base_cmd, str):
                        base_cmd = base_cmd.split()[0] if ' ' in base_cmd else base_cmd
                    commands_to_check.append(base_cmd)
                else:
                    commands_to_check.append(compiler_cmd)

            # Extract executor command
            if 'executor' in config:
                executor_cmd = config['executor']
                if isinstance(executor_cmd, list):
                    base_cmd = executor_cmd[0] if len(executor_cmd) > 0 else None
                    # If it's a conda command, extract the real command
                    if base_cmd == 'conda' and len(executor_cmd) > 4:
                        base_cmd = executor_cmd[4]
                    elif isinstance(base_cmd, str):
                        base_cmd = base_cmd.split()[0] if ' ' in base_cmd else base_cmd
                    commands_to_check.append(base_cmd)
                else:
                    commands_to_check.append(executor_cmd)

            # Remove duplicates and empty commands
            commands_to_check = [cmd for cmd in set(commands_to_check) if cmd and not cmd.startswith('-')]

            missing_commands = []
            for cmd in commands_to_check:
                if not self.check_command_available(cmd):
                    missing_commands.append(cmd)

            if missing_commands:
                print(f" command '{missing_commands[0]}' not found")
                continue

            # Execution test
            try:
                if self.test_language_execution(language, config):
                    self.available_languages[language] = config
                    print(" available")
                else:
                    print(" test failed")
            except Exception as e:
                print(f" error in test: {str(e)[:30]}...")

        print(f"\n Available languages: {len(self.available_languages)}")
        for lang in sorted(self.available_languages.keys()):
            print(f" • {lang.upper()}")

    def cleanup_temp_files(self, file_list):
        """Cleans up temporary files"""
        for file_path in file_list:
            try:
                if os.path.exists(file_path):
                    os.remove(file_path)
            except:
                pass

    def execute_code(self, code, language, task_name):
        """Executes the code for the benchmark"""
        if language not in self.available_languages:
            return {
                'success': False,
                'error': f'Language {language} not available',
                'output': '',
                'execution_time': 0
            }

        config = self.available_languages[language]
        start_time = time.time()

        try:
            # Create temporary file
            temp_file = self.create_temp_file(code, config['extension'], task_name, language)
            if not temp_file:
                return {
                    'success': False,
                    'error': 'Unable to create temporary file',
                    'output': '',
                    'execution_time': time.time() - start_time
                }

            executable_path = None

            # Compilation if necessary
            if config['type'] in ['compiled', 'transpiled']:
                compile_result = self.compile_code(temp_file, config, language)
                if not compile_result['success']:
                    self.cleanup_temp_files([temp_file])
                    return {
                        'success': False,
                        'error': f'Compilation error: {compile_result["error"]}',
                        'output': '',
                        'execution_time': time.time() - start_time
                    }
                executable_path = compile_result['executable']

            # Execution
            run_result = self.run_code(temp_file, config, language, executable_path)
            execution_time = time.time() - start_time

            # Cleanup
            files_to_clean = [temp_file]
            if executable_path and os.path.exists(executable_path):
                files_to_clean.append(executable_path)
            self.cleanup_temp_files(files_to_clean)

            return {
                'success': run_result['success'],
                'error': run_result.get('error', ''),
                'output': run_result.get('output', ''),
                'execution_time': execution_time
            }

        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'output': '',
                'execution_time': time.time() - start_time
            }
    
    # Modular integration methods
    def get_modular_config(self, language):
        """Get language config from modular system if available"""
        if self.config_manager:
            return self.config_manager.get_language_config(language)
        return None
    
    def get_modular_supported_languages(self):
        """Get supported languages from modular system"""
        if self.config_manager:
            return self.config_manager.get_all_supported_languages()
        return []
    
    def log_modular_execution(self, language, task_name, execution_time, success, error=None):
        """Log execution using modular logger if available"""
        if self.logger:
            self.logger.log_execution_end(language, task_name, execution_time, success, 
                                        error_type="ExecutionError" if error else None)
            if error:
                self.logger.log_error(language, task_name, "ExecutionError", str(error), "")


# Compatibility alias for new modular executor
ModernExecutor = EnhancedExecutor