#!/usr/bin/env python3
"""
Language Environment Tester - Test the availability of all programming languages
Check compilers, interpreters, and execution capabilities for 16 supported languages.
"""

import sys
import os
import subprocess
import tempfile
import json
import shutil
from pathlib import Path
from datetime import datetime


class LanguageTester:
    """
    Tests the availability and functionality of all languages supported by the CLAP system.

    # OOP: C++, C#, Java
    # Scripting: Python, Ruby, Javascript, typescript
    # "Imperative": C, GO, Rust, PHP
    # Functional: Haskell, Ocamel
    # Scientific: R, Matlab, Julia
    """

    def __init__(self):
        self.test_results = {}
        self.temp_dirs = []

        # Test configuration for each language
        self.test_programs = {
            # === OOP LANGUAGES ===
            'cpp': {
                'extension': '.cpp',
                'code': '''#include <iostream>
int main() {
    std::cout << "Hello from C++!" << std::endl;
    return 0;
}''',
                'run_cmd': ['./hello_cpp'],
                'compile_cmd': ['g++', '-o', 'hello_cpp'],
                'type': 'oop'
            },
            'java': {
                'extension': '.java',
                'code': '''public class HelloJava {
    public static void main(String[] args) {
        System.out.println("Hello from Java!");
    }
}''',
                'run_cmd': ['java', 'HelloJava'],
                'compile_cmd': ['javac'],
                'type': 'oop'
            },
            'csharp': {
                'extension': '.cs',
                'code': '''using System;
class Program {
    static void Main() {
        Console.WriteLine("Hello from C#!");
    }
}''',
                'run_cmd': ['mono', 'hello.exe'],
                'compile_cmd': ['mcs', '-out:hello.exe'],
                'type': 'oop'
            },

            # === SCRIPTING LANGUAGES ===
            'python': {
                'extension': '.py',
                'code': 'print("Hello from Python!")',
                'run_cmd': ['python'],
                'compile_cmd': None,
                'type': 'scripting'
            },
            'ruby': {
                'extension': '.rb',
                'code': 'puts "Hello from Ruby!"',
                'run_cmd': ['ruby'],
                'compile_cmd': None,
                'type': 'scripting'
            },
            'javascript': {
                'extension': '.js',
                'code': 'console.log("Hello from JavaScript!");',
                'run_cmd': ['node'],
                'compile_cmd': None,
                'type': 'scripting'
            },
            'typescript': {
                'extension': '.ts',
                'code': 'console.log("Hello from TypeScript!");',
                'run_cmd': ['node', 'hello_typescript.js'],
                'compile_cmd': ['tsc'],
                'type': 'scripting'
            },

            # === IMPERATIVE LANGUAGES ===
            'c': {
                'extension': '.c',
                'code': '''#include <stdio.h>
int main() {
    printf("Hello from C!\\n");
    return 0;
}''',
                'run_cmd': ['./hello_c'],
                'compile_cmd': ['gcc', '-o', 'hello_c'],
                'type': 'imperative'
            },
            'go': {
                'extension': '.go',
                'code': '''package main
import "fmt"
func main() {
    fmt.Println("Hello from Go!")
}''',
                'run_cmd': ['./hello_go'],
                'compile_cmd': ['go', 'build', '-o', 'hello_go'],
                'type': 'imperative'
            },
            'rust': {
                'extension': '.rs',
                'code': '''fn main() {
    println!("Hello from Rust!");
}''',
                'run_cmd': ['./hello_rust'],
                'compile_cmd': ['rustc', '-o', 'hello_rust'],
                'type': 'imperative'
            },
            'php': {
                'extension': '.php',
                'code': '<?php echo "Hello from PHP!\\n"; ?>',
                'run_cmd': ['php'],
                'compile_cmd': None,
                'type': 'imperative'
            },

            # === FUNCTIONAL LANGUAGES ===
            'haskell': {
                'extension': '.hs',
                'code': '''main :: IO ()
main = putStrLn "Hello from Haskell!"''',
                'run_cmd': ['./hello_haskell'],
                'compile_cmd': ['ghc', '-o', 'hello_haskell'],
                'type': 'functional'
            },
            'ocaml': {
                'extension': '.ml',
                'code': 'print_endline "Hello from OCaml!";;',
                'run_cmd': ['./hello_ocaml'],
                'compile_cmd': ['ocamlc', '-o', 'hello_ocaml'],
                'type': 'functional'
            },

            # === SCIENTIFIC LANGUAGES ===
            'r': {
                'extension': '.r',
                'code': 'cat("Hello from R!\\n")',
                'run_cmd': ['Rscript'],
                'compile_cmd': None,
                'type': 'scientific'
            },
            'matlab': {
                'extension': '.m',
                'code': 'fprintf("Hello from MATLAB!\\n");',
                'run_cmd': ['matlab', '-batch'],
                'compile_cmd': None,
                'type': 'scientific'
            },
            'julia': {
                'extension': '.jl',
                'code': 'println("Hello from Julia!")',
                'run_cmd': ['julia'],
                'compile_cmd': None,
                'type': 'scientific'
            }
        }

    def check_command_exists(self, command):
        """Check whether a command is available in the system"""
        try:
            result = subprocess.run(
                ['which', command] if os.name != 'nt' else ['where', command],
                capture_output=True,
                text=True,
                timeout=5
            )
            return result.returncode == 0
        except Exception:
            return False

    def create_test_file(self, language, config):
        """Create a test file for the specified language"""
        try:
            temp_dir = tempfile.mkdtemp(prefix=f"swam_test_{language}_")

            if language == 'java':
                filename = f"HelloJava{config['extension']}"
            else:
                filename = f"hello_{language}{config['extension']}"

            filepath = os.path.join(temp_dir, filename)
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(config['code'])

            self.temp_dirs.append(temp_dir)
            return filepath, temp_dir

        except Exception as e:
            print(f" Errore creazione file: {e}")
            return None, None

    def test_language(self, language):
        """
        Tests a single programming language.
        """
        config = self.test_programs[language]

        result = {
            'language': language,
            'type': config['type'],
            'available': False,
            'interpreter_found': False,
            'compiler_found': False,
            'compile_success': False,
            'run_success': False,
            'output': '',
            'error': '',
            'execution_time': 0
        }

        print(f" Testing {language.upper()} ({config['type']})...")

        if self._check_commands(config, result):
            return result

        filepath, temp_dir = self.create_test_file(language, config)
        if not filepath:
            result['error'] = "Unable to create test file"
            return result

        try:
            if not self._compile_if_needed(config, filepath, temp_dir, result):
                return result

            self._execute_program(language, config, filepath, temp_dir, result)

        except subprocess.TimeoutExpired:
            result['error'] = "Timeout during execution"
            print(f" Timeout")
        except Exception as e:
            result['error'] = f"Unexpected error: {str(e)}"
            print(f" Error: {e}")

        return result

    def _check_commands(self, config, result):
        """Check the availability of required commands"""
        run_command = config['run_cmd'][0]

        # For compiled languages that generate binaries, do not check for the existence of the binary
        # but only for the compiler
        if run_command.startswith('./'):
            result['interpreter_found'] = True  # The binary will be created by compilation
        else:
            result['interpreter_found'] = self.check_command_exists(run_command)
            if not result['interpreter_found']:
                result['error'] = f"Command '{run_command}' not found"
                print(f" {run_command} not available")
                return True

        if config['compile_cmd']:
            compile_command = config['compile_cmd'][0]
            result['compiler_found'] = self.check_command_exists(compile_command)
            if not result['compiler_found']:
                result['error'] = f"Compiler '{compile_command}' not found"
                print(f" {compile_command} not available")
                return True
        else:
            result['compiler_found'] = True

        return False

    def _compile_if_needed(self, config, filepath, temp_dir, result):
        """Compile the code if needed"""
        if not config['compile_cmd']:
            result['compile_success'] = True
            return True

        print(f" 🔨 Compiling...")
        compile_cmd = config['compile_cmd'] + [filepath]

        compile_result = subprocess.run(
            compile_cmd,
            capture_output=True,
            text=True,
            timeout=30,
            cwd=temp_dir
        )

        result['compile_success'] = compile_result.returncode == 0

        if result['compile_success']:
            print(f" Compilation successful")
            return True
        else:
            result['error'] = f"Compilation error: {compile_result.stderr[:200]}"
            print(f" Compilation failed")
            print(f" {compile_result.stderr[:100]}...")
            return False

    def _execute_program(self, language, config, filepath, temp_dir, result):
        """Execute the compiled or interpreted program"""
        print(f" Executing...")

        # For compiled languages that generate binaries
        if config['run_cmd'][0].startswith('./'):
            run_cmd = config['run_cmd']
        elif language == 'matlab':
            script_name = os.path.splitext(os.path.basename(filepath))[0]
            run_cmd = config['run_cmd'] + [script_name]
        else:
            run_cmd = config['run_cmd'] + [filepath]

        import time
        start_time = time.time()

        run_result = subprocess.run(
            run_cmd,
            capture_output=True,
            text=True,
            timeout=10,
            cwd=temp_dir
        )

        result['execution_time'] = round(time.time() - start_time, 3)
        result['run_success'] = run_result.returncode == 0
        result['output'] = run_result.stdout.strip()

        if result['run_success']:
            result['available'] = True
            print(f" Execution successful ({result['execution_time']}s): {result['output']}")
        else:
            result['error'] = f"Execution error: {run_result.stderr[:200]}"
            print(f" Execution failed")
            print(f" {run_result.stderr[:100]}...")

    def test_all_languages(self):
        """Execute the complete test of all supported languages"""
        print(" LANGUAGE ENVIRONMENT TESTER")
        print("=" * 50)
        print("Checking the availability of all languages...\n")

        by_type = {}
        for lang, config in self.test_programs.items():
            lang_type = config['type']
            if lang_type not in by_type:
                by_type[lang_type] = []
            by_type[lang_type].append(lang)

        for lang_type in ['oop', 'scripting', 'imperative', 'functional', 'scientific']:
            if lang_type in by_type:
                print(f" Testing {lang_type.upper()} languages:")
                for language in by_type[lang_type]:
                    result = self.test_language(language)
                    self.test_results[language] = result
                print()

        self.print_summary()
        self.save_results()
        self.cleanup()

    def print_summary(self):
        """Print a structured summary of the results"""
        print(" SUMMARY OF RESULTS")
        print("=" * 50)

        available = []
        unavailable = []
        by_type = {'oop': [], 'scripting': [], 'imperative': [], 'functional': [], 'scientific': []}

        for language, result in self.test_results.items():
            lang_type = self.test_programs[language]['type']

            if result['available']:
                available.append(language)
                by_type[lang_type].append((language, ''))
            else:
                unavailable.append(language)
                by_type[lang_type].append((language, ''))

        for lang_type, languages in by_type.items():
            if languages:
                print(f"\n🔹 {lang_type.upper()} LANGUAGES:")
                for lang, status in languages:
                    if status == '':
                        output = self.test_results[lang]['output'][:50]
                        exec_time = self.test_results[lang]['execution_time']
                        print(f" {status} {lang.upper()}: {output} ({exec_time}s)")
                    else:
                        error = self.test_results[lang]['error'][:60]
                        print(f" {status} {lang.upper()}: {error}...")

        total = len(self.test_programs)
        success_rate = len(available) / total * 100

        print(f"\n GENERAL STATISTICS:")
        print(f" Total languages tested: {total}")
        print(f" Available languages: {len(available)}")
        print(f" Unavailable languages: {len(unavailable)}")
        print(f" Success rate: {success_rate:.1f}%")

        if unavailable:
            print(f"\n INSTALLATION SUGGESTIONS:")
            missing_commands = set()
            for lang in unavailable:
                result = self.test_results[lang]
                config = self.test_programs[lang]

                if not result['interpreter_found'] and not config['run_cmd'][0].startswith('./'):
                    missing_commands.add(config['run_cmd'][0])
                if config['compile_cmd'] and not result['compiler_found']:
                    missing_commands.add(config['compile_cmd'][0])

            for cmd in sorted(missing_commands):
                if cmd == 'matlab':
                    print(f" {cmd}: Commercial license required")
                else:
                    # Platform-aware suggestions
                    if os.name == 'posix':  # Linux/Unix
                        if cmd in ['gcc', 'g++']:
                            print(f" {cmd}: sudo apt install build-essential")
                        elif cmd == 'javac':
                            print(f" {cmd}: sudo apt install openjdk-11-jdk")
                        elif cmd == 'node':
                            print(f" {cmd}: sudo apt install nodejs")
                        elif cmd == 'go':
                            print(f" {cmd}: sudo apt install golang-go")
                        elif cmd == 'rustc':
                            print(f" {cmd}: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
                        elif cmd == 'ghc':
                            print(f" {cmd}: sudo apt install ghc")
                        elif cmd == 'ocamlc':
                            print(f" {cmd}: sudo apt install ocaml")
                        elif cmd == 'Rscript':
                            print(f" {cmd}: sudo apt install r-base")
                        elif cmd == 'julia':
                            print(f" {cmd}: sudo snap install julia --classic")
                        else:
                            print(f" {cmd}: sudo apt install {cmd}")
                    else:  # macOS or others
                        print(f" {cmd}: brew install {cmd}")

    def save_results(self):
        """Save results in JSON format with timestamp"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = f"results/execution/language_test_results_{timestamp}.json"

        os.makedirs("results/execution", exist_ok=True)

        save_data = {
            'timestamp': timestamp,
            'total_languages': len(self.test_programs),
            'available_count': sum(1 for r in self.test_results.values() if r['available']),
            'results': self.test_results
        }

        try:
            with open(results_file, 'w', encoding='utf-8') as f:
                json.dump(save_data, f, indent=2)
            print(f"\n Results saved in: {results_file}")
        except Exception as e:
            print(f"\n Error saving: {e}")

    def cleanup(self):
        """Clean up all temporary files created during tests"""
        cleaned = 0
        for temp_dir in self.temp_dirs:
            try:
                shutil.rmtree(temp_dir)
                cleaned += 1
            except Exception as e:
                print(f" Errore cleanup {temp_dir}: {e}")

        if cleaned > 0:
            print(f" Cleaned {cleaned} temporary directories")


if __name__ == "__main__":
    tester = LanguageTester()
    tester.test_all_languages()