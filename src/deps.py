#!/usr/bin/env python3
"""
Sistema di installazione automatica dipendenze per SWAM
Installa automaticamente librerie mancanti durante l'esecuzione
"""

import subprocess
import sys
import re
import os
from pathlib import Path

class AutoDependencyInstaller:
    """Installatore automatico di dipendenze per linguaggi multiple"""
    
    def __init__(self):
        self.supported_installers = {
            'python': {
                'installer': 'pip',
                'command': [sys.executable, '-m', 'pip', 'install'],
                'import_patterns': [
                    r'import\s+(\w+)',
                    r'from\s+(\w+)\s+import'
                ]
            },
            'javascript': {
                'installer': 'npm',
                'command': ['npm', 'install'],
                'import_patterns': [
                    r'require\([\'"]([^\'"]+)[\'"]\)',
                    r'import.*from\s+[\'"]([^\'"]+)[\'"]'
                ]
            },
            'java': {
                'installer': 'maven',  # Più complesso, richiederebbe pom.xml
                'command': ['mvn', 'dependency:resolve'],
                'import_patterns': [
                    r'import\s+([\w\.]+);'
                ]
            },
            'ruby': {
                'installer': 'gem',
                'command': ['gem', 'install'],
                'import_patterns': [
                    r'require\s+[\'"]([^\'"]+)[\'"]',
                    r'gem\s+[\'"]([^\'"]+)[\'"]'
                ]
            },
            'php': {
                'installer': 'composer',
                'command': ['composer', 'require'],
                'import_patterns': [
                    r'use\s+([\w\\]+);',
                    r'require_once\s+[\'"]([^\'"]+)[\'"]'
                ]
            },
            'r': {
                'installer': 'install.packages',
                'command': ['Rscript', '-e'],
                'import_patterns': [
                    r'library\((\w+)\)',
                    r'require\((\w+)\)'
                ]
            }
        }
        
        # Librerie standard che non vanno installate
        self.standard_libraries = {
            'python': {
                'os', 'sys', 'json', 'time', 'datetime', 'math', 're', 
                'collections', 'itertools', 'functools', 'pathlib'
            },
            'javascript': {
                'fs', 'path', 'http', 'https', 'util', 'crypto', 'os'
            },
            'java': {
                'java.lang', 'java.util', 'java.io', 'java.math'
            },
            'ruby': {
                'date', 'time', 'json', 'fileutils', 'pathname'
            }
        }
    
    def extract_dependencies(self, code, language):
        """Estrae dipendenze dal codice sorgente"""
        if language not in self.supported_installers:
            return []
        
        dependencies = set()
        patterns = self.supported_installers[language]['import_patterns']
        
        for pattern in patterns:
            matches = re.findall(pattern, code, re.MULTILINE)
            dependencies.update(matches)
        
        # Filtra librerie standard
        standard_libs = self.standard_libraries.get(language, set())
        external_deps = [dep for dep in dependencies if dep not in standard_libs]
        
        return external_deps
    
    def install_dependency(self, dependency, language):
        """Installa una singola dipendenza"""
        if language not in self.supported_installers:
            return False
        
        config = self.supported_installers[language]
        
        try:
            if language == 'python':
                cmd = config['command'] + [dependency]
                result = subprocess.run(cmd, capture_output=True, timeout=60)
                return result.returncode == 0
                
            elif language == 'javascript':
                cmd = config['command'] + [dependency]
                result = subprocess.run(cmd, capture_output=True, timeout=60)
                return result.returncode == 0
                
            elif language == 'ruby':
                cmd = config['command'] + [dependency]
                result = subprocess.run(cmd, capture_output=True, timeout=60)
                return result.returncode == 0
                
            elif language == 'r':
                r_cmd = f'install.packages("{dependency}", repos="https://cran.r-project.org")'
                cmd = config['command'] + [r_cmd]
                result = subprocess.run(cmd, capture_output=True, timeout=120)
                return result.returncode == 0
                
            # Altri linguaggi richiedono setup più complesso
            return False
            
        except Exception as e:
            print(f"Errore installazione {dependency} per {language}: {e}")
            return False
    
    def install_all_dependencies(self, code, language):
        """Installa tutte le dipendenze rilevate nel codice"""
        dependencies = self.extract_dependencies(code, language)
        
        if not dependencies:
            return True, []
        
        print(f"🔧 Dipendenze rilevate per {language}: {dependencies}")
        
        installed = []
        failed = []
        
        for dep in dependencies:
            print(f"   Installando {dep}...", end=' ')
            
            if self.install_dependency(dep, language):
                print("✅")
                installed.append(dep)
            else:
                print("❌")
                failed.append(dep)
        
        success = len(failed) == 0
        return success, {'installed': installed, 'failed': failed}


# Integrazione con SmartExecutor
class EnhancedSmartExecutor:
    """SmartExecutor con installazione automatica dipendenze"""
    
    def __init__(self):
        # Importa SmartExecutor originale
        from src.smart_executor import SmartExecutor
        self.base_executor = SmartExecutor()
        self.auto_installer = AutoDependencyInstaller()
        
    def execute_code_with_auto_install(self, code, language, task_name):
        """Esegue codice con installazione automatica dipendenze"""
        
        # Prima tentativo di esecuzione normale
        result = self.base_executor.execute_code(code, language, task_name)
        
        # Se fallisce per import/require errors, prova installazione automatica
        if not result['success'] and self.is_dependency_error(result['error']):
            print(f"🔧 Rilevato errore dipendenze per {language}")
            
            # Installa dipendenze automaticamente
            install_success, install_result = self.auto_installer.install_all_dependencies(code, language)
            
            if install_success:
                print(f"✅ Dipendenze installate, nuovo tentativo...")
                # Riprova esecuzione
                result = self.base_executor.execute_code(code, language, task_name)
                result['auto_install'] = install_result
            else:
                print(f"❌ Installazione dipendenze fallita: {install_result['failed']}")
                result['auto_install'] = install_result
        
        return result
    
    def is_dependency_error(self, error_message):
        """Determina se l'errore è dovuto a dipendenze mancanti"""
        dependency_indicators = [
            'ModuleNotFoundError',
            'ImportError',
            'cannot find package',
            'LoadError',
            'require error',
            'Class not found',
            'package does not exist'
        ]
        
        return any(indicator in error_message for indicator in dependency_indicators)


if __name__ == "__main__":
    print("🔧 AUTO DEPENDENCY INSTALLER")
    print("=" * 40)
    
    # Test del sistema
    installer = AutoDependencyInstaller()
    
    # Esempio Python
    python_code = """
import numpy as np
import pandas as pd
import requests

print("Testing auto installation")
"""
    
    deps = installer.extract_dependencies(python_code, 'python')
    print(f"Dipendenze Python rilevate: {deps}")
    
    # Test installazione (commenta se non vuoi installare davvero)
    # success, result = installer.install_all_dependencies(python_code, 'python')
    # print(f"Installazione: {success}, Risultato: {result}")
