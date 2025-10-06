#!/usr/bin/env python3
"""
Enhanced Dependency Management for SWAM
Integrates with modern dependency analyzer for intelligent dependency resolution
"""

import subprocess
import sys
import re
import os
from pathlib import Path

# Import modern dependency analyzer
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
modules_path = os.path.join(project_root, 'modules')
sys.path.insert(0, modules_path)

from modules.modern_dependency_analyzer import ModernDependencyAnalyzer
from modules.modern_logger import get_logger


class EnhancedDependencyInstaller:
    """Enhanced dependency installer using modern analyzer"""
    
    def __init__(self):
        self.analyzer = ModernDependencyAnalyzer()
        self.logger = get_logger()
        
        # Enhanced dependency management with modern analyzer integration
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
                'installer': 'maven',  # Sistema automatico: crea pom.xml temporaneo e gestisce dipendenze
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
                'java.lang', 'java.util', 'java.io', 'java.math', 'java.text',
                'java.net', 'java.nio', 'java.security', 'java.sql', 'javax.swing',
                'javax.sql', 'javax.xml', 'org.w3c.dom', 'org.xml.sax'
            },
            'ruby': {
                'date', 'time', 'json', 'fileutils', 'pathname'
            }
        }
        
        # Mapping da import Java a coordinate Maven (groupId:artifactId)
        self.java_import_to_maven = {
            'com.google.gson': 'com.google.code.gson:gson:2.10.1',
            'org.apache.commons.lang3': 'org.apache.commons:commons-lang3:3.12.0',
            'org.apache.commons.io': 'commons-io:commons-io:2.11.0',
            'org.junit': 'junit:junit:4.13.2',
            'org.junit.jupiter': 'org.junit.jupiter:junit-jupiter:5.9.2',
            'com.fasterxml.jackson': 'com.fasterxml.jackson.core:jackson-core:2.15.2',
            'org.slf4j': 'org.slf4j:slf4j-api:2.0.7',
            'ch.qos.logback': 'ch.qos.logback:logback-classic:1.4.8',
            'org.apache.httpcomponents': 'org.apache.httpcomponents:httpclient:4.5.14',
            'com.squareup.okhttp3': 'com.squareup.okhttp3:okhttp:4.11.0',
            'org.springframework': 'org.springframework:spring-core:6.0.11',
            'com.mysql.cj.jdbc': 'mysql:mysql-connector-java:8.0.33',
            'org.postgresql': 'org.postgresql:postgresql:42.6.0',
            'redis.clients.jedis': 'redis.clients:jedis:4.4.3',
            'org.mongodb': 'org.mongodb:mongodb-driver-sync:4.10.2'
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
    
    def map_java_import_to_maven(self, java_import):
        """Mappa un import Java alla sua coordinata Maven"""
        # Cerca match esatto
        if java_import in self.java_import_to_maven:
            return self.java_import_to_maven[java_import]
        
        # Cerca match per prefisso (es: com.google.gson.JsonObject -> com.google.gson)
        for import_prefix, maven_coord in self.java_import_to_maven.items():
            if java_import.startswith(import_prefix):
                return maven_coord
        
        return None
    
    def create_temp_maven_project(self, dependencies):
        """Crea un progetto Maven temporaneo con le dipendenze specificate"""
        import tempfile
        import uuid
        
        # Crea directory temporanea
        temp_dir = Path(tempfile.gettempdir()) / f"swam_java_{uuid.uuid4().hex[:8]}"
        temp_dir.mkdir(exist_ok=True)
        
        # Crea struttura Maven
        src_main_java = temp_dir / "src" / "main" / "java"
        src_main_java.mkdir(parents=True, exist_ok=True)
        
        # Genera pom.xml
        pom_content = self.generate_pom_xml(dependencies)
        pom_file = temp_dir / "pom.xml"
        
        with open(pom_file, 'w', encoding='utf-8') as f:
            f.write(pom_content)
        
        return temp_dir, pom_file
    
    def generate_pom_xml(self, maven_dependencies):
        """Genera contenuto pom.xml con le dipendenze specificate"""
        dependencies_xml = ""
        
        for dep in maven_dependencies:
            # Parse groupId:artifactId:version
            parts = dep.split(':')
            if len(parts) >= 3:
                group_id, artifact_id, version = parts[0], parts[1], parts[2]
                dependencies_xml += f"""        <dependency>
            <groupId>{group_id}</groupId>
            <artifactId>{artifact_id}</artifactId>
            <version>{version}</version>
        </dependency>
"""
        
        pom_template = f"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <groupId>com.swam.temp</groupId>
    <artifactId>swam-java-deps</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>
    
    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
    
    <dependencies>
{dependencies_xml}    </dependencies>
    
    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <configuration>
                    <source>11</source>
                    <target>11</target>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>"""
        
        return pom_template
    
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
                
            elif language == 'java':
                # Usa il sistema Maven automatico
                return self.install_java_dependency(dependency)
                
            # Altri linguaggi richiedono setup più complesso
            return False
            
        except Exception as e:
            print(f"Errore installazione {dependency} per {language}: {e}")
            return False
    
    def install_java_dependency(self, java_import):
        """Installa dipendenza Java usando Maven temporaneo"""
        try:
            # Mappa import a coordinata Maven
            maven_coord = self.map_java_import_to_maven(java_import)
            if not maven_coord:
                print(f"  Mapping non trovato per: {java_import}")
                return False
            
            print(f" Mapped: {java_import} -> {maven_coord}")
            
            # Crea progetto Maven temporaneo
            temp_dir, pom_file = self.create_temp_maven_project([maven_coord])
            
            # Esegui Maven dependency:resolve
            cmd = ['mvn', 'dependency:resolve', '-f', str(pom_file)]
            result = subprocess.run(
                cmd, 
                capture_output=True, 
                timeout=300,  # 5 minuti per download
                cwd=temp_dir,
                text=True
            )
            
            success = result.returncode == 0
            
            if success:
                print(f"   ✅ Maven dependency resolved successfully")
                # Opzionale: copia JAR in una cache locale
                self._cache_maven_dependency(temp_dir, maven_coord)
            else:
                print(f"   ❌ Maven failed: {result.stderr[:200]}")
            
            # Cleanup del progetto temporaneo
            import shutil
            shutil.rmtree(temp_dir, ignore_errors=True)
            
            return success
            
        except subprocess.TimeoutExpired:
            print(f"  Timeout durante download Maven")
            return False
        except Exception as e:
            print(f" Errore Java dependency: {e}")
            return False
    
    def _cache_maven_dependency(self, temp_dir, maven_coord):
        """Opzionale: copia dependency in cache locale SWAM"""
        try:
            # Crea cache directory
            cache_dir = Path.home() / ".swam" / "java_deps"
            cache_dir.mkdir(parents=True, exist_ok=True)
            
            # Trova JAR nella repository locale Maven
            m2_repo = temp_dir / ".m2" / "repository"
            if not m2_repo.exists():
                m2_repo = Path.home() / ".m2" / "repository"
            
            # Cerca JAR files
            jar_files = list(m2_repo.rglob("*.jar"))
            
            for jar_file in jar_files:
                if jar_file.name not in ['maven-metadata-local.xml']:
                    # Copia in cache SWAM
                    cache_jar = cache_dir / jar_file.name
                    import shutil
                    shutil.copy2(jar_file, cache_jar)
                    print(f" Cached: {jar_file.name}")
                    
        except Exception as e:
            # Cache failure non è critico
            pass
    
    def install_all_dependencies(self, code, language):
        """Installa tutte le dipendenze rilevate nel codice"""
        dependencies = self.extract_dependencies(code, language)
        
        if not dependencies:
            return True, []
        
        print(f" Dipendenze rilevate per {language}: {dependencies}")
        
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
            print(f" Rilevato errore dipendenze per {language}")
            
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


# Compatibility alias for legacy code
AutoDependencyInstaller = EnhancedDependencyInstaller


if __name__ == "__main__":
    print(" AUTO DEPENDENCY INSTALLER")
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
