from datasets import load_dataset
import subprocess
import os
import json
import re
import logging
from datetime import datetime
from tqdm import tqdm
import time

TIMEOUT_SECONDS = 30  # Ridotto per velocizzare
MAX_SNIPPETS_PER_LANGUAGE = 25  # Ridotto per esecuzione più veloce
COMPILATION_TIMEOUT = 60  # Ridotto per velocizzare

# Definisco i colori ANSI per il terminale
class Colors:
    # Colori di base
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    MAGENTA = '\033[95m'
    CYAN = '\033[96m'
    WHITE = '\033[97m'
    ORANGE = '\033[38;5;208m'
    PURPLE = '\033[38;5;129m'
    PINK = '\033[38;5;213m'
    LIME = '\033[38;5;154m'
    TEAL = '\033[38;5;80m'
    
    # Stili
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    ITALIC = '\033[3m'
    
    # Reset
    END = '\033[0m'
    
    # Background colors
    BG_RED = '\033[101m'
    BG_GREEN = '\033[102m'
    BG_YELLOW = '\033[103m'
    BG_BLUE = '\033[104m'
    BG_MAGENTA = '\033[105m'
    BG_CYAN = '\033[106m'

# Mapping colori per categorie
CATEGORY_COLORS = {
    "OOP": Colors.BLUE + Colors.BOLD,
    "Scripting": Colors.GREEN + Colors.BOLD,
    "Imperative": Colors.RED + Colors.BOLD,
    "Functional": Colors.MAGENTA + Colors.BOLD,
    "Scientific": Colors.CYAN + Colors.BOLD
}

# Mapping colori per linguaggi specifici
LANGUAGE_COLORS = {
    # OOP
    "C++": Colors.BLUE,
    "C#": Colors.PURPLE,
    "Java": Colors.ORANGE,
    
    # Scripting
    "Python": Colors.YELLOW,
    "Ruby": Colors.RED,
    "Javascript": Colors.LIME,
    "TypeScript": Colors.BLUE,
    
    # Imperative
    "C": Colors.CYAN,
    "Go": Colors.TEAL,
    "Rust": Colors.ORANGE,
    "PHP": Colors.PURPLE,
    
    # Functional
    "Haskell": Colors.MAGENTA,
    "OCaml": Colors.PINK,
    
    # Scientific
    "R": Colors.CYAN,
    "MATLAB": Colors.BLUE,
    "Julia": Colors.PURPLE
}

# Colori per stati
STATUS_COLORS = {
    "SUCCESS": Colors.GREEN + Colors.BOLD,
    "ERROR": Colors.RED + Colors.BOLD,
    "WARNING": Colors.YELLOW + Colors.BOLD,
    "INFO": Colors.CYAN,
    "DEBUG": Colors.WHITE,
    "FILTERED": Colors.ORANGE,
    "TIMEOUT": Colors.MAGENTA,
    "COMPILATION": Colors.RED,
    "DEPENDENCY": Colors.YELLOW
}

def colored_print(text, color=Colors.WHITE, style="", end="\n"):
    """Stampa testo colorato nel terminale"""
    print(f"{style}{color}{text}{Colors.END}", end=end)

def get_language_color(language):
    """Ottiene il colore per un linguaggio specifico"""
    return LANGUAGE_COLORS.get(language, Colors.WHITE)

def get_category_color(category):
    """Ottiene il colore per una categoria"""
    return CATEGORY_COLORS.get(category, Colors.WHITE)

def get_status_color(status):
    """Ottiene il colore per uno stato"""
    return STATUS_COLORS.get(status.upper(), Colors.WHITE)

# Setup logging con formatter colorato
log_dir = "logs"
if not os.path.exists(log_dir):
    os.makedirs(log_dir)

class ColoredFormatter(logging.Formatter):
    """Formatter personalizzato per log colorati"""
    
    COLORS = {
        'DEBUG': Colors.WHITE,
        'INFO': Colors.CYAN,
        'WARNING': Colors.YELLOW,
        'ERROR': Colors.RED,
        'CRITICAL': Colors.RED + Colors.BOLD
    }

    def format(self, record):
        # Salva il messaggio originale
        original_msg = record.msg
        
        # Colora in base al livello
        color = self.COLORS.get(record.levelname, Colors.WHITE)
        record.msg = f"{color}{record.msg}{Colors.END}"
        
        # Usa il formatter base
        formatted = super().format(record)
        
        # Ripristina il messaggio originale
        record.msg = original_msg
        
        return formatted

# Configuro il logger con colori
console_handler = logging.StreamHandler()
console_handler.setFormatter(ColoredFormatter('%(asctime)s - %(levelname)s - %(message)s'))

file_handler = logging.FileHandler(os.path.join(log_dir, f'execution_{datetime.now().strftime("%Y%m%d_%H%M%S")}.log'))
file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))

logging.basicConfig(
    level=logging.INFO,
    handlers=[file_handler, console_handler]
)
logger = logging.getLogger(__name__)

# File per tracciare errori e statistiche
error_log_file = os.path.join(log_dir, "error_analysis.json")
success_stats_file = os.path.join(log_dir, "success_stats.json")

# Definisco i linguaggi organizzati per categorie con gestione dipendenze
language_categories = {
    "OOP": {
        "C++": {
            "ext": ".cpp", 
            "compile_cmd": ["g++", "-std=c++20"], 
            "run_cmd": "./",
            "install_deps": None,
            "common_includes": ["#include <iostream>", "#include <vector>", "#include <string>", "#include <algorithm>"]
        },
        "C#": {
            "ext": ".cs", 
            "compile_cmd": ["csc"], 
            "run_cmd": "mono ",
            "install_deps": None,
            "common_includes": ["using System;"]
        },
        "Java": {
            "ext": ".java", 
            "compile_cmd": ["javac"], 
            "run_cmd": "java ",
            "install_deps": None,
            "common_includes": ["import java.util.*;", "import java.io.*;"]
        }
    },
    "Scripting": {
        "Python": {
            "ext": ".py", 
            "compile_cmd": None, 
            "run_cmd": "conda run -n SWAM python ",
            "install_deps": "conda run -n SWAM pip install",
            "common_imports": ["import sys", "import os", "import re", "import math", "import random"]
        },
        "Ruby": {
            "ext": ".rb", 
            "compile_cmd": None, 
            "run_cmd": "ruby ",
            "install_deps": "gem install",
            "common_imports": ["require 'json'"]
        },
        "Javascript": {
            "ext": ".js", 
            "compile_cmd": None, 
            "run_cmd": "conda run -n SWAM node ",
            "install_deps": "conda run -n SWAM npm install -g",
            "common_imports": []
        },
        "TypeScript": {
            "ext": ".ts", 
            "compile_cmd": ["conda", "run", "-n", "SWAM", "tsc"], 
            "run_cmd": "conda run -n SWAM node ",
            "install_deps": "conda run -n SWAM npm install -g typescript",
            "common_imports": []
        }
    },
    "Imperative": {
        "C": {
            "ext": ".c", 
            "compile_cmd": ["gcc", "-std=c99"], 
            "run_cmd": "./",
            "install_deps": None,
            "common_includes": ["#include <stdio.h>", "#include <stdlib.h>", "#include <string.h>"]
        },
        "Go": {
            "ext": ".go", 
            "compile_cmd": ["conda", "run", "-n", "SWAM", "go", "build"], 
            "run_cmd": "./",
            "install_deps": "conda run -n SWAM go get",
            "common_imports": ["import \"fmt\""]
        },
        "Rust": {
            "ext": ".rs", 
            "compile_cmd": ["conda", "run", "-n", "SWAM", "rustc"], 
            "run_cmd": "./",
            "install_deps": "conda run -n SWAM cargo install",
            "common_imports": []
        },
        "PHP": {
            "ext": ".php", 
            "compile_cmd": None, 
            "run_cmd": "php ",
            "install_deps": None,
            "common_imports": ["<?php"]
        }
    },
    "Functional": {
        "Haskell": {
            "ext": ".hs", 
            "compile_cmd": ["ghc"], 
            "run_cmd": "./",
            "install_deps": "cabal install",
            "common_imports": ["import Data.List"]
        },
        "OCaml": {
            "ext": ".ml", 
            "compile_cmd": ["ocamlfind", "ocamlc"], 
            "run_cmd": "./",
            "install_deps": "opam install",
            "common_imports": []
        }
    },
    "Scientific": {
        "R": {
            "ext": ".r", 
            "compile_cmd": None, 
            "run_cmd": "conda run -n SWAM Rscript ",
            "install_deps": "conda run -n SWAM R -e 'install.packages'",
            "common_imports": []
        },
        "MATLAB": {
            "ext": ".m", 
            "compile_cmd": None, 
            "run_cmd": "octave ",
            "install_deps": None,
            "common_imports": []
        },
        "Julia": {
            "ext": ".jl", 
            "compile_cmd": None, 
            "run_cmd": "julia ",
            "install_deps": "julia -e 'using Pkg; Pkg.add'",
            "common_imports": []
        }
    }
}

# Statistiche globali
global_stats = {
    "total_files": 0,
    "successful_executions": 0,
    "compilation_errors": 0,
    "runtime_errors": 0,
    "timeout_errors": 0,
    "dependency_errors": 0,
    "filtered_files": 0,
    "by_language": {}
}

def log_error(language, task_name, error_type, error_message, file_path, code_snippet=""):
    """Logga un errore nel sistema di tracking con colori"""
    error_entry = {
        "timestamp": datetime.now().isoformat(),
        "language": language,
        "task_name": task_name,
        "error_type": error_type,
        "error_message": str(error_message),
        "file_path": file_path,
        "code_snippet": code_snippet[:500] if code_snippet else ""  # Primi 500 caratteri
    }
    
    # Carica errori esistenti
    errors = []
    if os.path.exists(error_log_file):
        try:
            with open(error_log_file, 'r') as f:
                errors = json.load(f)
        except:
            errors = []
    
    errors.append(error_entry)
    
    # Salva errori aggiornati
    with open(error_log_file, 'w') as f:
        json.dump(errors, f, indent=2)
    
    # Log colorato
    lang_color = get_language_color(language)
    status_color = get_status_color(error_type)
    
    colored_print(f"❌ {language}", lang_color, Colors.BOLD, end="")
    colored_print(f" [{task_name}] ", Colors.WHITE, end="")
    colored_print(f"{error_type}: {error_message}", status_color)

def extract_dependencies(code, language):
    """Estrae le dipendenze dal codice"""
    dependencies = set()
    
    if language == "Python":
        # Pattern per import Python
        import_patterns = [
            r"^import\s+([a-zA-Z_][a-zA-Z0-9_]*)",
            r"^from\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+import",
            r"import\s+([a-zA-Z_][a-zA-Z0-9_]*)"
        ]
        for pattern in import_patterns:
            matches = re.findall(pattern, code, re.MULTILINE)
            dependencies.update(matches)
    
    elif language in ["Javascript", "TypeScript"]:
        # Pattern per require/import JS
        patterns = [
            r"require\(['\"]([^'\"]+)['\"]\)",
            r"import.*from\s+['\"]([^'\"]+)['\"]"
        ]
        for pattern in patterns:
            matches = re.findall(pattern, code)
            dependencies.update(matches)
    
    elif language == "Ruby":
        # Pattern per require Ruby
        matches = re.findall(r"require\s+['\"]([^'\"]+)['\"]", code)
        dependencies.update(matches)
    
    elif language in ["C", "C++"]:
        # Pattern per #include
        matches = re.findall(r"#include\s*[<\"]([^>\"]+)[>\"]", code)
        dependencies.update(matches)
    
    return dependencies

def install_dependencies(language, dependencies, lang_config):
    """Tenta di installare le dipendenze automaticamente"""
    install_cmd = lang_config.get("install_deps")
    if not install_cmd or not dependencies:
        return True
    
    # Lista di pacchetti problematici da evitare
    problematic_packages = {
        "Python": ["tkinter", "turtle", "pygame", "numpy", "scipy", "matplotlib", "pandas"],
        "Javascript": ["fs", "path", "os", "crypto"],  # moduli built-in
        "Ruby": ["tk", "socket"],
        "R": ["base", "stats", "graphics"]
    }
    
    safe_deps = []
    for dep in dependencies:
        if dep not in problematic_packages.get(language, []):
            safe_deps.append(dep)
    
    if not safe_deps:
        return True
    
    lang_color = get_language_color(language)
    colored_print(f"📦 Installazione dipendenze per ", Colors.CYAN, end="")
    colored_print(f"{language}", lang_color, Colors.BOLD, end="")
    colored_print(f": {safe_deps}", Colors.CYAN)
    
    try:
        if language == "Python":
            for dep in safe_deps:
                result = subprocess.run(
                    ["pip3", "install", dep],
                    capture_output=True,
                    text=True,
                    timeout=60
                )
                if result.returncode == 0:
                    colored_print(f"  ✅ {dep} installato", Colors.GREEN)
                else:
                    colored_print(f"  ❌ {dep} fallito: {result.stderr[:100]}", Colors.RED)
        # Aggiungi altri linguaggi se necessario
        return True
    except Exception as e:
        colored_print(f"  ❌ Installazione fallita: {e}", Colors.RED)
        return False

def should_filter_code(code, language):
    """Determina se un codice dovrebbe essere filtrato per problemi noti"""
    
    # Pattern problematici per linguaggio (più aggressivi per velocizzare)
    problematic_patterns = {
        "Python": [
            r"import\s+tkinter",
            r"import\s+turtle", 
            r"import\s+pygame",
            r"from\s+tkinter",
            r"input\s*\(",  # Rimesso per evitare blocchi
            r"while\s+True:",  # Evita loop infiniti
            r"for.*in.*range\(\s*\d{6,}"  # Evita loop molto lunghi
        ],
        "Javascript": [
            r"document\.",
            r"window\.",
            r"prompt\s*\(",  # Rimesso per evitare blocchi
            r"while\s*\(\s*true\s*\)"  # Evita loop infiniti
        ],
        "Java": [
            r"JFrame",
            r"javax\.swing",
            r"Scanner.*System\.in",  # Rimesso per evitare blocchi
            r"while\s*\(\s*true\s*\)"  # Evita loop infiniti
        ],
        "C++": [
            r"getch\s*\(",
            r"cin\s*>>",  # Rimesso per evitare blocchi
            r"while\s*\(\s*1\s*\)|while\s*\(\s*true\s*\)"  # Evita loop infiniti
        ],
        "C": [
            r"getch\s*\(",
            r"scanf\s*\(",  # Rimesso per evitare blocchi
            r"while\s*\(\s*1\s*\)"  # Evita loop infiniti
        ]
    }
    
    patterns = problematic_patterns.get(language, [])
    for pattern in patterns:
        if re.search(pattern, code, re.IGNORECASE):
            return True, f"Codice filtrato: contiene pattern problematico '{pattern}'"
    
    # Filtra codici troppo corti o troppo lunghi (limiti più restrittivi per velocità)
    if len(code.strip()) < 10:  # Aumentato da 5 a 10
        return True, "Codice troppo corto"
    
    if len(code) > 5000:  # Ridotto da 20KB a 5KB per velocità
        return True, "Codice troppo lungo"
    
    # Filtra codici con troppe dipendenze esterne
    if language in ["C++", "C"]:
        include_count = len(re.findall(r"#include\s*[<\"]([^>\"]+)[>\"]", code))
        if include_count > 10:
            return True, "Troppe dipendenze (include)"
    
    return False, ""

def update_stats(language, status):
    """Aggiorna le statistiche globali"""
    global_stats["total_files"] += 1
    
    if language not in global_stats["by_language"]:
        global_stats["by_language"][language] = {
            "total": 0,
            "successful": 0,
            "failed": 0,
            "filtered": 0
        }
    
    global_stats["by_language"][language]["total"] += 1
    
    if status == "success":
        global_stats["successful_executions"] += 1
        global_stats["by_language"][language]["successful"] += 1
    elif status == "compilation_error":
        global_stats["compilation_errors"] += 1
        global_stats["by_language"][language]["failed"] += 1
    elif status == "runtime_error":
        global_stats["runtime_errors"] += 1
        global_stats["by_language"][language]["failed"] += 1
    elif status == "timeout":
        global_stats["timeout_errors"] += 1
        global_stats["by_language"][language]["failed"] += 1
    elif status == "dependency_error":
        global_stats["dependency_errors"] += 1
        global_stats["by_language"][language]["failed"] += 1
    elif status == "filtered":
        global_stats["filtered_files"] += 1
        global_stats["by_language"][language]["filtered"] += 1

def save_stats():
    """Salva le statistiche su file con output colorato"""
    with open(success_stats_file, 'w') as f:
        json.dump(global_stats, f, indent=2)
    
    colored_print("\n" + "="*60, Colors.CYAN, Colors.BOLD)
    colored_print("📊 STATISTICHE FINALI", Colors.CYAN, Colors.BOLD)
    colored_print("="*60, Colors.CYAN, Colors.BOLD)
    
    # Statistiche globali
    colored_print(f"📈 File totali processati: ", Colors.WHITE, end="")
    colored_print(f"{global_stats['total_files']}", Colors.BOLD)
    
    colored_print(f"✅ Esecuzioni riuscite: ", Colors.GREEN, end="")
    colored_print(f"{global_stats['successful_executions']}", Colors.GREEN, Colors.BOLD)
    
    colored_print(f"🔧 Errori di compilazione: ", Colors.RED, end="")
    colored_print(f"{global_stats['compilation_errors']}", Colors.RED, Colors.BOLD)
    
    colored_print(f"💥 Errori di runtime: ", Colors.RED, end="")
    colored_print(f"{global_stats['runtime_errors']}", Colors.RED, Colors.BOLD)
    
    colored_print(f"⏰ Timeout: ", Colors.MAGENTA, end="")
    colored_print(f"{global_stats['timeout_errors']}", Colors.MAGENTA, Colors.BOLD)
    
    colored_print(f"📦 Errori dipendenze: ", Colors.YELLOW, end="")
    colored_print(f"{global_stats['dependency_errors']}", Colors.YELLOW, Colors.BOLD)
    
    colored_print(f"🚫 File filtrati: ", Colors.ORANGE, end="")
    colored_print(f"{global_stats['filtered_files']}", Colors.ORANGE, Colors.BOLD)
    
    # Statistiche per linguaggio
    colored_print(f"\n📋 DETTAGLI PER LINGUAGGIO:", Colors.CYAN, Colors.BOLD)
    colored_print("-"*60, Colors.CYAN)
    
    for lang, stats in global_stats['by_language'].items():
        if stats['total'] > 0:
            lang_color = get_language_color(lang)
            success_rate = (stats['successful'] / stats['total']) * 100
            
            colored_print(f"🔹 {lang}", lang_color, Colors.BOLD, end="")
            colored_print(f" | Totale: {stats['total']} | ", Colors.WHITE, end="")
            colored_print(f"✅ {stats['successful']} | ", Colors.GREEN, end="")
            colored_print(f"❌ {stats['failed']} | ", Colors.RED, end="")
            colored_print(f"🚫 {stats['filtered']} | ", Colors.ORANGE, end="")
            
            if success_rate >= 80:
                rate_color = Colors.GREEN
            elif success_rate >= 50:
                rate_color = Colors.YELLOW
            else:
                rate_color = Colors.RED
                
            colored_print(f"📊 {success_rate:.1f}%", rate_color, Colors.BOLD)
    
    colored_print("="*60, Colors.CYAN, Colors.BOLD)

colored_print("🚀 Sistema di logging e gestione errori inizializzato...", Colors.GREEN, Colors.BOLD)
colored_print(f"⚙️ Configurazione: ", Colors.CYAN, end="")
colored_print(f"Max {MAX_SNIPPETS_PER_LANGUAGE} snippet per linguaggio", Colors.CYAN, Colors.BOLD, end="")
colored_print(f" | Timeout esecuzione: {TIMEOUT_SECONDS}s", Colors.CYAN, end="")
colored_print(f" | Timeout compilazione: {COMPILATION_TIMEOUT}s", Colors.CYAN)
colored_print(f"� Configurazione veloce: filtri aggressivi e timeout ridotti", Colors.YELLOW)

# Creo una lista piatta di tutti i linguaggi per facilità di accesso
all_languages = {}
for category, languages in language_categories.items():
    for lang, config in languages.items():
        all_languages[lang] = {
            **config,
            "category": category,
            "folder": os.path.join(category.lower(), lang.lower())
        }

colored_print("📊 Carico il dataset...", Colors.CYAN, Colors.BOLD)
ds = load_dataset("christopher/rosetta-code")
df = ds["train"].to_pandas()
colored_print(f"✅ Dataset caricato: ", Colors.GREEN, end="")
colored_print(f"{len(df)} righe", Colors.GREEN, Colors.BOLD)

# Analizzo le colonne del dataset
colored_print(f"📋 Colonne del dataset: ", Colors.CYAN, end="")
colored_print(f"{list(df.columns)}", Colors.WHITE)
colored_print(f"🔍 Prime 3 righe del dataset:", Colors.CYAN)
print(df.head(3))

# Trovo i task comuni tra tutti i linguaggi
colored_print("\n🔍 Analizzo i task comuni tra i linguaggi...", Colors.CYAN, Colors.BOLD)
target_languages = list(all_languages.keys())
colored_print(f"🎯 Linguaggi target: ", Colors.CYAN, end="")

# Stampa linguaggi colorati
for i, lang in enumerate(target_languages):
    if i > 0:
        colored_print(", ", Colors.WHITE, end="")
    lang_color = get_language_color(lang)
    colored_print(f"{lang}", lang_color, Colors.BOLD, end="")
colored_print("")

# Per ogni linguaggio, trovo i task disponibili
tasks_per_language = {}
for lang in target_languages:
    lang_df = df[df["language_name"] == lang]
    tasks = set(lang_df["task_name"].unique()) if "task_name" in df.columns else set()
    tasks_per_language[lang] = tasks
    
    lang_color = get_language_color(lang)
    colored_print(f"📊 {lang}: ", lang_color, Colors.BOLD, end="")
    colored_print(f"{len(tasks)} task unici", Colors.WHITE)

# Trovo l'intersezione di tutti i task
if tasks_per_language and any(tasks_per_language.values()):
    common_tasks = set.intersection(*[tasks for tasks in tasks_per_language.values() if tasks])
    colored_print(f"\n🎯 Task comuni a tutti i linguaggi: ", Colors.GREEN, Colors.BOLD, end="")
    colored_print(f"{len(common_tasks)}", Colors.GREEN, Colors.BOLD)
    colored_print(f"📝 Primi 10 task comuni: ", Colors.CYAN, end="")
    colored_print(f"{list(common_tasks)[:10]}", Colors.WHITE)
else:
    colored_print("⚠️ Non è stata trovata la colonna 'task_name' nel dataset o nessun task comune", Colors.YELLOW, Colors.BOLD)
    common_tasks = set()

# Creo le cartelle per le categorie e i linguaggi
colored_print("\n📁 Creo le cartelle per le categorie e i linguaggi...", Colors.CYAN, Colors.BOLD)
for category, languages in language_categories.items():
    category_folder = category.lower()
    category_color = get_category_color(category)
    
    if not os.path.exists(category_folder):
        os.makedirs(category_folder)
        colored_print(f"  📂 Cartella categoria creata: ", Colors.GREEN, end="")
        colored_print(f"{category_folder}", category_color, Colors.BOLD)
    
    for lang in languages.keys():
        lang_config = all_languages[lang]
        lang_folder = lang_config["folder"]
        lang_color = get_language_color(lang)
        
        if not os.path.exists(lang_folder):
            os.makedirs(lang_folder)
            colored_print(f"    📄 Cartella linguaggio creata: ", Colors.GREEN, end="")
            colored_print(f"{lang_folder}", lang_color, Colors.BOLD)
        else:
            colored_print(f"    ✅ Cartella già esistente: ", Colors.YELLOW, end="")
            colored_print(f"{lang_folder}", lang_color, Colors.BOLD)

# Estraggo snippet per ogni linguaggio, limitandoli ai task comuni e applicando filtri
colored_print("\n🎯 Estrazione e filtraggio snippet...", Colors.CYAN, Colors.BOLD)
all_codes = {}
for language, lang_config in all_languages.items():
    lang_color = get_language_color(language)
    colored_print(f"\n🔍 Filtro i codici in ", Colors.CYAN, end="")
    colored_print(f"{language}", lang_color, Colors.BOLD)
    
    lang_df = df[df["language_name"] == language]
    
    # Se abbiamo task comuni, filtro solo quelli, altrimenti prendo tutti
    if common_tasks and "task_name" in df.columns and len(common_tasks) > 10:
        lang_df = lang_df[lang_df["task_name"].isin(common_tasks)]
        colored_print(f"  📊 Filtrati per task comuni: ", Colors.GREEN, end="")
        colored_print(f"{len(lang_df)} snippet", Colors.GREEN, Colors.BOLD)
    else:
        colored_print(f"  📊 Uso tutti i task disponibili: ", Colors.CYAN, end="")
        colored_print(f"{len(lang_df)} snippet", Colors.CYAN, Colors.BOLD)
    
    # Prendo al massimo MAX_SNIPPETS_PER_LANGUAGE snippet
    lang_df = lang_df.head(MAX_SNIPPETS_PER_LANGUAGE)
    colored_print(f"  📝 Snippet candidati: ", Colors.CYAN, end="")
    colored_print(f"{len(lang_df)}", Colors.CYAN, Colors.BOLD)
    
    codes = []
    tasks = []
    filtered_count = 0
    
    for idx, row in lang_df.iterrows():
        code = row["code"]
        task_name = row.get("task_name", f"task_{len(codes)}")
        
        # Applica filtri per problemi noti
        should_filter, filter_reason = should_filter_code(code, language)
        if should_filter:
            colored_print(f"    🚫 Filtrato snippet per task '{task_name}': ", Colors.ORANGE, end="")
            colored_print(f"{filter_reason}", Colors.ORANGE)
            log_error(language, task_name, "FILTERED", filter_reason, "", code[:200])
            update_stats(language, "filtered")
            filtered_count += 1
            continue
        
        # Estrai e controlla dipendenze
        dependencies = extract_dependencies(code, language)
        if dependencies:
            colored_print(f"    📦 Dipendenze rilevate per task '{task_name}': ", Colors.YELLOW, end="")
            colored_print(f"{dependencies}", Colors.YELLOW)
            deps_ok = install_dependencies(language, dependencies, lang_config)
            if not deps_ok:
                log_error(language, task_name, "DEPENDENCY_ERROR", f"Fallita installazione dipendenze: {dependencies}", "", code[:200])
                update_stats(language, "dependency_error")
                continue
        
        codes.append(code)
        tasks.append(task_name)
        colored_print(f"    ✅ Accettato snippet {len(codes)-1} (task: {task_name}), lunghezza: {len(code)} caratteri", Colors.GREEN)
    
    colored_print(f"  📈 Snippet ", Colors.WHITE, end="")
    colored_print(f"{language} ", lang_color, Colors.BOLD, end="")
    colored_print(f"accettati: ", Colors.WHITE, end="")
    colored_print(f"{len(codes)}", Colors.GREEN, Colors.BOLD, end="")
    colored_print(f", filtrati: ", Colors.WHITE, end="")
    colored_print(f"{filtered_count}", Colors.ORANGE, Colors.BOLD)
    
    all_codes[language] = {"codes": codes, "tasks": tasks, "config": lang_config}

# Mostra statistiche di estrazione
total_extracted = sum(len(data["codes"]) for data in all_codes.values())
total_filtered = sum(len(df[df["language_name"] == lang]) - len(all_codes[lang]["codes"]) 
                    for lang in all_codes.keys() if lang in df["language_name"].values)

colored_print(f"\n📊 SOMMARIO ESTRAZIONE:", Colors.CYAN, Colors.BOLD)
colored_print(f"  📈 Totale snippet estratti: ", Colors.GREEN, end="")
colored_print(f"{total_extracted}", Colors.GREEN, Colors.BOLD)
colored_print(f"  🚫 Totale snippet filtrati: ", Colors.ORANGE, end="")
colored_print(f"{total_filtered}", Colors.ORANGE, Colors.BOLD)
colored_print(f"  🎯 Linguaggi processati: ", Colors.CYAN, end="")
colored_print(f"{len(all_codes)}", Colors.CYAN, Colors.BOLD)

# Salvo i file nelle rispettive cartelle
colored_print("\n💾 Salvo i file...", Colors.CYAN, Colors.BOLD)
for language, data in all_codes.items():
    lang_config = data["config"]
    folder = lang_config["folder"]
    ext = lang_config["ext"]
    codes = data["codes"]
    tasks = data["tasks"]
    lang_color = get_language_color(language)
    
    colored_print(f"📁 Salvo ", Colors.GREEN, end="")
    colored_print(f"{len(codes)} file {language}", lang_color, Colors.BOLD, end="")
    colored_print(f" in cartella '{folder}'...", Colors.GREEN)
    
    for i, (code, task_name) in enumerate(zip(codes, tasks)):
        # Uso il nome del task nel filename per identificare i file correlati
        safe_task_name = "".join(c for c in task_name if c.isalnum() or c in (' ', '-', '_')).rstrip()[:50]
        filename = os.path.join(folder, f"snippet_{i}_{safe_task_name.replace(' ', '_')}{ext}")
        with open(filename, "w", encoding="utf-8") as f:
            f.write(code)
        colored_print(f"  💾 File salvato: {filename}", Colors.GREEN)

# Funzione generica per compilare ed eseguire programmi con logging avanzato
def compile_and_run(language, file_index, codes_data):
    lang_config = codes_data["config"]
    folder = lang_config["folder"]
    ext = lang_config["ext"]
    compile_cmd = lang_config.get("compile_cmd")
    run_cmd = lang_config.get("run_cmd")
    
    source_file = None
    executable_file = None
    task_name = codes_data["tasks"][file_index] if file_index < len(codes_data["tasks"]) else f"task_{file_index}"
    
    # Trova il file sorgente
    for file in os.listdir(folder):
        if file.startswith(f"snippet_{file_index}_") and file.endswith(ext):
            source_file = os.path.join(folder, file)
            base_name = file.replace(ext, "")
            executable_file = os.path.join(folder, base_name)
            break
    
    if not source_file:
        error_msg = f"File sorgente non trovato per {language} snippet {file_index}"
        log_error(language, task_name, "FILE_NOT_FOUND", error_msg, "")
        update_stats(language, "runtime_error")
        return None
    
    try:
        # Compilazione (se necessaria)
        if compile_cmd:
            logger.info(f"Compilo: {source_file}")
            
            if language == "Java":
                # Per Java, compilazione speciale
                result = subprocess.run(
                    compile_cmd + [source_file],
                    capture_output=True,
                    text=True,
                    cwd=folder,
                    timeout=COMPILATION_TIMEOUT
                )
            elif language == "TypeScript":
                # Per TypeScript, genera .js
                result = subprocess.run(
                    compile_cmd + [source_file],
                    capture_output=True,
                    text=True,
                    timeout=COMPILATION_TIMEOUT
                )
                executable_file = source_file.replace(".ts", ".js")
            else:
                result = subprocess.run(
                    compile_cmd + [source_file, "-o", executable_file],
                    capture_output=True,
                    text=True,
                    timeout=COMPILATION_TIMEOUT
                )
            
            # Controlla errori di compilazione
            if result.returncode != 0:
                error_msg = f"Errore di compilazione: {result.stderr}"
                log_error(language, task_name, "COMPILATION_ERROR", error_msg, source_file, result.stderr)
                update_stats(language, "compilation_error")
                return None
            
            logger.info(f"Compilazione OK: {source_file}")
        
        # Esecuzione
        logger.info(f"Eseguo: {source_file} (timeout {TIMEOUT_SECONDS}s)")
        
        if language == "Java":
            class_name = os.path.basename(executable_file)
            cmd = ["java", class_name]
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=TIMEOUT_SECONDS,
                cwd=folder
            )
        elif run_cmd.endswith(" "):
            # Comando con argomento (python3, node, etc.)
            cmd = run_cmd.strip().split() + [source_file]
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=TIMEOUT_SECONDS
            )
        else:
            # Eseguibile diretto (./)
            result = subprocess.run(
                [executable_file],
                capture_output=True,
                text=True,
                timeout=TIMEOUT_SECONDS
            )
        
        # Controlla errori di runtime
        if result.returncode != 0:
            error_msg = f"Errore di runtime (exit code: {result.returncode}): {result.stderr}"
            log_error(language, task_name, "RUNTIME_ERROR", error_msg, source_file, result.stderr)
            update_stats(language, "runtime_error")
            return None
        else:
            lang_color = get_language_color(language)
            colored_print(f"✅ Esecuzione riuscita per ", Colors.GREEN, end="")
            colored_print(f"{language}", lang_color, Colors.BOLD, end="")
            colored_print(f": {os.path.basename(source_file)}", Colors.GREEN)
            
            # Mostra output limitato
            output_preview = result.stdout[:200] + "..." if len(result.stdout) > 200 else result.stdout
            if output_preview.strip():
                colored_print(f"  📤 Output: {output_preview}", Colors.WHITE)
            update_stats(language, "success")
            return True
        
            
        if result.stderr and result.returncode == 0:
            colored_print(f"⚠️ Warning per {source_file}: {result.stderr}", Colors.YELLOW)
            
    except subprocess.TimeoutExpired:
        error_msg = f"Esecuzione ha superato {TIMEOUT_SECONDS}s"
        log_error(language, task_name, "TIMEOUT", error_msg, source_file)
        update_stats(language, "timeout")
        colored_print(f"⏰ TIMEOUT: {source_file}", Colors.MAGENTA, Colors.BOLD)
        return None
    except subprocess.CalledProcessError as e:
        error_msg = f"Errore subprocess: {e}"
        log_error(language, task_name, "SUBPROCESS_ERROR", error_msg, source_file)
        update_stats(language, "runtime_error")
        return None
    except FileNotFoundError as e:
        error_msg = f"Comando non trovato: {e}"
        log_error(language, task_name, "COMMAND_NOT_FOUND", error_msg, source_file)
        update_stats(language, "dependency_error")
        lang_color = get_language_color(language)
        colored_print(f"❌ Comando non trovato per ", Colors.RED, end="")
        colored_print(f"{language}", lang_color, Colors.BOLD, end="")
        colored_print(f": {e}", Colors.RED)
        return None
    except Exception as e:
        error_msg = f"Errore generico: {e}"
        log_error(language, task_name, "GENERIC_ERROR", error_msg, source_file)
        update_stats(language, "runtime_error")
        return None# Eseguo tutti i programmi per categoria
colored_print("\n🚀 Inizio compilazione ed esecuzione dei programmi per categoria...", Colors.CYAN, Colors.BOLD)
for category, languages in language_categories.items():
    category_color = get_category_color(category)
    colored_print(f"\n{'='*20} CATEGORIA: ", Colors.CYAN, Colors.BOLD, end="")
    colored_print(f"{category}", category_color, Colors.BOLD, end="")
    colored_print(f" {'='*20}", Colors.CYAN, Colors.BOLD)
    
    for language in languages.keys():
        if language in all_codes and all_codes[language]["codes"]:
            lang_color = get_language_color(language)
            colored_print(f"\n🔧 Elaboro ", Colors.CYAN, end="")
            colored_print(f"{language}", lang_color, Colors.BOLD, end="")
            colored_print("...", Colors.CYAN)
            
            codes_data = all_codes[language]
            num_files = len(codes_data["codes"])
            
            # Esecuzione di tutti i file
            colored_print(f"  📋 Esecuzione di {num_files} file", Colors.CYAN)
            
            for i in tqdm(range(num_files), desc=f"Eseguo {language}", 
                         bar_format='{l_bar}%s{bar}%s{r_bar}' % (lang_color, Colors.END)):
                result = compile_and_run(language, i, codes_data)
                
        else:
            lang_color = get_language_color(language)
            colored_print(f"⚠️ Nessun codice trovato per ", Colors.YELLOW, end="")
            colored_print(f"{language}", lang_color, Colors.BOLD)

# Salva statistiche finali
save_stats()

colored_print("\n🎉 Processo completato! Controlla i file di log per dettagli:", Colors.GREEN, Colors.BOLD)
colored_print(f"  📄 Log esecuzione: logs/execution_*.log", Colors.CYAN)
colored_print(f"  🔍 Analisi errori: {error_log_file}", Colors.CYAN)
colored_print(f"  📊 Statistiche: {success_stats_file}", Colors.CYAN)