#!/usr/bin/env python3
"""
SWAM Project - Script di pulizia file temporanei
Elimina tutti i file temporanei, compilati e di log generati dal progetto multi-linguaggio
"""

import os
import shutil
import glob
from pathlib import Path
import argparse

# Colori per output
class Colors:
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    MAGENTA = '\033[95m'
    CYAN = '\033[96m'
    WHITE = '\033[97m'
    BOLD = '\033[1m'
    END = '\033[0m'

def colored_print(text, color=Colors.WHITE, style="", end="\n"):
    """Stampa testo colorato"""
    print(f"{style}{color}{text}{Colors.END}", end=end)

def get_file_size(filepath):
    """Ottiene la dimensione del file in formato leggibile"""
    try:
        size = os.path.getsize(filepath)
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size < 1024.0:
                return f"{size:.1f} {unit}"
            size /= 1024.0
        return f"{size:.1f} TB"
    except:
        return "0 B"

def remove_file_or_dir(path, dry_run=False):
    """Rimuove un file o directory con gestione errori"""
    try:
        if os.path.isfile(path):
            size = get_file_size(path)
            if not dry_run:
                os.remove(path)
            colored_print(f"  🗑️  File: {os.path.basename(path)} ({size})", Colors.RED)
            return True
        elif os.path.isdir(path):
            if not dry_run:
                shutil.rmtree(path)
            colored_print(f"  📁 Cartella: {path}", Colors.RED, Colors.BOLD)
            return True
    except Exception as e:
        colored_print(f"  ❌ Errore rimuovendo {path}: {e}", Colors.YELLOW)
        return False
    return False

def cleanup_compiled_files(dry_run=False):
    """Pulisce tutti i file compilati per ogni linguaggio"""
    colored_print("\n🔧 PULIZIA FILE COMPILATI", Colors.CYAN, Colors.BOLD)
    
    # Definisco le estensioni e pattern per ogni linguaggio
    cleanup_patterns = {
        "C/C++": [
            "**/*.o", "**/*.obj", "**/*.exe", "**/*.out", "**/a.out",
            "**/*.so", "**/*.dll", "**/*.dylib", "**/*.a", "**/*.lib"
        ],
        "Java": [
            "**/*.class", "**/*.jar", "**/bin/**", "**/target/**"
        ],
        "C#": [
            "**/*.exe", "**/*.dll", "**/*.pdb", "**/bin/**", "**/obj/**"
        ],
        "Haskell": [
            "**/*.hi", "**/*.o", "**/dist/**", "**/dist-newstyle/**"
        ],
        "OCaml": [
            "**/*.cmo", "**/*.cmi", "**/*.cmx", "**/*.cmxa", 
            "**/*.cma", "**/*.a", "**/*.o", "**/_build/**"
        ],
        "Rust": [
            "**/target/**", "**/Cargo.lock", "**/*.rlib", "**/*.rmeta"
        ],
        "Go": [
            "**/*.exe", "**/*.test", "**/*.prof", "**/go.sum"
        ],
        "TypeScript/JavaScript": [
            "**/*.js", "**/*.js.map", "**/node_modules/**", 
            "**/npm-debug.log*", "**/.npm/**"
        ]
    }
    
    total_removed = 0
    
    for language, patterns in cleanup_patterns.items():
        colored_print(f"\n📋 Pulizia file {language}:", Colors.BLUE, Colors.BOLD)
        lang_removed = 0
        
        for pattern in patterns:
            files = glob.glob(pattern, recursive=True)
            for file_path in files:
                if remove_file_or_dir(file_path, dry_run):
                    lang_removed += 1
                    total_removed += 1
        
        if lang_removed == 0:
            colored_print(f"  ✅ Nessun file da rimuovere", Colors.GREEN)
        else:
            colored_print(f"  📊 Rimossi {lang_removed} elementi", Colors.YELLOW, Colors.BOLD)
    
    return total_removed

def cleanup_generated_code(dry_run=False):
    """Pulisce tutti i file di codice generati dal dataset"""
    colored_print("\n📄 PULIZIA CODICE GENERATO", Colors.CYAN, Colors.BOLD)
    
    # Cartelle delle categorie di linguaggi
    category_folders = ["oop", "scripting", "imperative", "functional", "scientific"]
    
    total_removed = 0
    
    for folder in category_folders:
        if os.path.exists(folder):
            colored_print(f"\n📂 Rimuovo cartella categoria: {folder}", Colors.RED, Colors.BOLD)
            if remove_file_or_dir(folder, dry_run):
                total_removed += 1
        else:
            colored_print(f"  ✅ Cartella {folder} non esistente", Colors.GREEN)
    
    # File snippet isolati
    snippet_patterns = [
        "snippet_*",
        "snippet_*.cpp", "snippet_*.c", "snippet_*.java", "snippet_*.cs",
        "snippet_*.py", "snippet_*.rb", "snippet_*.js", "snippet_*.ts",
        "snippet_*.go", "snippet_*.rs", "snippet_*.php", "snippet_*.hs",
        "snippet_*.ml", "snippet_*.r", "snippet_*.m", "snippet_*.jl"
    ]
    
    colored_print(f"\n📄 Rimozione file snippet isolati:", Colors.BLUE, Colors.BOLD)
    for pattern in snippet_patterns:
        files = glob.glob(pattern)
        for file_path in files:
            if remove_file_or_dir(file_path, dry_run):
                total_removed += 1
    
    return total_removed

def cleanup_logs_and_stats(dry_run=False):
    """Pulisce i file di log e statistiche"""
    colored_print("\n📊 PULIZIA LOG E STATISTICHE", Colors.CYAN, Colors.BOLD)
    
    total_removed = 0
    
    # Cartella logs completa
    if os.path.exists("logs"):
        colored_print(f"\n📁 Rimuovo cartella logs completa", Colors.RED, Colors.BOLD)
        if remove_file_or_dir("logs", dry_run):
            total_removed += 1
    else:
        colored_print(f"  ✅ Cartella logs non esistente", Colors.GREEN)
    
    # File di log isolati
    log_patterns = [
        "*.log",
        "error_analysis.json",
        "success_stats.json",
        "execution_*.log"
    ]
    
    colored_print(f"\n📄 Rimozione file di log isolati:", Colors.BLUE, Colors.BOLD)
    for pattern in log_patterns:
        files = glob.glob(pattern)
        for file_path in files:
            if remove_file_or_dir(file_path, dry_run):
                total_removed += 1
    
    return total_removed

def cleanup_dependencies(dry_run=False):
    """Pulisce le cache e dipendenze dei package manager"""
    colored_print("\n📦 PULIZIA CACHE DIPENDENZE", Colors.CYAN, Colors.BOLD)
    
    dependency_patterns = [
        # Python
        "**/__pycache__/**", "**/*.pyc", "**/*.pyo", "**/*.pyd",
        "**/.pytest_cache/**", "**/.coverage", "**/htmlcov/**",
        "**/.tox/**", "**/.nox/**", "**/build/**", "**/dist/**",
        "**/*.egg-info/**",
        
        # Node.js
        "**/node_modules/**", "**/npm-debug.log*", "**/yarn-error.log*",
        "**/.npm/**", "**/.yarn/**", "**/package-lock.json", "**/yarn.lock",
        
        # Ruby
        "**/vendor/bundle/**", "**/.bundle/**", "**/Gemfile.lock",
        
        # R
        "**/.Rhistory", "**/.RData", "**/.Ruserdata",
        
        # Rust
        "**/Cargo.lock",
        
        # Go
        "**/go.mod", "**/go.sum"
    ]
    
    total_removed = 0
    
    for pattern in dependency_patterns:
        files = glob.glob(pattern, recursive=True)
        for file_path in files:
            if remove_file_or_dir(file_path, dry_run):
                total_removed += 1
    
    if total_removed == 0:
        colored_print(f"  ✅ Nessuna cache da rimuovere", Colors.GREEN)
    else:
        colored_print(f"  📊 Rimossi {total_removed} elementi di cache", Colors.YELLOW, Colors.BOLD)
    
    return total_removed

def cleanup_system_files(dry_run=False):
    """Pulisce i file di sistema e temporanei"""
    colored_print("\n💻 PULIZIA FILE SISTEMA", Colors.CYAN, Colors.BOLD)
    
    system_patterns = [
        # macOS
        "**/.DS_Store", "**/.AppleDouble", "**/.LSOverride",
        "**/._*", "**/.Spotlight-V100", "**/.Trashes",
        
        # Windows
        "**/Thumbs.db", "**/Desktop.ini", "**/*.tmp", "**/*.temp",
        
        # Linux
        "**/*~", "**/.directory", "**/.Trash-*",
        
        # Editor temporanei
        "**/*.swp", "**/*.swo", "**/*.backup", "**/*.bak",
        "**/*.old", "**/*.orig", "**/*.save",
        
        # IDE
        "**/.vscode/", "**/.idea/", "**/*.sublime-workspace"
    ]
    
    total_removed = 0
    
    for pattern in system_patterns:
        files = glob.glob(pattern, recursive=True)
        for file_path in files:
            # Mantieni .vscode/settings.json e simili se necessario
            if ".vscode" in file_path and any(keep in file_path for keep in ["settings.json", "tasks.json", "launch.json"]):
                continue
            if remove_file_or_dir(file_path, dry_run):
                total_removed += 1
    
    if total_removed == 0:
        colored_print(f"  ✅ Nessun file di sistema da rimuovere", Colors.GREEN)
    else:
        colored_print(f"  📊 Rimossi {total_removed} file di sistema", Colors.YELLOW, Colors.BOLD)
    
    return total_removed

def show_disk_usage():
    """Mostra l'utilizzo del disco prima e dopo la pulizia"""
    try:
        total, used, free = shutil.disk_usage(".")
        colored_print(f"\n💾 SPAZIO DISCO:", Colors.MAGENTA, Colors.BOLD)
        colored_print(f"  📊 Totale: {total // (1024**3):.1f} GB", Colors.CYAN)
        colored_print(f"  📈 Usato: {used // (1024**3):.1f} GB", Colors.YELLOW)
        colored_print(f"  📉 Libero: {free // (1024**3):.1f} GB", Colors.GREEN)
    except:
        colored_print(f"  ❌ Impossibile ottenere info disco", Colors.RED)

def main():
    parser = argparse.ArgumentParser(description="Script di pulizia SWAM Project")
    parser.add_argument("--dry-run", action="store_true", 
                       help="Simula la pulizia senza eliminare file")
    parser.add_argument("--all", action="store_true", 
                       help="Pulisce tutto (default)")
    parser.add_argument("--compiled", action="store_true", 
                       help="Pulisce solo file compilati")
    parser.add_argument("--generated", action="store_true", 
                       help="Pulisce solo codice generato")
    parser.add_argument("--logs", action="store_true", 
                       help="Pulisce solo log e statistiche")
    parser.add_argument("--deps", action="store_true", 
                       help="Pulisce solo cache dipendenze")
    parser.add_argument("--system", action="store_true", 
                       help="Pulisce solo file di sistema")
    
    args = parser.parse_args()
    
    # Header
    colored_print("="*60, Colors.CYAN, Colors.BOLD)
    colored_print("🧹 SWAM PROJECT - SCRIPT PULIZIA", Colors.CYAN, Colors.BOLD)
    colored_print("="*60, Colors.CYAN, Colors.BOLD)
    
    if args.dry_run:
        colored_print("🔍 MODALITÀ DRY-RUN: Nessun file sarà eliminato", Colors.YELLOW, Colors.BOLD)
    
    show_disk_usage()
    
    total_removed = 0
    
    # Determina cosa pulire
    clean_all = args.all or not any([args.compiled, args.generated, args.logs, args.deps, args.system])
    
    if clean_all or args.compiled:
        total_removed += cleanup_compiled_files(args.dry_run)
    
    if clean_all or args.generated:
        total_removed += cleanup_generated_code(args.dry_run)
    
    if clean_all or args.logs:
        total_removed += cleanup_logs_and_stats(args.dry_run)
    
    if clean_all or args.deps:
        total_removed += cleanup_dependencies(args.dry_run)
    
    if clean_all or args.system:
        total_removed += cleanup_system_files(args.dry_run)
    
    # Summary finale
    colored_print("\n" + "="*60, Colors.GREEN, Colors.BOLD)
    colored_print("📊 RIEPILOGO PULIZIA", Colors.GREEN, Colors.BOLD)
    colored_print("="*60, Colors.GREEN, Colors.BOLD)
    
    if args.dry_run:
        colored_print(f"🔍 Elementi che verrebbero rimossi: {total_removed}", Colors.YELLOW, Colors.BOLD)
        colored_print("💡 Rimuovi --dry-run per eseguire la pulizia reale", Colors.CYAN)
    else:
        colored_print(f"✅ Elementi rimossi con successo: {total_removed}", Colors.GREEN, Colors.BOLD)
        if total_removed > 0:
            colored_print("🎉 Pulizia completata!", Colors.GREEN, Colors.BOLD)
        else:
            colored_print("✨ Repository già pulito!", Colors.CYAN, Colors.BOLD)
    
    show_disk_usage()
    
    colored_print("\n💡 SUGGERIMENTI:", Colors.MAGENTA, Colors.BOLD)
    colored_print("  • Usa --dry-run per vedere cosa verrà rimosso", Colors.CYAN)
    colored_print("  • Usa --compiled, --generated, --logs, --deps, --system per pulizie mirate", Colors.CYAN)
    colored_print("  • Il file .gitignore previene la creazione di questi file temporanei", Colors.CYAN)

if __name__ == "__main__":
    main()
