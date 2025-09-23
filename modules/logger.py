"""
Sistema di logging semplificato
Gestisce il tracciamento degli errori e delle statistiche essenziali
"""

import os
import json
import logging
from datetime import datetime

# Configurazione directory di log
LOG_DIR = "../results/logs"
if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)

# File per tracciare errori e statistiche
ERROR_LOG_FILE = os.path.join(LOG_DIR, "error_analysis.json")
SUCCESS_STATS_FILE = os.path.join(LOG_DIR, "success_stats.json")

def setup_logger():
    """Configura il sistema di logging essenziale"""
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(os.path.join(LOG_DIR, f'execution_{datetime.now().strftime("%Y%m%d_%H%M%S")}.log')),
            logging.StreamHandler()
        ]
    )
    return logging.getLogger(__name__)

def log_error(language, task_name, error_type, error_message, file_path, code_snippet=""):
    """Logga un errore nel sistema di tracking"""
    error_entry = {
        "timestamp": datetime.now().isoformat(),
        "language": language,
        "task_name": task_name,
        "error_type": error_type,
        "error_message": str(error_message),
        "file_path": file_path,
        "code_snippet": code_snippet[:200] if code_snippet else ""
    }

    # Carica e salva errori
    errors = []
    if os.path.exists(ERROR_LOG_FILE):
        try:
            with open(ERROR_LOG_FILE, 'r') as f:
                errors = json.load(f)
        except:
            errors = []

    errors.append(error_entry)

    with open(ERROR_LOG_FILE, 'w') as f:
        json.dump(errors, f, indent=2)

# Statistiche globali semplificate
global_stats = {
    "total_files": 0,
    "successful_executions": 0,
    "errors": 0,
    "by_language": {}
}

def update_stats(language, status):
    """Aggiorna le statistiche globali"""
    global global_stats

    global_stats["total_files"] += 1

    if language not in global_stats["by_language"]:
        global_stats["by_language"][language] = {"total": 0, "successful": 0, "errors": 0}

    global_stats["by_language"][language]["total"] += 1

    if status == "success":
        global_stats["successful_executions"] += 1
        global_stats["by_language"][language]["successful"] += 1
    else:
        global_stats["errors"] += 1
        global_stats["by_language"][language]["errors"] += 1

def save_stats():
    """Salva le statistiche finali"""
    with open(SUCCESS_STATS_FILE, 'w') as f:
        json.dump(global_stats, f, indent=2)

    print(f"\n STATISTICHE: {global_stats['successful_executions']}/{global_stats['total_files']} successi")
    for lang, stats in global_stats['by_language'].items():
        if stats['total'] > 0:
            rate = (stats['successful'] / stats['total']) * 100
            print(f" {lang}: {stats['successful']}/{stats['total']} ({rate:.1f}%)")