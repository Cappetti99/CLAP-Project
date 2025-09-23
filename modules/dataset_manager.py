"""
Gestione del dataset e analisi del codice
Carica il dataset Rosetta Code e analizza i snippet di codice
"""

import re
import subprocess
from config import PROBLEMATIC_PATTERNS, PROBLEMATIC_PACKAGES
from logger import log_error, update_stats

def extract_dependencies(code, language):
    """Estrae le dipendenze dal codice per un dato linguaggio"""
    dependencies = set()

    if language == "Python":
        import_patterns = [
            r"^import\s+([a-zA-Z_][a-zA-Z0-9_]*)",
            r"^from\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+import",
            r"import\s+([a-zA-Z_][a-zA-Z0-9_]*)"
        ]
        for pattern in import_patterns:
            matches = re.findall(pattern, code, re.MULTILINE)
            dependencies.update(matches)

    elif language in ["Javascript", "TypeScript"]:
        patterns = [
            r"require\(['\"]([^'\"]+)['\"]\)",
            r"import.*from\s+['\"]([^'\"]+)['\"]"
        ]
        for pattern in patterns:
            matches = re.findall(pattern, code)
            dependencies.update(matches)

    elif language == "Ruby":
        matches = re.findall(r"require\s+['\"]([^'\"]+)['\"]", code)
        dependencies.update(matches)

    elif language in ["C", "C++"]:
        matches = re.findall(r"#include\s*[<\"]([^>\"]+)[>\"]", code)
        dependencies.update(matches)

    return dependencies

def install_dependencies(language, dependencies, lang_config):
    """Tenta di installare le dipendenze automaticamente"""
    install_cmd = lang_config.get("install_deps")
    if not install_cmd or not dependencies:
        return True

    # Filtra pacchetti problematici
    safe_deps = []
    for dep in dependencies:
        if dep not in PROBLEMATIC_PACKAGES.get(language, []):
            safe_deps.append(dep)

    if not safe_deps:
        return True

    print(f" Installazione dipendenze per {language}: {safe_deps}")

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
                    print(f" {dep} installato")
                else:
                    print(f" {dep} fallito: {result.stderr[:100]}")
        return True
    except Exception as e:
        print(f" Installazione fallita: {e}")
        return False

def should_filter_code(code, language):
    """Determina se un codice dovrebbe essere filtrato per problemi noti"""
    patterns = PROBLEMATIC_PATTERNS.get(language, [])

    # Controlla pattern problematici
    for pattern in patterns:
        if re.search(pattern, code, re.IGNORECASE):
            return True, f"Codice filtrato: contiene pattern problematico '{pattern}'"

    # Filtra codici troppo corti o troppo lunghi
    if len(code.strip()) < 10:
        return True, "Codice troppo corto"

    if len(code) > 5000:
        return True, "Codice troppo lungo"

    # Filtra codici con troppe dipendenze esterne
    if language in ["C++", "C"]:
        include_count = len(re.findall(r"#include\s*[<\"]([^>\"]+)[>\"]", code))
        if include_count > 10:
            return True, "Troppe dipendenze (include)"

    return False, ""

def process_language_codes(df, language, lang_config, common_tasks, max_snippets):
    """Processa i codici per un singolo linguaggio"""
    print(f"\n Filtro i codici in {language}")

    # Filtra il dataframe per il linguaggio
    lang_df = df[df["language_name"] == language]

    # Applica filtro per task comuni se disponibili
    if common_tasks and "task_name" in df.columns and len(common_tasks) > 10:
        lang_df = lang_df[lang_df["task_name"].isin(common_tasks)]
        print(f" Filtrati per task comuni: {len(lang_df)} snippet")
    else:
        print(f" Uso tutti i task disponibili: {len(lang_df)} snippet")

    # Limita il numero di snippet
    lang_df = lang_df.head(max_snippets)
    print(f" Snippet candidati: {len(lang_df)}")

    codes = []
    tasks = []
    filtered_count = 0

    # Processa ogni snippet
    for idx, row in lang_df.iterrows():
        code = row["code"]
        task_name = row.get("task_name", f"task_{len(codes)}")

        # Applica filtri per problemi noti
        should_filter, filter_reason = should_filter_code(code, language)
        if should_filter:
            print(f" Filtrato snippet per task '{task_name}': {filter_reason}")
            log_error(language, task_name, "FILTERED", filter_reason, "", code[:200])
            update_stats(language, "filtered")
            filtered_count += 1
            continue

        # Gestisce dipendenze
        dependencies = extract_dependencies(code, language)
        if dependencies:
            print(f" Dipendenze rilevate per task '{task_name}': {dependencies}")
            deps_ok = install_dependencies(language, dependencies, lang_config)
            if not deps_ok:
                log_error(language, task_name, "DEPENDENCY_ERROR",
                          f"Fallita installazione dipendenze: {dependencies}", "", code[:200])
                update_stats(language, "dependency_error")
                continue

        codes.append(code)
        tasks.append(task_name)
        print(f" Accettato snippet {len(codes)-1} (task: {task_name}), lunghezza: {len(code)} caratteri")

    # Statistiche finali per il linguaggio
    print(f" Snippet {language} accettati: {len(codes)}, filtrati: {filtered_count}")

    return {"codes": codes, "tasks": tasks, "config": lang_config}

def analyze_common_tasks(df, target_languages):
    """Analizza i task comuni tra tutti i linguaggi"""
    print("\n Analizzo i task comuni tra i linguaggi...")

    # Stampa linguaggi target
    print(f" Linguaggi target: {', '.join(target_languages)}")

    # Trova task per ogni linguaggio
    tasks_per_language = {}
    for lang in target_languages:
        lang_df = df[df["language_name"] == lang]
        tasks = set(lang_df["task_name"].unique()) if "task_name" in df.columns else set()
        tasks_per_language[lang] = tasks
        print(f" {lang}: {len(tasks)} task unici")

    # Trova intersezione
    if tasks_per_language and any(tasks_per_language.values()):
        common_tasks = set.intersection(*[tasks for tasks in tasks_per_language.values() if tasks])
        print(f"\n Task comuni a tutti i linguaggi: {len(common_tasks)}")
        print(f" Primi 10 task comuni: {list(common_tasks)[:10]}")
        return common_tasks
    else:
        print(" Non è stata trovata la colonna 'task_name' nel dataset o nessun task comune")
        return set()