"""
Gestione del dataset e analisi del codice
Carica il dataset Rosetta Code e analizza i snippet di codice
"""

import re
import subprocess
from config import PROBLEMATIC_PATTERNS, PROBLEMATIC_PACKAGES
from colors import colored_print, get_language_color, Colors
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

 lang_color = get_language_color(language)
 colored_print(f" Installazione dipendenze per ", Colors.CYAN, end="")
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
 colored_print(f" {dep} installato", Colors.GREEN)
 else:
 colored_print(f" {dep} fallito: {result.stderr[:100]}", Colors.RED)
 return True
 except Exception as e:
 colored_print(f" Installazione fallita: {e}", Colors.RED)
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
 lang_color = get_language_color(language)
 colored_print(f"\n Filtro i codici in ", Colors.CYAN, end="")
 colored_print(f"{language}", lang_color, Colors.BOLD)

 # Filtra il dataframe per il linguaggio
 lang_df = df[df["language_name"] == language]

 # Applica filtro per task comuni se disponibili
 if common_tasks and "task_name" in df.columns and len(common_tasks) > 10:
 lang_df = lang_df[lang_df["task_name"].isin(common_tasks)]
 colored_print(f" Filtrati per task comuni: ", Colors.GREEN, end="")
 colored_print(f"{len(lang_df)} snippet", Colors.GREEN, Colors.BOLD)
 else:
 colored_print(f" Uso tutti i task disponibili: ", Colors.CYAN, end="")
 colored_print(f"{len(lang_df)} snippet", Colors.CYAN, Colors.BOLD)

 # Limita il numero di snippet
 lang_df = lang_df.head(max_snippets)
 colored_print(f" 📝 Snippet candidati: ", Colors.CYAN, end="")
 colored_print(f"{len(lang_df)}", Colors.CYAN, Colors.BOLD)

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
 colored_print(f" 🚫 Filtrato snippet per task '{task_name}': ", Colors.ORANGE, end="")
 colored_print(f"{filter_reason}", Colors.ORANGE)
 log_error(language, task_name, "FILTERED", filter_reason, "", code[:200])
 update_stats(language, "filtered")
 filtered_count += 1
 continue

 # Gestisce dipendenze
 dependencies = extract_dependencies(code, language)
 if dependencies:
 colored_print(f" Dipendenze rilevate per task '{task_name}': ", Colors.YELLOW, end="")
 colored_print(f"{dependencies}", Colors.YELLOW)
 deps_ok = install_dependencies(language, dependencies, lang_config)
 if not deps_ok:
 log_error(language, task_name, "DEPENDENCY_ERROR",
 f"Fallita installazione dipendenze: {dependencies}", "", code[:200])
 update_stats(language, "dependency_error")
 continue

 codes.append(code)
 tasks.append(task_name)
 colored_print(f" Accettato snippet {len(codes)-1} (task: {task_name}), lunghezza: {len(code)} caratteri", Colors.GREEN)

 # Statistiche finali per il linguaggio
 colored_print(f" Snippet ", Colors.WHITE, end="")
 colored_print(f"{language} ", lang_color, Colors.BOLD, end="")
 colored_print(f"accettati: ", Colors.WHITE, end="")
 colored_print(f"{len(codes)}", Colors.GREEN, Colors.BOLD, end="")
 colored_print(f", filtrati: ", Colors.WHITE, end="")
 colored_print(f"{filtered_count}", Colors.ORANGE, Colors.BOLD)

 return {"codes": codes, "tasks": tasks, "config": lang_config}

def analyze_common_tasks(df, target_languages):
 """Analizza i task comuni tra tutti i linguaggi"""
 colored_print("\n Analizzo i task comuni tra i linguaggi...", Colors.CYAN, Colors.BOLD)

 # Stampa linguaggi target colorati
 colored_print(f" Linguaggi target: ", Colors.CYAN, end="")
 for i, lang in enumerate(target_languages):
 if i > 0:
 colored_print(", ", Colors.WHITE, end="")
 lang_color = get_language_color(lang)
 colored_print(f"{lang}", lang_color, Colors.BOLD, end="")
 colored_print("")

 # Trova task per ogni linguaggio
 tasks_per_language = {}
 for lang in target_languages:
 lang_df = df[df["language_name"] == lang]
 tasks = set(lang_df["task_name"].unique()) if "task_name" in df.columns else set()
 tasks_per_language[lang] = tasks

 lang_color = get_language_color(lang)
 colored_print(f" {lang}: ", lang_color, Colors.BOLD, end="")
 colored_print(f"{len(tasks)} task unici", Colors.WHITE)

 # Trova intersezione
 if tasks_per_language and any(tasks_per_language.values()):
 common_tasks = set.intersection(*[tasks for tasks in tasks_per_language.values() if tasks])
 colored_print(f"\n Task comuni a tutti i linguaggi: ", Colors.GREEN, Colors.BOLD, end="")
 colored_print(f"{len(common_tasks)}", Colors.GREEN, Colors.BOLD)
 colored_print(f"📝 Primi 10 task comuni: ", Colors.CYAN, end="")
 colored_print(f"{list(common_tasks)[:10]}", Colors.WHITE)
 return common_tasks
 else:
 colored_print(" Non è stata trovata la colonna 'task_name' nel dataset o nessun task comune",
 Colors.YELLOW, Colors.BOLD)
 return set()
