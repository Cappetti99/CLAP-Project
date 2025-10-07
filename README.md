# CLAP – Cross-Language Analysis Project

<div align="center">
    <img src="logoSWAM.png" alt="CLAP Project Logo" width="300"/>
</div>

Advanced modular system for multi-language code analysis and execution with CO2 emission tracking.

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-15+-green.svg)](#supported-languages)
[![CodeCarbon](https://img.shields.io/badge/CodeCarbon-Enabled-green.svg)](https://codecarbon.io)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## 📖 Description

**CLAP Project** is an advanced system for automated analysis and execution of code across 15+ programming languages. The project addresses cross-language comparison of algorithms and implementations and provides tools for:

- **Targeted search**: Find specific algorithm implementations in the dataset
- **Automated execution**: Run code across multiple languages simultaneously
- **Emissions analysis**: Track energy consumption and CO2 emissions of code
- **Benchmarking**: Compare performance and energy efficiency across languages

**Use cases**: academic research, teaching, comparative language analysis, and software energy optimization.

## 📑 Table of Contents

1. [Description](#-description)
2. [Installation](#️-installation)
3. [Usage](#-usage)
4. [Configuration](#️-configuration)
5. [Project Structure](#-project-structure)

## 🛠️ Installation

### Quick Setup (3 minutes)

```bash
# 1. Clone and initial setup
git clone https://github.com/Cappetti99/SWAM-Project.git
cd SWAM-Project

# 2. Python virtual environment
python3 -m venv swam_env
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows

# 3. Python dependencies
pip install --upgrade pip
pip install -r requirements.txt

# 4. System check
python main.py test
```

The system **automatically adapts** to available languages.

### Full Setup (All 16 languages)

For full functionality, install all supported languages:

**🐧 Ubuntu/Debian - One-liner:**
```bash
sudo apt update && sudo apt install -y build-essential openjdk-11-jdk nodejs npm golang-go php-cli ruby-full r-base ghc ocaml mono-complete && sudo npm install -g typescript && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh && sudo snap install julia --classic
```

**🍎 macOS (Homebrew):**
```bash
brew install gcc openjdk node go php ruby r ghc ocaml mono julia && npm install -g typescript && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Detailed Guides

- **📚 [SETUP_GUIDE.md](SETUP_GUIDE.md)** - Full step-by-step guide with troubleshooting
- **💻 [SYSTEM_REQUIREMENTS.md](SYSTEM_REQUIREMENTS.md)** - OS-specific installation commands
- **📋 [requirements.txt](requirements.txt)** - Python dependencies

### Verify Installation

```bash
python main.py test
# Target: 15-16/16 languages (MATLAB is optional)
```

## 🚀 Usage

### Main Commands

| Command | Description | When to use |
|---------|-------------|-------------|
| `find` | Targeted task search | To find algorithm implementations |
| `test` | Check available languages | Initial setup and troubleshooting |
| `smart` | Smart execution of TOP 10 tasks | Quick multi-language testing |
| `carbon` | Analyze existing CO2 data | View results |
| `benchmark` | CO2 benchmarking | Gather scientific data |
| `analyze` | Full analysis of common tasks | Deep research |
| `clean` | Clean temporary files | Maintenance |

### Practical Examples

**1. Targeted Search (Recommended):**
```bash
# Find bubble sort implementations
python main.py find --task "bubble"

# Example output:
# 🔍 Searching for task: 'bubble' (tested languages only)
# ✅ Found 14 tasks in 14 tested languages:
# 📁 PYTHON (1 file): snippet_39_Sorting_algorithmsBubble_sort.py
# 📁 JAVA (1 file): snippet_36_Sorting_algorithmsBubble_sort.java
# 🤔 Do you want to run one of these tasks? [y/N]

# Search for fibonacci algorithms
python main.py find --task "fibonacci"

# Search all sorting algorithms
python main.py find --task "sort"
```

**2. System Check:**
```bash
# Verify available languages
python main.py test

# Expected output:
# 📋 Testing 16 languages...
# ✅ Available languages: 15/16 (93.8%)
# [FAIL] MATLAB: command 'matlab' not found
```

**3. Smart Execution:**
```bash
# Run TOP 10 common tasks on available languages
python main.py smart

# Example output:
# 🧠 SMART EXECUTION
# Available languages: 15
# 🎉 TOTAL: 45/150 (30.0%) successful runs
```

**4. CO2 Emissions Analysis:**
```bash
# Show existing CO2 data
python main.py carbon

# Example output:
# 📊 CO2 DATA OVERVIEW
# 🔍 Found 127 CO2 session files
# 🌍 Total emissions: 2.45 kg CO2eq
# ⚡ Total energy: 15.67 Wh

# Collect new CO2 data
python main.py benchmark
```

### Expected I/O

**Common inputs:**
- Command-line arguments and optional flags
- Interactive selections during runs (y/N, numeric choices)
- No external input files required (uses internal dataset)

**Generated outputs:**
- Result files in `results/` organized by type
- Detailed logs for debugging and analysis
- JSON files with structured data for further processing
- CSV reports for statistical analysis

## ⚙️ Configuration

### Environment Variables

The system is designed to work with no configuration in most cases and adapts automatically to the host environment.

**Optional (advanced customization):**
```bash
# Custom dataset path (default: ./data/generated/)
export SWAM_DATASET_PATH="/path/to/custom/dataset"

# Custom output directory (default: ./results/)
export SWAM_OUTPUT_PATH="/path/to/custom/results"

# Logging level (default: INFO)
export SWAM_LOG_LEVEL="DEBUG"  # For detailed troubleshooting
```

### Configuration Files

**1. `requirements.txt`** - Python dependencies:
```
codecarbon>=3.0.0     # CO2 tracking
datasets>=2.14.0      # Dataset handling
pandas>=2.0.0         # Data analysis
numpy>=1.24.0         # Numerical computations
tqdm>=4.65.0          # Progress bars
colorama>=0.4.6       # Colored terminal output
psutil>=5.9.0         # System information
pyarrow>=12.0.0       # Fast I/O
```

**2. `modules/language_config.py`** - Language configuration:
- Compilation and execution commands for each language
- Supported file extensions
- Timeouts and safety parameters

### Personalizzazioni Disponibili

**1. Aggiunta Nuovi Linguaggi:**
```python
# In modules/language_config.py
LANGUAGES = {
    "NUOVO_LINGUAGGIO": {
        "compile_cmd": "compilatore {input} -o {output}",
        "run_cmd": "{executable}",
        "extension": ".ext",
        "category": "categoria"
    }
}
```

**2. Timeout Personalizzati:**
```python
# Default: 5 secondi per linguaggio
EXECUTION_TIMEOUT = 10  # Aumenta per task complesse
```

**3. Dataset Personalizzato:**
```bash
# Organizza file come:
data/custom_dataset/
├── code_snippets/
│   ├── python/
│   ├── java/
│   └── ...
```

## 📁 Struttura del Progetto

```
SWAM-Project/
├── main.py                    # 🎯 Interfaccia principale
├── requirements.txt           # 📋 Dipendenze Python (ben documentate)
├── SETUP_GUIDE.md            # 🚀 Guida installazione completa step-by-step
├── SYSTEM_REQUIREMENTS.md    # 💻 Comandi installazione per ogni OS
├── README.md                  # 📖 Documentazione principale
├── cleanup_sessions.py        # 🧹 Pulizia sessioni CO2
│
├── modules/                   # 📚 Moduli core del sistema
│   ├── language_config.py    #     Configurazioni linguaggi
│   ├── modern_dependency_analyzer.py  # Analisi dipendenze
│   └── modern_logger.py      #     Sistema logging avanzato
│
├── src/                      # 🚀 Esecutori e analizzatori
│   ├── task_searcher.py      #     🎯 Ricerca mirata task
│   ├── smart_executor.py     #     🧠 Esecutore adattivo
│   ├── executor.py           #     🔧 Esecutore base
│   ├── language_tester.py    #     🧪 Test linguaggi disponibili
│   ├── benchmark.py          #     📊 Benchmark performance
│   ├── carbon_benchmark.py   #     🌱 Benchmark emissioni CO2
│   ├── carbon_tracker.py     #     📈 Tracking emissioni
│   ├── finder.py            #     🔍 Ricerca task comuni
│   └── cleaner.py           #     🧹 Pulizia file temporanei
│
├── data/                     # 📊 Dataset e dati
│   └── generated/           #     Dataset generato automaticamente
│       └── code_snippets/   #     Snippet di codice organizzati
│           ├── algorithms/  #     Algoritmi vari
│           ├── basic/       #     Esempi base
│           ├── python/      #     Specifici per Python
│           ├── java/        #     Specifici per Java
│           └── ...          #     Altri linguaggi
│
├── results/                  # 📈 Output e risultati generati
│   ├── task_search/         #     🎯 Risultati ricerca mirata
│   ├── execution/           #     Risultati esecuzione
│   ├── carbon_benchmark/    #     Report CO2 dettagliati  
│   ├── carbon/             #     Tracking emissioni
│   ├── csv/                #     File CSV per analisi
│   ├── logs/               #     Log di sistema
│   └── compliance/         #     Report conformità
│
└── scripts/                 # 🛠️ Script di utilità
    ├── export_to_csv.py    #     Esportazione dati CSV
    └── extract_top10_csv.py #     Estrazione TOP 10 task
```

### File Principali

**🎯 main.py** - Punto di ingresso principale:
- Interfaccia unificata per tutti i comandi
- Routing verso moduli specializzati
- Gestione parametri da linea di comando

**🧠 src/smart_executor.py** - Cuore del sistema:
- Rilevamento automatico linguaggi disponibili
- Esecuzione adattiva e gestione errori
- Integrazione con carbon tracking

**🎯 src/task_searcher.py** - Ricerca mirata:
- Pattern matching intelligente nel dataset
- Interfaccia interattiva per selezione task
- Supporto multi-estensione e multi-linguaggio

**🌱 src/carbon_tracker.py** - Monitoraggio emissioni:
- Integrazione con codecarbon
- Tracking automatico durante esecuzioni
- Gestione sessioni e cleanup

---

**SWAM Project** © 2025 - Sistema Multi-linguaggio per Analisi Codice  
Autore: Lorenzo Cappettiject - Cross-Language Code Analysis System

Sistema Modulare Avanzato per Analisi e Esecuzione Multi-Linguaggio con tracciamento emissioni CO2.

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-15+-green.svg)](#linguaggi-supportati)
[![CodeCarbon](https://img.shields.io/badge/CodeCarbon-Enabled-green.svg)](https://codecarbon.io)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## �️ Installazione

### Setup Rapido (3 minuti)

```bash
# 1. Clone e setup base
git clone https://github.com/Cappetti99/SWAM-Project.git
cd SWAM-Project

# 2. Environment Python
python3 -m venv swam_env
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows

# 3. Dipendenze Python
pip install --upgrade pip
pip install -r requirements.txt

# 4. Test sistema
python main.py test
```

Il sistema **si adatta automaticamente** ai linguaggi disponibili!

### Setup Completo (Tutti i 16 linguaggi)

Per massima funzionalità, installa tutti i linguaggi supportati:

**🐧 Ubuntu/Debian - One-liner:**
```bash
sudo apt update && sudo apt install -y build-essential openjdk-11-jdk nodejs npm golang-go php-cli ruby-full r-base ghc ocaml mono-complete && sudo npm install -g typescript && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh && sudo snap install julia --classic
```

**🍎 macOS (con Homebrew):**
```bash
brew install gcc openjdk node go php ruby r ghc ocaml mono julia && npm install -g typescript && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Guide Dettagliate

- **📚 [SETUP_GUIDE.md](SETUP_GUIDE.md)** - Guida completa step-by-step con troubleshooting
- **💻 [SYSTEM_REQUIREMENTS.md](SYSTEM_REQUIREMENTS.md)** - Comandi specifici per ogni OS
- **📋 [requirements.txt](requirements.txt)** - Dipendenze Python documentate

### Verifica Installazione

```bash
python main.py test
# Obiettivo: 15-16/16 linguaggi (MATLAB è opzionale)
```

## 📑 Indice

1. [Descrizione](#-descrizione)
2. [Installazione](#️-installazione)
3. [Utilizzo](#-utilizzo)
4. [Configurazione](#️-configurazione)
5. [Struttura del progetto](#-struttura-del-progetto)

## �️ Installazione

### Requisiti di Sistema

**Python:**
- Python 3.8+ (raccomandato: 3.12+)
- pip (package installer)

**Linguaggi Supportati (automaticamente rilevati):**
- **OOP**: C++, Java, C#
- **Scripting**: Python, Ruby, JavaScript, TypeScript  
- **Imperative**: C, Go, Rust, PHP
- **Functional**: Haskell, OCaml
- **Scientific**: R, Julia, MATLAB*

*MATLAB richiede licenza commerciale

### Setup Rapido

1. **Clone del repository:**
```bash
git clone https://github.com/Cappetti99/SWAM-Project.git
cd SWAM-Project
```

2. **Creazione environment virtuale (raccomandato):**
```bash
# Crea environment
python3 -m venv swam_env

# Attiva environment
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows
```

3. **Installazione dipendenze Python:**
```bash
pip install --upgrade pip
pip install -r requirements.txt
```

4. **Test immediato:**
```bash
python main.py test
```

### Setup Completo (Tutti i Linguaggi)

**Ubuntu/Debian:**
```bash
# Linguaggi di base
sudo apt update && sudo apt install -y \
  build-essential openjdk-11-jdk nodejs npm \
  golang-go php-cli ruby-full r-base \
  ghc ocaml mono-complete

# Linguaggi speciali
sudo npm install -g typescript
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
sudo snap install julia --classic

# Riavvia terminale e verifica
source ~/.cargo/env  # Per Rust
python main.py test  # Obiettivo: 15-16/16 linguaggi
```

**macOS:**
```bash
# Installa Homebrew se necessario
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Linguaggi
brew install gcc openjdk node go php ruby r ghc ocaml mono julia
npm install -g typescript
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Verifica Installazione

```bash
# Test linguaggi disponibili
python main.py test

# Output atteso:
# Linguaggi disponibili: 15/16 (93.8%)
# Solo MATLAB tipicamente mancante
```

## � Utilizzo

### Comandi Principali

| Comando | Descrizione | Quando Usarlo |
|---------|-------------|---------------|
| `find` | Ricerca mirata task specifiche | Per trovare implementazioni di algoritmi |
| `test` | Verifica linguaggi disponibili | Setup iniziale e troubleshooting |
| `smart` | Esecuzione intelligente TOP 10 task | Test rapidi multi-linguaggio |
| `carbon` | Analisi dati CO2 esistenti | Visualizzazione risultati |
| `benchmark` | Benchmark emissioni CO2 | Raccolta dati scientifici |
| `analyze` | Analisi completa task comuni | Ricerca approfondita |
| `clean` | Pulizia file temporanei | Manutenzione sistema |

### Esempi Pratici

**1. Ricerca Mirata (Raccomandato):**
```bash
# Trova implementazioni di bubble sort
python main.py find --task "bubble"

# Output:
# 🔍 Ricerca task: 'bubble' (solo linguaggi testati)
# ✅ Trovate 14 task in 14 linguaggi testati:
# 📁 PYTHON (1 file): snippet_39_Sorting_algorithmsBubble_sort.py
# 📁 JAVA (1 file): snippet_36_Sorting_algorithmsBubble_sort.java
# 🤔 Vuoi eseguire una di queste task? [s/N]

# Cerca algoritmi fibonacci
python main.py find --task "fibonacci"

# Cerca tutti gli algoritmi di sorting
python main.py find --task "sort"
```

**2. Test e Verifica Sistema:**
```bash
# Verifica linguaggi disponibili
python main.py test

# Output atteso:
# 📋 Testing 16 languages...
# ✅ Linguaggi disponibili: 15/16 (93.8%)
# [FAIL] MATLAB: Comando 'matlab' non trovato
```

**3. Esecuzione Intelligente:**
```bash
# Esegue TOP 10 task comuni sui linguaggi funzionanti
python main.py smart

# Output:
# 🧠 ESECUZIONE INTELLIGENTE
# Linguaggi disponibili: 15
# 🎉 TOTALE: 45/150 (30.0%) esecuzioni riuscite
```

**4. Analisi Emissioni CO2:**
```bash
# Visualizza dati CO2 esistenti
python main.py carbon

# Output:
# 📊 VISUALIZZAZIONE DATI CO2
# 🔍 Trovati 127 file di sessione CO2
# 🌍 Emissioni totali: 2.45 kg CO2eq
# ⚡ Energia totale: 15.67 Wh

# Raccogli nuovi dati CO2
python main.py benchmark
```

### Input/Output Attesi

**Input tipici:**
- Comandi da terminale con parametri opzionali
- Selezioni interattive durante l'esecuzione (s/N, numeri)
- Nessun file di input richiesto (usa dataset interno)

**Output generati:**
- Risultati in `results/` organizzati per tipologia
- Log dettagliati per debug e analisi
- File JSON con dati strutturati per analisi successive
- Report CSV per analisi statistica

## ⚙️ Configurazione

### Variabili d'Ambiente

Il sistema è progettato per funzionare **senza configurazione** nella maggior parte dei casi, adattandosi automaticamente al sistema disponibile.

**Opzionali (per customizzazioni avanzate):**
```bash
# Percorso dataset personalizzato (default: ./data/generated/)
export SWAM_DATASET_PATH="/path/to/custom/dataset"

# Directory output personalizzata (default: ./results/)
export SWAM_OUTPUT_PATH="/path/to/custom/results"

# Livello logging (default: INFO)
export SWAM_LOG_LEVEL="DEBUG"  # Per troubleshooting dettagliato
```

### File di Configurazione

**1. requirements.txt** - Dipendenze Python:
```
codecarbon>=3.0.0     # Tracciamento CO2
datasets>=2.14.0      # Gestione dataset
pandas>=2.0.0         # Analisi dati  
numpy>=1.24.0         # Calcoli numerici
tqdm>=4.65.0          # Progress bars
colorama>=0.4.6       # Output colorato
psutil>=5.9.0         # Informazioni sistema
pyarrow>=12.0.0       # Performance I/O
```

**2. modules/language_config.py** - Configurazione linguaggi:
- Comandi di compilazione ed esecuzione per ogni linguaggio
- Estensioni file supportate
- Timeout e parametri di sicurezza

### Personalizzazioni Disponibili

**1. Aggiunta Nuovi Linguaggi:**
```python
# In modules/language_config.py
LANGUAGES = {
    "NUOVO_LINGUAGGIO": {
        "compile_cmd": "compilatore {input} -o {output}",
        "run_cmd": "{executable}",
        "extension": ".ext",
        "category": "categoria"
    }
}
```

**2. Timeout Personalizzati:**
```python
# Default: 5 secondi per linguaggio
EXECUTION_TIMEOUT = 10  # Aumenta per task complesse
```

**3. Dataset Personalizzato:**
```bash
# Organizza file come:
data/custom_dataset/
├── code_snippets/
│   ├── python/
│   ├── java/
│   └── ...
```

### Opzioni per Ricerca Avanzata

Il comando `find` supporta pattern avanzati:
```bash
# Ricerca case-insensitive
python main.py find --task "BUBBLE"  # Trova "bubble", "Bubble", "BUBBLE"

# Ricerca parziale
python main.py find --task "fib"     # Trova "fibonacci", "fib_sequence", etc.

# Ricerca su categoria specifica (future enhancement)
python main.py find --task "sort" --category "algorithms"
```

## 📁 Struttura del Progetto

```
SWAM-Project/
├── main.py                    # 🎯 Interfaccia principale
├── requirements.txt           # 📋 Dipendenze Python (ben documentate)
├── SETUP_GUIDE.md            # 🚀 Guida installazione completa step-by-step
├── SYSTEM_REQUIREMENTS.md    # 💻 Comandi installazione per ogni OS
├── README.md                  # 📖 Documentazione principale
├── cleanup_sessions.py        # 🧹 Pulizia sessioni CO2
│
├── modules/                   # 📚 Moduli core del sistema
│   ├── language_config.py    #     Configurazioni linguaggi
│   ├── modern_dependency_analyzer.py  # Analisi dipendenze
│   └── modern_logger.py      #     Sistema logging avanzato
│
├── src/                      # 🚀 Esecutori e analizzatori
│   ├── task_searcher.py      #     🎯 Ricerca mirata task (NUOVO)
│   ├── smart_executor.py     #     🧠 Esecutore adattivo (MIGLIORATO)
│   ├── executor.py           #     🔧 Esecutore base senza conda
│   ├── language_tester.py    #     🧪 Test linguaggi disponibili
│   ├── benchmark.py          #     📊 Benchmark performance
│   ├── carbon_benchmark.py   #     🌱 Benchmark emissioni CO2
│   ├── carbon_tracker.py     #     📈 Tracking emissioni
│   ├── finder.py            #     🔍 Ricerca task comuni
│   └── cleaner.py           #     🧹 Pulizia file temporanei
│
├── data/                     # 📊 Dataset e dati
│   └── generated/           #     Dataset generato automaticamente
│       └── code_snippets/   #     Snippet di codice organizzati
│           ├── algorithms/  #     Algoritmi vari
│           ├── basic/       #     Esempi base
│           ├── c++/         #     Specifici per C++
│           ├── java/        #     Specifici per Java
│           ├── python/      #     Specifici per Python
│           └── ...          #     Altri linguaggi
│
├── results/                  # 📈 Output e risultati generati
│   ├── task_search/         #     🎯 Risultati ricerca mirata (NUOVO)
│   ├── execution/           #     Risultati esecuzione
│   ├── carbon_benchmark/    #     Report CO2 dettagliati  
│   ├── carbon/             #     Tracking emissioni
│   ├── csv/                #     File CSV per analisi
│   ├── logs/               #     Log di sistema
│   └── compliance/         #     Report conformità
│
└── scripts/                 # 🛠️ Script di utilità
    ├── export_to_csv.py    #     Esportazione dati CSV
    └── extract_top10_csv.py #     Estrazione TOP 10 task
```

### File Principali

**🎯 main.py** - Punto di ingresso principale:
- Interfaccia unificata per tutti i comandi
- Routing verso moduli specializzati
- Gestione parametri da linea di comando

**🧠 src/smart_executor.py** - Cuore del sistema:
- Rilevamento automatico linguaggi disponibili
- Esecuzione adattiva e gestione errori
- Integrazione con carbon tracking

**🎯 src/task_searcher.py** - Ricerca mirata (NUOVO):
- Pattern matching intelligente nel dataset
- Interfaccia interattiva per selezione task
- Supporto multi-estensione e multi-linguaggio

**🌱 src/carbon_tracker.py** - Monitoraggio emissioni:
- Integrazione con codecarbon
- Tracking automatico durante esecuzioni
- Gestione sessioni e cleanup

### Directory di Output

**results/task_search/** - Risultati ricerca mirata:
```
task_search_bubble_20251002_110447.json  # Risultati ricerca "bubble"
task_search_sort_20251002_110523.json    # Risultati ricerca "sort"
```

**results/execution/** - Risultati esecuzione:
```
language_test_results_20251002_110447.json  # Test linguaggi
execution_log_20251002_110523.log          # Log esecuzione
```

**results/carbon/** - Dati emissioni CO2:
```
emissions_20251002_110447.json  # Sessione CO2 specifica
carbon_summary_2025-10.json     # Riassunto mensile
```

### Dataset Organizzazione

The dataset in `data/generated/code_snippets/` is organized by:
- **Category**: algorithms/, basic/, strings/, mathematics/
- **Language**: python/, java/, cpp/, go/, rust/, etc.
- **Task**: Using the naming pattern `snippet_ID_Category_TaskName.ext`

Example:
```
data/generated/code_snippets/algorithms/python/
├── snippet_39_Sorting_algorithmsBubble_sort.py
├── snippet_25_Sorting_algorithmsComb_sort.py
└── snippet_27_Sorting_algorithmsCounting_sort.py
```

---

## Additional Information

### Supported Languages (Adaptive System)

The system automatically detects available languages and adapts accordingly.

**Typically Available (15/16):**
| Category | Languages | Availability |
|-----------|-----------|---------------|
| **Interpreted** | Python, JavaScript, Ruby, PHP, R, Julia | 6/6 (100%) |
| **Compiled** | C, C++, Java, Go, Rust, Haskell, OCaml, TypeScript | 8/8 (100%) |
| **VM-based** | C# (Mono) | 1/1 (100%) |
| **Commercial** | MATLAB* | 0/1 (0%) |

*MATLAB requires a commercial license

### Typical Performance

| Operation | Time | Notes |
|------------|-------|------|
| Language test | 2-5 seconds | 15-16 languages |
| Task search | <1 second | Pattern matching |
| Single execution | 0.01-2.0s | Depends on language |
| CO2 benchmark | 5-10 minutes | For the full dataset |

### Troubleshooting

**Language missing:**
```bash
python main.py test  # Shows missing languages
# The system continues with available languages
```

**Compilation errors:**
```bash
python main.py smart  # Uses only functioning languages
python main.py clean  # Cleans temporary files
```

---

**CLAP Project** © 2025 - Cross-Language Code Analysis System  
Author: Lorenzo Cappetti
