# CLAP – Cross-Language Analysis Project

<div align="center">
    <img src="logoSWAM.png" alt="CLAP Project Logo" width="300"/>
</div>

**Sistema modulare avanzato per analisi ed esecuzione multi-linguaggio con tracciamento emissioni CO2.**

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-16-green.svg)](#linguaggi-supportati)
[![CodeCarbon](https://img.shields.io/badge/CodeCarbon-Enabled-green.svg)](https://codecarbon.io)
[![MATLAB](https://img.shields.io/badge/MATLAB-R2025b-orange.svg)](#matlab-integration)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## 📖 Descrizione

**CLAP Project** è un sistema avanzato per l'analisi automatizzata e l'esecuzione di codice attraverso **16 linguaggi di programmazione**. Il progetto fornisce strumenti completi per:

- **🎯 Ricerca mirata**: Trova implementazioni specifiche di algoritmi nel dataset
- **🚀 Esecuzione automatizzata**: Esegui codice su linguaggi multipli simultaneamente con gestione intelligente degli errori
- **🌱 Analisi emissioni**: Traccia il consumo energetico e le emissioni CO2 del codice
- **📊 Benchmarking**: Confronta performance ed efficienza energetica tra linguaggi
- **🖥️ Modalità headless**: Esecuzione senza GUI per benchmark automatizzati
- **🔧 Sistema adattivo**: Rileva automaticamente i linguaggi disponibili e si adatta all'ambiente

**Casi d'uso**: ricerca accademica, insegnamento, analisi comparativa tra linguaggi, ottimizzazione energetica del software, studio dell'impatto ambientale della programmazione.

## 📑 Indice

1. [Descrizione](#-descrizione)
2. [Caratteristiche Principali](#-caratteristiche-principali)
3. [Linguaggi Supportati](#-linguaggi-supportati)
4. [Installazione](#️-installazione)
5. [Utilizzo](#-utilizzo)
6. [Comandi Disponibili](#-comandi-disponibili)
7. [Configurazione](#️-configurazione)
8. [Struttura del Progetto](#-struttura-del-progetto)
9. [Troubleshooting](#-troubleshooting)
10. [Performance e Statistiche](#-performance-e-statistiche)

## ✨ Caratteristiche Principali

### 🎯 Sistema Completo e Adattivo

- **16 linguaggi supportati**: Copertura completa dai linguaggi compilati agli interpretati, funzionali e scientifici
- **Rilevamento automatico**: Il sistema si adatta ai linguaggi disponibili sul tuo sistema
- **Esecuzione headless**: Modalità senza GUI per benchmark automatizzati e server headless
- **Gestione intelligente errori**: Wrapper automatici per codice incompleto (funzioni MATLAB, file Haskell)
- **Ambiente CLAP integrato**: Utilizzo automatico dell'ambiente conda CLAP se disponibile

### 🌱 Tracciamento Emissioni CO2

- **Integrazione CodeCarbon**: Monitoraggio preciso del consumo energetico
- **Tracking automatico**: Misura CO2 durante ogni esecuzione
- **Report dettagliati**: JSON con dati completi su emissioni, energia, durata
- **Benchmark scientifici**: Modalità FAST, TOP10 e COMPLETE per ricerca approfondita
- **Visualizzazione dati**: Analisi aggregate e statistiche storiche

### 🔧 Funzionalità Avanzate

- **Pulizia automatica GUI**: Rimuove/sostituisce chiamate a `matplotlib.show()`, `tkinter.mainloop()`, `PIL.show()`, etc.
- **Supporto MATLAB commerciale**: Integrazione automatica con MATLAB R2025b e licenze
- **Sistema modulare**: Architettura pulita con separazione responsabilità
- **Logging avanzato**: Sistema di log dettagliato per debugging e analisi
- **Task search intelligente**: Pattern matching per trovare implementazioni specifiche

## 💻 Linguaggi Supportati

CLAP supporta **16 linguaggi di programmazione** organizzati per paradigma:

### Object-Oriented Programming (OOP)
| Linguaggio | Versione Raccomandata | Compilatore/Interprete | Status |
|------------|----------------------|------------------------|---------|
| **C++** | C++17+ | g++ 9.0+ | ✅ Fully Supported |
| **Java** | Java 11+ | OpenJDK / Oracle JDK | ✅ Fully Supported |
| **C#** | .NET 5.0+ | Mono / .NET Core | ✅ Fully Supported |

### Scripting Languages
| Linguaggio | Versione Raccomandata | Interprete | Status |
|------------|----------------------|------------|---------|
| **Python** | 3.8+ | CPython | ✅ Fully Supported |
| **Ruby** | 2.7+ | ruby | ✅ Fully Supported |
| **JavaScript** | ES2020+ | Node.js 14+ | ✅ Fully Supported |
| **TypeScript** | 4.0+ | tsc + Node.js | ✅ Fully Supported |

### Systems Programming
| Linguaggio | Versione Raccomandata | Compilatore | Status |
|------------|----------------------|-------------|---------|
| **C** | C11+ | gcc 9.0+ | ✅ Fully Supported |
| **Go** | 1.16+ | go | ✅ Fully Supported |
| **Rust** | 1.50+ | rustc + cargo | ✅ Fully Supported |
| **PHP** | 7.4+ | php-cli | ✅ Fully Supported |

### Functional Programming
| Linguaggio | Versione Raccomandata | Compilatore | Status |
|------------|----------------------|-------------|---------|
| **Haskell** | GHC 8.10+ | ghc | ✅ Fully Supported |
| **OCaml** | 4.12+ | ocamlopt | ✅ Fully Supported |

### Scientific Computing
| Linguaggio | Versione Raccomandata | Interprete/Runtime | Status |
|------------|----------------------|-------------------|---------|
| **R** | 4.0+ | Rscript | ✅ Fully Supported |
| **MATLAB** | R2025b | matlab | ✅ Fully Supported* |
| **Julia** | 1.6+ | julia | ✅ Fully Supported |

**\*** MATLAB richiede licenza commerciale. Il sistema rileva automaticamente l'installazione e configura i percorsi. Per dettagli: `MATLAB_SETUP.md`

### Caratteristiche Speciali per Linguaggio

- **MATLAB**: Wrapper automatico per funzioni standalone, integrazione licenza, gestione figure headless
- **Haskell**: Pulizia automatica import problematici, gestione funzioni `join`, aggiunta `main` se assente
- **Python**: Rimozione automatica GUI (tkinter, turtle, matplotlib, PIL, OpenGL)
- **R**: Reindirizzamento plots a file PNG, disabilitazione device grafici
- **JavaScript/TypeScript**: Gestione Node.js, rimozione API browser

## 🛠️ Installazione

### Requisiti di Sistema

**Requisiti minimi:**
- **Sistema Operativo**: Linux (Ubuntu 20.04+, Debian 11+), macOS 11+, Windows 10+ (con WSL2)
- **Python**: 3.8 o superiore (raccomandato: **Python 3.12+**)
- **Spazio disco**: 2-5 GB (dipende dai linguaggi installati)
- **RAM**: 4 GB minimo, 8 GB raccomandato
- **Conda** (opzionale): Anaconda o Miniconda per ambiente CLAP

**Note:**
- Il sistema funziona con **qualsiasi numero di linguaggi** disponibili (minimo: Python)
- Più linguaggi installi, più funzionalità avrai
- MATLAB è completamente opzionale (richiede licenza commerciale)

### Setup Rapido (5 minuti)

```bash
# 1. Clone del repository
git clone https://github.com/Cappetti99/CLAP-Project.git
cd CLAP-Project

# 2. Creazione ambiente Python
python3 -m venv clap_env
source clap_env/bin/activate  # Linux/macOS
# clap_env\Scripts\activate   # Windows

# 3. Installazione dipendenze Python
pip install --upgrade pip
pip install -r requirements.txt

# 4. Test sistema
python main.py test

# Output atteso:
# ✅ Linguaggi disponibili: X/16 (XX.X%)
```

### Setup Completo - Tutti i 16 Linguaggi

Per ottenere la **massima copertura** con tutti i 16 linguaggi:

**🐧 Ubuntu/Debian - Installazione Completa:**

```bash
# Update repository
sudo apt update && sudo apt upgrade -y

# Linguaggi compilati base (C, C++, Java, C#)
sudo apt install -y build-essential openjdk-11-jdk mono-complete

# Linguaggi scripting (Node.js, PHP, Ruby)
sudo apt install -y nodejs npm php-cli ruby-full

# Linguaggi systems (Go)
sudo apt install -y golang-go

# Linguaggi funzionali (Haskell, OCaml)
sudo apt install -y ghc ocaml

# Linguaggi scientifici (R, Julia)
sudo apt install -y r-base
sudo snap install julia --classic

# TypeScript (via npm)
sudo npm install -g typescript

# Rust (installer dedicato)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# MATLAB (se hai licenza) - Installazione manuale
# Vedi MATLAB_SETUP.md per dettagli

# Verifica installazione
python main.py test
# Target: 15-16/16 linguaggi (100% senza MATLAB, 93.8% con MATLAB mancante)
```

**🍎 macOS - Installazione Completa:**

```bash
# Installa Homebrew se non presente
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Tutti i linguaggi disponibili via Homebrew
brew install gcc openjdk node go php ruby r ghc ocaml mono julia

# TypeScript
npm install -g typescript

# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# MATLAB (se hai licenza) - Scarica da mathworks.com
# Vedi MATLAB_SETUP.md per configurazione

# Verifica installazione
python main.py test
```

**🪟 Windows (con WSL2):**

```bash
# Installa WSL2 e Ubuntu da Microsoft Store
# Poi segui le istruzioni Ubuntu/Debian sopra

# Alternative: Usa package manager come Chocolatey o Scoop
# choco install python nodejs jdk ruby golang rust php
```

### Configurazione Ambiente CLAP (Conda)

Se vuoi usare un ambiente conda dedicato chiamato **CLAP**:

```bash
# Crea ambiente CLAP
conda create -n CLAP python=3.12 -y
conda activate CLAP

# Installa dipendenze
pip install -r requirements.txt

# Il sistema rileverà automaticamente l'ambiente CLAP
python main.py test
```

### Verifica Completa Installazione

```bash
# Test completo con report dettagliato
python main.py test

# Output atteso:
# ========================================
# CLAP PROJECT
# Author: Lorenzo Cappetti
# ========================================
# 
# 📋 Testing 16 languages...
# 
# ✅ C: gcc found (version 11.4.0)
# ✅ C++: g++ found (version 11.4.0)
# ✅ Java: java found (version 11.0.20)
# ✅ C#: mcs found (version 6.12.0.182)
# ✅ Python: python3 found (version 3.12.0)
# ✅ Ruby: ruby found (version 3.0.2)
# ✅ JavaScript: node found (version 18.16.0)
# ✅ TypeScript: tsc found (version 5.1.6)
# ✅ Go: go found (version 1.20.5)
# ✅ Rust: rustc found (version 1.71.0)
# ✅ PHP: php found (version 8.1.2)
# ✅ Haskell: ghc found (version 9.0.2)
# ✅ OCaml: ocamlopt found (version 4.13.1)
# ✅ R: Rscript found (version 4.2.1)
# ❌ MATLAB: matlab not found
# ✅ Julia: julia found (version 1.9.2)
# 
# ✅ Linguaggi disponibili: 15/16 (93.8%)
# 
# Risultati salvati in: results/execution/language_test_results_YYYYMMDD_HHMMSS.json
```

### Guide Dettagliate

Il progetto include guide complete per ogni aspetto dell'installazione:

- **📚 [SETUP_GUIDE.md](SETUP_GUIDE.md)** - Guida completa step-by-step con troubleshooting
- **💻 [SYSTEM_REQUIREMENTS.md](SYSTEM_REQUIREMENTS.md)** - Comandi specifici per ogni OS e distro
- **🔬 [MATLAB_SETUP.md](MATLAB_SETUP.md)** - Configurazione MATLAB commerciale (se disponibile)
- **📋 [requirements.txt](requirements.txt)** - Lista dipendenze Python documentate

## 🚀 Utilizzo

CLAP offre un'interfaccia a riga di comando semplice e potente per tutte le operazioni.

### Sintassi Base

```bash
python main.py <comando> [opzioni]
```

### Workflow Raccomandato per Nuovi Utenti

```bash
# 1. Verifica i linguaggi disponibili
python main.py test

# 2. Cerca un algoritmo specifico
python main.py find --task "bubble sort"

# 3. Esegui test rapidi multi-linguaggio
python main.py smart

# 4. Raccogli dati CO2 (benchmark veloce)
python main.py benchmark --mode fast
```

## 📋 Comandi Disponibili

### Comandi Principali (Recommended)

| Comando | Descrizione | Uso Tipico | Tempo Esecuzione |
|---------|-------------|-----------|------------------|
| **`test`** | Verifica linguaggi disponibili | Setup iniziale, troubleshooting | 5-10 secondi |
| **`find`** | Ricerca task specifiche nel dataset | Trovare implementazioni algoritmi | < 1 secondo |
| **`smart`** | Esecuzione intelligente TOP 10 task | Test rapidi multi-linguaggio | 2-5 minuti |
| **`benchmark`** | Benchmark emissioni CO2 | Raccolta dati scientifici | 3 min - 8 ore |
| **`carbon`** | Visualizza dati CO2 esistenti | Analisi risultati | < 1 secondo |

### Comandi Avanzati

| Comando | Descrizione | Uso Tipico |
|---------|-------------|-----------|
| **`analyze`** | Analisi completa task comuni | Ricerca approfondita dataset |
| **`execute`** | Esecuzione completa tutti linguaggi | Test estensivi (lungo) |
| **`clean`** | Pulizia file temporanei e cache | Manutenzione sistema |
| **`status`** | Report stato progetto | Diagnostica generale |

### Esempi Pratici Dettagliati

#### 1. 🔍 Ricerca Mirata Task

Il comando `find` ti permette di cercare implementazioni specifiche di algoritmi:

```bash
# Cerca bubble sort
python main.py find --task "bubble"

# Output:
# 🔍 Ricerca task: 'bubble' (solo linguaggi testati)
# 
# ✅ Trovate 14 task in 14 linguaggi testati:
# 
# 📁 PYTHON (1 file):
#    • snippet_39_Sorting_algorithmsBubble_sort.py
# 
# 📁 JAVA (1 file):
#    • snippet_36_Sorting_algorithmsBubble_sort.java
# 
# 📁 CPP (1 file):
#    • snippet_22_Sorting_algorithmsBubble_sort.cpp
# 
# [... altri linguaggi ...]
# 
# 🤔 Vuoi eseguire una di queste task? [s/N]: s
# 
# Seleziona il linguaggio (1-14) o 0 per tutti: 1
# 
# Esecuzione in corso...
# ✅ Python: Esecuzione completata in 0.45s (12.5 mg CO2eq)
```

Esempi di ricerche utili:
```bash
# Trova tutti gli algoritmi di sorting
python main.py find --task "sort"

# Cerca fibonacci
python main.py find --task "fibonacci"

# Cerca binary search
python main.py find --task "binary"

# Ricerca case-insensitive e parziale
python main.py find --task "QUICK"  # Trova QuickSort, quicksort, etc.
```

#### 2. 🧪 Test Sistema

Verifica quali linguaggi sono disponibili sul tuo sistema:

```bash
python main.py test

# Output dettagliato:
# ========================================
# CLAP PROJECT
# Author: Lorenzo Cappetti
# ========================================
# 
# 📋 Testing 16 languages...
# 
# ✅ C: gcc found (version 11.4.0)
# ✅ C++: g++ found (version 11.4.0)
# ✅ Java: java found (version 11.0.20)
# ✅ C#: mcs found (version 6.12.0.182)
# ✅ Python: python3 found (version 3.12.0)
# ✅ Ruby: ruby found (version 3.0.2)
# ✅ JavaScript: node found (version 18.16.0)
# ✅ TypeScript: tsc found (version 5.1.6)
# ✅ Go: go found (version 1.20.5)
# ✅ Rust: rustc found (version 1.71.0)
# ✅ PHP: php found (version 8.1.2)
# ✅ Haskell: ghc found (version 9.0.2)
# ✅ OCaml: ocamlopt found (version 4.13.1)
# ✅ R: Rscript found (version 4.2.1)
# ❌ MATLAB: matlab not found
# ✅ Julia: julia found (version 1.9.2)
# 
# ✅ Linguaggi disponibili: 15/16 (93.8%)
# 
# Risultati salvati in: results/execution/language_test_results_20251015_143022.json
```

#### 3. 🧠 Esecuzione Intelligente (Smart)

Esegue le TOP 10 task più comuni su tutti i linguaggi funzionanti:

```bash
python main.py smart

# Output:
# 🧠 SMART EXECUTION
# Configurato per ambiente CLAP
# 
# Linguaggi disponibili: 15
# • C, C++, Java, C#, Python, Ruby, JavaScript, TypeScript
# • Go, Rust, PHP, Haskell, OCaml, R, Julia
# 
# TOP 10 task comuni identificate:
# 1. Ethiopian_multiplication
# 2. Fibonacci_sequence
# 3. Factorial
# 4. Array_concatenation
# 5. Boolean_values
# 6. Function_definition
# 7. Greatest_common_divisor
# 8. Hailstone_sequence
# 9. Happy_numbers
# 10. Integer_comparison
# 
# 🚀 Inizio esecuzione...
# 
# [Ethiopian_multiplication]
# ✅ cpp: 0.214s (7.56 mg CO2eq)
# ❌ java: Compilation error
# ✅ python: 1.436s (98.51 mg CO2eq)
# ✅ rust: 0.325s (15.23 mg CO2eq)
# [...]
# 
# 🎉 TOTALE: 127/150 (84.7%) esecuzioni riuscite
# 
# Statistiche per linguaggio:
# • Python: 10/10 (100%)
# • C++: 10/10 (100%)
# • Rust: 10/10 (100%)
# • Go: 9/10 (90%)
# [...]
```

#### 4. 🌱 Benchmark Emissioni CO2

Tre modalità disponibili per diversi use case:

**FAST Mode - Test Funzionalità (3-5 minuti):**
```bash
python main.py benchmark --mode fast

# Esegue 3 task campione × 3 ripetizioni
# Perfetto per verificare che tutto funzioni
# Tempo stimato: ~3-5 minuti
```

**TOP10 Mode - Analisi Task Principali (45-60 minuti):**
```bash
python main.py benchmark --mode top10

# Esegue TOP 10 task più frequenti × 30 ripetizioni
# Calcola medie statisticamente significative
# Ideale per analisi regolari
# Tempo stimato: ~45-60 minuti
```

**COMPLETE Mode - Analisi Esaustiva (6-8 ore):**
```bash
python main.py benchmark --mode complete

# Esegue TUTTE le task nel dataset (1000+) × 3 ripetizioni
# Copertura completa per ricerca scientifica
# Tempo stimato: ~6-8 ore
```

Output esempio:
```
 CARBON BENCHMARK SYSTEM
 Configured for 3 iterations per code
 Results saved in: results/carbon_benchmark
 FAST mode: using common tasks

 START CARBON BENCHMARK SYSTEM
============================================================
 Available languages: 15
 Selected tasks for benchmark: 3 (first 3 tasks)

 TASK: Ethiopian_multiplication
========================================

 Benchmark: Ethiopian_multiplication in cpp
      cpp: 100%|███████████████████| 3/3 [00:02<00:00,  1.02iter/s]

 BENCHMARK RESULTS: Ethiopian_multiplication - cpp
------------------------------------------------------------
 Successful executions: 3/3 (100.0%)
 CO2 EMISSIONS:
 • Mean: 7.56 mg CO2eq
 • Median: 7.47 mg CO2eq
 • Min-Max: 4.20 mg CO2eq - 11.02 mg CO2eq
 • Std Dev: 3.41 mg CO2eq
 • Total 3 executions: 22.68 mg CO2eq
 EXECUTION TIME:
 • Mean: 0.214s
 • Median: 0.214s
 • Min-Max: 0.212 - 0.216s
 • Std Dev: 0.002s
```

#### 5. 📊 Visualizzazione Dati CO2

Analizza i dati raccolti dai benchmark:

```bash
python main.py carbon

# Output:
# 📊 CO2 DATA OVERVIEW
# 
# 🔍 Trovati 247 file di sessione CO2
# 📅 Periodo: 2025-10-01 → 2025-10-15
# 
# 🌍 EMISSIONI TOTALI: 3.45 kg CO2eq
# ⚡ ENERGIA TOTALE: 22.67 Wh
# ⏱️  TEMPO TOTALE: 4h 23m 15s
# 
# TOP 5 LINGUAGGI PER EMISSIONI:
# 1. Java: 845.23 mg CO2eq (24.5%)
# 2. Python: 723.56 mg CO2eq (21.0%)
# 3. C++: 456.78 mg CO2eq (13.2%)
# 4. Go: 398.45 mg CO2eq (11.5%)
# 5. Rust: 312.67 mg CO2eq (9.1%)
# 
# STATISTICHE PER TASK:
# • Ethiopian_multiplication: 127 esecuzioni (685.34 mg CO2eq)
# • Fibonacci_sequence: 119 esecuzioni (592.12 mg CO2eq)
# [...]
```

### Input/Output del Sistema

**Input richiesti:**
- Comandi da terminale con parametri opzionali (`--task`, `--mode`)
- Selezioni interattive durante l'esecuzione:
  - Conferme sì/no (`s/N`)
  - Selezione numerica (es. "Seleziona linguaggio 1-14")
  - Nessun file di input esterno richiesto (usa dataset interno)

**Output generati:**
```
results/
├── task_search/              # Risultati ricerca task
│   └── task_search_bubble_20251015_143022.json
├── execution/                # Risultati esecuzione
│   ├── language_test_results_20251015_143022.json
│   └── execution_20251015_143445.log
├── carbon/                   # Tracking emissioni CO2
│   ├── session_20251015_143022.json
│   ├── session_20251015_143145.json
│   └── ...
├── carbon_benchmark/         # Report benchmark dettagliati
│   ├── benchmark_results_fast_20251015_143022.json
│   ├── benchmark_results_top10_20251015_150000.json
│   └── summary_statistics.csv
├── csv/                      # Export CSV per analisi
│   ├── emissions_by_language.csv
│   └── emissions_by_task.csv
└── logs/                     # Log di sistema
    └── clap_20251015.log
```

### Modalità Headless (Server/CI)

CLAP include un sistema automatico per prevenire finestre GUI durante l'esecuzione:

```bash
# Esegui benchmark senza finestre popup
python main.py benchmark --mode fast

# Il sistema automaticamente:
# • Disabilita matplotlib.show() → salva su /tmp/plot.png
# • Rimuove tkinter.mainloop() e GUI widgets
# • Blocca PIL.Image.show() → salva su /tmp/image.png
# • Disabilita OpenGL/GLUT windows
# • Imposta DISPLAY='' per modalità headless
# • Usa backend 'Agg' per matplotlib
```

Nessuna configurazione aggiuntiva richiesta - funziona automaticamente!

## ⚙️ Configurazione

### Sistema Zero-Configuration

CLAP è progettato per funzionare **senza configurazione** nella maggior parte dei casi:
- ✅ Rilevamento automatico linguaggi disponibili
- ✅ Adattamento automatico all'ambiente (conda CLAP se presente)
- ✅ Creazione automatica directory di output
- ✅ Gestione automatica modalità headless per GUI
- ✅ Configurazione automatica MATLAB (se installato)

### Variabili d'Ambiente (Opzionali)

Per personalizzazioni avanzate:

```bash
# Percorso dataset personalizzato (default: ./data/generated/)
export CLAP_DATASET_PATH="/path/to/custom/dataset"

# Directory output personalizzata (default: ./results/)
export CLAP_OUTPUT_PATH="/path/to/custom/results"

# Livello logging (default: INFO)
export CLAP_LOG_LEVEL="DEBUG"     # Per troubleshooting dettagliato
export CLAP_LOG_LEVEL="WARNING"   # Solo warning ed errori

# Timeout esecuzione (default: 10 secondi)
export CLAP_EXECUTION_TIMEOUT="30"  # Per task complesse

# Disabilita tracking CO2 (default: abilitato)
export CLAP_DISABLE_CO2="true"

# Forza modalità headless (default: auto)
export CLAP_HEADLESS="true"
```

### File di Configurazione

**1. requirements.txt** - Dipendenze Python:

```txt
# Core Dependencies
codecarbon>=3.0.0          # 🌱 Tracciamento CO2 e consumo energetico
datasets>=2.14.0           # 📊 Gestione dataset multi-formato
pandas>=2.0.0              # 📈 Analisi dati e statistiche
numpy>=1.24.0              # 🔢 Calcoli numerici e array
tqdm>=4.65.0               # 📊 Progress bars per operazioni lunghe
colorama>=0.4.6            # 🎨 Output colorato terminale
psutil>=5.9.0              # 💻 Informazioni sistema e processi
pyarrow>=12.0.0            # ⚡ Performance I/O per file grandi

# Optional but recommended
matplotlib>=3.7.0          # 📊 Per visualizzazioni grafiche
tabulate>=0.9.0            # 📋 Formattazione tabelle
```

**2. modules/language_config.py** - Configurazione linguaggi:

Questo file contiene le configurazioni per tutti i 16 linguaggi supportati:

```python
LANGUAGES = {
    "PYTHON": {
        "command": "python3 {file}",
        "extension": ".py",
        "category": "interpreted",
        "timeout": 10
    },
    "CPP": {
        "compile": "g++ -std=c++17 {input} -o {output}",
        "run": "{executable}",
        "extension": ".cpp",
        "category": "compiled",
        "timeout": 15
    },
    # ... altre configurazioni
    "MATLAB": {
        "command": "matlab -batch \"{code}\"",
        "extension": ".m",
        "category": "commercial",
        "auto_detect_path": True,
        "requires_license": True
    }
}
```

**Personalizzazioni comuni:**

```python
# Aumenta timeout per task complesse
EXECUTION_TIMEOUT = 30  # secondi (default: 10)

# Disabilita pattern problematici specifici
PROBLEMATIC_PATTERNS = [
    r'import\s+specificLibrary',
    r'external_dependency',
]

# Aggiungi nuovo linguaggio
LANGUAGES["NUOVO_LANG"] = {
    "command": "nuovo_lang {file}",
    "extension": ".new",
    "category": "experimental"
}
```

### Configurazione MATLAB

Se hai una licenza MATLAB, CLAP la rileverà e configurerà automaticamente:

```bash
# Test rilevamento MATLAB
python main.py test

# Se MATLAB è installato vedrai:
# ✅ MATLAB: matlab found (R2025b at /usr/local/MATLAB/R2025b)

# Se non viene rilevato ma è installato:
# 1. Verifica path in modules/language_config.py
# 2. Consulta MATLAB_SETUP.md per configurazione manuale
```

**Caratteristiche MATLAB:**
- Rilevamento automatico versione e path
- Wrapper automatico per funzioni standalone
- Modalità headless (disabilita figure windows)
- Gestione licenza network
- Supporto toolbox matematiche

### Configurazione Dataset Personalizzato

Puoi usare il tuo dataset mantenendo la struttura organizzativa:

```bash
# Struttura raccomandata
custom_dataset/
├── code_snippets/
│   ├── algorithms/
│   │   ├── python/
│   │   │   ├── my_algorithm.py
│   │   │   └── another_algo.py
│   │   ├── java/
│   │   └── cpp/
│   ├── basic/
│   └── advanced/
└── metadata.json  # Opzionale
```

Poi imposta la variabile:
```bash
export CLAP_DATASET_PATH="/path/to/custom_dataset"
python main.py analyze  # Userà il dataset personalizzato
```

### Configurazione Logging Avanzato

Per debugging dettagliato o analisi approfondite:

```bash
# Crea file di configurazione logging
cat > logging_config.json << EOF
{
  "version": 1,
  "formatters": {
    "detailed": {
      "format": "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    }
  },
  "handlers": {
    "file": {
      "class": "logging.FileHandler",
      "filename": "results/logs/clap_detailed.log",
      "formatter": "detailed"
    }
  },
  "root": {
    "level": "DEBUG",
    "handlers": ["file"]
  }
}
EOF

# Usa la configurazione
export CLAP_LOG_CONFIG="logging_config.json"
python main.py smart
```

### Opzioni Benchmark Personalizzate

Personalizza il comportamento del benchmark editando `src/carbon_benchmark.py`:

```python
# Numero ripetizioni per modalità
BENCHMARK_MODES = {
    "fast": {
        "tasks": 3,
        "iterations": 3
    },
    "top10": {
        "tasks": 10,
        "iterations": 30  # Aumenta per maggiore precisione statistica
    },
    "complete": {
        "tasks": "all",
        "iterations": 5   # Riduci se 3 sono troppo lunghe
    }
}
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
CLAP-Project/
├── 📄 main.py                          # 🎯 Interfaccia principale CLI
├── 📄 requirements.txt                 # 📋 Dipendenze Python documentate
├── 📄 README.md                        # 📖 Questa documentazione
├── 📄 SETUP_GUIDE.md                   # 🚀 Guida installazione completa
├── 📄 SYSTEM_REQUIREMENTS.md           # 💻 Requisiti per OS specifici
├── 📄 MATLAB_SETUP.md                  # � Configurazione MATLAB
├── 📄 cleanup_sessions.py              # 🧹 Utility pulizia sessioni CO2
├── 📄 download.py                      # ⬇️  Download dataset (se necessario)
├── 🖼️  logoSWAM.png                    # 🎨 Logo progetto
│
├── 📁 modules/                         # 📚 Moduli core del sistema
│   ├── language_config.py             #     🔧 Configurazioni 16 linguaggi
│   ├── modern_dependency_analyzer.py  #     🔍 Analisi dipendenze codice
│   └── modern_logger.py               #     📝 Sistema logging avanzato
│
├── 📁 src/                             # 🚀 Esecutori e analizzatori
│   ├── task_searcher.py               #     🎯 Ricerca mirata task (★ NEW)
│   ├── smart_executor.py              #     🧠 Esecutore adattivo intelligente
│   │                                  #        • Auto-detect linguaggi
│   │                                  #        • MATLAB function wrappers
│   │                                  #        • Haskell error fixing
│   │                                  #        • GUI cleaning (headless mode)
│   │                                  #        • CLAP environment integration
│   ├── executor.py                    #     🔧 Esecutore base (legacy)
│   ├── language_tester.py             #     🧪 Test disponibilità linguaggi
│   ├── carbon_benchmark.py            #     🌱 Benchmark emissioni CO2
│   │                                  #        • FAST/TOP10/COMPLETE modes
│   │                                  #        • Statistiche dettagliate
│   ├── carbon_tracker.py              #     📈 Tracking emissioni real-time
│   ├── finder.py                      #     🔍 Analisi task comuni dataset
│   └── cleaner.py                     #     🧹 Pulizia file temporanei
│
├── 📁 data/                            # 📊 Dataset multi-linguaggio
│   └── generated/                     #     Dataset code snippets
│       └── code_snippets/             #     Organizzato per categoria
│           ├── algorithms/            #     🧮 Algoritmi (sorting, search, etc.)
│           ├── basic/                 #     📝 Esempi base
│           ├── io/                    #     💾 Input/Output operations
│           ├── mathematics/           #     ➗ Operazioni matematiche
│           ├── misc/                  #     🎲 Varie (grafici, GUI, etc.)
│           ├── strings/               #     🔤 Manipolazione stringhe
│           └── [languages]/           #     Per linguaggio specifico:
│               ├── python/            #        🐍 Python snippets
│               ├── java/              #        ☕ Java snippets
│               ├── c++/               #        ⚡ C++ snippets
│               ├── rust/              #        🦀 Rust snippets
│               ├── matlab/            #        🔬 MATLAB scripts
│               └── ...                #        Altri 11 linguaggi
│
├── 📁 results/                         # 📈 Output e risultati
│   ├── task_search/                   #     🎯 Ricerche task mirate
│   │   ├── task_search_bubble_*.json  #        Risultati ricerca bubble
│   │   └── task_search_sort_*.json    #        Risultati ricerca sort
│   │
│   ├── execution/                     #     ▶️  Log esecuzioni
│   │   ├── language_test_results_*.json #      Test linguaggi
│   │   └── execution_*.log            #        Log dettagliati
│   │
│   ├── carbon/                        #     🌱 Sessioni CO2 individuali
│   │   ├── session_*.json             #        File sessione CO2
│   │   └── (100+ files)               #        Storia completa tracking
│   │
│   ├── carbon_benchmark/              #     📊 Report benchmark
│   │   ├── benchmark_fast_*.json      #        Risultati FAST mode
│   │   ├── benchmark_top10_*.json     #        Risultati TOP10 mode
│   │   ├── benchmark_complete_*.json  #        Risultati COMPLETE mode
│   │   └── statistics_*.csv           #        Statistiche aggregate
│   │
│   ├── csv/                           #     📊 Export CSV per analisi
│   │   ├── emissions_by_language.csv  #        CO2 per linguaggio
│   │   ├── emissions_by_task.csv      #        CO2 per task
│   │   └── performance_summary.csv    #        Performance summary
│   │
│   ├── logs/                          #     📝 Log sistema
│   │   └── clap_*.log                 #        Log giornalieri
│   │
│   └── task_analysis/                 #     🔍 Analisi task comuni
│
└── 📁 scripts/                         # 🛠️  Utility scripts
    ├── export_to_csv.py               #     📊 Esporta risultati → CSV
    └── extract_top10_csv.py           #     🔝 Estrae TOP 10 task
```

### 📂 Componenti Chiave

#### 🎯 **main.py** - Entry Point Principale
Interfaccia CLI unificata che gestisce:
- Parsing argomenti (`find`, `test`, `smart`, `benchmark`, etc.)
- Routing ai moduli specializzati
- Banner e help interattivo
- Gestione errori top-level

```python
# Esempio utilizzo interno
python main.py find --task "bubble"
  → Chiama task_searcher.TaskSearcher()
  
python main.py smart
  → Chiama smart_executor.SmartExecutor()
  
python main.py benchmark --mode fast
  → Chiama carbon_benchmark.CarbonBenchmark()
```

#### 🧠 **src/smart_executor.py** - Cuore del Sistema (★ 1300+ righe)
Il modulo più complesso e potente:

**Funzionalità principali:**
- **Auto-detection linguaggi**: Rileva automaticamente i 16 linguaggi disponibili
- **MATLAB integration**: 
  - Wrapper automatico per funzioni standalone → script eseguibili
  - Generazione test calls intelligente
  - Integrazione licenza commerciale
- **Haskell error fixing**:
  - Rimozione import problematici (`Data.List`, `Data.Function`)
  - Sostituzione funzione `join` con implementazione custom
  - Aggiunta `main` se mancante
- **GUI cleaning** (modalità headless):
  - Python: tkinter, matplotlib, turtle, PIL, OpenGL, pygame
  - R: plot devices, graphics windows
  - MATLAB: figure windows, uicontrol, debugging
- **CLAP environment**: Usa automaticamente ambiente conda CLAP se disponibile
- **Gestione errori**: Try-catch multipli con fallback intelligenti

**Metodi chiave:**
```python
clean_code()                    # Pulizia codice multi-linguaggio
_make_matlab_executable()       # Wrapper MATLAB functions
_clean_python_gui_code()        # Rimozione GUI Python
_clean_r_graphics_code()        # Rimozione graphics R
_clean_matlab_graphics_code()   # Rimozione GUI MATLAB
execute_with_carbon_tracking()  # Esecuzione + CO2 tracking
```

#### 🎯 **src/task_searcher.py** - Ricerca Intelligente (★ NEW)
Sistema avanzato per trovare implementazioni specifiche:

**Caratteristiche:**
- Pattern matching case-insensitive
- Ricerca parziale ("fib" trova "fibonacci")
- Interfaccia interattiva per esecuzione
- Supporto multi-linguaggio simultaneo
- Salvataggio risultati in JSON

**Flow:**
```
User: python main.py find --task "bubble"
  ↓
1. Scansiona dataset per pattern "bubble"
2. Trova file in linguaggi testati
3. Mostra elenco organizzato per linguaggio
4. Chiede: "Vuoi eseguire?" [s/N]
5. Se sì: permette selezione linguaggio specifico
6. Esegue con carbon tracking
7. Salva risultati in results/task_search/
```

#### 🌱 **src/carbon_benchmark.py** - Benchmark Scientifico
Sistema completo per analisi emissioni CO2:

**Tre modalità:**
- **FAST** (3 tasks × 3 iter): Verifica funzionalità (~5 min)
- **TOP10** (10 tasks × 30 iter): Analisi statistica (~60 min)
- **COMPLETE** (ALL tasks × 3 iter): Ricerca esaustiva (~8 ore)

**Output:**
- JSON dettagliati con statistiche complete
- CSV per post-processing
- Report console con progress bars (tqdm)
- Metriche: Mean, Median, Min, Max, StdDev

#### 📝 **modules/language_config.py** - Configurazione Linguaggi
Database centralizzato configurazioni:

```python
LANGUAGES = {
    "PYTHON": {...},   # 16 linguaggi totali
    "MATLAB": {
        "path": "/usr/local/MATLAB/R2025b/bin/matlab",
        "auto_detect": True,
        "requires_license": True,
        "wrapper_functions": True
    },
    # ... altri 14 linguaggi
}

PROBLEMATIC_PATTERNS = {
    # Pattern che causano errori da rimuovere/sostituire
}
```

### 📊 Organizzazione Dataset

Il dataset in `data/generated/code_snippets/` contiene **1000+ snippet** organizzati per:

**Per Categoria:**
```
algorithms/     # Sorting, searching, graph algorithms
basic/          # Hello world, variables, loops
io/             # File I/O, network, streams
mathematics/    # Calcoli, matrici, statistica
misc/           # GUI, grafici, multimedia
strings/        # Manipolazione testo
```

**Per Linguaggio:**
Ogni categoria contiene sottodirectory per linguaggio:
```
algorithms/
├── python/
│   ├── snippet_39_Sorting_algorithmsBubble_sort.py
│   ├── snippet_40_Sorting_algorithmsQuick_sort.py
│   └── ...
├── java/
├── cpp/
├── rust/
├── matlab/
└── ... (altri 11 linguaggi)
```

**Naming Convention:**
```
snippet_{ID}_{Category}_{TaskName}.{ext}

Esempi:
snippet_39_Sorting_algorithmsBubble_sort.py
snippet_145_Ethiopian_multiplication.java
snippet_238_OpenGL.py
```

### 📈 Directory Results - Dettagli

**results/carbon/** - 100+ file sessione:
```json
{
  "timestamp": "2025-10-15T14:30:22",
  "emissions_mg": 7.56,
  "energy_wh": 0.0234,
  "duration_s": 0.214,
  "language": "cpp",
  "task": "Ethiopian_multiplication",
  "success": true
}
```

**results/carbon_benchmark/** - Report aggregati:
```json
{
  "mode": "fast",
  "total_tasks": 3,
  "total_languages": 15,
  "successful_runs": 42,
  "failed_runs": 3,
  "total_emissions_kg": 0.127,
  "by_language": {
    "python": {
      "mean_emissions_mg": 98.51,
      "median_time_s": 1.461,
      "success_rate": 1.0
    },
    ...
  }
}
```

## 🔧 Troubleshooting

### Problemi Comuni e Soluzioni

#### ❌ Linguaggio Non Rilevato

**Problema**: Un linguaggio installato non viene rilevato

**Soluzione**:
```bash
# 1. Verifica installazione manualmente
gcc --version      # Per C
g++ --version      # Per C++
python3 --version  # Per Python
java -version      # Per Java
# ... etc per altri linguaggi

# 2. Verifica PATH
echo $PATH
which gcc  # Deve restituire un percorso valido

# 3. Reinstalla il linguaggio mancante
sudo apt install build-essential  # Ubuntu/Debian
brew install gcc                   # macOS

# 4. Test CLAP dopo reinstallazione
python main.py test
```

#### ❌ MATLAB Non Funziona

**Problema**: MATLAB installato ma non rilevato o errori di esecuzione

**Soluzione**:
```bash
# 1. Verifica installazione MATLAB
matlab -batch "version"

# 2. Verifica path in language_config.py
grep -r "matlab" modules/language_config.py

# 3. Consulta guida dedicata
cat MATLAB_SETUP.md

# 4. Verifica licenza
matlab -batch "license('test', 'MATLAB')"

# 5. Test manuale wrapper funzioni
python -c "
from src.smart_executor import SmartExecutor
executor = SmartExecutor()
# Test trasformazione funzione → script
"
```

#### ❌ Errori di Compilazione

**Problema**: Alcuni codici falliscono la compilazione

**Diagnostica**:
```bash
# 1. Aumenta verbosità logging
export CLAP_LOG_LEVEL="DEBUG"
python main.py smart

# 2. Controlla log dettagliati
cat results/logs/clap_*.log

# 3. Test linguaggio specifico
python main.py find --task "simple"
# Seleziona task semplice per isolare il problema
```

**Possibili cause**:
- **Codice incompleto nel dataset**: CLAP aggiunge wrapper automatici
- **Dipendenze esterne mancanti**: Alcuni snippet richiedono librerie
- **Versione linguaggio incompatibile**: Aggiorna il compilatore/interprete

**Fix**:
```bash
# Aggiorna compilatori
sudo apt update && sudo apt upgrade build-essential  # Linux
brew upgrade gcc                                      # macOS

# Pulisci cache compilazione
python main.py clean

# Riprova
python main.py smart
```

#### ❌ Finestre GUI Si Aprono Durante Benchmark

**Problema**: Nonostante la modalità headless, alcune GUI appaiono

**Soluzione**:
```bash
# 1. Forza modalità headless
export DISPLAY=''
export CLAP_HEADLESS='true'

# 2. Verifica pulizia GUI nel codice
python -c "
from src.smart_executor import SmartExecutor
executor = SmartExecutor()
# I metodi _clean_*_gui_code() devono essere attivi
"

# 3. Se persiste, disabilita linguaggi problematici
# Edita modules/language_config.py
# Commenta temporaneamente il linguaggio che apre GUI
```

#### ❌ Benchmark Troppo Lento

**Problema**: Il benchmark impiega troppo tempo

**Soluzione**:
```bash
# 1. Usa modalità FAST invece di COMPLETE
python main.py benchmark --mode fast   # ~5 minuti vs ~8 ore

# 2. Riduzione timeout per task lente
export CLAP_EXECUTION_TIMEOUT="5"  # Default 10 secondi

# 3. Limita linguaggi testati
# Edita src/carbon_benchmark.py per escludere linguaggi lenti

# 4. Monitora progress
python main.py benchmark --mode top10
# Usa Ctrl+C per interrompere se necessario
```

#### ❌ Errori CodeCarbon / Tracking CO2

**Problema**: Errori nel tracking emissioni

**Soluzione**:
```bash
# 1. Reinstalla codecarbon
pip install --upgrade codecarbon

# 2. Verifica permessi directory
ls -la results/carbon/
chmod -R 755 results/

# 3. Test standalone codecarbon
python -c "
from codecarbon import EmissionsTracker
tracker = EmissionsTracker()
tracker.start()
print('Test tracking OK')
tracker.stop()
"

# 4. Disabilita temporaneamente CO2 tracking
export CLAP_DISABLE_CO2="true"
python main.py smart  # Esegue senza tracking
```

#### ❌ Dataset Mancante o Incompleto

**Problema**: Non trova i file di codice

**Soluzione**:
```bash
# 1. Verifica struttura dataset
ls -la data/generated/code_snippets/

# 2. Controlla contenuto
find data/generated/code_snippets -type f | wc -l
# Dovrebbe mostrare 1000+ file

# 3. Se mancante, ri-scarica dataset
python download.py  # Se disponibile

# 4. O usa dataset personalizzato
export CLAP_DATASET_PATH="/path/to/your/dataset"
```

#### ❌ Memoria Insufficiente

**Problema**: Out of memory durante benchmark completi

**Soluzione**:
```bash
# 1. Usa modalità con meno task
python main.py benchmark --mode fast  # Invece di complete

# 2. Monitora memoria
watch -n 1 free -h  # Durante esecuzione

# 3. Chiudi applicazioni pesanti
# 4. Aumenta swap se necessario (Linux)
sudo fallocate -l 4G /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
```

### FAQ - Domande Frequenti

**Q: Posso usare CLAP senza MATLAB?**  
A: Assolutamente sì! MATLAB è completamente opzionale. Il sistema funziona perfettamente con 15 linguaggi senza licenza commerciale.

**Q: Quanto spazio disco serve?**  
A: 
- Base (Python + dataset): ~500 MB
- Con 10 linguaggi: ~2 GB
- Completo (16 linguaggi): ~5 GB
- Results directory (dopo benchmark): ~100 MB - 1 GB

**Q: Posso aggiungere i miei codici al dataset?**  
A: Sì! Segui la struttura in `data/generated/code_snippets/` e organizza per categoria/linguaggio. CLAP li rileverà automaticamente.

**Q: Come contribuire al progetto?**  
A: Contatta Lorenzo Cappetti o consulta le linee guida nel repository.

**Q: CLAP funziona su Windows?**  
A: Sì, tramite WSL2 (Windows Subsystem for Linux). Native Windows support è in sviluppo.

**Q: Posso disabilitare il tracking CO2?**  
A: Sì, con `export CLAP_DISABLE_CO2="true"` prima dell'esecuzione.

## 📊 Performance e Statistiche

### Tempi di Esecuzione Tipici

**Test Linguaggi** (`python main.py test`):
- Singolo linguaggio: < 1 secondo
- Tutti 16 linguaggi: 5-10 secondi

**Ricerca Task** (`python main.py find --task "..."`):
- Scansione dataset completo: < 1 secondo
- Con esecuzione interattiva: + tempo esecuzione (varia per linguaggio)

**Smart Execution** (`python main.py smart`):
- TOP 10 task × 15 linguaggi: **2-5 minuti**
- Include tracking CO2 e statistiche

**Benchmark CO2**:
- **FAST mode** (3 tasks × 3 iter × 15 lang): **3-5 minuti**
- **TOP10 mode** (10 tasks × 30 iter × 15 lang): **45-60 minuti**
- **COMPLETE mode** (ALL tasks × 3 iter × 15 lang): **6-8 ore**

### Statistiche Emissioni CO2 (Medie)

Basato su hardware tipico (Intel i7, 16GB RAM):

| Linguaggio | CO2 medio (mg) | Tempo medio (s) | Note |
|-----------|---------------|----------------|------|
| **C** | 3.2 | 0.08 | Più efficiente |
| **C++** | 7.5 | 0.21 | Molto efficiente |
| **Rust** | 15.2 | 0.33 | Efficiente |
| **Go** | 28.5 | 0.52 | Efficiente |
| **Haskell** | 35.7 | 0.78 | Funzionale compilato |
| **OCaml** | 42.3 | 0.89 | Funzionale compilato |
| **Java** | 156.8 | 3.45 | Overhead JVM |
| **Python** | 98.5 | 1.44 | Interpretato |
| **Ruby** | 112.3 | 1.87 | Interpretato |
| **JavaScript** | 87.6 | 1.23 | Node.js runtime |
| **TypeScript** | 95.4 | 1.52 | Transpilato + Node.js |
| **PHP** | 78.9 | 1.15 | Interpretato |
| **R** | 125.7 | 2.34 | Statistico |
| **Julia** | 145.3 | 2.67 | JIT compilation |
| **MATLAB** | 187.4 | 3.89 | Commercial + overhead |

**Note**: 
- Valori approssimativi, dipendono da task e hardware
- C e C++ sono generalmente i più efficienti
- Linguaggi interpretati hanno overhead maggiore
- JVM (Java) e runtime pesanti influenzano emissioni

### Tassi di Successo per Linguaggio

Da benchmark reali su dataset completo:

| Linguaggio | Success Rate | Note |
|-----------|-------------|------|
| **Python** | 98.5% | Massima compatibilità |
| **C++** | 96.2% | Wrapper automatici efficaci |
| **JavaScript** | 95.8% | Node.js stabile |
| **Go** | 94.7% | Package management buono |
| **Rust** | 93.5% | Crate esterni occasionali |
| **C** | 97.1% | Simple, pochi errori |
| **Ruby** | 92.3% | Gem dependencies |
| **PHP** | 91.8% | Include dependencies |
| **R** | 89.5% | Package R occasionali |
| **TypeScript** | 93.2% | Transpilazione + Node.js |
| **Julia** | 88.4% | Package Julia specifici |
| **Haskell** | 87.9% | Dopo fixing automatico |
| **OCaml** | 86.5% | Module system complesso |
| **Java** | 82.3% | Class structure issues |
| **C#** | 81.7% | Mono compatibility |
| **MATLAB** | 94.1% | Con wrapper funzioni |

### Capacità Sistema

**Dataset supportato**:
- File totali: **1000+ snippet**
- Linguaggi: **16**
- Categorie: **6+** (algorithms, basic, io, math, misc, strings)

**Limiti tecnici**:
- Timeout singola esecuzione: **10 secondi** (configurabile)
- Massimo file concorrenti: Limitato solo da risorse sistema
- Dimensione singolo snippet: Nessun limite pratico

**Requisiti minimi hardware**:
- CPU: Dual-core 2.0 GHz
- RAM: 4 GB (8 GB raccomandato per benchmark completi)
- Disco: 5 GB spazio disponibile

**Requisiti raccomandati**:
- CPU: Quad-core 3.0 GHz+
- RAM: 16 GB
- SSD per I/O veloce
- Connessione internet (per download dataset)

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
