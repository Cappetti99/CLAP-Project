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

## Descrizione

**CLAP Project** è un sistema avanzato per l'analisi automatizzata e l'esecuzione di codice attraverso **16 linguaggi di programmazione**. Il progetto fornisce strumenti completi per:

- **Ricerca mirata**: Trova implementazioni specifiche di algoritmi nel dataset
- **Esecuzione automatizzata**: Esegui codice su linguaggi multipli simultaneamente con gestione intelligente degli errori
- **Analisi emissioni**: Traccia il consumo energetico e le emissioni CO2 del codice
- **Benchmarking**: Confronta performance ed efficienza energetica tra linguaggi
- **Modalità headless**: Esecuzione senza GUI per benchmark automatizzati
- **Sistema adattivo**: Rileva automaticamente i linguaggi disponibili e si adatta all'ambiente

**Casi d'uso**: ricerca accademica, analisi comparativa tra linguaggi, ottimizzazione energetica del software, studio dell'impatto ambientale della programmazione.

## Indice

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

## Caratteristiche Principali

### Sistema Completo e Adattivo

- **16 linguaggi supportati**: Copertura completa dei linguaggi più comuni
- **Rilevamento automatico**: Il sistema si adatta ai linguaggi disponibili sul tuo sistema
- **Esecuzione headless**: Modalità senza GUI per benchmark automatizzati e server headless
- **Gestione intelligente errori**: Wrapper automatici per codice incompleto (funzioni MATLAB, file Haskell)
- **Ambiente CLAP integrato**: Utilizzo automatico dell'ambiente conda CLAP se disponibile

### Tracciamento Emissioni CO2

- **Integrazione CodeCarbon**: Monitoraggio preciso del consumo energetico
- **Tracking automatico**: Misura CO2 durante ogni esecuzione
- **Report dettagliati**: JSON con dati completi su emissioni, energia, durata
- **Benchmark scientifici**: Modalità FAST, TOP10 e COMPLETE per ricerca approfondita
- **Visualizzazione dati**: Analisi aggregate

### Funzionalità Avanzate

- **Pulizia automatica GUI**: Rimuove/sostituisce chiamate a `matplotlib.show()`, `tkinter.mainloop()`, `PIL.show()`... così da rendere il tutto di più veloce esecuzione.
- **Supporto MATLAB commerciale**: Integrazione automatica con MATLAB R2025b e licenze
- **Sistema modulare**: Architettura pulita con separazione responsabilità
- **Logging avanzato**: Sistema di log dettagliato per debugging e analisi
- **Task search intelligente**: Pattern matching per trovare implementazioni specifiche

## Linguaggi Supportati

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

**\*** MATLAB richiede licenza commerciale. Il sistema rileva automaticamente l'installazione e configura i percorsi.

### Caratteristiche Speciali per Linguaggio

- **MATLAB**: Wrapper automatico per funzioni standalone, integrazione licenza, gestione figure headless
- **Haskell**: Pulizia automatica import problematici, gestione funzioni `join`, aggiunta `main` se assente
- **Python**: Rimozione automatica GUI (tkinter, turtle, matplotlib, PIL, OpenGL)
- **R**: Reindirizzamento plots a file PNG, disabilitazione device grafici
- **JavaScript/TypeScript**: Gestione Node.js, rimozione API browser

## Installazione

### Requisiti di Sistema

**Requisiti minimi:**
- **Sistema Operativo**: Linux (Ubuntu 20.04+, Debian 11+), macOS 11+, Windows 10+ (con WSL2) (Il sistema è stato sviluppato e pensato per Ubuntu 24.04).
- **Python**: 3.8 o superiore (raccomandato: **Python 3.12+**)
- **Spazio disco**: 2-5 GB (dipende dai linguaggi installati)
- **RAM**: 4 GB minimo, 8 GB raccomandato
- **Conda** (opzionale): Anaconda o Miniconda per ambiente CLAP

**Note:**
- Il sistema funziona con **qualsiasi numero di linguaggi** disponibili (minimo: Python)

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

** Ubuntu/Debian - Installazione Completa:**

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

** macOS - Installazione Completa:**

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

** Windows (con WSL2):**

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

### File di Riferimento

- ** [requirements.txt](requirements.txt)** - Dipendenze Python con versioni esatte installate nell'ambiente CLAP
- ** [environment.yml](environment.yml)** - Ambiente conda completo (372 pacchetti) per riproducibilità totale
- ** [DEPENDENCIES.md](DEPENDENCIES.md)** - Guida alla gestione delle dipendenze (pip vs conda)

## Utilizzo

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

## Comandi Disponibili

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

#### 1. Ricerca Mirata Task

Il comando `find` ti permette di cercare implementazioni specifiche di algoritmi:

```bash
# Cerca bubble sort
python main.py find --task "bubble"

# Output:
#  Ricerca task: 'bubble' (solo linguaggi testati)
# 
# ✅ Trovate 14 task in 14 linguaggi testati:
# 
#  PYTHON (1 file):
#    • snippet_39_Sorting_algorithmsBubble_sort.py
# 
#  JAVA (1 file):
#    • snippet_36_Sorting_algorithmsBubble_sort.java
# 
#  CPP (1 file):
#    • snippet_22_Sorting_algorithmsBubble_sort.cpp
# 
# [... altri linguaggi ...]
# 
#  Vuoi eseguire una di queste task? [s/N]: s
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

#### 2.  Test Sistema

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

#### 3. Esecuzione Intelligente (Smart)

Esegue le TOP 10 task più comuni su tutti i linguaggi funzionanti:

```bash
python main.py smart

# Output:
# SMART EXECUTION
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
#  Inizio esecuzione...
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

#### 4. Benchmark Emissioni CO2

Tre modalità disponibili per diversi use case:

**FAST Mode - Test Funzionalità (3-5 minuti):**
```bash
python main.py benchmark --mode fast

# Esegue 3 task campione × 3 ripetizioni
# Perfetto per verificare che tutto funzioni
# Tempo stimato: ~3-5 minuti
```

**TOP10 Mode - Analisi Task Principali (60-90 minuti):**
```bash
python main.py benchmark --mode top10

# Esegue TOP 10 task più frequenti × 30 ripetizioni
# Calcola medie statisticamente significative
# Ideale per analisi regolari
# Tempo stimato: ~45-60 minuti
```

**COMPLETE Mode - Analisi Esaustiva (40-45 ore):**
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

#### 5. Visualizzazione Dati CO2

Analizza i dati raccolti dai benchmark:

```bash
python main.py carbon

# Output:
# CO2 DATA OVERVIEW
# 
# Trovati 247 file di sessione CO2
# Periodo: 2025-10-01 → 2025-10-15
# 
# EMISSIONI TOTALI: 3.45 kg CO2eq
# ENERGIA TOTALE: 22.67 Wh
#  TEMPO TOTALE: 4h 23m 15s
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
# Core Dependencies (versioni installate nell'ambiente CLAP)
codecarbon==3.0.1          # Tracciamento CO2 e consumo energetico
datasets==4.0.0            # Gestione dataset multi-formato
pandas==2.3.1              # Analisi dati e statistiche
numpy==2.3.2               # Calcoli numerici e array
tqdm==4.67.1               # Progress bars per operazioni lunghe
colorama==0.4.6            # Output colorato terminale
psutil==7.0.0              # Informazioni sistema e processi
pyarrow==21.0.0            # Performance I/O per file grandi

# Additional Dependencies
fastapi==0.112.2           # API framework
httpx==0.27.0              # HTTP client avanzato
click==8.2.1               # CLI framework
cryptography==45.0.5       # Sicurezza e crittografia

# Optional (da installare se necessario)
# matplotlib>=3.7.0        # Visualizzazioni grafiche
# tabulate>=0.9.0          # Formattazione tabelle
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

## Struttura del Progetto

```
SWAM-Project/
├── main.py                    # Interfaccia principale
├── requirements.txt           # Dipendenze Python (ben documentate)
├── SETUP_GUIDE.md            # Guida installazione completa step-by-step
├── SYSTEM_REQUIREMENTS.md    # Comandi installazione per ogni OS
├── README.md                  # Documentazione principale
├── cleanup_sessions.py        # Pulizia sessioni CO2
│
├── modules/                   # Moduli core del sistema
│   ├── language_config.py    #    Configurazioni linguaggi
│   ├── modern_dependency_analyzer.py  # Analisi dipendenze
│   └── modern_logger.py      #     Sistema logging avanzato
│
├── src/                      # Esecutori e analizzatori
│   ├── task_searcher.py      #     Ricerca mirata task
│   ├── smart_executor.py     #     Esecutore adattivo
│   ├── executor.py           #     Esecutore base
│   ├── language_tester.py    #     Test linguaggi disponibili
│   ├── benchmark.py          #     Benchmark performance
│   ├── carbon_benchmark.py   #     Benchmark emissioni CO2
│   ├── carbon_tracker.py     #     Tracking emissioni
│   ├── finder.py            #     Ricerca task comuni
│   └── cleaner.py           #     Pulizia file temporanei
│
├── data/                     # Dataset e dati
│   └── generated/           #     Dataset generato automaticamente
│       └── code_snippets/   #     Snippet di codice organizzati
│           ├── algorithms/  #     Algoritmi vari
│           ├── basic/       #     Esempi base
│           ├── python/      #     Specifici per Python
│           ├── java/        #     Specifici per Java
│           └── ...          #     Altri linguaggi
│
├── results/                  # Output e risultati generati
│   ├── task_search/         #     Risultati ricerca mirata
│   ├── execution/           #     Risultati esecuzione
│   ├── carbon_benchmark/    #     Report CO2 dettagliati  
│   ├── carbon/             #     Tracking emissioni
│   ├── csv/                #     File CSV per analisi
│   ├── logs/               #     Log di sistema
│   └── compliance/         #     Report conformità
│
└── scripts/                 # Script di utilità
    ├── export_to_csv.py    #     Esportazione dati CSV
    └── extract_top10_csv.py #     Estrazione TOP 10 task
```

### File Principali

** main.py** - Punto di ingresso principale:
- Interfaccia unificata per tutti i comandi
- Routing verso moduli specializzati
- Gestione parametri da linea di comando

** src/smart_executor.py** - Cuore del sistema:
- Rilevamento automatico linguaggi disponibili
- Esecuzione adattiva e gestione errori
- Integrazione con carbon tracking

** src/task_searcher.py** - Ricerca mirata:
- Pattern matching intelligente nel dataset
- Interfaccia interattiva per selezione task
- Supporto multi-estensione e multi-linguaggio

** src/carbon_tracker.py** - Monitoraggio emissioni:
- Integrazione con codecarbon
- Tracking automatico durante esecuzioni
- Gestione sessioni e cleanup

---

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

### Statistiche Emissioni CO2 (Medie) ???????



---

**CLAP Project** © 2025 - Cross-Language Analysis System  
Author: Lorenzo Cappetti
