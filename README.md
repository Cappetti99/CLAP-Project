# CLAP – Cross-Language Analysis Project

<div align="center">
  <img src="logoSWAM.png" alt="SWAM Project Logo" width="300"/>
</div>

Sistema Modulare Avanzato per Analisi e Esecuzione Multi-Linguaggio con tracciamento emissioni CO2.

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-15+-green.svg)](#linguaggi-supportati)
[![CodeCarbon](https://img.shields.io/badge/CodeCarbon-Enabled-green.svg)](https://codecarbon.io)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## 📖 Descrizione

SWAM Project è un sistema avanzato per l'analisi automatica e l'esecuzione di codice in 15+ linguaggi di programmazione diversi. Il progetto risolve il problema della comparazione cross-linguaggio di algoritmi e implementazioni, fornendo strumenti per:

- **Ricerca mirata**: Trova implementazioni specifiche di algoritmi nel dataset
- **Esecuzione automatica**: Testa codice in multiple lingue simultaneamente  
- **Analisi delle emissioni**: Traccia il consumo energetico e le emissioni CO2 del codice
- **Benchmarking**: Confronta performance ed efficienza energetica tra linguaggi

**Contesto d'uso**: Ricerca accademica, didattica, analisi comparative di linguaggi di programmazione, ottimizzazione energetica del software.

## 📑 Indice

1. [Descrizione](#-descrizione)
2. [Installazione](#️-installazione)
3. [Utilizzo](#-utilizzo)
4. [Configurazione](#️-configurazione)
5. [Struttura del progetto](#-struttura-del-progetto)

## 🛠️ Installazione

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

## 🚀 Utilizzo

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

Il dataset in `data/generated/code_snippets/` è organizzato per:
- **Categoria**: algorithms/, basic/, strings/, mathematics/
- **Linguaggio**: python/, java/, cpp/, go/, rust/, etc.
- **Task**: Con naming pattern `snippet_ID_Category_TaskName.ext`

Esempio:
```
data/generated/code_snippets/algorithms/python/
├── snippet_39_Sorting_algorithmsBubble_sort.py
├── snippet_25_Sorting_algorithmsComb_sort.py
└── snippet_27_Sorting_algorithmsCounting_sort.py
```

---

## � Informazioni Aggiuntive

### Linguaggi Supportati (Sistema Adattivo)

Il sistema rileva automaticamente i linguaggi disponibili e si adatta di conseguenza.

**Tipicamente Disponibili (15/16):**
| Categoria | Linguaggi | Disponibilità |
|-----------|-----------|---------------|
| **Interpretati** | Python, JavaScript, Ruby, PHP, R, Julia | 6/6 (100%) |
| **Compilati** | C, C++, Java, Go, Rust, Haskell, OCaml, TypeScript | 8/8 (100%) |
| **VM-based** | C# (Mono) | 1/1 (100%) |
| **Commerciali** | MATLAB* | 0/1 (0%) |

*Solo MATLAB richiede licenza commerciale

### Performance Tipiche

| Operazione | Tempo | Note |
|------------|-------|------|
| Test linguaggi | 2-5 secondi | 15-16 linguaggi |
| Ricerca task | <1 secondo | Pattern matching |
| Esecuzione singola | 0.01-2.0s | Dipende da linguaggio |
| Benchmark CO2 | 5-10 minuti | Per dataset completo |

### Troubleshooting

**Linguaggio non disponibile:**
```bash
python main.py test  # Mostra linguaggi mancanti
# Il sistema continua con quelli disponibili
```

**Errori di compilazione:**
```bash
python main.py smart  # Usa solo linguaggi funzionanti
python main.py clean  # Pulisce file temporanei
```

---

**SWAM Project** © 2025 - Sistema Multi-linguaggio per Analisi Codice  
Autore: Lorenzo Cappetti
