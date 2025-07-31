# SWAM Project - Sistema di Analisi Multi-linguaggio

Un sistema completo per l'analisi e l'esecuzione di snippet di codice dal dataset Rosetta Code, organizzato in una struttura modulare e facile da comprendere.

## 🎯 Obiettivo

Questo progetto analizza snippet di codice in diversi linguaggi di programmazione, li compila/esegue automaticamente e genera statistiche dettagliate sui risultati.

## � Struttura del Progetto

Il progetto è ora organizzato in moduli separati per una migliore leggibilità:

```
SWAM-Project/
├── main.py                 # File principale - coordina tutto il processo
├── config.py              # Configurazioni e impostazioni
├── colors.py              # Sistema di output colorato
├── logger.py              # Gestione logging e statistiche
├── dataset_manager.py     # Gestione dataset e analisi codice
├── execution_manager.py   # Compilazione ed esecuzione
├── cleanup.py             # Script di pulizia
├── requirements.txt       # Dipendenze Python
├── environment.yml        # Ambiente Conda
└── README.md              # Questa documentazione
```

## 🔧 Moduli del Sistema

### 1. `main.py` - Coordinatore Principale
- **Funzione**: Orchestrare tutto il processo
- **Responsabilità**: 
  - Inizializzazione del sistema
  - Chiamata delle funzioni in sequenza logica
  - Gestione del flusso principale

### 2. `config.py` - Configurazioni
- **Funzione**: Centralizzare tutte le configurazioni
- **Contenuto**:
  - Definizioni dei linguaggi per categoria
  - Comandi di compilazione/esecuzione
  - Pattern problematici da filtrare
  - Timeout e limiti

### 3. `colors.py` - Output Colorato
- **Funzione**: Gestire la visualizzazione colorata
- **Caratteristiche**:
  - Colori ANSI per terminale
  - Funzioni helper per stampe colorate
  - Mappatura colori per linguaggi e categorie

### 4. `logger.py` - Logging e Statistiche
- **Funzione**: Tracciare errori e statistiche
- **Caratteristiche**:
  - Sistema di logging colorato
  - Tracciamento errori dettagliato
  - Generazione statistiche finali
  - Salvataggio su file JSON

### 5. `dataset_manager.py` - Gestione Dataset
- **Funzione**: Analizzare e processare il dataset
- **Responsabilità**:
  - Estrazione dipendenze dal codice
  - Filtri per codici problematici
  - Analisi task comuni tra linguaggi
  - Installazione automatica dipendenze

### 6. `execution_manager.py` - Esecuzione
- **Funzione**: Compilare ed eseguire i programmi
- **Caratteristiche**:
  - Gestione compilazione per linguaggi compilati
  - Esecuzione con timeout
  - Gestione errori dettagliata
  - Salvataggio file sorgente

## �🚀 Come Utilizzare

### Prerequisiti
```bash
# Installare le dipendenze
pip install -r requirements.txt

# O usare conda
conda env create -f environment.yml
conda activate SWAM
```

### Esecuzione
```bash
python main.py
```

## 📊 Categorie di Linguaggi Supportate

- **OOP**: C++, C#, Java
- **Scripting**: Python, Ruby, JavaScript, TypeScript  
- **Imperative**: C, Go, Rust, PHP
- **Functional**: Haskell, OCaml
- **Scientific**: R, MATLAB, Julia

## 🔍 Processo di Esecuzione

1. **Inizializzazione**: Setup logger e configurazioni
2. **Caricamento Dataset**: Import del dataset Rosetta Code
3. **Analisi**: Identificazione task comuni e struttura dati
4. **Preparazione**: Creazione cartelle per categorie/linguaggi
5. **Filtraggio**: Estrazione e filtro dei codici validi
6. **Salvataggio**: Creazione file sorgente nelle cartelle appropriate
7. **Esecuzione**: Compilazione/esecuzione di tutti i programmi
8. **Statistiche**: Generazione report dettagliati

## 📈 Output e Risultati

### File di Log Generati
- `logs/execution_YYYYMMDD_HHMMSS.log`: Log dettagliato esecuzione
- `logs/error_analysis.json`: Analisi dettagliata degli errori
- `logs/success_stats.json`: Statistiche di successo per linguaggio

### Struttura Cartelle Generate
```
categoria/
└── linguaggio/
    ├── snippet_0_TaskName.ext
    ├── snippet_1_AnotherTask.ext
    └── ...
```

## 🎨 Vantaggi della Nuova Struttura

### ✅ **Leggibilità Migliorata**
- Ogni modulo ha una responsabilità specifica
- Codice organizzato logicamente
- Documentazione integrata

### ✅ **Manutenibilità**
- Modifiche isolate per singola funzionalità
- Facilità di debugging
- Test modulari possibili

### ✅ **Scalabilità**
- Facile aggiungere nuovi linguaggi
- Estensibile con nuove funzionalità
- Configurazioni centralizzate

### ✅ **Comprensione**
- Flusso logico chiaro
- Separazione delle responsabilità
- Funzioni con scopo ben definito

## 🛠 Personalizzazione

### Aggiungere un Nuovo Linguaggio
1. Modificare `config.py` nella sezione `LANGUAGE_CATEGORIES`
2. Aggiungere pattern di filtro in `PROBLEMATIC_PATTERNS`
3. Aggiungere colore in `colors.py` se desiderato

### Modificare Configurazioni
- Timeout: modificare `TIMEOUT_SECONDS` in `config.py`
- Limite snippet: modificare `MAX_SNIPPETS_PER_LANGUAGE`
- Pattern filtri: aggiornare `PROBLEMATIC_PATTERNS`

## 🎯 Prossimi Sviluppi

- [ ] Test automatizzati per ogni modulo
- [ ] Interfaccia web per visualizzazione risultati
- [ ] Supporto per più dataset
- [ ] Analisi performance comparative
- [ ] Export risultati in formati multipli

Sistema di analisi multi-linguaggio del dataset Rosetta Code per misurare le performance di esecuzione di codice sorgente in 15 linguaggi diversi.

## 🎯 Panoramica

Questo progetto implementa la richiesta del Professor Roberto per:
- Analizzare il dataset Rosetta Code di Hugging Face
- Estrarre task comuni tra tutti i linguaggi
- Compilare ed eseguire codice in 15 linguaggi di programmazione
- Loggare automaticamente errori e statistiche di performance
- Supportare 5 categorie di linguaggi: OOP, Scripting, Imperative, Functional, Scientific

## 📋 Linguaggi Supportati

### 🔵 **OOP (Object-Oriented Programming)**
- C++ (g++)
- C# (csc/mono)  
- Java (javac/java)

### 🟢 **Scripting**
- Python (conda environment)
- Ruby (ruby)
- JavaScript (Node.js - conda environment)
- TypeScript (tsc - conda environment)

### 🔴 **Imperative**
- C (gcc)
- Go (conda environment)
- Rust (conda environment)
- PHP (php)

### 🟣 **Functional**
- Haskell (ghc)
- OCaml (ocamlc)

### 🔵 **Scientific**
- R (conda environment)
- MATLAB/Octave (octave)
- Julia (julia)

## ⚙️ Setup e Installazione

### 1. **Prerequisiti Sistema**

Installa i compilatori/interpreti di sistema (macOS con Homebrew):

```bash
# Compilatori di base
xcode-select --install          # C, C++, gcc, g++

# Java
brew install openjdk

# Mono per C#
brew install mono

# Linguaggi funzionali
brew install ghc cabal-install  # Haskell
brew install ocaml opam        # OCaml

# Altri
brew install php               # PHP
brew install octave           # MATLAB alternative
brew install julia            # Julia
```

### 2. **Ambiente Python/Conda**

#### Opzione A: Usa environment.yml (Raccomandato)
```bash
# Crea ambiente conda con tutte le dipendenze
conda env create -f environment.yml

# Attiva ambiente
conda activate SWAM
```

#### Opzione B: Setup manuale
```bash
# Crea ambiente conda
conda create -n SWAM python=3.11 -y
conda activate SWAM

# Installa linguaggi nell'ambiente
conda install -c conda-forge nodejs typescript go rust r-base -y

# Installa dipendenze Python
pip install -r requirements.txt
```

### 3. **Verifica Installazione**

Testa che tutti i linguaggi siano disponibili:

```bash
# Attiva ambiente
conda activate SWAM

# Verifica linguaggi (dovrebbero tutti rispondere con versione)
gcc --version
g++ --version
java -version
javac -version
csc --version || mono --version
python --version
ruby --version
node --version
tsc --version
go version
rustc --version
php --version
ghc --version
ocaml -version
Rscript --version
octave --version
julia --version
```

## 🚀 Utilizzo

### Esecuzione Base
```bash
# Attiva ambiente conda
conda activate SWAM

# Esegui analisi completa
python main.py
```

### Configurazione

Modifica le costanti nel file `main.py`:

```python
TIMEOUT_SECONDS = 30           # Timeout esecuzione singolo programma
MAX_SNIPPETS_PER_LANGUAGE = 25 # Max snippet per linguaggio
COMPILATION_TIMEOUT = 60       # Timeout compilazione
```

### Output e Log

Il sistema genera:
- **Log colorati** nel terminale con progress real-time
- **File di log** in `logs/execution_YYYYMMDD_HHMMSS.log`
- **Analisi errori** in `logs/error_analysis.json`
- **Statistiche** in `logs/success_stats.json`
- **Codice estratto** in cartelle organizzate per categoria

```
oop/
├── c++/
├── c#/
└── java/
scripting/
├── python/
├── ruby/
├── javascript/
└── typescript/
imperative/
├── c/
├── go/
├── rust/
└── php/
functional/
├── haskell/
└── ocaml/
scientific/
├── r/
├── matlab/
└── julia/
```

## 🧹 Manutenzione

### Pulizia File Temporanei
```bash
# Pulizia completa (raccomandato)
python cleanup.py

# Anteprima pulizia (senza eliminare)
python cleanup.py --dry-run

# Pulizie mirate
python cleanup.py --compiled    # Solo file compilati
python cleanup.py --generated   # Solo codice generato
python cleanup.py --logs        # Solo log
```

## 📊 Funzionalità Avanzate

### Sistema di Filtraggio Intelligente
- **Pattern problematici**: Evita codice con input utente, loop infiniti, GUI
- **Dipendenze automatiche**: Installa automaticamente librerie comuni
- **Timeout management**: Gestisce programmi che si bloccano
- **Error recovery**: Log dettagliato per debugging

### Statistiche Dettagliate
- Tasso di successo per linguaggio
- Analisi errori per categoria (compilazione, runtime, timeout)
- Performance comparison tra linguaggi
- File filtrati vs eseguiti con successo

## 🔧 Troubleshooting

### Errori Comuni

**"Command not found" per un linguaggio:**
```bash
# Verifica installazione e PATH
which gcc python node go rustc ghc ocaml php octave julia

# Per linguaggi conda, assicurati di aver attivato SWAM
conda activate SWAM
```

**Errori di timeout frequenti:**
```bash
# Aumenta timeout in main.py
TIMEOUT_SECONDS = 60
COMPILATION_TIMEOUT = 120
```

## 📈 Risultati Attesi

Il sistema dovrebbe raggiungere:
- **70-90%** tasso di successo per linguaggi moderni (Python, JavaScript, Go)
- **50-70%** per linguaggi con dipendenze complesse (C++, Haskell)
- **30-50%** per linguaggi con setup specifico (C#, OCaml)

---

**Versione**: 1.0  
**Autore**: Lorenzo Cappetti  
**Corso**: SWAM Project  
**Data**: Luglio 2025