# SWAM Project 🚀

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