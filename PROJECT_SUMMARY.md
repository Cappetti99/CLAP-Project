# 🚀 SWAM PROJECT - Sistema Modulare Avanzato

## 📋 Panoramica

Hai ora un sistema completo e modulare per:

1. **🔍 Trovare task comuni** tra linguaggi di programmazione
2. **⚙️ Eseguire codice automaticamente** con gestione delle dipendenze
3. **🧹 Pulire automaticamente** file temporanei e dipendenze
4. **📊 Monitorare e analizzare** i risultati con report dettagliati

## 🎯 Funzionalità Implementate

### 1. Advanced Task Finder (`src/advanced_task_finder.py`)
- ✅ Trova task presenti in tutti i linguaggi
- ✅ Ricerca flessibile con soglia di copertura personalizzabile
- ✅ Analisi qualitativa del codice (leggibilità, complessità, commenti)
- ✅ Report dettagliati con metriche di qualità

### 2. Enhanced Executor (`src/enhanced_executor.py`)
- ✅ Supporto multi-linguaggio (Python, JavaScript, Java, C++, C, Go, Rust)
- ✅ Gestione automatica delle dipendenze
- ✅ Compilazione automatica per linguaggi compilati
- ✅ Timeout e gestione errori
- ✅ Cleanup automatico file temporanei

### 3. Simple Executor (`src/simple_executor.py`)
- ✅ Versione semplificata per Python e JavaScript
- ✅ Facile da usare e debuggare
- ✅ Perfetto per test iniziali

### 4. Cleanup Manager (`src/cleanup_manager.py`)
- ✅ Pulizia automatica file temporanei
- ✅ Rimozione dipendenze installate temporaneamente
- ✅ Pulizia cache Python
- ✅ Pulizia file compilati
- ✅ Modalità emergency per problemi gravi

### 5. Results Monitor (`src/results_monitor.py`)
- ✅ Analisi tassi di successo per linguaggio e task
- ✅ Categorizzazione automatica degli errori
- ✅ Report completi con raccomandazioni
- ✅ Executive summary per management

### 6. Main Orchestrator (`src/main_modular.py`)
- ✅ Esecuzione automatica di tutti i moduli
- ✅ Gestione errori e fallback
- ✅ Report finale con statistiche

## 🛠️ Come Usare il Sistema

### Test Rapido
```bash
# Verifica che tutto sia configurato
python quick_test.py

# Test capacità di esecuzione
python test_executor.py
```

### Esecuzione Completa
```bash
# Esegue tutto il pipeline automaticamente
python src/main_modular.py
```

### Moduli Singoli
```bash
# Solo ricerca task comuni
python src/advanced_task_finder.py

# Solo esecuzione semplice
python src/simple_executor.py

# Solo pulizia
python src/cleanup_manager.py

# Solo monitoring
python src/results_monitor.py
```

## 📊 Struttura dei Risultati

Il sistema genera automaticamente:

```
results/
├── task_analysis/
│   ├── common_tasks.json              # Lista task comuni
│   ├── detailed_analysis_*.json       # Analisi qualitativa dettagliata
│   └── code_snippets/                 # Codice per ogni task/linguaggio
├── execution/
│   └── execution_results_*.json       # Risultati esecuzioni
├── reports/
│   └── comprehensive_report_*.json    # Report completi
└── logs/
    └── *.json                         # Log errori e statistiche
```

## 🎯 Task Disponibili (dal tuo dataset)

1. **Ethiopian multiplication** - 16 linguaggi
2. **Greatest common divisor** - 16 linguaggi  
3. **Sorting algorithms/Quicksort** - 16 linguaggi
4. **N'th** - 16 linguaggi
5. **Palindrome detection** - 16 linguaggi
6. **Gray code** - 16 linguaggi
7. **Averages/Arithmetic mean** - 16 linguaggi

## 🔧 Linguaggi Supportati

### ✅ Completamente Funzionanti
- **Python** - Esecuzione diretta + gestione dipendenze pip
- **JavaScript** - Esecuzione con Node.js

### 🚧 In Sviluppo
- **Java** - Compilazione con javac + esecuzione
- **C/C++** - Compilazione con gcc/g++
- **Go** - Compilazione con go build
- **Rust** - Compilazione con rustc

### 📋 Pianificati
- **Ruby, PHP, Haskell, OCaml, R, MATLAB, Julia**

## 💡 Caratteristiche Avanzate

### 🔍 Analisi Qualitativa
- **Leggibilità del codice** (0-100)
- **Densità di commenti**
- **Complessità ciclomatica**
- **Gestione errori**
- **Modularità (funzioni)**

### 📦 Gestione Dipendenze
- **Rilevamento automatico** import/require
- **Installazione temporanea** con pip/npm
- **Cleanup automatico** alla fine
- **Verifica disponibilità** prima dell'uso

### 🛡️ Sicurezza e Robustezza
- **Timeout** per evitare hang
- **Sandboxing** con file temporanei
- **Gestione errori** completa
- **Cleanup garantito** anche in caso di crash

### 📈 Monitoring e Analytics
- **Tassi di successo** per linguaggio
- **Tempi di esecuzione**
- **Pattern di errori comuni**
- **Raccomandazioni automatiche**

## 🚨 Risoluzione Problemi

### Se l'esecuzione fallisce:
1. **Verifica ambiente**: `python quick_test.py`
2. **Test semplice**: `python test_executor.py` 
3. **Check dipendenze**: Installa compilatori mancanti
4. **Usa versione semplice**: `python src/simple_executor.py`

### Linguaggi non supportati:
- Il sistema si adatta automaticamente
- Mostra solo errori per linguaggi che dovrebbero funzionare
- Continua con i linguaggi disponibili

### Cleanup problemi:
```bash
# Pulizia manuale
python src/cleanup_manager.py

# Emergency cleanup
python -c "from src.cleanup_manager import CleanupManager; CleanupManager().emergency_cleanup()"
```

## 🎉 Prossimi Passi

1. **Aggiungi più linguaggi** modificando `language_config` in `enhanced_executor.py`
2. **Migliora analisi qualitativa** aggiungendo metriche in `advanced_task_finder.py`
3. **Personalizza report** modificando `results_monitor.py`
4. **Integra con CI/CD** usando i JSON di output

## 🏆 Risultati Ottenuti

Hai ora un sistema che può:
- ✅ Analizzare automaticamente dataset di codice
- ✅ Identificare task comuni tra linguaggi
- ✅ Eseguire e testare codice in multiple lingue
- ✅ Gestire dipendenze temporaneamente
- ✅ Generare report analitici dettagliati
- ✅ Pulire automaticamente l'ambiente

**🎯 Perfetto per ricerca, benchmark, e analisi comparativa di algoritmi!**
