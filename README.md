# SWAM Project - Cross-Language Code Analysis System

**Sistema Modulare Avanzato per Analisi e Esecuzione Multi-Linguaggio** - Trova task comuni, esegue codice automaticamente e genera report dettagliati in 16 linguaggi di programmazione con **tracciamento emissioni CO2**.

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-16-green.svg)](#linguaggi-supportati)
[![CodeCarbon](https://img.shields.io/badge/CodeCarbon-Enabled-green.svg)](https://codecarbon.io)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## Quick Start

### Modalità Semplice (Raccomandato per iniziare)
```bash
# Analisi semplificata - genera CSV puliti
python main.py simple

# Test disponibilità linguaggi
python main.py test

# Pulizia rapida
python main.py clean
```

### Modalità Avanzata (Sistema Completo)
```bash
# Esecuzione intelligente (usa solo linguaggi disponibili)
python main.py smart

# Analisi completa task comuni
python main.py analyze

# Esecuzione completa (tutti i moduli)
python main.py execute

# Stato dettagliato del progetto
python main.py status
```

### **NUOVO**: Benchmark CO2 (Analisi Emissioni)
```bash
# Benchmark CO2 completo (30 iterazioni per precisione)
python main.py benchmark

# Modalità veloce (10 iterazioni)
echo "veloce" | python main.py benchmark

# Modalità debug super veloce (3 iterazioni)
echo "debug" | python main.py benchmark
```

## Comandi Disponibili

| Comando | Modalità | Descrizione |
|---------|----------|------------|
| `simple` | Base | Genera CSV delle task comuni (veloce) |
| `test` | | Testa la disponibilità di tutti i 16 linguaggi |
| `smart` | Avanzata | Esegue codici solo nei linguaggi disponibili |
| `analyze` | Avanzata | Analisi completa task comuni con metriche |
| `execute` | Avanzata | Esegue codici in tutti i linguaggi (può fallire) |
| **`benchmark`** | **CO2** | **Benchmark emissioni CO2 con statistiche** |
| `clean` | | Pulisce file temporanei e cache |
| `status` | | Mostra stato del progetto e statistiche |

## Funzionalità del Sistema

### Modalità Semplice
- **Focus**: Task comuni a tutti i 16 linguaggi
- **Output**: CSV puliti e utilizzabili
- **Veloce**: Analisi rapida senza complessità
- **Comandi**: `python main.py simple`

#### Output CSV Generati
```
results/
├── common_tasks_20250809_172724.csv # 10 task comuni a tutti i 16 linguaggi
├── task_statistics_20250809_172724.csv # Statistiche dettagliate (lunghezza codice, URL)
└── task_language_matrix_20250809_172724.csv # Matrice presenza per ogni task
```

### Modalità Avanzata
- **Advanced Task Finder**: Ricerca flessibile con soglia personalizzabile
- **Enhanced Executor**: Supporto multi-linguaggio con gestione dipendenze
- **Smart Executor**: Esecuzione adattiva solo linguaggi disponibili
- **Results Monitor**: Analisi tassi successo e categorizzazione errori
- **Cleanup Manager**: Pulizia automatica file temporanei

### **Benchmark CO2** (Novità!)
- **Carbon Tracking**: Misura emissioni CO2 per ogni esecuzione con [CodeCarbon](https://codecarbon.io)
- **Analisi Statistica**: 30 iterazioni per task per dati precisi
- **Ranking Efficienza**: Classifica linguaggi per impatto ambientale
- **Report Dettagliati**: Statistiche complete (media, mediana, deviazione standard)
- **Modalità Multiple**: Standard (30 iter), Veloce (10 iter), Debug (3 iter)

#### Output Benchmark CO2
```
results/carbon_benchmark/
├── carbon_benchmark_detailed_*.json # Dati completi 30 iterazioni
├── carbon_benchmark_summary_*.json # Riassunto per linguaggio
└── session_*.json # Dettagli singole sessioni

results/carbon/
├── emissions.csv # Log emissioni CodeCarbon
└── session_*.json # Tracking dettagliato per esecuzione
```

#### Output Avanzati
```
results/
├── task_analysis/
│ ├── common_tasks.json # Lista task comuni
│ ├── detailed_analysis_*.json # Analisi qualitativa dettagliata
│ └── code_snippets/ # Codice per ogni task/linguaggio
├── execution/
│ └── execution_results_*.json # Risultati esecuzioni
├── reports/
│ └── comprehensive_report_*.json # Report completi
└── logs/
 └── *.json # Log errori e statistiche
```

## Caratteristiche Avanzate

### Analisi Qualitativa
- **Leggibilità del codice** (0-100)
- **Densità di commenti**
- **Complessità ciclomatica**
- **Gestione errori**
- **Modularità (funzioni)**

### Gestione Dipendenze
- **Rilevamento automatico** import/require
- **Installazione temporanea** con pip/npm
- **Cleanup automatico** alla fine
- **Verifica disponibilità** prima dell'uso

### Sicurezza e Robustezza
- **Timeout** per evitare hang
- **Sandboxing** con file temporanei
- **Gestione errori** completa
- **Cleanup garantito** anche in caso di crash

## Linguaggi Supportati

### Interpretati (7)
Python • JavaScript • Ruby • PHP • R • Julia • MATLAB*

### Compilati (8)
C • C++ • Java • Go • Rust • Haskell • OCaml • TypeScript

### VM-based (1)
C# (Mono)

*\*MATLAB richiede licenza commerciale*

## Struttura Progetto

```
SWAM-Project/
├── main.py # Interface principale
├── cleanup.py # Script pulizia completo
├── install_languages.sh # Installer linguaggi
├── modules/ # 📚 Moduli core
│ ├── config.py # Configurazioni
│ ├── colors.py # 🎨 Output colorato
│ ├── dataset_manager.py # Gestione dataset
│ ├── dependency_analyzer.py # Analisi dipendenze
│ └── logger.py # 📝 Logging
├── src/ # Esecutori e analyzer
│ ├── enhanced_executor.py # Esecutore completo
│ ├── smart_executor.py # Esecutore adattivo
│ ├── carbon_benchmark.py # Benchmark CO2 (NUOVO)
│ ├── carbon_tracker.py # Tracking emissioni (NUOVO)
│ ├── language_tester.py # Test linguaggi
│ ├── advanced_task_finder.py # Ricerca task
│ └── task_finder.py # Ricerca base
└── results/ # Output generati
 ├── execution/ # Risultati esecuzione
 ├── task_analysis/ # Analisi task
 ├── carbon_benchmark/ # Report CO2 (NUOVO)
 ├── carbon/ # Tracking emissioni (NUOVO)
 └── logs/ # Log sistema
```

## **Benchmark CO2 - Guida Completa**

### � Modalità Disponibili

| Modalità | Iterazioni | Task | Utilizzo | Tempo |
|----------|------------|------|----------|-------|
| **Standard** | 30 | 5 | Dati precisi per ricerca | ~15 min |
| **Veloce** | 10 | 3 | Test rapidi | ~5 min |
| **Debug** | 3 | 2 | Sviluppo/test | ~2 min |

### Esempi di Uso

#### Benchmark Completo (Raccomandato)
```bash
python main.py benchmark
# Scegli "standard" per 30 iterazioni precise
```

#### Test Rapido
```bash
echo "debug" | python main.py benchmark
# 3 iterazioni super veloci per test
```

#### Benchmark via Script
```bash
# Python diretto
python src/carbon_benchmark.py # Standard: 30 iter, 5 task
python src/carbon_benchmark.py quick # Veloce: 10 iter, 3 task
python src/carbon_benchmark.py debug # Debug: 3 iter, 2 task
```

### Output Benchmark

#### Report Console (dati reali dal 23/09/2025)
```
🏅 RANKING LINGUAGGI PER EFFICIENZA ENERGETICA:
 1. JavaScript : 0.000171 kg CO2eq/run (100_doors)
 2. Ruby       : 0.000186 kg CO2eq/run (100_doors)  
 3. C          : 0.000221 kg CO2eq/run (100_doors)
 4. R          : 0.000075 kg CO2eq/run (Arbitrary-precision)
 5. JavaScript : 0.000590 kg CO2eq/run (Array_length)

� STATISTICHE REALI:
 Sessioni tracciate: 9,748
 Emissioni totali: 15.85 kg CO2eq
 Periodo: Agosto-Settembre 2025
```

#### File JSON Generati (19 benchmark completati)
- `carbon_benchmark_detailed_*.json`: Tutti i dati delle 30 iterazioni per task
- `carbon_benchmark_summary_*.json`: Riassunto statistiche per linguaggio  
- `emissions.csv`: 10,621 misurazioni con CodeCarbon (compatibile dashboard)
- Ultima sessione: `carbon_benchmark_detailed_20250923_080615.json`

## � Installazione Linguaggi

### Prerequisiti per Benchmark CO2
```bash
# Installa CodeCarbon
pip install codecarbon

# Verifica installazione
python -c "import codecarbon; print('CodeCarbon ready!')"
```

### Automatica (macOS)
```bash
./install_languages.sh
```

### Manuale
```bash
# C/C++
xcode-select --install

# Altri linguaggi
brew install go rust ghc ocaml
npm install -g typescript

# Verifica installazione
python main.py test
```

## Performance Tipiche

| Categoria | Linguaggi Disponibili | Tasso Successo Tipico |
|-----------|----------------------|----------------------|
| **Interpretati** | 6/7 (86%) | Python, JS, Ruby, PHP, R, Julia |
| **Compilati** | 8/8 (100%) | Tutti se installati correttamente |
| **VM-based** | 1/1 (100%) | C# (Mono) |
| **Totale** | **15/16 (93.8%)** | Solo MATLAB manca |

## Esempi Risultati

### Test Linguaggi
```bash
$ python main.py test
 Testing PYTHON (interpreted)...
 Esecuzione riuscita (0.011s): Hello from Python!

 STATISTICHE GENERALI:
 Totale linguaggi testati: 16
 Linguaggi disponibili: 15
 Tasso di successo: 93.8%
```

### Smart Execution
```bash
$ python main.py smart
 ESECUZIONE INTELLIGENTE
 Linguaggi disponibili: 8
 Trovate 10 task comuni

 Esecuzione task: Array sum
 python: successo (0.01s)
 javascript: successo (0.06s)
 java: successo (0.27s)

🎉 TOTALE: 32/80 (40.0%) esecuzioni riuscite
```

### Task Comuni Analizzate (CSV Reali)
```
1. Array sum          - 16 linguaggi (100%)
2. Binary search      - 16 linguaggi (100%)
3. Factorial          - 16 linguaggi (100%)  
4. Fibonacci sequence - 16 linguaggi (100%)
5. GCD calculation    - 16 linguaggi (100%)
6. Hello World        - 16 linguaggi (100%)
7. Palindrome check   - 16 linguaggi (100%)
8. Prime numbers      - 16 linguaggi (100%)
9. Sorting algorithm  - 16 linguaggi (100%)
10. String reverse    - 16 linguaggi (100%)
```

### Benchmark CO2 Recenti
```
🏅 RANKING LINGUAGGI PER EFFICIENZA ENERGETICA:
 1. C           : 0.000221 kg CO2eq/run (task: 100_doors)
 2. JavaScript  : 0.000171 kg CO2eq/run (task: 100_doors)
 3. Ruby        : 0.000186 kg CO2eq/run (task: 100_doors)
 4. R           : 0.000075 kg CO2eq/run (task: Arbitrary-precision)

💡 Totale sessioni tracciate: 9,748
💡 Emissioni totali misurate: 15.85 kg CO2eq
```

## Esempi Risultati

### Test Linguaggi
```bash
$ python main.py test
 Testing PYTHON (interpreted)...
 Esecuzione riuscita (0.011s): Hello from Python!

 STATISTICHE GENERALI:
 Totale linguaggi testati: 16
 Tasso di successo: 93.8%
```

### Smart Execution
```bash
$ python main.py smart
 ESECUZIONE INTELLIGENTE
 Linguaggi disponibili: 8
 Trovate 10 task comuni

 Esecuzione task: Array_length
 python: successo (0.01s)
 java: successo (0.27s)

🎉 TOTALE: 32/79 (40.5%) esecuzioni riuscite
```

### Modalità Semplice CSV
```bash
$ python main.py simple
 ANALISI SEMPLIFICATA
 Task comuni trovate: 10

 File generati:
 common_tasks_20250809_172724.csv
 task_statistics_20250809_172724.csv  
 task_language_matrix_20250809_172724.csv
```

## Task Disponibili (dal dataset)

Le 10 task comuni presenti in tutti i 16 linguaggi (dati reali CSV):

1. **Array sum** - 16 linguaggi (avg: 44 caratteri)
2. **Binary search** - 16 linguaggi (avg: 48 caratteri)  
3. **Factorial** - 16 linguaggi (avg: 44 caratteri)
4. **Fibonacci sequence** - 16 linguaggi (avg: 53 caratteri)
5. **GCD calculation** - 16 linguaggi (avg: 50 caratteri)
6. **Hello World** - 16 linguaggi (avg: 46 caratteri)
7. **Palindrome check** - 16 linguaggi (avg: 51 caratteri)
8. **Prime numbers** - 16 linguaggi (avg: 48 caratteri)
9. **Sorting algorithm** - 16 linguaggi (avg: 52 caratteri)
10. **String reverse** - 16 linguaggi (avg: 49 caratteri)

## Performance Tipiche

| Categoria | Linguaggi Disponibili | Tasso Successo Tipico |
|-----------|----------------------|----------------------|
| **Interpretati** | 6/7 (86%) | Python, JS, Ruby, PHP, R, Julia |
| **Compilati** | 8/8 (100%) | Tutti se installati correttamente |
| **VM-based** | 1/1 (100%) | C# (Mono) |
| **Totale** | **15/16 (93.8%)** | Solo MATLAB manca |

## Troubleshooting

### Se l'esecuzione fallisce:
1. **Verifica ambiente**: `python main.py test`
2. **Test semplice**: `python main.py simple`
3. **Check dipendenze**: Installa compilatori mancanti
4. **Usa versione adattiva**: `python main.py smart`

### Linguaggi non supportati:
- Il sistema si adatta automaticamente
- Mostra solo errori per linguaggi che dovrebbero funzionare
- Continua con i linguaggi disponibili

### Cleanup problemi:
```bash
# Pulizia manuale
python main.py clean

# Emergency cleanup (se esiste)
python -c "from src.cleanup_manager import CleanupManager; CleanupManager().emergency_cleanup()"
```

## Manutenzione

### Pulizia File Temporanei
```bash
# Pulizia completa (include duplicati CSV)
python main.py clean

# Solo duplicati CSV
python cleanup_results.py

# Solo file temporanei di compilazione
find . -name "*.o" -o -name "*.class" -o -name "*.exe" | xargs rm -f
```

### Monitoraggio Spazio
```bash
python main.py status # Stato progetto
python cleanup.py --all # Libera spazio
```

## Troubleshooting

### Linguaggio Non Disponibile
```bash
# Verifica installazione
which python node java gcc

# Installa manualmente
brew install <linguaggio>

# Testa di nuovo
python main.py test
```

### Errori di Compilazione
```bash
# Pulisci cache
python main.py clean

# Usa solo linguaggi funzionanti
python main.py smart
```

## Monitoraggio

Il sistema salva automaticamente:
- **Risultati test**: `results/execution/language_test_results_*.json`
- **Risultati esecuzione**: `results/execution/smart_execution_results_*.json`
- **Log errori**: `results/logs/`

## 🎉 Risultati Ottenuti

Hai ora un sistema completo che può:
- Analizzare automaticamente dataset di codice
- Identificare task comuni tra linguaggi
- Eseguire e testare codice in 16 linguaggi
- Gestire dipendenze temporaneamente
- Generare report analitici dettagliati
- Pulire automaticamente l'ambiente

** Perfetto per ricerca, benchmark, e analisi comparativa di algoritmi!**

## Installazione Completa

```bash
# 1. Clone repository
git clone https://github.com/Cappetti99/SWAM-Project.git
cd SWAM-Project

# 2. Installa dipendenze Python
pip install -r requirements.txt

# 3. Installa linguaggi (macOS)
./install_languages.sh

# 4. Test rapido
python main.py test

# 5. Prima esecuzione
python main.py simple
```

## 🤝 Contributi

1. Testa il sistema: `python main.py test`
2. Verifica funzionalità: `python main.py smart`
3. Segnala problemi con output completo
4. Proponi miglioramenti

---

**SWAM Project** © 2025 - Sistema Multi-linguaggio per Analisi Codice • **Scegli la modalità che preferisci**: `simple` per CSV rapidi, `smart` per analisi complete
