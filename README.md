# 🌊 SWAM Project - Cross-Language Code Analysis System

**Sistema di Analisi e Esecuzione Multi-Linguaggio** - Analizza ed esegue automaticamente codice in 16 linguaggi di programmazione.

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-16-green.svg)](#linguaggi-supportati)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## 🚀 Quick Start

```bash
# 1. Test disponibilità linguaggi
python main.py test

# 2. Esecuzione intelligente (usa solo linguaggi disponibili)
python main.py smart

# 3. Analisi task comuni
python main.py analyze

# 4. Stato del progetto
python main.py status
```

## 📋 Comandi Disponibili

| Comando | Descrizione |
|---------|------------|
| `test` | Testa la disponibilità di tutti i 16 linguaggi |
| `smart` | Esegue codici solo nei linguaggi disponibili |
| `analyze` | Analizza task comuni tra linguaggi |
| `execute` | Esegue codici in tutti i linguaggi (può fallire) |
| `clean` | Pulisce file temporanei e cache |
| `status` | Mostra stato del progetto e statistiche |

## 🎯 Linguaggi Supportati

### ✅ Interpretati (7)
Python • JavaScript • Ruby • PHP • R • Julia • MATLAB*

### ✅ Compilati (8) 
C • C++ • Java • Go • Rust • Haskell • OCaml • TypeScript

### ✅ VM-based (1)
C# (Mono)

*\*MATLAB richiede licenza commerciale*

## 📁 Struttura Progetto

```
SWAM-Project/
├── main.py                      # 🎯 Interface principale
├── cleanup.py                   # 🧹 Script pulizia completo
├── install_languages.sh         # 📦 Installer linguaggi
├── modules/                     # 📚 Moduli core
│   ├── config.py               # ⚙️  Configurazioni
│   ├── colors.py               # 🎨 Output colorato
│   ├── dataset_manager.py      # 📊 Gestione dataset
│   ├── dependency_analyzer.py  # 🔍 Analisi dipendenze
│   └── logger.py               # 📝 Logging
├── src/                        # 🔧 Esecutori e analyzer
│   ├── enhanced_executor.py    # 🚀 Esecutore completo
│   ├── smart_executor.py       # 🧠 Esecutore adattivo
│   ├── language_tester.py      # 🧪 Test linguaggi
│   ├── advanced_task_finder.py # 🎯 Ricerca task
│   └── task_finder.py          # 🔍 Ricerca base
└── results/                    # 📊 Output generati
    ├── execution/              # Risultati esecuzione
    ├── task_analysis/          # Analisi task
    └── logs/                   # Log sistema
```

## 🛠️ Installazione Linguaggi

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

## 📊 Performance Tipiche

| Categoria | Linguaggi Disponibili | Tasso Successo Tipico |
|-----------|----------------------|----------------------|
| **Interpretati** | 6/7 (86%) | Python, JS, Ruby, PHP, R, Julia |
| **Compilati** | 8/8 (100%) | Tutti se installati correttamente |
| **VM-based** | 1/1 (100%) | C# (Mono) |
| **Totale** | **15/16 (93.8%)** | Solo MATLAB manca |

## 🎯 Esempi Risultati

### Test Linguaggi
```bash
$ python main.py test
🧪 Testing PYTHON (interpreted)...
  ✅ Esecuzione riuscita (0.011s): Hello from Python!

📊 STATISTICHE GENERALI:
  🎯 Totale linguaggi testati: 16
  ✅ Linguaggi disponibili: 15
  📊 Tasso di successo: 93.8%
```

### Smart Execution
```bash
$ python main.py smart
🧠 ESECUZIONE INTELLIGENTE
🎯 Linguaggi disponibili: 8
🎯 Trovate 10 task comuni

🎯 Esecuzione task: Array_length
  ✅ python: successo (0.01s)
  ✅ javascript: successo (0.06s)
  ✅ java: successo (0.27s)
  
🎉 TOTALE: 32/79 (40.5%) esecuzioni riuscite
```

## 🧹 Manutenzione

### Pulizia File Temporanei
```bash
python cleanup.py --all              # Pulizia completa
python cleanup.py --compiled         # Solo file compilati
python cleanup.py --dry-run          # Simula senza cancellare
```

### Monitoraggio Spazio
```bash
python main.py status                # Stato progetto
python cleanup.py --all              # Libera spazio
```

## 🔧 Troubleshooting

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

## 📈 Monitoraggio

Il sistema salva automaticamente:
- **Risultati test**: `results/execution/language_test_results_*.json`
- **Risultati esecuzione**: `results/execution/smart_execution_results_*.json` 
- **Log errori**: `results/logs/`

## 🤝 Contributi

1. Testa il sistema: `python main.py test`
2. Verifica funzionalità: `python main.py smart`
3. Segnala problemi con output completo
4. Proponi miglioramenti

---

**SWAM Project** © 2025 - Sistema Multi-linguaggio per Analisi Codice
