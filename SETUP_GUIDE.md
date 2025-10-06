# 🚀 GUIDA COMPLETA: Setup Environment SWAM Project

Questa guida ti aiuta a configurare un environment completo per eseguire tutto il progetto SWAM con tutti i 16 linguaggi supportati.

## 🎯 OBIETTIVO
- Environment Python isolato ✅
- Tutti i 16 linguaggi funzionanti ✅  
- Monitoraggio CO2 attivo ✅
- Dataset disponibili ✅

---

## 📋 PREREQUISITI

### Sistema Operativo
- **Linux** (Ubuntu/Debian) - Raccomandato
- **macOS** - Supportato  
- **Windows** - Possibile ma più complesso

### Strumenti Base
```bash
# Verifica che ci siano:
git --version
python3 --version  # >= 3.8
pip3 --version
```

---

## 🔧 STEP 1: CLONE E SETUP DIRECTORY

```bash
# 1. Clone del progetto
git clone https://github.com/Cappetti99/SWAM-Project.git
cd SWAM-Project

# 2. Verifica struttura
ls -la
# Dovresti vedere: main.py, requirements.txt, src/, modules/, data/, etc.
```

---

## 🐍 STEP 2: PYTHON ENVIRONMENT

### Opzione A: venv (Raccomandato)
```bash
# 1. Crea environment virtuale
python3 -m venv swam_env

# 2. Attiva environment
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows

# 3. Aggiorna pip
pip install --upgrade pip

# 4. Installa dipendenze Python
pip install -r requirements.txt

# 5. Verifica installazione
pip list | grep codecarbon  # Dovrebbe mostrare codecarbon>=3.0.0
```

### Opzione B: conda (Alternativa)
```bash
# 1. Crea environment conda
conda create -n swam_env python=3.10

# 2. Attiva environment
conda activate swam_env

# 3. Installa dipendenze
pip install -r requirements.txt
```

---

## 🛠️ STEP 3: COMPILATORI E INTERPRETI

### Ubuntu/Debian (apt)
```bash
# 1. Update sistema
sudo apt update

# 2. Installa linguaggi di base
sudo apt install -y \
  build-essential \    # gcc, g++, make
  openjdk-11-jdk \     # Java
  nodejs npm \         # JavaScript
  golang-go \          # Go
  php-cli \            # PHP
  ruby-full \          # Ruby
  r-base \             # R
  ghc \                # Haskell (se non già presente)
  ocaml \              # OCaml (se non già presente)
  mono-complete        # C# Mono (se non già presente)

# 3. TypeScript (via npm)
sudo npm install -g typescript

# 4. Rust (installer speciale)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# 5. Julia (via snap - più affidabile)
sudo snap install julia --classic

# 6. RIAVVIA IL TERMINALE per caricare tutti i PATH
```

### macOS (Homebrew)
```bash
# 1. Installa Homebrew se non presente
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 2. Installa linguaggi
brew install \
  gcc \
  openjdk \
  node \
  go \
  php \
  ruby \
  r \
  ghc \
  ocaml \
  mono

# 3. TypeScript
npm install -g typescript

# 4. Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# 5. Julia
brew install julia
```

---

## ✅ STEP 4: VERIFICA INSTALLAZIONE

```bash
# 1. Attiva environment Python (se non già attivo)
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows

# 2. Test completo linguaggi
python main.py test

# 3. Obiettivo: vedere questo risultato
# "Linguaggi disponibili: 16"
# "Tasso di successo: 100.0%"
```

### 🐛 Debug se qualcosa non funziona:
```bash
# Controlla singoli comandi:
gcc --version      # C
g++ --version      # C++
javac -version     # Java
node --version     # JavaScript
go version         # Go
php --version      # PHP
ruby --version     # Ruby
rustc --version    # Rust
ghc --version      # Haskell
ocaml -version     # OCaml
Rscript --version  # R
julia --version    # Julia
mono --version     # C#
tsc --version      # TypeScript
matlab -help       # MATLAB (se disponibile)
```

---

## 🌱 STEP 5: SETUP CODECARBON (Monitoraggio CO2)

```bash
# 1. Verifica codecarbon
python -c "import codecarbon; print('CodeCarbon OK!')"

# 2. Test monitoraggio CO2
python main.py carbon

# 3. Se da errori, reinstalla:
pip uninstall codecarbon
pip install codecarbon>=3.0.0
```

---

## 📊 STEP 6: DOWNLOAD DATASET (Opzionale)

```bash
# Solo se vuoi scaricare dataset freschi (altrimenti usa quelli inclusi)
python download.py

# Verifica dataset
ls -la data/generated/code_snippets/
# Dovresti vedere cartelle per vari linguaggi
```

---

## 🧪 STEP 7: TEST COMPLETO SISTEMA

```bash
# 1. Test linguaggi
python main.py test
# Obiettivo: 16/16 linguaggi ✅

# 2. Test ricerca task
python main.py find --task "hello"
# Dovrebbe trovare task e permettere esecuzione

# 3. Test benchmark veloce
python main.py benchmark
# Scegli modalità "veloce" per test rapido

# 4. Visualizza risultati CO2
python main.py carbon
```

---

## 📁 STEP 8: STRUTTURA FINALE

Dopo il setup completo dovresti avere:

```
SWAM-Project/
├── swam_env/                    # Environment Python
├── main.py                      # Entry point
├── requirements.txt             # Dipendenze Python ✅
├── SYSTEM_REQUIREMENTS.md       # Guida compilatori ✅
├── src/                         # Codice sorgente
├── modules/                     # Moduli SWAM
├── data/generated/              # Dataset code snippets
├── results/                     # Risultati analisi (vuoti dopo pulizia)
└── export_to_csv.py            # Export risultati
```

---

## 🎯 VERIFICA FINALE SUCCESS

Esegui questa checklist per verificare che tutto funzioni:

```bash
# ✅ Environment attivo
echo $VIRTUAL_ENV  # Dovrebbe mostrare path swam_env

# ✅ Python dependencies
python -c "import codecarbon, pandas, datasets; print('All Python deps OK!')"

# ✅ System compilers  
python main.py test | grep "Tasso di successo: 100.0%"

# ✅ CO2 tracking
python main.py carbon --help

# ✅ Task search
python main.py find --task "sort" | head -10

# ✅ Export CSV
python export_to_csv.py --help
```

---

## 🚨 TROUBLESHOOTING COMUNE

### Problema: "Command not found"
```bash
# Soluzione: Riavvia terminale o ricarica PATH
source ~/.bashrc
source ~/.cargo/env  # Per Rust
```

### Problema: "Permission denied"
```bash
# Soluzione: Aggiungi sudo dove necessario
sudo apt install <linguaggio>
```

### Problema: Julia non trovato
```bash
# Soluzione alternativa: Download manuale
wget https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.3-linux-x86_64.tar.gz
tar zxvf julia-1.9.3-linux-x86_64.tar.gz
sudo mv julia-1.9.3 /opt/julia
sudo ln -s /opt/julia/bin/julia /usr/local/bin/julia
```

### Problema: MATLAB non disponibile
```bash
# È normale - richiede licenza commerciale
# Alternative: GNU Octave
sudo apt install octave
```

---

## 🎉 RISULTATO FINALE

Dopo aver seguito tutti gli step dovresti avere:

- ✅ **Environment Python** isolato e funzionante
- ✅ **16/16 linguaggi** disponibili e testati  
- ✅ **Monitoraggio CO2** attivo con CodeCarbon
- ✅ **Dataset** code snippets disponibili
- ✅ **Tutti i comandi** SWAM funzionanti

### Comandi pronti all'uso:
```bash
python main.py test      # Test linguaggi  
python main.py analyze   # Analisi task comuni
python main.py smart     # Esecuzione intelligente
python main.py benchmark # Benchmark CO2
python main.py find      # Ricerca task specifica
python main.py carbon    # Report CO2
```

**🚀 Il tuo environment SWAM è pronto per l'analisi multi-linguaggio con monitoraggio CO2!**