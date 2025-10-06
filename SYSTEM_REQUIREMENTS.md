# SWAM Project - System Dependencies
# File per installazione compilatori e interpreti di sistema

## LINGUAGGI ATTUALMENTE DISPONIBILI ✅
# - C (gcc) - già installato
# - C++ (g++) - già installato  
# - C# (mono) - già installato
# - Python - già installato
# - Haskell (ghc) - già installato
# - OCaml - già installato

## LINGUAGGI MANCANTI DA INSTALLARE ❌

### UBUNTU/DEBIAN (apt):
```bash
# Linguaggi di base
sudo apt update
sudo apt install -y \
  openjdk-11-jdk \     # Java
  nodejs npm \         # JavaScript/TypeScript  
  golang-go \          # Go
  php-cli \            # PHP
  ruby-full \          # Ruby
  r-base \             # R

# TypeScript (richiede Node.js prima)
sudo npm install -g typescript

# Rust (installer speciale)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Julia (da Snap)
sudo snap install julia --classic
```

### ALTERNATIVE PER JULIA:
```bash
# Opzione 1: Download manuale (raccomandato)
wget https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.3-linux-x86_64.tar.gz
tar zxvf julia-1.9.3-linux-x86_64.tar.gz
sudo mv julia-1.9.3 /opt/julia
sudo ln -s /opt/julia/bin/julia /usr/local/bin/julia

# Opzione 2: Snap (più facile)
sudo snap install julia --classic
```

### VERIFICA INSTALLAZIONE:
```bash
# Dopo l'installazione, verifica:
python main.py test
```

### MATLAB 🔒
MATLAB richiede licenza commerciale:
- Studenti: licenza gratuita via università
- Commerciale: mathworks.com
- Alternative open: GNU Octave (`sudo apt install octave`)

### LINGUAGGI OPZIONALI (per completezza):
```bash
# Altri linguaggi non ancora nel tester
sudo apt install -y \
  perl \               # Perl
  lua5.3 \             # Lua  
  scala \              # Scala
  kotlin \             # Kotlin (via snap)
  dart                 # Dart

# Swift (complesso su Linux)
# Vedi: https://swift.org/download/
```

## INSTALLAZIONE RAPIDA TUTTI I LINGUAGGI:
```bash
# Comando one-liner per installare tutto (eccetto Rust e Julia):
sudo apt update && sudo apt install -y openjdk-11-jdk nodejs npm golang-go php-cli ruby-full r-base && sudo npm install -g typescript

# Poi aggiungi Rust:
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh && source ~/.cargo/env

# E Julia:
sudo snap install julia --classic

# Verifica finale:
python main.py test
```

## OBIETTIVO: 16/16 LINGUAGGI FUNZIONANTI! 🎯