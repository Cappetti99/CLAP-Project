# CLAP Project - System Dependencies
# File for installing system compilers and interpreters

## CURRENTLY AVAILABLE LANGUAGES ✅
# - C (gcc) - already installed
# - C++ (g++) - already installed  
# - C# (mono) - already installed
# - Python - already installed
# - Haskell (ghc) - already installed
# - OCaml - already installed

## MISSING LANGUAGES TO INSTALL ❌

### UBUNTU/DEBIAN (apt):
```bash
# Basic languages
sudo apt update
sudo apt install -y \
  openjdk-11-jdk \     # Java
  nodejs npm \         # JavaScript/TypeScript  
  golang-go \          # Go
  php-cli \            # PHP
  ruby-full \          # Ruby
  r-base \             # R

# TypeScript (requires Node.js first)
sudo npm install -g typescript

# Rust (special installer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Julia (via Snap)
sudo snap install julia --classic
```

### ALTERNATIVES FOR JULIA:
```bash
# Option 1: Manual download (recommended)
wget https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.3-linux-x86_64.tar.gz
tar zxvf julia-1.9.3-linux-x86_64.tar.gz
sudo mv julia-1.9.3 /opt/julia
sudo ln -s /opt/julia/bin/julia /usr/local/bin/julia

# Option 2: Snap (easier)
sudo snap install julia --classic
```

### INSTALLATION VERIFICATION:
```bash
# After installation, verify:
python main.py test
```

### MATLAB 🔒
MATLAB requires a commercial license:
- Students: free license via university
- Commercial: mathworks.com
- Open alternatives: GNU Octave (`sudo apt install octave`)

### OPTIONAL LANGUAGES (for completeness):
```bash
# Other languages not yet in the tester
sudo apt install -y \
  perl \               # Perl
  lua5.3 \             # Lua  
  scala \              # Scala
  kotlin \             # Kotlin (via snap)
  dart                 # Dart

# Swift (complex on Linux)
# See: https://swift.org/download/
```

## QUICK INSTALLATION OF ALL LANGUAGES:
```bash
# One-liner command to install everything (except Rust and Julia):
sudo apt update && sudo apt install -y openjdk-11-jdk nodejs npm golang-go php-cli ruby-full r-base && sudo npm install -g typescript

# Then add Rust:
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh && source ~/.cargo/env

# And Julia:
sudo snap install julia --classic

# Final verification:
python main.py test
```

## GOAL: 16/16 LANGUAGES WORKING! 🎯