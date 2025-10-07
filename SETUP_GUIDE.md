# 🚀 COMPLETE GUIDE: CLAP Project Environment Setup

This guide helps you configure a complete environment to run the entire CLAP project with all 16 supported languages.


## 🎯 OBJECTIVE
- Isolated Python environment ✅
- All 16 languages working ✅  
- CO2 monitoring active ✅
- Datasets available ✅

---

## 📋 PREREQUISITES

### Operating System
- **Linux** (Ubuntu/Debian) - Recommended
- **macOS** - Supported  
- **Windows** - Possible but more complex

### Basic Tools
```bash
# Ensure the following are installed:
git --version
python3 --version  # >= 3.8
pip3 --version
```

---

## 🔧 STEP 1: CLONE AND SETUP DIRECTORY

```bash
# 1. Clone the project
git clone https://github.com/Cappetti99/SWAM-Project.git
cd SWAM-Project

# 2. Verify structure
ls -la
# You should see: main.py, requirements.txt, src/, modules/, data/, etc.
```

---

## 🐍 STEP 2: PYTHON ENVIRONMENT

### Option A: venv (Recommended)
```bash
# 1. Create virtual environment
python3 -m venv swam_env

# 2. Activate environment
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows

# 3. Upgrade pip
pip install --upgrade pip

# 4. Install Python dependencies
pip install -r requirements.txt

# 5. Verify installation
pip list | grep codecarbon  # Should show codecarbon>=3.0.0
```

### Option B: conda (Alternative)
```bash
# 1. Create conda environment
conda create -n swam_env python=3.10

# 2. Activate environment
conda activate swam_env

# 3. Install dependencies
pip install -r requirements.txt
```

---

## 🛠️ STEP 3: COMPILERS AND INTERPRETERS

### Ubuntu/Debian (apt)
```bash
# 1. Update system
sudo apt update

# 2. Install base languages
sudo apt install -y \
  build-essential \    # gcc, g++, make
  openjdk-11-jdk \     # Java
  nodejs npm \         # JavaScript
  golang-go \          # Go
  php-cli \            # PHP
  ruby-full \          # Ruby
  r-base \             # R
  ghc \                # Haskell (if not already installed)
  ocaml \              # OCaml (if not already installed)
  mono-complete        # C# Mono (if not already installed)

# 3. TypeScript (via npm)
sudo npm install -g typescript

# 4. Rust (special installer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# 5. Julia (via snap - more reliable)
sudo snap install julia --classic

# 6. RESTART TERMINAL to load all PATHs
```

### macOS (Homebrew)
```bash
# 1. Install Homebrew if not present
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 2. Install languages
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

## ✅ STEP 4: VERIFY INSTALLATION

```bash
# 1. Activate Python environment (if not already active)
source swam_env/bin/activate  # Linux/macOS
# swam_env\Scripts\activate   # Windows

# 2. Complete language test
python main.py test

# 3. Expected result:
# "Available languages: 16"
# "Success rate: 100.0%"
```

### 🐛 Debug if something doesn't work:
```bash
# Check individual commands:
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
matlab -help       # MATLAB (if available)
```

---

## 🌱 STEP 5: SETUP CODECARBON (CO2 Monitoring)

```bash
# 1. Verify codecarbon
python -c "import codecarbon; print('CodeCarbon OK!')"

# 2. Test CO2 monitoring
python main.py carbon

# 3. If errors occur, reinstall:
pip uninstall codecarbon
pip install codecarbon>=3.0.0
```

---

## 📊 STEP 6: DOWNLOAD DATASET (Optional)

```bash
# Only if you want to download fresh datasets (otherwise use included ones)
python download.py

# Verify dataset
ls -la data/generated/code_snippets/
# You should see folders for various languages
```

---

## 🧪 STEP 7: COMPLETE SYSTEM TEST

```bash
# 1. Test languages
python main.py test
# Goal: 16/16 languages ✅

# 2. Test task search
python main.py find --task "hello"
# Should find task and allow execution

# 3. Quick benchmark test
python main.py benchmark
# Choose "quick" mode for a fast test

# 4. View CO2 results
python main.py carbon
```

---

## 📁 STEP 8: FINAL STRUCTURE

After completing the setup, you should have:

```
SWAM-Project/
├── swam_env/                    # Python environment
├── main.py                      # Entry point
├── requirements.txt             # Python dependencies ✅
├── SYSTEM_REQUIREMENTS.md       # Compiler guide ✅
├── src/                         # Source code
├── modules/                     # CLAP modules
├── data/generated/              # Dataset code snippets
├── results/                     # Analysis results (empty after cleanup)
└── export_to_csv.py             # Export results
```

---

## 🎯 FINAL SUCCESS CHECK

Run this checklist to verify everything works:

```bash
# ✅ Active environment
echo $VIRTUAL_ENV  # Should show swam_env path

# ✅ Python dependencies
python -c "import codecarbon, pandas, datasets; print('All Python deps OK!')"

# ✅ System compilers  
python main.py test | grep "Success rate: 100.0%"

# ✅ CO2 tracking
python main.py carbon --help

# ✅ Task search
python main.py find --task "sort" | head -10

# ✅ Export CSV
python export_to_csv.py --help
```

---

## 🚨 COMMON TROUBLESHOOTING

### Issue: "Command not found"
```bash
# Solution: Restart terminal or reload PATH
source ~/.bashrc
source ~/.cargo/env  # For Rust
```

### Issue: "Permission denied"
```bash
# Solution: Add sudo where necessary
sudo apt install <language>
```

### Issue: Julia not found
```bash
# Alternative solution: Manual download
wget https://julialang-s3.julialang.org/bin/linux/x64/1.9/julia-1.9.3-linux-x86_64.tar.gz
tar zxvf julia-1.9.3-linux-x86_64.tar.gz
sudo mv julia-1.9.3 /opt/julia
sudo ln -s /opt/julia/bin/julia /usr/local/bin/julia
```

### Issue: MATLAB not available
```bash
# This is normal - requires a commercial license
# Alternative: GNU Octave
sudo apt install octave
```

---

## 🎉 FINAL RESULT

After following all the steps, you should have:

- ✅ **Isolated Python environment** working
- ✅ **16/16 languages** available and tested  
- ✅ **CO2 monitoring** active with CodeCarbon
- ✅ **Code snippets dataset** available
- ✅ **All CLAP commands** functioning

### Ready-to-use commands:
```bash
python main.py test      # Test languages  
python main.py analyze   # Analyze common tasks
python main.py smart     # Intelligent execution
python main.py benchmark # CO2 benchmark
python main.py find      # Search specific tasks
python main.py carbon    # CO2 report
```

**🚀 Your CLAP environment is ready for multi-language analysis with CO2 monitoring!**