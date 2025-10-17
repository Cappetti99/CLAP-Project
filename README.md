# CLAP – Cross-Language Analysis Project

<div align="center">
    <img src="logoSWAM.png" alt="CLAP Project Logo" width="300"/>
</div>

**Advanced modular system for multi-language analysis and execution with CO2 emissions tracking.**

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://python.org)
[![Languages](https://img.shields.io/badge/Languages-16-green.svg)](#supported-languages)
[![CodeCarbon](https://img.shields.io/badge/CodeCarbon-Enabled-green.svg)](https://codecarbon.io)
[![Status](https://img.shields.io/badge/Status-Active-brightgreen.svg)](https://github.com)

## Overview

**CLAP** is an automated system for executing and analyzing code across **16 programming languages** with real-time CO2 emissions tracking. Perfect for:

- 🔬 **Academic research** on language efficiency
- ⚡ **Performance benchmarking** across languages
- 🌱 **Energy consumption analysis**
- 📊 **Comparative programming studies**

**Supported Languages:** C, C++, C#, Java, Python, JavaScript, TypeScript, Ruby, PHP, Go, Rust, Haskell, OCaml, R, MATLAB, Julia.

## 🚀 Quick Start (5 minutes)

```bash
# 1. Clone repository
git clone https://github.com/Cappetti99/CLAP-Project.git
cd CLAP-Project

# 2. Install dependencies (macOS/Linux)
python3 -m pip install -r requirements.txt

# 3. Test available languages on your system
python3 main.py test

# 4. Run smart execution (TOP 10 tasks)
python3 main.py smart

# 5. Run CO2 benchmark (fast mode - 3 minutes)
python3 main.py benchmark --mode fast
```

That's it! 🎉

## 📖 Main Commands

| Command | Description | Time |
|---------|-------------|------|
| `python3 main.py test` | Check available languages | ~10s |
| `python3 main.py find --task "bubble sort"` | Search specific algorithms | <1s |
| `python3 main.py analyze` | Find TOP 10 common tasks (required before smart) | <1s |
| `python3 main.py smart` | Execute tasks found by analyze | 2-5 min |
| `python3 main.py benchmark --mode fast` | Quick CO2 benchmark | 3-5 min |
| `python3 main.py benchmark --mode top10` | Full benchmark (30 iterations) | 45-60 min |
| `python3 main.py carbon` | Display CO2 statistics | <1s |
| `python3 main.py clean --stats` | Show disk usage statistics | <1s |
| `python3 main.py clean --execute` | Cleanup old session files | <1s |

**Recommended workflow:**
```bash
# 1. Detect available languages
python3 main.py test

# 2. Find TOP 10 common tasks
python3 main.py analyze

# 3. Execute those tasks
python3 main.py smart

# 4. (Optional) Run full CO2 benchmark
python3 main.py benchmark --mode top10
```

## 📊 Benchmark Results & Visualizations

CLAP generates professional visualizations of benchmark results:

```bash
# Generate all charts
python3 scripts/visualize_results.py --all

# Or generate specific charts
python3 scripts/visualize_results.py --ranking
python3 scripts/visualize_results.py --tasks
```

### 1. Language Energy Ranking

<div align="center">
    <img src="results/visualizations/language_energy_ranking.png" alt="Language Energy Ranking" width="700"/>
</div>

**Shows:** CO2 emissions and execution time by programming language. Compiled languages (C, C++, Rust) typically show lower emissions than interpreted languages (Python, Ruby).

### 2. CO2 vs Execution Time

<div align="center">
    <img src="results/visualizations/co2_vs_time_scatter.png" alt="CO2 vs Time Scatter" width="650"/>
</div>

**Shows:** Relationship between execution time and CO2 emissions. Bubble size = number of successful runs. Clear correlation: longer execution → higher emissions.

### 3. Top 10 Tasks Heatmap

<div align="center">
    <img src="results/visualizations/top_tasks_co2_heatmap.png" alt="Top Tasks Heatmap" width="650"/>
</div>

**Shows:** CO2 emissions for the 10 most common tasks across all languages. Reveals which algorithms are most energy-intensive and language-specific optimizations.

### 4. Success Rate by Language

<div align="center">
    <img src="results/visualizations/success_rate_comparison.png" alt="Success Rate Comparison" width="600"/>
</div>

**Shows:** Reliability indicator - percentage of successful executions vs failures for each language. Green = 100% success rate.

### 5. CO2 Distribution

<div align="center">
    <img src="results/visualizations/co2_distribution_boxplot.png" alt="CO2 Distribution Boxplot" width="650"/>
</div>

**Shows:** Statistical distribution of emissions per language. Box height indicates variability, median line shows typical emissions.

## 📈 Example Benchmark Results

Based on TOP10 benchmark (10 tasks × 30 iterations × 15 languages):

| Category | Languages | Avg CO2 (mg) | Avg Time (s) |
|----------|-----------|--------------|--------------|
| ⚡ **Low Emissions** | C, C++, Rust, Go | 8-13 | 0.18-0.29 |
| 🟢 **Medium** | Java, JavaScript, PHP | 38-45 | 0.95-1.12 |
| 🔴 **High** | Python, Ruby, Julia, R | 48-67 | 1.38-1.89 |

**Key Insights:**
- **Compiled languages** (C, C++, Rust) are 5-6× more efficient than interpreted languages.
- **Python** has highest success rate (99.6%) but higher emissions.
- **C/C++/Rust** optimal for compute-intensive tasks.
- **Python/JavaScript** good balance between productivity and performance.

## 📁 Project Structure

```
CLAP-Project/
├── main.py                 # Main CLI interface
├── requirements.txt        # Python dependencies
├── README.md              # This file
│
├── modules/               # Core configuration
│   ├── language_config.py
│   └── modern_logger.py
│
├── src/                   # Execution engines
│   ├── smart_executor.py
│   ├── task_searcher.py
│   ├── carbon_tracker.py
│   └── carbon_benchmark.py
│
├── data/                  # Code snippets dataset
│   └── generated/
│       └── code_snippets/
│
├── results/               # Output and visualizations
│   ├── carbon/           # CO2 tracking data
│   ├── csv/              # Exported CSV
│   ├── visualizations/   # Generated charts
│   └── logs/
│
└── scripts/               # Utility scripts
    ├── export_to_csv.py
    ├── extract_top_10.py
    └── visualize_results.py
    
```

## 🔧 Troubleshooting

**Language not detected?**
```bash
which python3  # or gcc, node, javac, etc.
python3 main.py test
```

**CO2 tracking disabled?**
```bash
python3 -m pip install codecarbon
```

**Permission errors?**
```bash
chmod +x main.py
chmod -R 755 src/ modules/
```

## 📚 Additional Resources

- **Dataset:** [Rosetta Code](https://huggingface.co/datasets/christopher/rosetta-code) implementations (1000+ tasks)
- **CO2 Tracking:** Powered by [CodeCarbon](https://codecarbon.io)
- **Python Libraries:** pandas, matplotlib, seaborn
- **Supported Paradigms:** OOP, Functional, Systems, Scientific

## 👨‍💻 Author

**Lorenzo Cappetti**  