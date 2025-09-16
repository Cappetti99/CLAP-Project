"""
Configurazione centrale per il progetto SWAM
Contiene tutte le impostazioni e configurazioni per i linguaggi di programmazione
"""

# Configurazioni di timeout e limiti
TIMEOUT_SECONDS = 30 # Timeout per l'esecuzione dei programmi
MAX_SNIPPETS_PER_LANGUAGE = 50 # Numero massimo di snippet per linguaggio
COMPILATION_TIMEOUT = 60 # Timeout per la compilazione

# Definizione dei linguaggi organizzati per categorie
LANGUAGE_CATEGORIES = {
 "OOP": {
 "C++": {
 "ext": ".cpp",
 "compile_cmd": ["g++", "-std=c++20"],
 "run_cmd": "./",
 "install_deps": None,
 "common_includes": ["#include <iostream>", "#include <vector>", "#include <string>", "#include <algorithm>"],
 "folder": "../data/generated/code_snippets/oop/c++"
 },
 "C#": {
 "ext": ".cs",
 "compile_cmd": ["csc"],
 "run_cmd": "mono ",
 "install_deps": None,
 "common_includes": ["using System;"],
 "folder": "../data/generated/code_snippets/oop/csharp"
 },
 "Java": {
 "ext": ".java",
 "compile_cmd": ["javac"],
 "run_cmd": "java ",
 "install_deps": None,
 "common_includes": ["import java.util.*;", "import java.io.*;"],
 "folder": "../data/generated/code_snippets/oop/java"
 }
 },
 "Scripting": {
 "Python": {
 "ext": ".py",
 "compile_cmd": None,
 "run_cmd": "conda run -n SWAM python ",
 "install_deps": "conda run -n SWAM pip install",
 "common_imports": ["import sys", "import os", "import re", "import math", "import random"],
 "folder": "../data/generated/code_snippets/scripting/python"
 },
 "Ruby": {
 "ext": ".rb",
 "compile_cmd": None,
 "run_cmd": "ruby ",
 "install_deps": "gem install",
 "common_imports": ["require 'json'"],
 "folder": "../data/generated/code_snippets/scripting/ruby"
 },
 "JavaScript": {
 "ext": ".js",
 "compile_cmd": None,
 "run_cmd": "conda run -n SWAM node ",
 "install_deps": "conda run -n SWAM npm install -g",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scripting/javascript"
 },
 "TypeScript": {
 "ext": ".ts",
 "compile_cmd": ["conda", "run", "-n", "SWAM", "tsc"],
 "run_cmd": "conda run -n SWAM node ",
 "install_deps": "conda run -n SWAM npm install -g typescript",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scripting/typescript"
 }
 },
 "Imperative": {
 "C": {
 "ext": ".c",
 "compile_cmd": ["gcc", "-std=c99"],
 "run_cmd": "./",
 "install_deps": None,
 "common_includes": ["#include <stdio.h>", "#include <stdlib.h>", "#include <string.h>"],
 "folder": "../data/generated/code_snippets/imperative/c"
 },
 "Go": {
 "ext": ".go",
 "compile_cmd": ["conda", "run", "-n", "SWAM", "go", "build"],
 "run_cmd": "./",
 "install_deps": "conda run -n SWAM go get",
 "common_imports": ["import \"fmt\""],
 "folder": "../data/generated/code_snippets/imperative/go"
 },
 "Rust": {
 "ext": ".rs",
 "compile_cmd": ["conda", "run", "-n", "SWAM", "rustc"],
 "run_cmd": "./",
 "install_deps": "conda run -n SWAM cargo install",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/imperative/rust"
 },
 "PHP": {
 "ext": ".php",
 "compile_cmd": None,
 "run_cmd": "php ",
 "install_deps": None,
 "common_imports": ["<?php"],
 "folder": "../data/generated/code_snippets/imperative/php"
 }
 },
 "Functional": {
 "Haskell": {
 "ext": ".hs",
 "compile_cmd": ["ghc"],
 "run_cmd": "./",
 "install_deps": "cabal install",
 "common_imports": ["import Data.List"],
 "folder": "../data/generated/code_snippets/functional/haskell"
 },
 "OCaml": {
 "ext": ".ml",
 "compile_cmd": ["ocamlfind", "ocamlc"],
 "run_cmd": "./",
 "install_deps": "opam install",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/functional/ocaml"
 }
 },
 "Scientific": {
 "R": {
 "ext": ".r",
 "compile_cmd": None,
 "run_cmd": "conda run -n SWAM Rscript ",
 "install_deps": "conda run -n SWAM R -e 'install.packages'",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scientific/r"
 },
 "MATLAB": {
 "ext": ".m",
 "compile_cmd": None,
 "run_cmd": "octave ",
 "install_deps": None,
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scientific/matlab"
 },
 "Julia": {
 "ext": ".jl",
 "compile_cmd": None,
 "run_cmd": "julia ",
 "install_deps": "julia -e 'using Pkg; Pkg.add'",
 "common_imports": [],
 "folder": "../data/generated/code_snippets/scientific/julia"
 }
 }
}

# Pattern problematici per linguaggio (codici da filtrare)
PROBLEMATIC_PATTERNS = {
 "Python": [
 r"import\s+tkinter",
 r"import\s+turtle",
 r"import\s+pygame",
 r"from\s+tkinter",
 r"input\s*\(",
 r"while\s+True:",
 r"for.*in.*range\(\s*\d{6,}"
 ],
 "Javascript": [
 r"document\.",
 r"window\.",
 r"prompt\s*\(",
 r"while\s*\(\s*true\s*\)"
 ],
 "Java": [
 r"JFrame",
 r"javax\.swing",
 r"Scanner.*System\.in",
 r"while\s*\(\s*true\s*\)"
 ],
 "C++": [
 r"getch\s*\(",
 r"cin\s*>>",
 r"while\s*\(\s*1\s*\)|while\s*\(\s*true\s*\)"
 ],
 "C": [
 r"getch\s*\(",
 r"scanf\s*\(",
 r"while\s*\(\s*1\s*\)"
 ]
}

# Pacchetti problematici da evitare nell'installazione automatica
PROBLEMATIC_PACKAGES = {
 "Python": ["tkinter", "turtle", "pygame", "numpy", "scipy", "matplotlib", "pandas"],
 "Javascript": ["fs", "path", "os", "crypto"],
 "Ruby": ["tk", "socket"],
 "R": ["base", "stats", "graphics"]
}
