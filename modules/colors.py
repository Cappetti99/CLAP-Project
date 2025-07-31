"""
Sistema di output colorato per il terminale
Gestisce la visualizzazione colorata dei messaggi e lo stato del progetto
"""

import logging

# Definizione dei colori ANSI per il terminale
class Colors:
    # Colori di base
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    MAGENTA = '\033[95m'
    CYAN = '\033[96m'
    WHITE = '\033[97m'
    ORANGE = '\033[38;5;208m'
    PURPLE = '\033[38;5;129m'
    PINK = '\033[38;5;213m'
    LIME = '\033[38;5;154m'
    TEAL = '\033[38;5;80m'
    
    # Stili
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    ITALIC = '\033[3m'
    
    # Reset
    END = '\033[0m'
    
    # Background colors
    BG_RED = '\033[101m'
    BG_GREEN = '\033[102m'
    BG_YELLOW = '\033[103m'
    BG_BLUE = '\033[104m'
    BG_MAGENTA = '\033[105m'
    BG_CYAN = '\033[106m'

# Mapping colori per categorie
CATEGORY_COLORS = {
    "OOP": Colors.BLUE + Colors.BOLD,
    "Scripting": Colors.GREEN + Colors.BOLD,
    "Imperative": Colors.RED + Colors.BOLD,
    "Functional": Colors.MAGENTA + Colors.BOLD,
    "Scientific": Colors.CYAN + Colors.BOLD
}

# Mapping colori per linguaggi specifici
LANGUAGE_COLORS = {
    # OOP
    "C++": Colors.BLUE,
    "C#": Colors.PURPLE,
    "Java": Colors.ORANGE,
    
    # Scripting
    "Python": Colors.YELLOW,
    "Ruby": Colors.RED,
    "Javascript": Colors.LIME,
    "TypeScript": Colors.BLUE,
    
    # Imperative
    "C": Colors.CYAN,
    "Go": Colors.TEAL,
    "Rust": Colors.ORANGE,
    "PHP": Colors.PURPLE,
    
    # Functional
    "Haskell": Colors.MAGENTA,
    "OCaml": Colors.PINK,
    
    # Scientific
    "R": Colors.CYAN,
    "MATLAB": Colors.BLUE,
    "Julia": Colors.PURPLE
}

# Colori per stati
STATUS_COLORS = {
    "SUCCESS": Colors.GREEN + Colors.BOLD,
    "ERROR": Colors.RED + Colors.BOLD,
    "WARNING": Colors.YELLOW + Colors.BOLD,
    "INFO": Colors.CYAN,
    "DEBUG": Colors.WHITE,
    "FILTERED": Colors.ORANGE,
    "TIMEOUT": Colors.MAGENTA,
    "COMPILATION": Colors.RED,
    "DEPENDENCY": Colors.YELLOW
}

class ColoredFormatter(logging.Formatter):
    """Formatter personalizzato per log colorati"""
    
    COLORS = {
        'DEBUG': Colors.WHITE,
        'INFO': Colors.CYAN,
        'WARNING': Colors.YELLOW,
        'ERROR': Colors.RED,
        'CRITICAL': Colors.RED + Colors.BOLD
    }

    def format(self, record):
        # Salva il messaggio originale
        original_msg = record.msg
        
        # Colora in base al livello
        color = self.COLORS.get(record.levelname, Colors.WHITE)
        record.msg = f"{color}{record.msg}{Colors.END}"
        
        # Usa il formatter base
        formatted = super().format(record)
        
        # Ripristina il messaggio originale
        record.msg = original_msg
        
        return formatted

def colored_print(text, color=Colors.WHITE, style="", end="\n"):
    """Stampa testo colorato nel terminale"""
    print(f"{style}{color}{text}{Colors.END}", end=end)

def get_language_color(language):
    """Ottiene il colore per un linguaggio specifico"""
    return LANGUAGE_COLORS.get(language, Colors.WHITE)

def get_category_color(category):
    """Ottiene il colore per una categoria"""
    return CATEGORY_COLORS.get(category, Colors.WHITE)

def get_status_color(status):
    """Ottiene il colore per uno stato"""
    return STATUS_COLORS.get(status.upper(), Colors.WHITE)

def print_header(text):
    """Stampa un'intestazione colorata"""
    colored_print(f"\n{'='*60}", Colors.CYAN, Colors.BOLD)
    colored_print(text, Colors.CYAN, Colors.BOLD)
    colored_print(f"{'='*60}", Colors.CYAN, Colors.BOLD)

def print_section(text):
    """Stampa un titolo di sezione"""
    colored_print(f"\n🔹 {text}", Colors.CYAN, Colors.BOLD)

def print_success(text):
    """Stampa un messaggio di successo"""
    colored_print(f"✅ {text}", Colors.GREEN, Colors.BOLD)

def print_error(text):
    """Stampa un messaggio di errore"""
    colored_print(f"❌ {text}", Colors.RED, Colors.BOLD)

def print_warning(text):
    """Stampa un messaggio di avvertimento"""
    colored_print(f"⚠️ {text}", Colors.YELLOW, Colors.BOLD)

def print_info(text):
    """Stampa un messaggio informativo"""
    colored_print(f"ℹ️ {text}", Colors.CYAN)
