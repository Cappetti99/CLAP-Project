#!/bin/bash
# Script di installazione automatica per linguaggi SWAM
# Per macOS con Homebrew

echo "🚀 INSTALLAZIONE LINGUAGGI SWAM"
echo "================================="

# Verifica Homebrew
if ! command -v brew &> /dev/null; then
    echo "❌ Homebrew non trovato. Installa prima Homebrew:"
    echo "   /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
    exit 1
fi

echo "✅ Homebrew trovato"

# Installa Xcode Command Line Tools (per C/C++)
echo ""
echo "🔧 Installazione Xcode Command Line Tools..."
xcode-select --install 2>/dev/null || echo "✅ Xcode Command Line Tools già installati"

# Installa linguaggi via Homebrew
echo ""
echo "📦 Installazione linguaggi via Homebrew..."

languages=("go" "rust" "ghc" "ocaml")
for lang in "${languages[@]}"; do
    echo "  🔄 Installando $lang..."
    if brew list "$lang" &>/dev/null; then
        echo "  ✅ $lang già installato"
    else
        brew install "$lang"
        echo "  ✅ $lang installato"
    fi
done

# Installa TypeScript globalmente
echo ""
echo "📝 Installazione TypeScript..."
if command -v tsc &> /dev/null; then
    echo "  ✅ TypeScript già installato"
else
    npm install -g typescript
    echo "  ✅ TypeScript installato"
fi

echo ""
echo "🎉 INSTALLAZIONE COMPLETATA!"
echo "================================="
echo ""
echo "📋 Note:"
echo "  • MATLAB richiede licenza commerciale"
echo "  • Riavvia il terminale dopo l'installazione"
echo "  • Esegui 'python src/language_tester.py' per verificare"
echo ""
echo "🔄 Per testare i linguaggi installati:"
echo "   python main.py test"
