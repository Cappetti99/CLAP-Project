#!/usr/bin/env python3
"""
Script di pulizia per file di sessione SWAM
Mantiene solo i file session_*.json degli ultimi 2 giorni
"""

import os
import glob
import time
from datetime import datetime, timedelta
from pathlib import Path

def cleanup_session_files(days_to_keep=2, dry_run=True):
    """
    Pulisce i file di sessione mantenendo solo quelli degli ultimi N giorni
    
    Args:
        days_to_keep (int): Giorni da mantenere (default: 2)
        dry_run (bool): Se True, mostra solo cosa verrebbe eliminato senza farlo
    """
    
    # Directory dei risultati carbon
    carbon_dir = "results/carbon"
    
    if not os.path.exists(carbon_dir):
        print(f"❌ Directory {carbon_dir} non trovata")
        return
    
    # Calcola la data limite (2 giorni fa)
    cutoff_date = datetime.now() - timedelta(days=days_to_keep)
    cutoff_timestamp = cutoff_date.timestamp()
    
    print(f"🧹 PULIZIA FILE SESSIONE SWAM")
    print(f"📅 Data limite: {cutoff_date.strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"📁 Directory: {carbon_dir}")
    print("=" * 50)
    
    # Trova tutti i file session_*.json
    session_pattern = os.path.join(carbon_dir, "session_*.json")
    session_files = glob.glob(session_pattern)
    
    print(f"📊 File sessione trovati: {len(session_files)}")
    
    # Categorizza i file
    files_to_keep = []
    files_to_delete = []
    
    for file_path in session_files:
        # Ottieni il timestamp di modifica
        file_mtime = os.path.getmtime(file_path)
        file_date = datetime.fromtimestamp(file_mtime)
        
        if file_mtime > cutoff_timestamp:
            files_to_keep.append((file_path, file_date))
        else:
            files_to_delete.append((file_path, file_date))
    
    # Mostra statistiche
    print(f"✅ File da mantenere: {len(files_to_keep)}")
    print(f"🗑️  File da eliminare: {len(files_to_delete)}")
    
    if not files_to_delete:
        print("\n🎉 Nessun file da eliminare! Tutto pulito.")
        return 0
    
    # Mostra alcuni esempi di file da eliminare
    print(f"\n📋 Esempi file da eliminare:")
    for i, (file_path, file_date) in enumerate(files_to_delete[:5]):
        filename = os.path.basename(file_path)
        print(f"  • {filename} ({file_date.strftime('%Y-%m-%d %H:%M')})")
    
    if len(files_to_delete) > 5:
        print(f"  ... e altri {len(files_to_delete) - 5} file")
    
    # Mostra alcuni esempi di file da mantenere
    print(f"\n📋 Esempi file da mantenere:")
    for i, (file_path, file_date) in enumerate(files_to_keep[:5]):
        filename = os.path.basename(file_path)
        print(f"  • {filename} ({file_date.strftime('%Y-%m-%d %H:%M')})")
    
    if len(files_to_keep) > 5:
        print(f"  ... e altri {len(files_to_keep) - 5} file")
    
    # Calcola spazio liberato (approssimativo)
    total_size = 0
    for file_path, _ in files_to_delete:
        try:
            total_size += os.path.getsize(file_path)
        except:
            pass
    
    size_mb = total_size / (1024 * 1024)
    print(f"\n💾 Spazio da liberare: ~{size_mb:.1f} MB")
    
    if dry_run:
        print("\n⚠️  MODALITÀ DRY-RUN: Nessun file eliminato")
        print("💡 Per eliminare effettivamente, usa: --execute")
        return len(files_to_delete)
    
    # Eliminazione effettiva
    print(f"\n🗑️  Eliminazione {len(files_to_delete)} file...")
    deleted_count = 0
    
    for file_path, file_date in files_to_delete:
        try:
            os.remove(file_path)
            deleted_count += 1
            if deleted_count % 100 == 0:  # Progress ogni 100 file
                print(f"   Eliminati {deleted_count}/{len(files_to_delete)} file...")
        except Exception as e:
            print(f"❌ Errore eliminando {os.path.basename(file_path)}: {e}")
    
    print(f"\n✅ Pulizia completata!")
    print(f"🗑️  File eliminati: {deleted_count}")
    print(f"✅ File mantenuti: {len(files_to_keep)}")
    print(f"💾 Spazio liberato: ~{size_mb:.1f} MB")
    
    return deleted_count

def show_file_distribution():
    """Mostra la distribuzione temporale dei file"""
    carbon_dir = "results/carbon"
    session_pattern = os.path.join(carbon_dir, "session_*.json")
    session_files = glob.glob(session_pattern)
    
    if not session_files:
        print("❌ Nessun file sessione trovato")
        return
    
    print(f"📊 DISTRIBUZIONE TEMPORALE FILE SESSIONE")
    print("=" * 50)
    
    # Raggruppa per data
    dates = {}
    for file_path in session_files:
        file_mtime = os.path.getmtime(file_path)
        file_date = datetime.fromtimestamp(file_mtime)
        date_key = file_date.strftime("%Y-%m-%d")
        
        if date_key not in dates:
            dates[date_key] = 0
        dates[date_key] += 1
    
    # Mostra distribuzione
    total_files = len(session_files)
    for date_key in sorted(dates.keys(), reverse=True):
        count = dates[date_key]
        percentage = (count / total_files) * 100
        print(f"📅 {date_key}: {count:4d} file ({percentage:5.1f}%)")
    
    print(f"\n📊 Totale: {total_files} file")
    
    # Suggerisce pulizia
    if len(dates) > 3:
        oldest_dates = sorted(dates.keys())[:-2]  # Tutte tranne le ultime 2
        files_to_clean = sum(dates[date] for date in oldest_dates)
        print(f"\n💡 Suggerimento: mantenendo 2 giorni, elimineresti {files_to_clean} file")

def main():
    """Funzione principale"""
    import sys
    
    # Controlla argomenti
    dry_run = True
    days_to_keep = 2
    show_stats = False
    
    if "--execute" in sys.argv:
        dry_run = False
        print("🚨 MODALITÀ ESECUZIONE: I file verranno eliminati!")
    
    if "--stats" in sys.argv:
        show_stats = True
    
    if "--help" in sys.argv or "-h" in sys.argv:
        print("🧹 SCRIPT PULIZIA SESSIONI SWAM")
        print("=" * 40)
        print("Mantiene solo i file session_*.json degli ultimi N giorni")
        print()
        print("Opzioni:")
        print("  --stats              Mostra distribuzione temporale file")
        print("  --days N             Giorni da mantenere (default: 2)")
        print("  --execute            Esegue effettivamente la pulizia")
        print("  --help, -h           Mostra questo aiuto")
        print()
        print("Esempi:")
        print("  python cleanup_sessions.py                    # Dry-run con 2 giorni")
        print("  python cleanup_sessions.py --stats            # Mostra statistiche")
        print("  python cleanup_sessions.py --days 1           # Dry-run con 1 giorno")
        print("  python cleanup_sessions.py --days 3 --execute # Elimina mantenendo 3 giorni")
        print()
        print("⚠️  SICUREZZA:")
        print("• Modalità dry-run di default (nessuna eliminazione)")
        print("• Mantiene SEMPRE file benchmark e analisi")
        print("• Elimina SOLO file session_*.json più vecchi")
        return
    
    if "--days" in sys.argv:
        try:
            idx = sys.argv.index("--days")
            days_to_keep = int(sys.argv[idx + 1])
        except (IndexError, ValueError):
            print("❌ Errore: --days richiede un numero")
            sys.exit(1)
    
    # Controlla se siamo nella directory corretta
    if not os.path.exists("main.py") or not os.path.exists("results"):
        print("❌ Errore: Esegui lo script dalla directory SWAM-Project")
        sys.exit(1)
    
    # Mostra statistiche se richiesto
    if show_stats:
        show_file_distribution()
        return
    
    try:
        deleted_count = cleanup_session_files(days_to_keep=days_to_keep, dry_run=dry_run)
        
        if dry_run and deleted_count > 0:
            print(f"\n🔄 Per eseguire la pulizia:")
            print(f"   python cleanup_sessions.py --execute")
            print(f"\n🔄 Per cambiare giorni da mantenere:")
            print(f"   python cleanup_sessions.py --days 3 --execute")
        elif dry_run and deleted_count == 0:
            print(f"\n💡 Opzioni disponibili:")
            print(f"   python cleanup_sessions.py --stats          # Mostra distribuzione file")
            print(f"   python cleanup_sessions.py --days 1         # Testa con 1 giorno") 
            print(f"   python cleanup_sessions.py --help           # Mostra aiuto")
            
    except KeyboardInterrupt:
        print("\n⚠️  Operazione annullata dall'utente")
    except Exception as e:
        print(f"❌ Errore imprevisto: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
