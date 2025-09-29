#!/usr/bin/env python3
"""
Carbon Tracker - Sistema di monitoraggio impatto ambientale per SWAM
Integra CodeCarbon per tracciare le emissioni CO2 delle esecuzioni multi-linguaggio
"""

import os
import json
import sys
from datetime import datetime
from pathlib import Path

try:
    from codecarbon import EmissionsTracker, track_emissions
    CODECARBON_AVAILABLE = True
except ImportError:
    CODECARBON_AVAILABLE = False
    print(" CodeCarbon non installato. Installa con: pip install codecarbon")


class SWAMCarbonTracker:
    """Tracker dedicato per misurare l'impatto ambientale delle esecuzioni SWAM"""

    def __init__(self, project_name="SWAM-Project"):
        self.project_name = project_name
        self.results_dir = "results/carbon"
        self.tracker = None
        self.current_session = None

        # Crea directory per i risultati
        os.makedirs(self.results_dir, exist_ok=True)

        if CODECARBON_AVAILABLE:
            # Configurazione tracker - DISABILITIAMO IL SALVATAGGIO CSV PER EVITARE ERRORI
            self.tracker = EmissionsTracker(
                project_name=project_name,
                output_dir=self.results_dir,
                log_level="ERROR",  # Riduciamo i log
                save_to_file=False,  # DISABILITATO per evitare errore CSV
                save_to_api=False,  # Disabilitato per privacy 
                experiment_id=f"swam_session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            )
        else:
            print(" CodeCarbon non disponibile - tracking CO2 disabilitato")

    def start_tracking(self, task_name=None, language=None):
        """Inizia il tracking delle emissioni per una sessione"""
        if not CODECARBON_AVAILABLE or not self.tracker:
            return None

        session_info = {
            "task_name": task_name,
            "language": language,
            "start_time": datetime.now().isoformat(),
            "session_id": f"{task_name}_{language}_{datetime.now().strftime('%H%M%S')}" if task_name and language else None
        }

        self.current_session = session_info
        self.tracker.start()

        print(f" Tracking CO2 avviato per: {session_info.get('session_id', 'sessione generica')}")
        return session_info

    def stop_tracking(self):
        """Ferma il tracking e salva i risultati"""
        if not CODECARBON_AVAILABLE or not self.tracker:
            return None

        try:
            emissions = self.tracker.stop()
            
            # Gestisci il caso in cui emissions sia None
            if emissions is None:
                emissions = 0.0
        except Exception as e:
            print(f"⚠️  Errore durante misurazione CO2: {e}")
            emissions = 0.0

        if self.current_session:
            self.current_session["end_time"] = datetime.now().isoformat()
            self.current_session["emissions_kg"] = emissions

            # Salva dettagli sessione
            session_file = os.path.join(self.results_dir, f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json")
            with open(session_file, 'w') as f:
                json.dump(self.current_session, f, indent=2)

            # Converti in mg per leggibilità (1 kg = 1,000,000 mg)
            emissions_mg = emissions * 1_000_000
            print(f" Tracking CO2 completato: {emissions_mg:.3f} mg CO2eq")
            print(f" Dettagli salvati in: {session_file}")

        return emissions

    def track_function(self, func_name=None):
        """Decorator per tracciare automaticamente le funzioni"""
        if not CODECARBON_AVAILABLE:
            # Decorator vuoto se CodeCarbon non è disponibile
            def dummy_decorator(func):
                return func
            return dummy_decorator

        return track_emissions(
            project_name=self.project_name,
            output_dir=self.results_dir,
            experiment_id=func_name or "swam_function"
        )

    def get_summary_report(self):
        """Genera un report riassuntivo delle emissioni"""
        carbon_files = list(Path(self.results_dir).glob("*.csv"))
        session_files = list(Path(self.results_dir).glob("session_*.json"))

        summary = {
            "total_sessions": len(session_files),
            "total_carbon_files": len(carbon_files),
            "latest_emissions_file": None,
            "total_estimated_emissions": 0.0,
            "sessions_by_language": {},
            "sessions_by_task": {}
        }

        # Analizza file CSV di emissioni
        if carbon_files:
            latest_file = max(carbon_files, key=lambda x: x.stat().st_mtime)
            summary["latest_emissions_file"] = str(latest_file)

            try:
                import pandas as pd
                df = pd.read_csv(latest_file)
                if 'emissions' in df.columns:
                    summary["total_estimated_emissions"] = df['emissions'].sum()
            except:
                pass

        # Analizza sessioni dettagliate
        for session_file in session_files:
            try:
                with open(session_file) as f:
                    session = json.load(f)

                language = session.get('language', 'unknown')
                task = session.get('task_name', 'unknown')

                if language not in summary["sessions_by_language"]:
                    summary["sessions_by_language"][language] = 0
                summary["sessions_by_language"][language] += 1

                if task not in summary["sessions_by_task"]:
                    summary["sessions_by_task"][task] = 0
                summary["sessions_by_task"][task] += 1

            except:
                pass

        return summary

    def print_impact_report(self):
        """Stampa un report dell'impatto ambientale"""
        print("\n SWAM PROJECT - CARBON IMPACT REPORT")
        print("=" * 50)

        if not CODECARBON_AVAILABLE:
            print(" CodeCarbon non installato")
            print(" Installa con: pip install codecarbon")
            return

        summary = self.get_summary_report()

        print(f" Sessioni tracciate: {summary['total_sessions']}")
        print(f" File emissioni: {summary['total_carbon_files']}")
        # Converti in mg per leggibilità
        emissions_mg = summary['total_estimated_emissions'] * 1_000_000
        print(f" Emissioni totali stimate: {emissions_mg:.3f} mg CO2eq")

        if summary['sessions_by_language']:
            print(f"\n Sessioni per linguaggio:")
            for lang, count in sorted(summary['sessions_by_language'].items()):
                print(f" • {lang}: {count} sessioni")

        if summary['sessions_by_task']:
            print(f"\n Sessioni per task:")
            for task, count in sorted(summary['sessions_by_task'].items()):
                print(f" • {task}: {count} sessioni")

        if summary['latest_emissions_file']:
            print(f"\n File emissioni più recente:")
            print(f" {summary['latest_emissions_file']}")

        print(f"\n Per visualizzare i dettagli: carbonboard --filepath=\"{self.results_dir}/emissions.csv\"")


# Istanza globale per uso semplice
swam_carbon_tracker = SWAMCarbonTracker()


# Funzioni di utilità
def start_carbon_tracking(task_name=None, language=None):
    """Avvia il tracking CO2 per una sessione"""
    return swam_carbon_tracker.start_tracking(task_name, language)


def stop_carbon_tracking():
    """Ferma il tracking CO2"""
    return swam_carbon_tracker.stop_tracking()


def track_emissions_decorator(func_name=None):
    """Decorator per tracciare automaticamente le emissioni di una funzione"""
    return swam_carbon_tracker.track_function(func_name)


def print_carbon_report():
    """Stampa il report dell'impatto ambientale"""
    swam_carbon_tracker.print_impact_report()


if __name__ == "__main__":
    # Test del sistema di tracking
    print(" Test SWAM Carbon Tracker")

    # Test tracking manuale
    start_carbon_tracking("test_task", "python")

    # Simula del lavoro
    import time
    time.sleep(1)

    stop_carbon_tracking()

    # Mostra report
    print_carbon_report()