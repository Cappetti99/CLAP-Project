#!/usr/bin/env python3
"""
Carbon Tracker - Environmental impact monitoring system for CLAP
Integrates CodeCarbon to track CO2 emissions from multi-language executions
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
    print(" CodeCarbon not installed. Install with: pip install codecarbon")


class CLAPCarbonTracker:
    """Tracker dedicated to measuring the environmental impact of CLAP executions"""

    def __init__(self, project_name="CLAP-Project"):
        self.project_name = project_name
        self.results_dir = "results/carbon"
        self.tracker = None
        self.current_session = None

        # Create directory for results
        os.makedirs(self.results_dir, exist_ok=True)

        if CODECARBON_AVAILABLE:
            # Tracker configuration - DISABLE CSV SAVING TO AVOID ERRORS
            self.tracker = EmissionsTracker(
                project_name=project_name,
                output_dir=self.results_dir,
                log_level="ERROR",  # Reduce logging
                save_to_file=False,  # DISABLED to avoid CSV error
                save_to_api=False,  # Disabled for privacy
                experiment_id=f"clap_session_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
            )
        else:
            print(" CodeCarbon not available - CO2 tracking disabled")

    def start_tracking(self, task_name=None, language=None):
        """Start tracking emissions for a session"""
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

        print(f" Tracking CO2 started for: {session_info.get('session_id', 'generic session')}")
        return session_info

    def stop_tracking(self):
        """Stop tracking and save results"""
        if not CODECARBON_AVAILABLE or not self.tracker:
            return None

        try:
            emissions = self.tracker.stop()

            # Handle case where emissions is None
            if emissions is None:
                emissions = 0.0
        except Exception as e:
            print(f" Error during CO2 measurement: {e}")
            emissions = 0.0

        if self.current_session:
            self.current_session["end_time"] = datetime.now().isoformat()
            self.current_session["emissions_kg"] = emissions

            # Save session details
            session_file = os.path.join(self.results_dir, f"session_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json")
            with open(session_file, 'w') as f:
                json.dump(self.current_session, f, indent=2)

            # Convert to mg for readability (1 kg = 1,000,000 mg)
            emissions_mg = emissions * 1_000_000
            print(f" Tracking CO2 completed: {emissions_mg:.3f} mg CO2eq")
            print(f" Details saved in: {session_file}")

        return emissions

    def track_function(self, func_name=None):
        """Decorator to automatically track functions"""
        if not CODECARBON_AVAILABLE:
            # Empty decorator if CodeCarbon is not available
            def dummy_decorator(func):
                return func
            return dummy_decorator

        return track_emissions(
            project_name=self.project_name,
            output_dir=self.results_dir,
            experiment_id=func_name or "clap_function"
        )

    def get_summary_report(self):
        """Generate a summary report of emissions"""
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

        # Analyze emission CSV files
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

        # Analyze detailed sessions
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
        """Print environmental impact report"""
        print("\n CLAP PROJECT - CARBON IMPACT REPORT")
        print("=" * 50)

        if not CODECARBON_AVAILABLE:
            print(" CodeCarbon not installed")
            print(" Installa con: pip install codecarbon")
            return

        summary = self.get_summary_report()

        print(f" Tracked sessions: {summary['total_sessions']}")
        print(f" Emissions files: {summary['total_carbon_files']}")
        # Convert to mg for readability
        emissions_mg = summary['total_estimated_emissions'] * 1_000_000
        print(f" Estimated total emissions: {emissions_mg:.3f} mg CO2eq")

        if summary['sessions_by_language']:
            print(f"\n Sessions by language:")
            for lang, count in sorted(summary['sessions_by_language'].items()):
                print(f" • {lang}: {count} sessions")

        if summary['sessions_by_task']:
            print(f"\n Sessions by task:")
            for task, count in sorted(summary['sessions_by_task'].items()):
                print(f" • {task}: {count} sessions")

        if summary['latest_emissions_file']:
            print(f"\n Latest emissions file:")
            print(f" {summary['latest_emissions_file']}")

        print(f"\n To view details: carbonboard --filepath=\"{self.results_dir}/emissions.csv\"")


# Global instance for simple usage
clap_carbon_tracker = CLAPCarbonTracker()


# Utility functions
def start_carbon_tracking(task_name=None, language=None):
    """Start CO2 tracking for a session"""
    return clap_carbon_tracker.start_tracking(task_name, language)


def stop_carbon_tracking():
    """Stop CO2 tracking"""
    return clap_carbon_tracker.stop_tracking()


def track_emissions_decorator(func_name=None):
    """Decorator to automatically track emissions for a function"""
    return clap_carbon_tracker.track_function(func_name)


def print_carbon_report():
    """Print environmental impact report"""
    clap_carbon_tracker.print_impact_report()


if __name__ == "__main__":
    # Test carbon tracking system
    print(" Test CLAP Carbon Tracker")

    # Test manual tracking
    start_carbon_tracking("test_task", "python")

    # Simulate work
    import time
    time.sleep(1)

    stop_carbon_tracking()

    # Show report
    print_carbon_report()