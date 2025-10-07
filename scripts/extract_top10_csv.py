#!/usr/bin/env python3
"""
Script to extract only the TOP10 tasks from the complete CSV file
"""

import json
import csv
import os

def load_top10_tasks():
    """Load the names of the TOP10 tasks from the common_tasks.json file"""

    common_tasks_file = "results/task_analysis/common_tasks.json"
    
    if not os.path.exists(common_tasks_file):
        print(f"❌ File not found: {common_tasks_file}")
        return None
    
    with open(common_tasks_file, 'r') as f:
        data = json.load(f)

    # Extract task names from the "common_tasks" section
    top10_tasks = []
    for task in data.get('common_tasks', []):
        task_name = task['name']
        # Normalize the name for matching with the CSV
        normalized_name = task_name.replace('_', ' ').replace('-', ' ')
        normalized_name = ' '.join(word.capitalize() for word in normalized_name.split())
        top10_tasks.append(normalized_name)

    print(f" TOP10 tasks found:")
    for i, task in enumerate(top10_tasks, 1):
        print(f"  {i:2d}. {task}")
    
    return top10_tasks

def extract_top10_csv(input_csv, output_csv, top10_tasks):
    """Extract only the TOP10 tasks from the complete CSV"""
    
    if not os.path.exists(input_csv):
        print(f"❌ CSV file not found: {input_csv}")
        return False

    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_csv), exist_ok=True)
    
    extracted_count = 0
    total_tasks = len(top10_tasks)
    
    with open(input_csv, 'r', encoding='utf-8') as infile, \
         open(output_csv, 'w', newline='', encoding='utf-8') as outfile:
        
        reader = csv.DictReader(infile)
        writer = csv.DictWriter(outfile, fieldnames=reader.fieldnames)
        writer.writeheader()
        
        for row in reader:
            task_name = row['task']

            # Normalize the task name (replace underscores with spaces)
            normalized_task_name = task_name.replace('_', ' ')

            # Check if this task is in the TOP10 (case-insensitive comparison)
            if any(normalized_task_name.lower() == top10_task.lower() for top10_task in top10_tasks):
                writer.writerow(row)
                extracted_count += 1
                print(f"✅ Extracted: {task_name} -> {normalized_task_name}")

    print(f"\n EXTRACTION COMPLETED:")
    print(f"   Tasks extracted: {extracted_count}/{total_tasks}")
    print(f"   File created: {output_csv}")

    if extracted_count < total_tasks:
        csv_tasks = {row['task'].replace('_', ' ').lower() for row in csv.DictReader(open(input_csv))}
        top10_lower = {task.lower() for task in top10_tasks}
        missing = top10_lower - csv_tasks
        # Convert missing names back to original format for display
        missing = {task for task in top10_tasks if task.lower() in missing}
        print(f"\n  Tasks not found in CSV:")
        for task in missing:
            print(f"     - {task}")
    
    return extracted_count > 0

def add_benchmark_results(top10_csv, benchmark_json):
    """Add benchmark results to the TOP10 CSV"""
    
    if not os.path.exists(benchmark_json):
        print(f"  Benchmark file not found: {benchmark_json}")
        return
    
    with open(benchmark_json, 'r') as f:
        benchmark_data = json.load(f)

    # Read the existing CSV
    with open(top10_csv, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        rows = list(reader)
        fieldnames = reader.fieldnames

    # Add new fields
    new_fieldnames = list(fieldnames) + [
        'benchmark_tested',
        'successful_languages',
        'avg_emissions_mg_co2eq',
        'fastest_language',
        'most_efficient_language'
    ]

    # Update each row with benchmark data
    for row in rows:
        task_name = row['task']

        # Mapping of task names between CSV and benchmark
        name_mapping = {
            'Balanced Brackets': 'Balanced_brackets',
            'Loop Over Multiple Arrays Simultaneously': 'Loop_over_multiple_arrays_simultaneously', 
            'Fractal Tree': 'Fractal_tree',
            'Greatest Common Divisor': 'Greatest_common_divisor',
            'Gray Code': 'Gray_code',
            'Function Composition': 'Function_composition',
            'Sorting Algorithmsquicksort': 'Sorting_algorithmsQuicksort',
            'Pascals Triangle': 'Pascals_triangle',
            'Identity Matrix': 'Identity_matrix',
            'Palindrome Detection': 'Palindrome_detection'
        }
        
        benchmark_task_name = name_mapping.get(task_name, task_name.replace(' ', '_'))
        
        if benchmark_task_name in benchmark_data:
            task_data = benchmark_data[benchmark_task_name]

            # Calculate statistics
            successful_langs = []
            emissions_data = []
            time_data = []
            
            for lang, stats in task_data.items():
                if stats.get('success_rate', 0) == 100.0:
                    successful_langs.append(lang)
                    emissions_data.append((lang, stats['mean_emissions']))
                    time_data.append((lang, stats['mean_execution_time']))

            # Find the fastest and most efficient
            fastest_lang = min(time_data, key=lambda x: x[1])[0] if time_data else "N/A"
            most_efficient_lang = min(emissions_data, key=lambda x: x[1])[0] if emissions_data else "N/A"
            avg_emissions = sum(x[1] for x in emissions_data) / len(emissions_data) if emissions_data else 0
            
            row.update({
                'benchmark_tested': 'Yes',
                'successful_languages': ','.join(successful_langs),
                'avg_emissions_mg_co2eq': f"{avg_emissions * 1000:.3f}",
                'fastest_language': fastest_lang,
                'most_efficient_language': most_efficient_lang
            })
        else:
            row.update({
                'benchmark_tested': 'No',
                'successful_languages': '',
                'avg_emissions_mg_co2eq': '',
                'fastest_language': '',
                'most_efficient_language': ''
            })

    # Rewrite the file with the new data
    with open(top10_csv, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=new_fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"✅ Benchmark data added to CSV")

def main():
    """Main function"""

    print(" EXTRACTION OF TOP10 TASK CSV")
    print("=" * 50)
    
    # 1. Load the TOP10 tasks
    top10_tasks = load_top10_tasks()
    if not top10_tasks:
        return

    # 2. Extract from the complete CSV
    # Find the most recent carbon_benchmark_summary file
    import glob
    csv_files = glob.glob("results/csv/carbon_benchmark_summary_*.csv")
    if not csv_files:
        print("❌ No carbon_benchmark_summary file found!")
        return

    input_csv = max(csv_files)  # Take the most recent
    output_csv = "results/csv/top10_tasks_analysis.csv"
    
    success = extract_top10_csv(input_csv, output_csv, top10_tasks)
    if not success:
        return

    # 3. Add benchmark results if available
    benchmark_json = "results/carbon_benchmark/carbon_benchmark_summary_20250923_174626.json"
    if os.path.exists(benchmark_json):
        print(f"\n Adding benchmark results...")
        add_benchmark_results(output_csv, benchmark_json)

    print(f"\n TOP10 CSV created successfully!")
    print(f" File: {output_csv}")

    # Show preview
    print(f"\n PREVIEW:")
    with open(output_csv, 'r') as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            if i < 5:
                print(f"{row['task']:<30} | {row['language']:<10} | CO2: {row['avg_co2_mg']:<8} mg | Time: {row['avg_time_s']:<6} s")

if __name__ == "__main__":
    main()
