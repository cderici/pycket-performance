import os
import re
import numpy as np
import matplotlib.pyplot as plt
import argparse

def parse_benchmark_file(file_path):
    cpu_pattern = re.compile(r'RESULT-cpu:\s+([\d.]+)')
    gc_pattern = re.compile(r'RESULT-gc:\s+([\d.]+)')
    total_pattern = re.compile(r'RESULT-total:\s+([\d.]+)')

    cpu_times, gc_times, total_times = [], [], []

    with open(file_path, 'r') as f:
        for line in f:
            if cpu_match := cpu_pattern.search(line):
                cpu_times.append(float(cpu_match.group(1)))
            elif gc_match := gc_pattern.search(line):
                gc_times.append(float(gc_match.group(1)))
            elif total_match := total_pattern.search(line):
                total_times.append(float(total_match.group(1)))

    cpu_avg = np.mean(cpu_times) if cpu_times else 0
    gc_avg = np.mean(gc_times) if gc_times else 0
    total_avg = np.mean(total_times) if total_times else 0

    return cpu_avg, gc_avg, total_avg

def extract_benchmark_info(filename):
    match = re.match(r'(new|old|racket)-pycket?-(.*?)(?:-(with|no)-warmup)?.rst', filename)
    if match:
        variant, benchmark_name, warmup = match.groups()
        warmup = warmup if warmup else "with"  # Default to "with" if warmup info is missing
        return variant, benchmark_name, warmup
    return None, None, None

def process_directory(directory):
    results = {"with": {}, "no": {}}

    for filename in os.listdir(directory):
        if filename.endswith('.rst'):
            file_path = os.path.join(directory, filename)
            variant, benchmark_name, warmup = extract_benchmark_info(filename)
            if benchmark_name:
                cpu_avg, gc_avg, total_avg = parse_benchmark_file(file_path)
                if benchmark_name not in results[warmup]:
                    results[warmup][benchmark_name] = {"new": {}, "old": {}, "racket": {}}

                # Store the averages for the specific variant
                results[warmup][benchmark_name][variant] = {
                    "cpu": cpu_avg,
                    "gc": gc_avg,
                    "total": total_avg
                }

    return results

def plot_results(results, warmup, category, ylabel, output_file):
    benchmark_names = sorted(results.keys())
    new_avgs, old_avgs, racket_avgs = [], [], []

    for bench in benchmark_names:
        new_avgs.append(results[bench]["new"].get(category, 0))
        old_avgs.append(results[bench]["old"].get(category, 0))
        racket_avgs.append(results[bench]["racket"].get(category, 0))

    x = np.arange(len(benchmark_names))
    width = 0.2

    plt.figure(figsize=(12, 8))
    plt.bar(x - width, new_avgs, width, label="New", color="red")
    plt.bar(x, old_avgs, width, label="Old", color="blue")
    plt.bar(x + width, racket_avgs, width, label="Racket", color="magenta")

    plt.xlabel("Benchmark")
    plt.ylabel(ylabel)
    plt.title(f"Average {category.upper()} Time per Benchmark ({warmup}-warmup)")
    plt.xticks(x, benchmark_names, rotation=45, ha="right")
    plt.legend()
    plt.tight_layout()
    plt.savefig(output_file)
    plt.close()

def main():
    parser = argparse.ArgumentParser(description="Process benchmark results and generate plots.")
    parser.add_argument("directory", help="Path to the directory containing benchmark result files.")
    args = parser.parse_args()

    results = process_directory(args.directory)

    # Generate separate plots for each warmup type and runtime category
    for warmup in ["with", "no"]:
        if warmup in results:
            plot_results(results[warmup], warmup, "cpu", "Average CPU Time (ms)", f"../average_cpu_times_{warmup}_warmup.png")
            plot_results(results[warmup], warmup, "gc", "Average GC Time (ms)", f"../average_gc_times_{warmup}_warmup.png")
            plot_results(results[warmup], warmup, "total", "Average Total Time (ms)", f"../average_total_times_{warmup}_warmup.png")
            print(f"Plots saved for {warmup}-warmup: average_cpu_times_{warmup}_warmup.png, average_gc_times_{warmup}_warmup.png, average_total_times_{warmup}_warmup.png")

if __name__ == "__main__":
    main()
