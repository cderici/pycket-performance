import os
import re
import numpy as np
import matplotlib.pyplot as plt
import argparse

import types

"""
This is for processing and plotting runtime duration results for benchmarks
running Pycket and Racket. The benchmarks produce files containing arbitrarily many runtime durations expressed by the following triplets:

RESULT-cpu: 22676.0
RESULT-gc: 10693.0
RESULT-total: 170929.0
...

There can be some additional info printed in the files, some benchmarks produce
output that's not relevant to the runtimes (those lines are ignored).

The file names have two variations for Pycket and Racket.

- Pycket benchmark files are formatted as follows:

    (new|old)-pycket-[benchmark-name]-(with|no)-warmup.rst

    e.g. "new-pycket-ack-with-warmup.rst", "old-pycket-ack-no-warmup.rst"

 - Racket benchmark files are formatted as follows:

    racket-[benchmark-name].rst

    e.g. "racket-ack.rst"

The general regexps used are below.

The script takes a directory path and processes each file that conforms to the
file name formats (ignores other files).

It extracts the runtime for each benchmark and processes the durations (at the time of writing this, only takes the average), and plots the results using mathplotlib.
"""

BENCH_FILE_PYCKET_REGEXP = r'(new|old)-pycket?-(.*?)(?:-(with|no)-warmup)?.rst'
BENCH_FILE_RACKET_REGEXP = 'racket-(.*?).rst'

RESULT_CPU_REGEXP = r'RESULT-cpu:\s+([\d.]+)'
RESULT_GC_REGEXP = r'RESULT-gc:\s+([\d.]+)'
RESULT_TOTAL_REGEXP = r'RESULT-total:\s+([\d.]+)'

def parse_benchmark_file(file_path):
    """Extracts runtime duration values from the given file path.

    Returns:
        Three values for cpu time, gc time, and total time. The values are averages of all triplets within the file.
    """
    cpu_pattern = re.compile(RESULT_CPU_REGEXP)
    gc_pattern = re.compile(RESULT_GC_REGEXP)
    total_pattern = re.compile(RESULT_TOTAL_REGEXP)

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

def extract_benchmark_info(file_name):
    """Extract info from a given file name (see initial comment above for format info).

    Returns:
        Three values for:
            - interpreter; "new" | "old" | "racket"
            - benchmark name
            - warmup setting; bool
    """
    if match := re.match(BENCH_FILE_PYCKET_REGEXP, file_name):
        interpreter, benchmark_name, warmup = match.groups()
        return interpreter, benchmark_name, warmup == "with"
    elif match := re.match(BENCH_FILE_RACKET_REGEXP, file_name):
        return "racket", match.group(1), False

    return None, None, None

def benchmark_data_ingress(directory):
    """Main entry for processing a directory containing benchmark results.

    Returns: BenchmarkCollection
    """
    results = types.BenchmarkCollection()

    for filename in os.listdir(directory):
        if filename.endswith('.rst'):
            file_path = os.path.join(directory, filename)
            interpreter, benchmark_name, warmup = extract_benchmark_info(filename)

            if benchmark_name:
                # Parse the file and extract the average runtime values
                cpu_avg, gc_avg, total_avg = parse_benchmark_file(file_path)

                # Create a BenchmarkResult object and add it to the collection
                bResult = types.BenchmarkResult(benchmark_name, interpreter, warmup, cpu_avg, gc_avg, total_avg)

                results.add(bResult)

    return results

def plot_results(results, warmup, category, ylabel, output_file):
    """Produces and saves a png file containing plots for the given results.

    Args:
        results is a three level deep dict indexed by:
            [benchmark_name][pycket variant][category]

            - pycket variant; can be "new" or "old"

        warmup: "with" | "no"
        category: "cpu" | "gc" | "total"
    """

    # Sort benchmarks by runtime duration values in non-decreasing order

    benchmark_names = sorted(results.keys())
    new_avgs, old_avgs, racket_avgs = [], [], []

    for bench in benchmark_names:
        new_avgs.append(results[bench]["new"].get(category, 0))
        old_avgs.append(results[bench]["old"].get(category, 0))
        racket_avgs.append(results[bench]["racket"].get(category, 0))

    x = np.arange(len(benchmark_names))
    width = 0.5

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

    results = benchmark_data_ingress(args.directory)

    # Generate separate plots for each warmup type and runtime category
    for warmup in ["with", "no"]:
        if warmup in results:
            plot_results(results[warmup], warmup, "cpu", "Average CPU Time (ms)", f"../average_cpu_times_{warmup}_warmup.png")
            plot_results(results[warmup], warmup, "gc", "Average GC Time (ms)", f"../average_gc_times_{warmup}_warmup.png")
            plot_results(results[warmup], warmup, "total", "Average Total Time (ms)", f"../average_total_times_{warmup}_warmup.png")
            print(f"Plots saved for {warmup}-warmup: average_cpu_times_{warmup}_warmup.png, average_gc_times_{warmup}_warmup.png, average_total_times_{warmup}_warmup.png")

if __name__ == "__main__":
    main()
