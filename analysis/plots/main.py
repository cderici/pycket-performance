import os
import re
import numpy as np
import matplotlib.pyplot as plt
import argparse

from results import BenchmarkCollection, BenchmarkResult, \
                    NEW_PYCKET, OLD_PYCKET, RACKET, \
                    CompareConfig

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
        sys = NEW_PYCKET if interpreter == "new" else OLD_PYCKET
        return sys, benchmark_name, warmup == "with"
    elif match := re.match(BENCH_FILE_RACKET_REGEXP, file_name):
        return RACKET, match.group(1), False

    return None, None, None

def benchmark_data_ingress(directory):
    """Main entry for processing a directory containing benchmark results.

    Returns: BenchmarkCollection
    """
    collection = BenchmarkCollection()

    for filename in os.listdir(directory):
        if filename.endswith('.rst'):
            file_path = os.path.join(directory, filename)
            interpreter, benchmark_name, warmup = extract_benchmark_info(filename)

            if benchmark_name:
                # Parse the file and extract the average runtime values
                cpu_avg, gc_avg, total_avg = parse_benchmark_file(file_path)

                # Create a BenchmarkResult object and add it to the collection
                bResult = BenchmarkResult(benchmark_name, interpreter, warmup, cpu_avg, gc_avg, total_avg)

                collection.add_benchmark(bResult)

    return collection

def main():
    parser = argparse.ArgumentParser(description="Process benchmark results and generate plots.")
    parser.add_argument("directory", help="Path to the directory containing benchmark result files.")

    interp_group = parser.add_argument_group()
    interp_group.add_argument("--new", dest="interpreters", action="append_const", const=NEW_PYCKET, help="Include benchmarks for New Pycket.")
    interp_group.add_argument("--old", dest="interpreters", action="append_const", const=OLD_PYCKET, help="Include benchmarks for Old Pycket.")
    interp_group.add_argument("--racket", dest="interpreters", action="append_const", const=RACKET, help="Include benchmarks for Racket.")

    warmup_group = parser.add_argument_group()
    warmup_group.add_argument("--with-warmup", dest="with_warmup", action="store_true", help="Include only benchmarks with warmup.")
    warmup_group.add_argument("--no-warmup", dest="no_warmup", action="store_true", help="Include only benchmarks without warmup.")
    parser.set_defaults(with_warmup=False, no_warmup=False)

    category_group = parser.add_mutually_exclusive_group()
    category_group.add_argument("--cpu", dest="category_type", action="store_const", const="cpu", help="Use CPU time for benchmarks.")
    category_group.add_argument("--gc", dest="category_type", action="store_const", const="gc", help="Use GC time for benchmarks.")
    category_group.add_argument("--total", dest="category_type", action="store_const", const="total", help="Use total time for benchmarks.")
    parser.set_defaults(category_type="total")

    parser.add_argument("--relative", choices=["new", "old", "racket"], help="Set the relative baseline interpreter.")

    args = parser.parse_args()

    # Check if at least one interpreter is specified
    try:
        len(args.interpreters)
    except:
        parser.error("Please specify at least one interpreter to include in the comparison.")

    # If relative is set, make sure it's one of the interpreters that are given
    relative_plot = False
    if args.relative:
        relative_plot = True
        err = False
        # This is a bit hacky, but we have do it unless we want users to type
        # "New Pycket" instead of "new" on the command line
        if "new" in args.relative and NEW_PYCKET not in args.interpreters:
            err = True
        elif "old" in args.relative and OLD_PYCKET not in args.interpreters:
            err = True
        elif "racket" in args.relative and RACKET not in args.interpreters:
            err = True

        if err:
            parser.error("The relative interpreter must be one of the interpreters that are being compared.")

    # Generate CompareConfigs based on the given arguments
    configs = []
    outfile_name = ""
    for interpreter in args.interpreters:
        relative = relative_plot and args.relative in interpreter.lower()

        if args.with_warmup:
            outfile_name += f"vs {interpreter} with warmup "
            configs.append(CompareConfig(interpreter, True, args.category_type, relative))

        if args.no_warmup:
            outfile_name += f"vs {interpreter} no warmup "
            configs.append(CompareConfig(interpreter, False, args.category_type, relative))

    """
    if args.with_warmup and args.no_warmup:
        outfile_name += f"mixed warmup"
    else:
        outfile_name += f"{'with' if args.with_warmup else 'no'} warmup"
    """

    outfile_name += f"{args.category_type} times"

    if relative_plot:
        outfile_name += f" relative to {args.relative}"
    outfile_name = outfile_name.replace(" ", "_")
    outfile_name += ".png"

    print("Collecting benchmark data...")
    benchmark_collection = benchmark_data_ingress(args.directory)

    benchmark_collection.plot(configs, outfile_name[3:], relative_plot)

if __name__ == "__main__":
    main()
