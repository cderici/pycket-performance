import os
import re
import numpy as np
import matplotlib.pyplot as plt
import argparse

from results import BenchmarkCollection, BenchmarkResult, \
                    NEW_PYCKET, OLD_PYCKET, RACKET, \
                    CompareConfig, BenchmarkIngress

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
    parser.add_argument("--single", dest="single_benchmark_name", default=None, type=str, help="Plot only a single benchmark with all interpreters and configs to inspect warmup effects. Use \"all\" for producing plots for all benchmarks.")

    args = parser.parse_args()

    b_param = args.single_benchmark_name

    if b_param:
        args.interpreters = [NEW_PYCKET, OLD_PYCKET, RACKET]
        args.with_warmup = True
        args.no_warmup = True

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
            outfile_name += f"vs {interpreter} "
            if interpreter != RACKET:
                outfile_name += "with warmup "
            configs.append(CompareConfig(interpreter, True, args.category_type, relative))

        if args.no_warmup:
            outfile_name += f"vs {interpreter} no warmup "
            configs.append(CompareConfig(interpreter, False, args.category_type, relative))

    outfile_name += f"{args.category_type} times"

    if relative_plot:
        outfile_name += f" relative to {args.relative}"
    outfile_name = outfile_name.replace(" ", "_")
    outfile_name += ".png"

    if b_param:
        # Check the singles dir, and create if it doesn't exist
        if not os.path.exists("singles"):
            os.makedirs("singles")

        outfile_name = f"singles/{b_param}.png"
    else:
        outfile_name = outfile_name[3:]

    benchmark_collection = BenchmarkIngress(args.directory).consume_create_collection()

    if b_param == "all":
        for b_name in benchmark_collection.benchmark_names:
            benchmark_collection.plot(configs, f"singles/{b_name}.png", relative_plot, b_name)
        return

    benchmark_collection.plot(configs, outfile_name, relative_plot, args.single_benchmark_name)

if __name__ == "__main__":
    main()
