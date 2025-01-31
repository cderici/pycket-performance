import os
import argparse

from results import NEW_PYCKET, OLD_PYCKET, RACKET, \
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

    parser.add_argument("--relative", choices=["new-with-warmup", "new-no-warmup", "old-with-warmup", "old-no-warmup", "racket"], help="Set the relative baseline interpreter.")
    parser.add_argument("--single", dest="single_benchmark_name", default=None, type=str, help="Plot only a single benchmark with all interpreters and configs to inspect warmup effects. Use \"all\" for producing plots for all benchmarks.")

    args = parser.parse_args()

    b_param = args.single_benchmark_name

    # Check if at least one interpreter is specified
    try:
        len(args.interpreters)
    except:
        parser.error("Please specify at least one interpreter to include in the comparison.")

    # If relative is set, make sure it's one of the interpreters that are given
    relative_plot = False
    relative_interpreter = None
    if args.relative:
        relative_plot = True
        err = False
        # This is a bit hacky, but we have do it unless we want users to type
        # "New Pycket" instead of "new" on the command line
        if "new" in args.relative:
            relative_interpreter = NEW_PYCKET
            if NEW_PYCKET not in args.interpreters:
                err = True
        elif "old" in args.relative:
            relative_interpreter = OLD_PYCKET
            if OLD_PYCKET not in args.interpreters:
                err = True
        elif "racket" in args.relative:
            relative_interpreter = RACKET
            if RACKET not in args.interpreters:
                err = True

        if err:
            parser.error("The relative interpreter must be one of the interpreters that are being compared.")

    # Generate CompareConfigs based on the given arguments
    rel_config = None
    configs = []
    outfile_name = ""
    for interpreter in args.interpreters:
        # Handle racket separately
        if interpreter == RACKET:
            continue

        # With warmup and no-warmup are not mutually exclusive. They can be both
        # set, in which case we will plot both configurations.
        if args.with_warmup:
            outfile_name += f"vs {interpreter}WW "
            relative = relative_plot and interpreter == relative_interpreter and "with-warmup" in args.relative
            c  = CompareConfig(interpreter, True, args.category_type, relative)
            if relative:
                rel_config = c
            else:
                configs.append(c)

        if args.no_warmup:
            outfile_name += f"vs {interpreter}NW "
            relative = relative_plot and interpreter == relative_interpreter and "no-warmup" in args.relative
            c = CompareConfig(interpreter, False, args.category_type, relative)
            if relative:
                rel_config = c
            else:
                configs.append(c)

    # Handle racket separately (because of warmup stuff)
    if RACKET in args.interpreters:
        outfile_name += f"vs {RACKET} "
        relative = relative_plot and RACKET == relative_interpreter and "racket" in args.relative
        c = CompareConfig(RACKET, True, args.category_type, relative)
        if relative:
            rel_config = c
        else:
            configs.append(c)

    outfile_name += f"{args.category_type} times"

    if relative_plot:
        outfile_name += f" relative to {args.relative}"

    outfile_name = outfile_name.replace(" ", "_")
    outfile_name = outfile_name[3:]

    if b_param:
        # Check the singles dir, and create if it doesn't exist
        if not os.path.exists("singles"):
            os.makedirs("singles")
    else:
        outfile_name += ".png"

    HARDCODED_EXCLUDES = ["sumrec"]

    benchmark_collection = BenchmarkIngress(args.directory, excluded_benchmarks=HARDCODED_EXCLUDES).consume_create_collection()

    if b_param == "all":
        for b_name in benchmark_collection.benchmark_names:
            filename = f"singles/{outfile_name}_{b_name}.png"
            benchmark_collection.plot(configs, filename, relative_plot, b_name)
        return

    benchmark_collection.plot(configs, outfile_name, rel_config, args.single_benchmark_name)

if __name__ == "__main__":
    main()
