import os
import argparse

from results import *


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

    (NO)P-(WN)W-[benchmark-name].rst

    e.g. "NP-WW-ack.rst", "OP-NW-ack.rst"

 - Racket benchmark files are formatted as follows:

    R-[benchmark-name].rst

    e.g. "R-ack.rst"

The general regexps used are below.

The script takes a directory path and processes each file that conforms to the
file name formats (ignores other files).

It extracts the runtime for each benchmark and processes the durations (at the time of writing this, only takes the average), and plots the results using mathplotlib.
"""

UNSPECIFIED = "unspecified"

INTERP_HUMAN_TO_INTERNAL = {
    "new-with-warmup"   : NP_WW,
    "new-no-warmup"     : NP_NW,
    "old-with-warmup"   : OP_WW,
    "old-no-warmup"     : OP_NW,
    "racket"            : R,
}

def _pick_sort_interp(user_selected_interps):
    priority = [NP_WW, NP_NW, OP_WW, OP_NW, R]

    for i in priority:
        if i in user_selected_interps:
            return i

    raise Exception(f"Unable to pick a sort interp out of {user_selected_interps}")

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

    parser.add_argument("--sort", default=UNSPECIFIED, choices=["new-with-warmup", "new-no-warmup", "old-with-warmup", "old-no-warmup", "racket"], help="Set the relative baseline interpreter.")

    parser.add_argument("--single", dest="single_benchmark_name", default=None, type=str, help="Plot only a single benchmark with all interpreters and configs to inspect warmup effects. Use \"all\" for producing plots for all benchmarks.")

    parser.add_argument("--label", dest="run_label", default="", type=str, help="label for experiment, e.g. 4th-run to show in plots")

    args = parser.parse_args()

    b_param = args.single_benchmark_name

    # Check if at least one interpreter is specified
    try:
        len(args.interpreters)
    except:
        parser.error("Please specify at least one interpreter to include in the comparison.")

    # Determine which interpreter settings user wants to see, e.g. NP_WW
    user_selected_interps = []
    for user_param_interp in args.interpreters:
        if user_param_interp == RACKET:
            user_selected_interps.append(R)
        elif user_param_interp == NEW_PYCKET:
            if args.with_warmup:
                user_selected_interps.append(NP_WW)
            if args.no_warmup:
                user_selected_interps.append(NP_NW)
        elif user_param_interp == OLD_PYCKET:
            if args.with_warmup:
                user_selected_interps.append(OP_WW)
            if args.no_warmup:
                user_selected_interps.append(OP_NW)
        else:
            raise Exception(f"Unrecognized interpreter selected: {user_param_interp}")

    # Generate CompareConfigs for selected interpreter settings
    configs = set()
    outfile_name = ""
    for selected_interp in user_selected_interps:
        config = CONFIG_SELECT[selected_interp](args.category_type)
        configs.add(config)
        outfile_name += f"vs {config.interp} "

    sort_interp = None
    if not b_param: # multi-plot
        # We'll pick a sort interp if nothing is specified
        if args.sort == UNSPECIFIED:
            sort_interp = _pick_sort_interp(user_selected_interps)
        else:
            sort_interp = INTERP_HUMAN_TO_INTERNAL[args.sort]

    relative_interp = INTERP_HUMAN_TO_INTERNAL[args.relative] if args.relative else None

    if relative_interp and relative_interp not in user_selected_interps:
        raise Exception(f"Selected relative interp {relative_interp} is not given as one of compare interpreters.")

    if relative_interp:
        outfile_name += f"relative to {relative_interp} "

    outfile_name += f"{args.category_type} times"
    outfile_name = outfile_name.replace(" ", "_")
    outfile_name = outfile_name[3:]
    outfile_name = f"{args.run_label}_{outfile_name}"

    if b_param:
        # Check the singles dir, and create if it doesn't exist
        if not os.path.exists("singles"):
            os.makedirs("singles")

    HARDCODED_EXCLUDES = ["sumrec"]

    # Ingest the data from the given directory
    benchmark_collection = BenchmarkIngress(args.directory, excluded_benchmarks=HARDCODED_EXCLUDES).consume_create_collection()

    # Generate PlotConfig(s)
    plot_configs = []
    benchmark_names = []
    if not b_param:
        # single (multi) plot config with benchmark_names = everything we collected from the given directory
        filename = f"{outfile_name}.png"
        benchmark_names = list(benchmark_collection.benchmark_names)
        plot_configs.append(PlotConfig(filename, sort_interp, relative_interp, False, configs, benchmark_names, args.run_label))
    else:
        # possibly multiple (e.g. "all") single plot configs
        benchmark_names = list(benchmark_collection.benchmark_names) if b_param == "all" else [b_param]
        for b_name in benchmark_names:
            filename = f"singles/{outfile_name}_{b_name}.png"
            plot_configs.append(PlotConfig(filename, sort_interp, relative_interp, True, configs, [b_name], args.run_label))

    benchmark_collection.generate_plots(plot_configs)

if __name__ == "__main__":
    main()
