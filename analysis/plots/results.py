import os, re
import numpy as np
import matplotlib.pyplot as plt
from numpy.core.fromnumeric import std
import scipy.stats as stats

from pathlib import Path
from scipy.interpolate import make_interp_spline
from random import uniform as ru
from functools import partial

plt.style.use('fivethirtyeight')

# Interpreter Identifiers
NP_WW = "New Pycket With Warmup"
NP_NW = "New Pycket No Warmup"
OP_WW = "Old Pycket With Warmup"
OP_NW = "Old Pycket No Warmup"
R     = "Racket"

VALID_INTERPRETERS = [NP_WW, NP_NW, OP_WW, OP_NW, R]

NEW_PYCKET = "NP"
OLD_PYCKET = "OP"
RACKET = "R"

BENCH_FILE_PYCKET_REGEXP = r'(N|O)P-(W|N)W-(.*?).rst'
BENCH_FILE_RACKET_REGEXP = r'R-(.*?).rst'

RESULT_CPU_REGEXP = r'RESULT-cpu:\s+([\d.]+)'
RESULT_GC_REGEXP = r'RESULT-gc:\s+([\d.]+)'
RESULT_TOTAL_REGEXP = r'RESULT-total:\s+([\d.]+)'

class BenchmarkIngress:
    """Processes the benchmark file and extracts the runtime durations from it.
    """
    def __init__(self, directory_path, excluded_benchmarks=[]):
        """
        Args:
            directory_path: str
        """
        self.directory = directory_path
        self.excluded_benchmarks = excluded_benchmarks

    def _parse_and_extract(self, file_path):
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
                elif "exn:fail" in line:
                    print(f"WARNING: File {file_path} contains an error: {line}")

        return cpu_times, gc_times, total_times

    def _analyze_filename(self, file_name):
        """Extract info from a given file name (see initial comment above for format info).

        Returns:
            Three values for:
                - interpreter; NEW_PYCKET | OLD_PYCKET | RACKET
                - benchmark name
                - warmup setting; bool
        """
        if match := re.match(BENCH_FILE_PYCKET_REGEXP, file_name):
            _interpreter, warmup, benchmark_name = match.groups()
            if _interpreter == 'N' and warmup == 'W':
                return NP_WW, benchmark_name
            elif _interpreter == 'N' and warmup == 'N':
                return NP_NW, benchmark_name
            elif _interpreter == 'O' and warmup == 'W':
                return OP_WW, benchmark_name
            elif _interpreter == 'O' and warmup == 'N':
                return OP_NW, benchmark_name
            else:
                raise Exception(f"Unrecognized format in file_name: {file_name}")

        elif match := re.match(BENCH_FILE_RACKET_REGEXP, file_name):
            return R, match.group(1)

        return None, None

    def consume_create_collection(self):
        """Consumes the benchmark data from the directory path and produces a BenchmarkCollection object.

        Main entry for processing a directory containing benchmark results.

        Returns:
            BenchmarkCollection
        """
        print("Collecting benchmark data...")
        collection = BenchmarkCollection()

        for filename in os.listdir(self.directory):
            if filename.endswith('.rst'):
                file_path = os.path.join(self.directory, filename)
                interp, benchmark_name = self._analyze_filename(filename)
                if benchmark_name in self.excluded_benchmarks:
                    print(f"Excluding benchmark {benchmark_name}")
                    continue

                if benchmark_name:
                    # Parse the file and extract the average runtime values
                    cpu_times, gc_times, total_times = self._parse_and_extract(file_path)

                    # Create a BenchmarkResult object and add it to the collection
                    bResult = BenchmarkResult(benchmark_name, interp, cpu_times, gc_times, total_times)

                    collection.add_benchmark(bResult)

        return collection

class Result:
    def __init__(self, values):
        np_arr = np.array(values)

        self.all_values = np_arr
        self.raw_values = values
        self.best = np.min(np_arr)
        self.mean = np.mean(np_arr)
        self.std_dev = np.std(np_arr, ddof=1)
        self.sample_size = len(np_arr)
        t_score = stats.t.ppf(0.975, df=self.sample_size-1) # 95% confidence interval
        self.ci_half_range = t_score * (self.std_dev / np.sqrt(self.sample_size))

class BenchmarkResult:
    """Keeps a record of the results of a benchmark run for each category (CPU, GC, Total).
    """
    def __init__(self, benchmark_name, interp, cpu_values, gc_values, total_values):
        """
        Args:
            benchmark_name: str
            interpreter: str, "New Pycket" | "Old Pycket" | "Racket"
            with_warmup: bool
            value: float
        """
        self.benchmark_name = benchmark_name
        self.interp = interp
        self.cpu_values = cpu_values
        self.gc_values = gc_values
        self.total_values = total_values

        self.cpu_result = Result(cpu_values)
        self.gc_result = Result(gc_values)
        self.total_result = Result(total_values)

    def get_result(self, category):
        """Returns the Result object for the given category.

        Args:
            category: str, "cpu" | "gc" | "total"

        Returns:
            list of float
        """
        if category == "cpu":
            return self.cpu_result
        elif category == "gc":
            return self.gc_result
        elif category == "total":
            return self.total_result

        raise ValueError(f"Invalid category: {category}")

    def get_series(self, category):
        """Returns the series for the given category.

        Args:
            category: str, "cpu" | "gc" | "total"

        Returns:
            list of float
        """
        if category == "cpu":
            return self.cpu_values
        elif category == "gc":
            return self.gc_values
        elif category == "total":
            return self.total_values

        raise ValueError(f"Invalid category: {category}")

    def get_single_value(self, category, is_with_warmup=False):
        """Returns the value for the given category, caches if not computed yet.

        If warmup is enabled, we'll use the average of the fastest 10 runs.

        Args:
            category: str, "cpu" | "gc" | "total"

        Returns:
            float
        """
        if category == "cpu" and self.cpu_result.best:
            return self.cpu_result.best
        elif category == "gc" and self.gc_result.best:
            return self.gc_result.best
        elif category == "total" and self.total_result.best:
            return self.total_result.best

        raise ValueError(f"Invalid category: {category}")

    def __str__(self):
        return f"{self.interp} {self.benchmark_name} : CPU {self.cpu_result.best}, GC {self.gc_result.best}, Total {self.total_result.best}"

class CompareConfig():
    def __init__(self, interp, with_warmup, category="total"):
        """
        Args:
            interp: str (NP_WW, NP_NW, ...)
            with_warmup: bool
            category: str, can be "cpu", "gc", "total"
                (default: "total")
        """
        self.interp = interp
        self.with_warmup = with_warmup
        self.category = category
    
    @staticmethod
    def make(interp, with_warmup, category="total"):
        return CompareConfig(interp, with_warmup, category)

    def __str__(self):
        return f"{self.interp} {self.category} time"

# Pre-built configuration objects
NP_WW_Config = partial(CompareConfig.make, NP_WW, True)
NP_NW_Config = partial(CompareConfig.make, NP_NW, False)
OP_WW_Config = partial(CompareConfig.make, OP_WW, True)
OP_NW_Config = partial(CompareConfig.make, OP_NW, False)
R_Config     = partial(CompareConfig.make, R, True)

CONFIG_SELECT = {
    NP_WW: NP_WW_Config,
    NP_NW: NP_NW_Config,
    OP_WW: OP_WW_Config,
    OP_NW: OP_NW_Config,
    R: R_Config,
}

# A config object for precisely one plot file
class PlotConfig:
    def __init__(self,
                 output_file_name,
                 sort_interp=None,
                 relative_interp=None,
                 is_single=False,
                 compare_configs={},
                 caption=""):
        # A single plot is we compare multiple interpreters on a single benchmark
        # Single plots are graph plots (whereas non-single ones are bar charts)
        self.is_single = is_single

        self.output_file_name = output_file_name
        self.caption = caption

        # Validate interpreters
        for i in compare_configs:
            assert i.interp in VALID_INTERPRETERS, f"{i} is not a valid. Valid interpreters are : {VALID_INTERPRETERS}"

        self.compare_configs = compare_configs
        self.sort_interp = sort_interp
        self.relative_interp = relative_interp

    def plot_single(self, benchmark_name, benchmark_results):
        """
        self.benchmark_names
        self.compare_configs

        Given benchmark_results are already filtered to contain only the requested
        interpreters and benchmarks.

        benchmark_results
            b_results
                {
                    NP_WW: {
                                ack: Result
                                tail: Result
                                ...
                            },
                    OP_NW: {
                                ack: Result
                                ...
                            },
                    ...
                }

        """
        self._plot_preamble()

        plt.xlabel("Iterations")

        for interp, result_dict in benchmark_results.items():
            if self.relative_interp:
                # Add a horizontal line for relative interp baseline (normalized to 1)
                plt.axhline(y=1, color="magenta", linewidth=2, linestyle="-", label=self.relative_interp)
                continue

            # Sanity check to make sure we have the result of the correct benchmark.
            if benchmark_name not in result_dict:
                raise Exception(f"{benchmark_name} not in result_dict {result_dict}. See collect_benchmark_results method.")

            result = result_dict[benchmark_name]

            x = np.arange(result.sample_size)

            y = result.all_values # Already an np.array

            # Generate smooth x values for interpolation
            x_smooth = np.linspace(x.min(), x.max(), 300)

            # Create a smooth spline curve for y over x
            y_smooth = make_interp_spline(x, y, k=3)(x_smooth)

            # Plot the smooth curve
            plt.plot(x_smooth, y_smooth, label=interp, linewidth=2)

        plt.grid(True)
        self._plot_postamble()

    def _compute_sorted_benchmark_names(self, benchmark_results):
        """
            Get the benchmark names in order where the results of 
            the self.sort_interp for those benchmarks are ascending 
            order.
        """
        s_interp_results = benchmark_results[self.sort_interp] # {bname: Result}
    
        return [bname for bname, _ in sorted(s_interp_results.items(), key=lambda x: x[1].best)]


    def _plot_preamble(self):
        plt.figure(figsize=(12, 8))
        # Prepare a caption using output_file
        caption = Path(self.output_file_name.replace("_", " ")).stem
        plt.title(f"{self.caption} : {caption}")
        plt.ylabel("Runtime (ms)")

    def _plot_postamble(self):
        plt.legend()
        plt.tight_layout()
        print(f"Saving plot to {self.output_file_name}")
        plt.savefig(self.output_file_name)
        plt.close()

    def plot_multi(self, benchmark_names, benchmark_results):
        """
        self.benchmark_names
        self.compare_configs

        Given benchmark_results are already filtered to contain only the requested
        interpreters and benchmarks.

        benchmark_results
            b_results
                {
                    NP_WW: {
                                ack: Result
                                tail: Result
                                ...
                            },
                    OP_NW: {
                                ack: Result
                                ...
                            },
                    ...
                }

        """
        self._plot_preamble()

        colors = {
            f"{NP_WW} With Warmup": "#0c590c",
            f"{NP_NW} No Warmup": "#5ae8b8",
            f"{OP_WW} With Warmup": "#941616",
            f"{OP_NW} No Warmup": "#f77474",
            f"{R} With Warmup": "#4558e6"
        }

        # Multi will always be sorted based on an interp (e.g., NP_WW)
        # Get the benchmark names that are sorted based on the 
        # result of the interp used (e.g. OP_NW)
        sorted_benchmark_names = self._compute_sorted_benchmark_names(benchmark_results)

        x = np.arange(len(sorted_benchmark_names))
        width = 0.15
        group_gap = 0.2

        for i, (interp, results) in enumerate(benchmark_results.items()):
            if self.relative_interp:
                # For relative interp itself, just plot a horizontal line
                # and skip
                plt.axhline(y=1, color="magenta", linewidth=2, linestyle="-", label=self.relative_interp)
                continue

            # Get the results of interp for sorted benchmark_results
            results_for_interp = []
            for benchmark_name in sorted_benchmark_names:
                results_for_interp.append(results[benchmark_name])

            y_values_for_interp = [r.mean for r in results_for_interp]
            confidence_for_interp = [r.ci_half_range for r in results_for_interp]

            if self.relative_interp:
                # Prepare values for computing relative values
                # relative_interp_values: np.array
                relative_interp_values = benchmark_results[self.relative_interp][benchmark_name].all_values

                # Piecewise divide with relative interp values to compute relative values
                y_values_for_interp /= relative_interp_values

            label = interp
            color = colors[label]

            plt.bar(x + (i * (width + (group_gap * (i // len(sorted_benchmark_names))))),
                    y_values_for_interp, yerr=confidence_for_interp, width=width, label=label, color=color)

        plt.xticks(x + width, benchmark_names, rotation=45, ha="right")
        self._plot_postamble()

    def plot(self, benchmark_names, benchmark_results):
        if self.is_single:

            # Check if benchmark_names contain only one benchmark if this is a single plot
            assert len(benchmark_names) == 1, f"multiple benchmarks are given for a \"single\" plot: {benchmark_names}"

            return self.plot_single(benchmark_names[0], benchmark_results)

        return self.plot_multi(benchmark_names, benchmark_results)

class BenchmarkCollection():
    """Keeps a collection of BenchmarkResult objects, and knows how to sort, process, and analyse them.
    """
    def __init__(self):
        """
            benchmark_results_dict
                {
                    NP_WW: {
                                ack: BenchmarkResult
                                tail: BenchmarkResult
                                ...
                            },
                    OP_NW: {
                                ack: BenchmarkResult
                                ...
                            },
                    ...
                }
        """
        self.benchmark_results_dict = {}
        self.benchmark_names = set()

    def add_benchmark(self, b_result):
        """
        Args:
            benchmark: BenchmarkResult
        """
        if b_result.interp in self.benchmark_results_dict:
            self.benchmark_results_dict[b_result.interp][b_result.benchmark_name] = b_result
        else:
            self.benchmark_results_dict[b_result.interp] = {b_result.benchmark_name: b_result}

        self.benchmark_names.add(b_result.benchmark_name)

    # TODO: remove
    def _get_b_label(self, interpreter, benchmark_name, with_warmup):
        if interpreter == RACKET:
            return f"{interpreter}_{benchmark_name}"
        return f"{interpreter}_{benchmark_name}_{with_warmup}"

    def _get_y_label(self, config):
        return f"{config.interpreter} {'With' if config.with_warmup else 'No'} Warmup"

    def _pick_sort_config(self, configs):
        """Pick the configuration based on priority, which is new pycket, old pycket, racket.
        If there's multiple of the same pycket variant, pick the first one.

        Used by _compare to sort the benchmarks based on the selected configuration.
        (e.g. I wanna see the New Pycket results in ascending order in the plot,
        sort the benchmarks based on the New Pycket results)

        One of the configurations might be a relative config (meaning the others will be
        computed relative to that one). In that case, the values for that congiuration will
        be set to 1, so don't pick that one as the sort configuration.

        Args:
            configs: list of CompareConfig

        Returns:
            sort_config: CompareConfig
            remaining_configs: list of CompareConfig
        """

        if len(configs) == 0:
            raise ValueError("No configurations to compare.")

        PRIORITY = [NEW_PYCKET, OLD_PYCKET, RACKET]

        # Current focus is on the no warmup ones.
        PRIORITY = [
            {"interp": NEW_PYCKET, "warmup": False},
            {"interp": NEW_PYCKET, "warmup": True},
            {"interp": OLD_PYCKET, "warmup": False},
            {"interp": OLD_PYCKET, "warmup": True},
            {"interp": RACKET, "warmup": True}
        ]

        for p in PRIORITY:
            for c in configs:
                if c.interpreter == p["interp"] and c.with_warmup == p["warmup"] and not c.relative:
                    return c, [c for c in configs if c != c]
        raise ValueError("No appropriate configurations found.")

    def _filter_benchmarks_for_config(self, config):
        """Filters the benchmarks for the given configuration.

        Args:
            config: CompareConfig

        Returns:
            list of BenchmarkResult
        """
        filtered_benchmarks = []
        for _, b in self.benchmark_results_dict.items():
            if b.interpreter == config.interpreter and b.with_warmup == config.with_warmup:
                filtered_benchmarks.append(b)
        return filtered_benchmarks

    def _sort_y_values(self, benchmark_names, y_values, sort_config):
        """Sorts the y-values (and the benchmark_names) based on the sort_config for the plot.

        Args:
            benchmark_names: list of str
            y_values: dict
                    {
                        label: str (config.interpreter),
                        values: list of float
                    }
            sort_config: CompareConfig
        Returns:
            sorted_benchmark_names: list of str
            sorted_y_values: dict
                    {
                        label: str (config.interpreter),
                        values: list of float
                    }
        """
        # Pick the y-values for the sort config, which will determine the order
        # that all y-values will use to be sorted.
        sort_values = y_values[self._get_y_label(sort_config)]
        sorted_indices = sorted(range(len(sort_values)), key=lambda x: sort_values[x])

        # sort the y-values for each config based on the sort indices (that are
        # computed based on the sort config)
        # TODO (cderici): It's not ideal to sort the y-values for the sort
        # config again
        for interp in y_values:
            y_values[interp] = [y_values[interp][i] for i in sorted_indices]

        # Sort the benchmark names based on the sort indices
        sorted_benchmark_names = [benchmark_names[i] for i in sorted_indices]

        return sorted_benchmark_names, y_values

    def _construct_y_values(self, benchmark_names, configs, rel_config=None):
        """Constructs the y-values for the given configurations.

        If any of the given configs is marked as a relative config, then the
        y-values for the other configs will be computed relative to that one.

        Args:
            sorted_benchmark_names: list of str
            configs: list of CompareConfig

        Returns:
            dict
            {
                label: str,
                values: list of float
            }
        """
        y_values = {}
        # We'll create an entry for each config, containing label and y-values
        # to be plotted
        for c in configs:
            y_values[self._get_y_label(c)] = []
            # For each benchmark in the sorted names list, get the value for the
            # given configuration and append it to the y-values list
            for benchmark_name in benchmark_names:

                # Label to index the benchmark collection
                b_label = self._get_b_label(c.interpreter, benchmark_name, c.with_warmup)
                if b_label not in self.benchmark_results_dict:
                    raise ValueError(f"Benchmark {b_label} not found.")

                # Benchmark result for the given configuration and benchmark
                b_result = self.benchmark_results_dict[b_label]

                # Compute the y-value for the given benchmark and configuration
                raw_value = b_result.get_single_value(c.category, c.with_warmup)
                if rel_config:
                    # If we're doing a relative comparison, then compute the
                    # y-value for this config relative to the rel_config
                    #
                    # Divide the value for this config by the value for the
                    # relative config
                    rel_label = self._get_b_label(rel_config.interpreter,
                                                  benchmark_name,
                                                  rel_config.with_warmup)
                    relative_value = self.benchmark_results_dict[rel_label].get_single_value(c.category, c.with_warmup)
                    y_value = raw_value / relative_value
                else:
                    y_value = raw_value

                # Append the value to the y-values list of the configuration
                y_values[self._get_y_label(c)].append(y_value)
        return y_values

    def _random_dark_color(self, base_color):
        base_shades = {
            "green": (0, ru(0.3, 0.6), 0),  # Dark green
            "blue": (0, 0, ru(0.3, 0.6)),   # Dark blue
            "red": (ru(0.3, 0.6), 0, 0)     # Dark red
        }
        return base_shades[base_color]

    def _plot_single_benchmark(self, single_benchmark_name, y_values, output_file, relative_label="", run_tag=""):
        """Plots the given plottable benchmark data produced by the _compare_on_single_benchmark() method and saves it to a png file.

        This is a line plot, where each configuration has a line for the benchmark, x-axis is the number of iterations.

        Args:
            single_benchmark_name: str
            y_values: dict
                    {
                        label: str,
                        values: list of float
                    }

        e.g. y_values looks like this:
            (recall that this is for a single benchmark)

                                      single_benchmark_name: ack
            {
                NewPWithWarmup   [1.0, 1.2, 1.1, ...],
                OldPWithWarmup   [1.5, 1.6, 1.7, ..., ..., ...],
                Racket           [2.0, 2.1, 2.2, ...]
            }
            (Note that the lengths of the lists can be different)
        """

        # Plot the data
        plt.figure(figsize=(12, 8))
        # Prepare a caption using output_file
        caption = Path(output_file.replace("_", " ")).stem
        plt.title(f"{run_tag} : {caption}")
        plt.xlabel("Iterations")
        plt.ylabel("Runtime (ms)")

        x = np.arange(len(y_values))
        width = 0.25

        cmap = plt.cm.get_cmap('hsv', 50)


        for label, values in y_values.items():
            if len(values) == 0:
                raise ValueError(f"No values for {label} in {single_benchmark_name}")

            # X values as the iteration indices
            x = np.arange(len(values))
            y = np.array(values)

            # Generate smooth x values for interpolation
            x_smooth = np.linspace(x.min(), x.max(), 300)

            # Create a smooth spline curve for y over x
            y_smooth = make_interp_spline(x, y, k=3)(x_smooth)

            # Plot the smooth curve
            plt.plot(x_smooth, y_smooth, label=label, linewidth=2)

        if relative_label:
            # Add a horizontal line for Racket baseline (normalized to 1)
            plt.axhline(y=1, color="magenta", linewidth=2, linestyle="-", label=relative_label)

        plt.grid(True)
        # plt.xticks(x + width, benchmark_names, rotation=45, ha="right")
        plt.legend()
        plt.tight_layout()
        print(f"Saving plot to {output_file}")
        plt.savefig(output_file)
        plt.close()

    def _plot_multi_benchmark(self, benchmark_names, y_values, output_file, relative_label="", run_tag=""):
        """Plots the given plottable benchmark data produced by the _compare_on_multi_benchmark() method and saves it to a png file.

        This is a bar plot, where each benchmark has a bar for each configuration.

        Args:
            benchmark_names: list of str,
            y_values: dict
                    {
                        label: str,
                        values: list of float
                    }
        """
        # Plot the data
        plt.figure(figsize=(12, 8))

        # Prepare a caption using output_file
        # Turn _ into a whitespace so it'll look better in plot caption
        caption = Path(output_file.replace("_", " ")).stem
        plt.title(f"{run_tag} : {caption}")
        # plt.xlabel(f"{run_tag}-{caption}")
        plt.ylabel("Runtime (ms)")

        x = np.arange(len(benchmark_names))
        width = 0.15
        group_gap = 0.2

        colors = {
            f"{NEW_PYCKET} With Warmup": "#0c590c",
            f"{NEW_PYCKET} No Warmup": "#5ae8b8",
            f"{OLD_PYCKET} With Warmup": "#941616",
            f"{OLD_PYCKET} No Warmup": "#f77474",
            f"{RACKET} With Warmup": "#4558e6" # FIXME: "Racket"
        }

        cmap = plt.cm.get_cmap('hsv', 50)
        for i, (label, values) in enumerate(y_values.items()):
            color=cmap(i*(50//len(y_values)))
            if NEW_PYCKET in label:
                color = self._random_dark_color("green")
            elif OLD_PYCKET in label:
                color = self._random_dark_color("red")
            elif RACKET in label:
                color = self._random_dark_color("blue")
            # override the color for now
            color=colors[label]
            plt.bar(x + i * (width) + group_gap * (i // len(y_values)), values, width, label=label, color=color)

        if relative_label:
            # Add a horizontal line for Racket baseline (normalized to 1)
            plt.axhline(y=1, color="magenta", linewidth=2, linestyle="-", label=relative_label)

        plt.xticks(x + width, benchmark_names, rotation=45, ha="right")
        plt.legend()
        plt.tight_layout()
        print(f"Saving plot to {output_file}")
        plt.savefig(output_file)

    def _compare_on_single_benchmark(self, single_benchmark_name, configs, rel_config=None):
        """
            Produces plottable data for comparing each given configuration on a single benchmark.

            Args:
                single_benchmark_name: str
                configs: list of CompareConfig

            Returns:
                y_values: dict
                        {
                            label: str,
                            value: list of float
                        }

            e.g. y_values looks like this:
            (recall that this is for a single benchmark)

                                      ack
            {
                NewPWithWarmup   [1.0, 1.2, 1.1, ...],
                OldPWithWarmup   [1.5, 1.6, 1.7, ...],
                Racket           [2.0, 2.1, 2.2, ...]
            }
        """
        if len(self.benchmark_results_dict) == 0:
            raise ValueError("No benchmarks to compare.")

        if len(configs) == 0:
            raise ValueError("No configurations to compare.")

        y_values = {}
        for c in configs:
            # Label to index the benchmark collection
            b_label = self._get_b_label(c.interpreter, single_benchmark_name, c.with_warmup)
            if b_label not in self.benchmark_results_dict:
                raise ValueError(f"Benchmark {b_label} not found.")

            # Benchmark result for the given configuration and benchmark
            b_result = self.benchmark_results_dict[b_label]

            # Compute the y-value series for the given benchmark and configuration
            raw_values = b_result.get_series(c.category)
            if rel_config:
                # If we're doing a relative comparison, then compute the
                # y-value for this config relative to the rel_config
                #
                # Divide the value for this config by the value for the
                # relative config
                assert c.with_warmup == rel_config.with_warmup
                rel_label = self._get_b_label(rel_config.interpreter,
                                              single_benchmark_name,
                                              rel_config.with_warmup)
                relative_values = self.benchmark_results_dict[rel_label].get_series(c.category)

                # y_values is the piecewise division of the raw values by the relative values
                y_values[self._get_y_label(c)] = [raw / relative for raw, relative in zip(raw_values, relative_values)]

            else:
                y_values[self._get_y_label(c)] = raw_values

        return y_values

    def _compare_on_multi_benchmark(self, configs, rel_config=None, single_benchmark_name=None):
        """
            Produces plottable data for comparing each given configuration on a single plot.

            Args:
                configs: list of CompareConfig

            Returns:
                _plot_config: dict
                {
                    benchmark_names: list of str,
                    y_values: dict
                            {
                                label: str,
                                values: list of float
                            }
                }
                Both benchmark_names and the inner list of y_values have the same lengths and order.
                Values and benchmarks are sorted in non-decreasing order based on the runtime.

                e.g. y_values looks like this:
                                      ack ctak fib ....
                {
                    NewPWithWarmup   [1.0, 1.2, 1.1, ...]
                    OldPWithWarmup   [1.5, 1.6, 1.7, ...]
                    Racket           [2.0, 2.1, 2.2, ...]
                }
        """
        if len(self.benchmark_results_dict) == 0:
            raise ValueError("No benchmarks to compare.")

        if len(configs) == 0:
            raise ValueError("No configurations to compare.")

        all_configs = configs
        # Get the configuration that matches the sort order (i.e. look for new
        # pycket first, then old pycket, then racket)
        sort_config, configs = self._pick_sort_config(configs)

        # When found, pop it from configs, filter and sort the benchmarks for
        # the "sort" configuration
        benchmarks = self._filter_benchmarks_for_config(sort_config)
        benchmark_names = [b.name for b in benchmarks]

        # Then construct the y-values for other configurations, selecting the
        # benchmark name from the benchmark names list
        y_values = self._construct_y_values(benchmark_names, all_configs, rel_config)

        # Sort the y_values based on the sort_config info
        sorted_benchmark_names, y_values = self._sort_y_values(benchmark_names, y_values, sort_config)

        return sorted_benchmark_names, y_values

    def collect_benchmark_results(self, benchmark_names, compare_configs):
        """
            Gather benchmark results from the collection for the given benchmarks and compare_configs

            Args:
                benchmark_names [str]: which benchmarks' results are being plotted
                compare_configs set(CompareConfig): which interp settings are requested (NP_WW, OP_NW, etc ...)

            Returns:
                TODO: not sure yet
        """

        """
            b_results
                {
                    NP_WW: {
                                ack: Result
                                tail: Result
                                ...
                            },
                    OP_NW: {
                                ack: Result
                                ...
                            },
                    ...
                }

        """
        b_results = {}
        for compare_config in compare_configs:

            if compare_config.interp not in self.benchmark_results_dict:
                raise Exception(f"Results for {compare_config.interp} not found in the result collection")

            # Initialize b_results
            b_results[compare_config.interp] = {}

            for benchmark_name in benchmark_names:
                # Get BenchmarkResult object that contains results for all categories
                b_result = self.benchmark_results_dict[compare_config.interp][benchmark_name]
                # Get Result object that contains values and stats for the requested category
                result = b_result.get_result(compare_config.category)
                b_results[compare_config.interp][b_result.benchmark_name] = result

        return b_results

    def generate_plots(self, benchmark_names, plot_configs):
        for plot_config in plot_configs:
            self.generate_plot(benchmark_names, plot_config)

    def generate_plot(self, benchmark_names, plot_config):
        print(f"Generating comparison plot data for {plot_config.output_file_name}...")

        # Collect benchmark results for given compare_configs and benchmark names
        benchmark_results = self.collect_benchmark_results(benchmark_names,
                                                           plot_config.compare_configs)

        plot_config.plot(benchmark_names, benchmark_results)



    # def plot(self, configs, output_file, relative_interpreter="", single_benchmark_name=None, run_tag=""):
    #     print(f"Generating comparison plot data for {output_file}...")
    #     rel_config_plot_label = relative_interpreter
    #     if not single_benchmark_name:
    #         benchmark_names, y_values = self._compare_on_multi_benchmark(configs, rel_config, single_benchmark_name)
    #         return self._plot_multi_benchmark(benchmark_names, y_values, output_file, rel_config_plot_label, run_tag)
    #
    #     y_values = self._compare_on_single_benchmark(single_benchmark_name, configs, rel_config)
    #     return self._plot_single_benchmark(single_benchmark_name, y_values, output_file, rel_config_plot_label, run_tag)

