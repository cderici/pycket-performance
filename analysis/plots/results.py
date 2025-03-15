import os, re
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

from pathlib import Path
from scipy.interpolate import make_interp_spline
from functools import partial

plt.style.use('fivethirtyeight')

# Interpreter Identifiers
NP_WW = "NPWW"
NP_NW = "NPNW"
OP_WW = "OPWW"
OP_NW = "OPNW"
R     = "R"

def interp_human(interp):
    if interp == NP_WW:
        return "New Pycket With Warmup"
    elif interp == NP_NW:
        return "New Pycket No Warmup"
    elif interp == OP_WW:
        return "Old Pycket With Warmup"
    elif interp == OP_NW:
        return "Old Pycket No Warmup"
    elif interp == R:
        return "Racket"

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

        # What to use when we need a single value to represent the whole sample
        self.representative = self.best

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
                 output_file_path,
                 sort_interp=None,
                 relative_interp=None,
                 is_single=False,
                 compare_configs={},
                 benchmark_names=[],
                 caption=""):
        # A single plot is we compare multiple interpreters on a single benchmark
        # Single plots are graph plots (whereas non-single ones are bar charts)
        self.is_single = is_single

        self.output_file_path = output_file_path
        self.caption = caption

        # Validate interpreters
        for i in compare_configs:
            assert i.interp in VALID_INTERPRETERS, f"{i} is not a valid. Valid interpreters are : {VALID_INTERPRETERS}"

        if is_single:
            # Check if benchmark_names contain only one benchmark if this is a single plot
            assert len(benchmark_names) == 1, f"multiple benchmarks are given for a \"single\" plot: {benchmark_names}"

        self.benchmark_names = benchmark_names
        self.compare_configs = compare_configs
        self.sort_interp = sort_interp
        self.relative_interp = relative_interp

    def plot_single(self, benchmark_results):
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
        benchmark_name = self.benchmark_names[0]

        self._plot_preamble()

        plt.xlabel("Iterations")

        for interp, result_dict in benchmark_results.items():
            if self.relative_interp:
                # Add a horizontal line for relative interp baseline (normalized to 1)
                plt.axhline(y=1, color="magenta", linewidth=2, linestyle="-", label=interp_human(self.relative_interp))
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
            plt.plot(x_smooth, y_smooth, color=self._interp_color(interp), label=interp_human(interp), linewidth=2)

        plt.grid(True)
        self._plot_postamble()

    def _compute_sorted_benchmark_names(self, benchmark_results):
        """
            Get the benchmark names in order where the results of
            the self.sort_interp for those benchmarks are ascending
            order.

            If we have a relative interp, then we need to consider the
            divided values instead of absolute values when ordering.
        """
        s_interp_results = benchmark_results[self.sort_interp] # {bname: Result}
        if self.relative_interp:
            relative_interp_results = benchmark_results[self.relative_interp]
            return [bname for bname, _ in sorted(s_interp_results.items(), key=lambda x: x[1].representative / relative_interp_results[x[0]].representative)]
        return [bname for bname, _ in sorted(s_interp_results.items(), key=lambda x: x[1].representative)]


    def _plot_preamble(self):
        plt.figure(figsize=(12, 8))
        # Prepare a caption using output_file
        plt.title(self.output_file_path.stem.replace("_", " "))
        plt.ylabel("Runtime (ms)")

    def _plot_postamble(self):
        plt.legend()
        plt.tight_layout()
        print(f"Saving plot to {self.output_file_path}")

        # Make sure the directory exists
        self.output_file_path.parent.mkdir(parents=True, exist_ok=True)

        plt.savefig(self.output_file_path)
        plt.close()

    def _interp_color(self, interp):

        colors = {
            NP_WW: "#0c590c",
            NP_NW: "#5ae8b8",
            OP_WW: "#941616",
            OP_NW: "#f77474",
            R: "#4558e6"
        }

        return colors[interp]

    def plot_multi(self, benchmark_results):
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

        # Multi will always be sorted based on an interp (e.g., NP_WW)
        # Get the benchmark names that are sorted based on the 
        # result of the interp used (e.g. OP_NW)
        sorted_benchmark_names = self._compute_sorted_benchmark_names(benchmark_results)

        x = np.arange(len(sorted_benchmark_names))
        width = 0.15
        group_gap = 0.2

        # Prepare values for computing relative values
        # relative_interp_values: np.array
        relative_interp_values = np.empty(len(sorted_benchmark_names), dtype=float)
        confidence_for_relative = np.empty(len(sorted_benchmark_names), dtype=float)
        if self.relative_interp:
            relative_interp_results = benchmark_results[self.relative_interp]
            for i, b in enumerate(sorted_benchmark_names):
                relative_interp_values[i] = relative_interp_results[b].representative
                confidence_for_relative[i] = relative_interp_results[b].ci_half_range
            # For relative interp itself, just plot a horizontal line
            # and skip
            plt.axhline(y=1, color="magenta", linewidth=2, linestyle="-", label=interp_human(self.relative_interp))
            # Plot the results of the rest
            del benchmark_results[self.relative_interp]

        for i, (interp, results) in enumerate(benchmark_results.items()):
            # Get the results of interp for sorted benchmark_results
            results_for_interp = []
            for benchmark_name in sorted_benchmark_names:
                results_for_interp.append(results[benchmark_name])

            y_values_for_interp = np.array([r.representative for r in results_for_interp])
            confidence_for_interp = np.array([r.ci_half_range for r in results_for_interp])

            if self.relative_interp:
                relative_runtimes = y_values_for_interp / relative_interp_values

                # Compute the relative error confidence intervals
                confidence_for_interp = relative_runtimes * np.sqrt(np.square(confidence_for_interp / y_values_for_interp) + np.square(confidence_for_relative / relative_interp_values))

                y_values_for_interp = relative_runtimes

            label = interp_human(interp)
            color = self._interp_color(interp)

            plt.bar(x + (i * (width + (group_gap * (i // len(sorted_benchmark_names))))),
                    y_values_for_interp,
                    width=width,
                    label=label,
                    color=color
                    )

            plt.errorbar(x + (i * (width + (group_gap * (i // len(sorted_benchmark_names))))),
                         y_values_for_interp,
                         yerr=confidence_for_interp,
                         fmt='none',
                         ecolor='black',
                         capsize=4,
                         elinewidth=1
                         )

        plt.xticks(x + width, sorted_benchmark_names, rotation=45, ha="right")
        self._plot_postamble()

    def plot(self, benchmark_results):
        if self.is_single:

            return self.plot_single(benchmark_results)

        return self.plot_multi(benchmark_results)

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

    def generate_plots(self, plot_configs):
        for plot_config in plot_configs:
            self.generate_plot(plot_config)

    def generate_plot(self, plot_config):
        print(f"Generating comparison plot data for {plot_config.output_file_path}...")

        # Collect benchmark results for given compare_configs and benchmark names
        benchmark_results = self.collect_benchmark_results(plot_config.benchmark_names,
                                                           plot_config.compare_configs)

        plot_config.plot(benchmark_results)

