import os, re

import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import make_interp_spline

plt.style.use('fivethirtyeight')

NEW_PYCKET = "New Pycket"
OLD_PYCKET = "Old Pycket"
RACKET = "Racket"

BENCH_FILE_PYCKET_REGEXP = r'(new|old)-pycket?-(.*?)(?:-(with|no)-warmup)?.rst'
BENCH_FILE_RACKET_REGEXP = 'racket-(.*?).rst'

RESULT_CPU_REGEXP = r'RESULT-cpu:\s+([\d.]+)'
RESULT_GC_REGEXP = r'RESULT-gc:\s+([\d.]+)'
RESULT_TOTAL_REGEXP = r'RESULT-total:\s+([\d.]+)'

class BenchmarkIngress:
    """Processes the benchmark file and extracts the runtime durations from it.
    """
    def __init__(self, directory_path):
        """
        Args:
            directory_path: str
        """
        self.directory = directory_path

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

        return cpu_times, gc_times, total_times

    def _analyze_filename(self, file_name):
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
                interpreter, benchmark_name, is_with_warmup = self._analyze_filename(filename)

                if benchmark_name:
                    # Parse the file and extract the average runtime values
                    cpu_times, gc_times, total_times = self._parse_and_extract(file_path)

                    # Create a BenchmarkResult object and add it to the collection
                    bResult = BenchmarkResult(benchmark_name, interpreter, is_with_warmup, cpu_times, gc_times, total_times)

                    collection.add_benchmark(bResult)

        return collection


class BenchmarkResult:
    """Keeps a record of the results of a benchmark run for each category (CPU, GC, Total).
    """
    def __init__(self, benchmark_name, interpreter, with_warmup, cpu_values, gc_values, total_values):
        """
        Args:
            benchmark_name: str
            interpreter: str, "New Pycket" | "Old Pycket" | "Racket"
            with_warmup: bool
            value: float
        """
        self.name = benchmark_name
        self.interpreter = interpreter
        self.with_warmup = with_warmup
        self.cpu_values = cpu_values
        self.gc_values = gc_values
        self.total_values = total_values

        # Cached values for the average of the values
        self.cpu_value = None
        self.gc_value = None
        self.total_value = None

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
        if category == "cpu" and self.cpu_value:
            return self.cpu_value
        elif category == "gc" and self.gc_value:
            return self.gc_value
        elif category == "total" and self.total_value:
            return self.total_value

        if not is_with_warmup:
            self.cpu_value = np.mean(self.cpu_times) if self.cpu_times else 0
            self.gc_value = np.mean(self.gc_times) if self.gc_times else 0
            self.total_value = np.mean(self.total_times) if self.total_times else 0
        else:
            # If warmup is enabled, we'll use the average of the fastest 10 runs.
            self.cpu_value = np.mean(sorted(self.cpu_times)[:10]) if self.cpu_times else 0
            self.gc_value = np.mean(sorted(self.gc_times)[:10]) if self.gc_times else 0
            self.total_value = np.mean(sorted(self.total_times)[:10]) if self.total_times else 0

        return self.get_single_value(category, is_with_warmup)

    def __str__(self):
        return f"{self.interpreter} {self.name} {'With Warmup' if self.with_warmup else 'No Warmup'}: CPU {self.cpu_value}, GC {self.gc_value}, Total {self.total_value}"

class CompareConfig():
    def __init__(self, interpreter, with_warmup, category="total", relative=False):
        """
        Args:
            interpreter: str
            with_warmup: bool
            category: str, can be "cpu", "gc", "total"
                (default: "total")
        """
        self.interpreter = interpreter
        self.with_warmup = with_warmup
        self.category = category
        self.relative = relative

    def __str__(self):
        return f"{self.interpreter} {"With Warmup" if self.with_warmup else "No Warmup"} {self.category} time"

class BenchmarkCollection():
    """Keeps a collection of BenchmarkResult objects, and knows how to sort, process, and analyse them.

    Use:
        - compare(): to produce a plot comparing the benchmarks.
    """
    def __init__(self):
        self.benchmark_results_dict = {}

    def add_benchmark(self, b_result):
        """
        Args:
            benchmark: BenchmarkResult
        """
        b_label = self._get_b_label(b_result.interpreter, b_result.name, b_result.with_warmup)
        self.benchmark_results_dict[b_label] = b_result

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

        priority_picks = [None]*len(PRIORITY)
        picked_config = None
        # Select a configuration based on priority
        # Skip the relative config (see above comment)
        # If the highest priority config is found, stop looking
        # Keep an array for priority picks
        for c in configs:
            if c.interpreter == PRIORITY[0] and not c.relative:
                # Found the highest priority config, stop looking
                picked_config = c
                break
            elif not priority_picks[1] and c.interpreter == PRIORITY[1] and not c.relative:
                priority_picks[1] = c
            elif not any(priority_picks) and c.interpreter == PRIORITY[2] and not c.relative:
                priority_picks[2] = c

        if not picked_config:
            picked_config = priority_picks[1] or priority_picks[2]

        return picked_config, [c for c in configs if c != picked_config]

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
                    assert c.with_warmup == rel_config.with_warmup
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

    def _plot_single_benchmark(self, single_benchmark_name, y_values, output_file, relative_label=""):
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
        caption = output_file.replace("_", " ")[:-4]
        plt.title(caption)
        plt.xlabel("Iterations")
        plt.ylabel("Runtime (ms)")

        x = np.arange(len(y_values))
        width = 0.25

        cmap = plt.cm.get_cmap('hsv', 50)


        for label, values in y_values.items():
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

    def _plot_multi_benchmark(self, benchmark_names, y_values, output_file, relative_label=""):
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
        caption = output_file.replace("_", " ")[:-4]
        plt.xlabel(caption)
        plt.ylabel("Runtime (ms)")

        x = np.arange(len(benchmark_names))
        width = 0.25

        cmap = plt.cm.get_cmap('hsv', 50)
        for i, (label, values) in enumerate(y_values.items()):
            """
            color = "red"
            if OLD_PYCKET in label:
                color = "green"
            elif RACKET in label:
                color = "blue"
            """
            plt.bar(x + i * width, values, width, label=label, color=cmap(i*(50//len(y_values))))

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

    def plot(self, _configs, output_file, is_relative=False, single_benchmark_name=None):
        print("Generating comparison plot data...")
        rel_config_plot_label = ""
        rel_config = None

        if not is_relative:
            configs = _configs
        else:
            # Find out which config is the relative one and pop it out from
            # configs, keep the label for the plot
            configs = []
            for c in _configs:
                if c.relative:
                    rel_config = c
                    rel_config_plot_label = c.interpreter
                else:
                    configs.append(c)
        if not single_benchmark_name:
            benchmark_names, y_values = self._compare_on_multi_benchmark(configs, rel_config, single_benchmark_name)
            return self._plot_multi_benchmark(benchmark_names, y_values, output_file, rel_config_plot_label)

        y_values = self._compare_on_single_benchmark(single_benchmark_name, configs, rel_config)
        return self._plot_single_benchmark(single_benchmark_name, y_values, output_file, rel_config_plot_label)