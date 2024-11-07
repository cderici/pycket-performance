import numpy as np
import matplotlib.pyplot as plt

plt.style.use('fivethirtyeight')

NEW_PYCKET = "New Pycket"
OLD_PYCKET = "Old Pycket"
RACKET = "Racket"

class BenchmarkResult:
    """Keeps a record of the results of a benchmark run for each category (CPU, GC, Total).
    """
    def __init__(self, benchmark_name, interpreter, with_warmup, cpu_value, gc_value, total_value):
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
        self.cpu_value = cpu_value
        self.gc_value = gc_value
        self.total_value = total_value

    def get_value(self, category):
        """Returns the value for the given category.

        Args:
            category: str, "cpu" | "gc" | "total"

        Returns:
            float
        """
        if category == "cpu":
            return self.cpu_value
        elif category == "gc":
            return self.gc_value
        else:
            return self.total_value

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
        if b_result.interpreter == RACKET:
            b_label = f"{b_result.interpreter}_{b_result.name}"
        else:
            b_label = f"{b_result.interpreter}_{b_result.name}_{b_result.with_warmup}"
        self.benchmark_results_dict[b_label] = b_result

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

    def _filter_benchmarks_for(self, config):
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

    def _sort_benchmarks_for_config(self, benchmarks, category):
        """Sorts the benchmarks based on the given configuration.

        Args:
            benchmarks: list of BenchmarkResult
            category: "cpu" | "gc" | "total"

        Returns:
            list of BenchmarkResult
        """
        if category == "cpu":
            return sorted(benchmarks, key=lambda b: b.cpu_value)
        elif category == "gc":
            return sorted(benchmarks, key=lambda b: b.gc_value)
        else:
            return sorted(benchmarks, key=lambda b: b.total_value)

    def _construct_y_values(self, sorted_benchmark_names, configs):
        """Constructs the y-values for the given configurations.

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
        for c in configs:
            y_values[c.interpreter] = []
            for benchmark_name in sorted_benchmark_names:
                if c.interpreter == RACKET:
                    b_label = f"{c.interpreter}_{benchmark_name}"
                else:
                    b_label = f"{c.interpreter}_{benchmark_name}_{c.with_warmup}"
                if b_label not in self.benchmark_results_dict:
                    raise ValueError(f"Benchmark {b_label} not found.")
                b_result = self.benchmark_results_dict[b_label]
                y_values[c.interpreter].append(b_result.get_value(c.category))
        return y_values

    def _plot(self, benchmark_names, y_values, output_file, relative_label=""):
        """Plots the given plottable benchmark data produced by the compare() method and saves it to a png file.

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
        plt.xlabel("Benchmarks")
        plt.ylabel("Runtime (ms)")

        x = np.arange(len(benchmark_names))
        width = 0.25

        for i, (label, values) in enumerate(y_values.items()):
            color = "red"
            if OLD_PYCKET in label:
                color = "green"
            elif RACKET in label:
                color = "blue"
            plt.bar(x + i * width, values, width, label=label, color=color)

        plt.xticks(x + width, benchmark_names, rotation=45, ha="right")
        plt.legend()
        plt.tight_layout()
        print(f"Saving plot to {output_file}")
        plt.savefig(output_file)

    def _compare(self, configs):
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
        benchmarks_for_sort_config = self._filter_benchmarks_for(sort_config)

        sorted_benchmarks_for_sort_config = self._sort_benchmarks_for_config(benchmarks_for_sort_config, sort_config)

        sorted_benchmark_names = [b.name for b in sorted_benchmarks_for_sort_config]

        # Then construct the y-values for other configurations, selecting the
        # benchmark from the sorted list
        y_values = self._construct_y_values(sorted_benchmark_names, all_configs)

        return sorted_benchmark_names, y_values

    def plot(self, configs, output_file, is_relative=False):
        print("Generating comparison plot data...")
        relative_label = ""
        if is_relative:
            benchmark_names, y_values, relative_label = self._relative_compare(configs)
        else:
            benchmark_names, y_values = self._compare(configs)
        return self._plot(benchmark_names, y_values, output_file, relative_label)