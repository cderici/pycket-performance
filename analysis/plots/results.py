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

class CompareConfig():
    def __init__(self, interpreter, with_warmup, category="total"):
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

class BenchmarkCollection():
    """Keeps a collection of BenchmarkResult objects, and knows how to sort, process, and analyse them.

    Use:
        - compare(): to produce a plot comparing the benchmarks.
    """
    def __init__(self):
        self.benchmark_results = []

    def add_benchmark(self, benchmark):
        """
        Args:
            benchmark: BenchmarkResult
        """
        self.benchmark_results.append(benchmark)

    def _plot(self, _plot_config):
        """Plots the given plottable benchmark data produced by the compare() method and saves it to a png file.

        Args:
            _plot_config: dict
                {
                    benchmark_names: list of str,
                    y_values: dict
                            {
                                label: str,
                                values: list of float
                            }
                }
        """

    def _pick_sort_config(self, configs):
        """
        Args:
            configs: list of CompareConfig

        Returns:
            sort_config: CompareConfig
            remaining_configs: list of CompareConfig
        """
        # Benchmark sort order: new pycket, old pycket, racket
        potential_old, potential_racket = None, None
        for config in configs:
            if config.interpreter == NEW_PYCKET:
                return config, [c for c in configs if c != config]
            elif not potential_old and config.interpreter == OLD_PYCKET:
                potential_old = config
            elif not potential_racket and config.interpreter == RACKET:
                potential_racket = config
        return potential_old or potential_racket

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
        if len(self.benchmark_results) == 0:
            raise ValueError("No benchmarks to compare.")

        # Get the configuration that matches the sort order (i.e. look for new
        # pycket first, then old pycket, then racket)
        sort_config, configs = self._pick_sort_config(configs)

        # When found, pop it from configs, filter and sort the benchmarks for
        # the "sort" configuration
        sorted_benchmarks_for_sort_config = self._filter_benchmarks_for(sort_config)

        sorted_benchmark_names = [b.name for b in sorted_benchmarks_for_sort_config]

        # Then construct the y-values for other configurations, selecting the
        # benchmark from the sorted list
        y_values = self._construct_y_values(sorted_benchmarks_for_sort_config, configs)

        return {
            "benchmark_names": sorted_benchmark_names,
            "y_values": y_values
        }

    def compare_and_plot(self, configs):
        return self._plot(self._compare(configs))