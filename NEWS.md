# medstats 0.1.0

* `plot_meanse()` and `plot_stacked()` format plain labels with Chinese and Latin font families internally, configured through font arguments or session options. `plot_meanse()` also accepts `time_labels` for visit labels.
* `plot_stacked()` accepts `show_print = FALSE` to return the plot without automatically printing or opening a graphics device.
* `make_table1()` accepts a `type` argument to override automatic variable type inference.
* `make_table1()` accepts `statistic`, `digits`, and `label` for presentation settings.
* `make_table1()` defaults to mean ± SD, median (Q1, Q3) for `specific_vars`, and n (%).
* `make_table1()` can omit comparisons and the overall column in grouped descriptive tables.
* `plot_meanse()` supports single-group paired t, Wilcoxon signed-rank, and exact McNemar tests against an explicit reference visit, with ID matching, P-value adjustment, optional error bars and printing, and configurable significance-label size and maximum stars.
