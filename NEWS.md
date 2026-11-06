# medstats 0.1.0

* `make_table1()` accepts a `type` argument to override automatic variable type inference.
* `make_table1()` accepts `statistic`, `digits`, and `label` for presentation settings.
* `make_table1()` defaults to mean ± SD, median (Q1, Q3) for `specific_vars`, and n (%).
* `make_table1()` can omit comparisons and the overall column in grouped descriptive tables.
