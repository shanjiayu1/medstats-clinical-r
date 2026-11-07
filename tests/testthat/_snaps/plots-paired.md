# paired tests reject ambiguous IDs, visits, and binary codes

    Code
      plot_meanse(data, target_var = "value", time_var = "time", test_method = "t")
    Condition
      Error:
      ! Single-group tests require `id_var` naming a subject ID column.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
        ref_time = 99, test_method = "t")
    Condition
      Error:
      ! `ref_time` must match one observed value in the time column.

---

    Code
      plot_meanse(rbind(data, data[1, ]), target_var = "value", time_var = "time",
      id_var = "id", ref_time = 0, test_method = "t")
    Condition
      Error:
      ! Each subject must have at most one row per time point for paired tests.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
        ref_time = 0, test_method = "t")
    Condition
      Error:
      ! Subject IDs must not be missing for paired tests.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
        ref_time = 0, test_method = "mcnemar")
    Condition
      Error:
      ! McNemar outcomes must be 0, `event_value`, or missing.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
        ref_time = "Baseline", test_method = "t")
    Condition
      Error:
      ! Paired-test time values must map to distinct finite numeric positions.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", p_adjust_method = "invalid")
    Condition
      Error:
      ! `p_adjust_method` must be a method from stats::p.adjust.methods.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", max_stars = 5)
    Condition
      Error:
      ! `max_stars` must be an integer from 1 to 4.

---

    Code
      plot_meanse(data, target_var = "value", time_var = "time", group_var = "id",
        test_method = "mcnemar")
    Condition
      Error:
      ! Paired tests and reference visits require `group_var = NULL`.
