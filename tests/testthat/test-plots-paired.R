test_that("single-group tests pair by ID and adjust all visit comparisons", {
  reference <- c(12, 9, 15, 8, 11, NA)
  followup <- list(c(10, NA, 11, 7, 7, 5), c(5, 7, 8, NA, 6, 2))
  data <- data.frame(
    id = rep(1:6, 3),
    time = rep(0:2, each = 6),
    value = c(reference, unlist(followup))
  )
  data <- data[order((seq_len(nrow(data)) * 7) %% 19), ]
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  for (method in c("t", "wilcox")) {
    result <- plot_meanse(
      data, target_var = "value", time_var = "time", id_var = "id",
      ref_time = 0, test_method = method, p_adjust_method = "bonferroni",
      show_ci = FALSE, max_stars = 3, xlab = "Time", ylab = "Value"
    )
    expected <- vapply(followup, function(value) {
      keep <- stats::complete.cases(reference, value)
      if (method == "t") {
        stats::t.test(value[keep], reference[keep], paired = TRUE)$p.value
      } else {
        stats::wilcox.test(value[keep], reference[keep], paired = TRUE,
                           exact = FALSE)$p.value
      }
    }, numeric(1))
    expect_equal(result$test_data$time, 1:2)
    expect_equal(result$test_data$paired_n, c(4L, 4L))
    expect_equal(result$test_data$reference_time, c(0, 0))
    expect_equal(result$test_data$p_value, expected)
    expect_equal(result$test_data$p_adjusted, pmin(1, expected * 2))
    expect_equal(result$test_data$status, rep("ok", 2))
    expect_equal(result$summary_data$n, c(5L, 5L, 5L))
    expect_length(Filter(function(layer) inherits(layer$geom, "GeomErrorbar"),
                         result$plot$layers), 0)
    expect_no_error(ggplot2::ggplot_build(result$plot))
  }
})

test_that("single-group McNemar tests use discordant pairs and adjusted stars", {
  data <- data.frame(
    id = rep(1:20, 3), time = rep(0:2, each = 20),
    value = c(rep(0, 20), rep(100, 20), rep(100, 15), rep(0, 5))
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  result <- plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "mcnemar", event_value = 100,
    p_adjust_method = "bonferroni", show_ci = FALSE,
    max_stars = 3, xlab = "Time", ylab = "Percent"
  )
  expected <- c(stats::binom.test(20, 20)$p.value,
                stats::binom.test(15, 15)$p.value)
  expect_equal(result$test_data$p_value, expected)
  expect_equal(result$test_data$p_adjusted, expected * 2)
  expect_equal(result$test_data$paired_n, c(20L, 20L))
  expect_equal(result$test_data$significance, c("***", "***"))
  expect_equal(result$summary_data$target_mean, c(0, 100, 75))
  annotation <- Filter(function(layer) inherits(layer$geom, "GeomText"),
                       result$plot$layers)[[1]]
  expect_equal(annotation$data$significance, result$test_data$significance)
  expect_equal(annotation$data$time_num, c(1, 2))
  expect_gt(min(annotation$data$y_position - c(100, 75)), 0)

  data$value <- data$value / 100
  binary <- plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "mcnemar", xlab = "Time", ylab = "Proportion"
  )
  expect_equal(binary$test_data$p_adjusted, expected)
  expect_equal(binary$test_data$significance, c("****", "****"))
})

test_that("paired-test boundary cases have explicit results", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  data <- data.frame(
    id = rep(1:3, 3), time = rep(0:2, each = 3),
    value = c(1:3, 2:4, NA, NA, NA)
  )
  result <- plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "t", xlab = "Time", ylab = "Value"
  )
  expect_equal(result$test_data$status, c("zero_variance", "no_pairs"))
  expect_equal(result$test_data$paired_n, c(3L, 0L))
  expect_equal(result$test_data$p_value, c(NA_real_, NA_real_))

  data$value <- c(1:3, 1, 2, 4, NA, NA, NA)
  result <- plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "t", p_adjust_method = "bonferroni",
    xlab = "Time", ylab = "Value"
  )
  expect_equal(result$test_data$p_adjusted[1], min(1, result$test_data$p_value[1] * 2))
  expect_equal(result$test_data$status[2], "no_pairs")

  data$value <- c(1:3, 1:3, 1, NA, NA)
  result <- plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "wilcox", xlab = "Time", ylab = "Value"
  )
  expect_equal(result$test_data$status, c("ok", "insufficient_pairs"))
  expect_equal(result$test_data$p_value, c(1, NA_real_))

  data$value <- c(0, 1, 0, 0, 1, 0, NA, NA, NA)
  result <- plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "mcnemar", xlab = "Time", ylab = "Value"
  )
  expect_equal(result$test_data$p_value, c(1, NA_real_))
  expect_equal(result$test_data$status, c("ok", "no_pairs"))
})

test_that("paired tests reject ambiguous IDs, visits, and binary codes", {
  data <- data.frame(id = rep(1:3, 2), time = rep(0:1, each = 3), value = c(1:3, 2:4))
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", test_method = "t")
  })
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
                ref_time = 99, test_method = "t")
  })
  expect_snapshot(error = TRUE, {
    plot_meanse(rbind(data, data[1, ]), target_var = "value", time_var = "time",
                id_var = "id", ref_time = 0, test_method = "t")
  })
  data$id[1] <- NA
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
                ref_time = 0, test_method = "t")
  })
  data$id[1] <- 1
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
                ref_time = 0, test_method = "mcnemar")
  })
  data$time <- rep(c("Baseline", "Month 1"), each = 3)
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", id_var = "id",
                ref_time = "Baseline", test_method = "t")
  })
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", p_adjust_method = "invalid")
  })
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", max_stars = 5)
  })
  expect_snapshot(error = TRUE, {
    plot_meanse(data, target_var = "value", time_var = "time", group_var = "id",
                test_method = "mcnemar")
  })
})

test_that("reference labels and internal-looking variable names are supported", {
  data <- data.frame(
    id = rep(1:4, 2), time = factor(rep(c("Day 0", "Day 7"), each = 4)),
    dummy_group_var = c(2:5, 1, 2, 2, 4)
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  result <- plot_meanse(
    data, target_var = "dummy_group_var", time_var = "time", id_var = "id",
    ref_time = "Day 0", test_method = "t", xlab = "Time", ylab = "Value"
  )
  expect_equal(result$summary_data$target_mean, c(3.5, 2.25))
  expect_equal(result$test_data$paired_n, 4L)
  expect_equal(result$test_data$reference_time, "Day 0")
  expect_equal(result$test_data$time_num, 7)
  names(data)[3] <- ".pair_id"
  renamed <- plot_meanse(
    data, target_var = ".pair_id", time_var = "time", id_var = "id",
    ref_time = "Day 0", test_method = "t", xlab = "Time", ylab = "Value"
  )
  expect_equal(renamed$test_data, result$test_data)
})

test_that("plots can be returned without printing with a chosen label size", {
  data <- data.frame(id = rep(1:20, 2), time = rep(0:1, each = 20),
                     value = c(rep(0, 20), rep(1, 20)))
  device <- grDevices::dev.cur()
  result <- withVisible(plot_meanse(
    data, target_var = "value", time_var = "time", id_var = "id",
    ref_time = 0, test_method = "mcnemar", show_print = FALSE,
    show_ci = FALSE, label_size = 3.2
  ))
  expect_false(result$visible)
  expect_identical(grDevices::dev.cur(), device)
  labels <- Filter(function(layer) inherits(layer$geom, "GeomText"),
                    result$value$plot$layers)
  expect_length(labels, 1)
  expect_equal(labels[[1]]$aes_params$size, 3.2)
  expect_no_error(ggplot2::ggplot_build(result$value$plot))
})
