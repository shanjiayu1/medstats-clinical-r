test_that("plot_meanse returns list with plot and data", {
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(time_str = paste0("第", Time, "天"))

  result <- plot_meanse(
    data = test_data,
    target_var = "weight",
    time_var = "time_str",
    group_var = NULL
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("summary_data" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("plot_stacked returns list with plot and data", {
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(Time_str = factor(paste0("第", Time, "天")))

  result <- plot_stacked(
    data = test_data,
    target_var = "weight",
    time_var = "Time_str",
    breaks = c(-Inf, 50, 100, 200, Inf),
    labels = c("<=50", "51-100", "101-200", ">200"),
    colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
    label_size = 5.5
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$plot$layers[[2]]$aes_params$size, 5.5)
})

test_that("plot_meanse performs time-specific two-group tests", {
  test_data <- data.frame(
    time = rep(c("Day 0", "Day 7"), each = 20),
    group = rep(rep(c("Control", "Treatment"), each = 10), times = 2),
    value = c(
      1:10, 21:30,
      11:20, 12:21
    )
  )

  t_result <- plot_meanse(
    data = test_data,
    target_var = "value",
    time_var = "time",
    group_var = "group",
    test_method = "t",
    xlab = "Time",
    ylab = "Value"
  )
  wilcox_result <- plot_meanse(
    data = test_data,
    target_var = "value",
    time_var = "time",
    group_var = "group",
    test_method = "wilcox",
    xlab = "Time",
    ylab = "Value"
  )

  expect_equal(nrow(t_result$test_data), 2L)
  expect_equal(t_result$test_data$method, rep("t", 2L))
  expect_equal(wilcox_result$test_data$method, rep("wilcox", 2L))
  expect_equal(t_result$test_data$significance[1], "****")
  expect_equal(t_result$test_data$significance[2], "")
  expect_no_error(ggplot2::ggplot_build(t_result$plot))
  expect_no_error(ggplot2::ggplot_build(wilcox_result$plot))
})

test_that("plot_stacked supports grouped stacked bars", {
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(
      Time_str = factor(
        paste0("Day ", Time),
        levels = c("Day 0", "Day 10", "Day 20")
      )
    )

  result <- plot_stacked(
    data = test_data,
    target_var = "weight",
    time_var = "Time_str",
    breaks = c(-Inf, 50, 100, 200, Inf),
    labels = c("<=50", "51-100", "101-200", ">200"),
    colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
    group_var = "Diet"
  )

  pct_sums <- result$summary_data |>
    dplyr::group_by(Time_str, Diet) |>
    dplyr::summarise(pct_sum = sum(pct), .groups = "drop")

  expect_true("Diet" %in% names(result$summary_data))
  expect_equal(pct_sums$pct_sum, rep(1, nrow(pct_sums)))
  expect_no_error(ggplot2::ggplot_build(result$plot))
})

test_that("plot_roc returns list with correct elements", {
  skip_if_not_installed("pROC")
  model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
  mtcars2 <- mtcars
  mtcars2$pred_prob <- predict(model, newdata = mtcars2, type = "response")

  result <- plot_roc(
    data = mtcars2,
    true_var = "am",
    pred_var = "pred_prob",
    title = "Training ROC curve",
    show_print = FALSE,
    xlab = "1 - Specificity",
    ylab = "Sensitivity"
  )
  expect_type(result, "list")
  expect_true("auc" %in% names(result))
  expect_true("plot" %in% names(result))
  expect_true(result$auc > 0.5)
  expect_s3_class(result$hl_test, "htest")
  expect_false(is.na(result$hl_test$p.value))
  expect_equal(result$plot$labels$x, "1 - Specificity")
  expect_equal(result$plot$labels$y, "Sensitivity")

  plot_data <- ggplot2::ggplot_build(result$plot)$data
  expect_match(plot_data[[3]]$label, "H-L test p =", fixed = TRUE)
  expect_false(any(grepl("检验", plot_data[[3]]$label, fixed = TRUE)))

  default_result <- plot_roc(
    data = mtcars2,
    true_var = "am",
    pred_var = "pred_prob",
    show_print = FALSE
  )
  expect_equal(default_result$plot$labels$title, "ROC curve")
  expect_equal(
    default_result$plot$labels$x,
    "1 - Specificity (False Positive Rate)"
  )
  expect_equal(
    default_result$plot$labels$y,
    "Sensitivity (True Positive Rate)"
  )

  console_output <- capture.output(
    plot_roc(
      data = mtcars2,
      true_var = "am",
      pred_var = "pred_prob",
      show_print = TRUE
    )
  )
  expect_match(
    paste(console_output, collapse = "\n"),
    "Performance Evaluation",
    fixed = TRUE
  )
})

test_that("plot_rcs returns list with plot", {
  skip_if_not_installed("rms")
  result <- plot_rcs(
    data = mtcars,
    exposure = "wt",
    outcome = "mpg",
    model_type = "linear"
  )
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, "ggplot")
})

test_that("plot_rcs resolves Surv for Cox models", {
  skip_if_not_installed("rms")
  lung_rcs <- survival::lung
  lung_rcs$status_event <- as.integer(lung_rcs$status == 2)

  result <- plot_rcs(
    data = lung_rcs,
    exposure = "age",
    outcome = "Surv(time, status_event)",
    covars = c("sex", "ph.ecog"),
    model_type = "cox",
    ylab = "Hazard ratio"
  )

  expect_type(result, "list")
  expect_s3_class(result$fit, "cph")
  expect_s3_class(result$plot, "ggplot")
})

test_that("plot_sankey returns ggplot object", {
  skip_if_not_installed("ggalluvial")
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(
      Visit = factor(paste0("Day", Time), levels = c("Day0", "Day10", "Day20")),
      Status = dplyr::case_when(
        weight < 80 ~ "Light",
        weight >= 80 & weight < 150 ~ "Normal",
        weight >= 150 ~ "Heavy"
      )
    )

  result <- plot_sankey(
    data = test_data,
    id_var = "Chick",
    time_var = "Visit",
    state_var = "Status",
    na_strategy = "drop"
  )
  expect_s3_class(result, "ggplot")
})

test_that("plot_sankey resolves the ggalluvial stratum stat with missing visits", {
  skip_if_not_installed("ggalluvial")
  test_data <- ChickWeight |>
    dplyr::filter(Time %in% c(0, 10, 20)) |>
    dplyr::mutate(
      Visit = factor(paste0("Day", Time), levels = c("Day0", "Day10", "Day20")),
      Status = dplyr::case_when(
        weight < 80 ~ "Light",
        weight >= 80 & weight < 150 ~ "Normal",
        weight >= 150 ~ "Heavy"
      )
    )

  result <- plot_sankey(
    data = test_data,
    id_var = "Chick",
    time_var = "Visit",
    state_var = "Status",
    na_strategy = "show"
  )

  expect_s3_class(result, "ggplot")
  expect_no_error(ggplot2::ggplot_build(result))
})
