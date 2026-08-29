test_that("run_glm_auto linear regression works", {
  result <- run_glm_auto(
    data = mtcars,
    vars = c("hp", "wt"),
    outcome_var = "mpg",
    family = "gaussian"
  )
  expect_s3_class(result, "data.frame")
  expect_true("Variable" %in% names(result))
  expect_true(grepl("Beta", names(result)[2]))
  expect_equal(
    attr(result, "medstats_regression_footnotes"),
    c(`1` = "Univariable analysis.", `2` = "Multivariable analysis.")
  )
})

test_that("run_glm_auto adjusted results match glm", {
  result <- run_glm_auto(
    data = mtcars,
    vars = c("hp", "wt"),
    outcome_var = "mpg",
    covars = "qsec",
    family = "gaussian"
  )
  model <- glm(mpg ~ hp + qsec, data = mtcars, family = gaussian)
  model_summary <- summary(model)$coefficients
  critical_value <- qt(0.975, df = df.residual(model))

  hp_p <- if (model_summary["hp", "Pr(>|t|)"] < 0.001) {
    "<0.001"
  } else {
    sprintf("%.3f", model_summary["hp", "Pr(>|t|)"])
  }
  hp_ci <- sprintf(
    "%.3f (%.3f, %.3f)",
    model_summary["hp", "Estimate"],
    model_summary["hp", "Estimate"] - critical_value * model_summary["hp", "Std. Error"],
    model_summary["hp", "Estimate"] + critical_value * model_summary["hp", "Std. Error"]
  )

  expect_equal(result$`P value [2]`[result$Variable == "hp"], hp_p)
  expect_equal(result$`Beta (95% CI) [2]`[result$Variable == "hp"], hp_ci)
})

test_that("run_glm_auto logistic regression works", {
  skip_if_not_installed("gtsummary")
  result <- run_glm_auto(
    data = gtsummary::trial,
    vars = c("age"),
    outcome_var = "response",
    family = "binomial"
  )
  expect_s3_class(result, "data.frame")
  expect_true(grepl("OR", names(result)[2]))
})

test_that("run_glm_auto logistic Wald confidence intervals match Wald p-values", {
  set.seed(20260814)
  test_data <- data.frame(
    x1 = rnorm(300),
    x2 = rnorm(300)
  )
  probability <- plogis(-0.2 + 0.5 * test_data$x1 - 0.4 * test_data$x2)
  test_data$y <- rbinom(300, 1, probability)

  result <- run_glm_auto(
    data = test_data,
    vars = "x1",
    outcome_var = "y",
    covars = "x2",
    family = "binomial"
  )
  model <- glm(y ~ x1 + x2, data = test_data, family = binomial)
  model_summary <- summary(model)$coefficients
  critical_value <- qnorm(0.975)
  coefficient <- model_summary["x1", "Estimate"]
  std_error <- model_summary["x1", "Std. Error"]

  expected_p <- sprintf("%.3f", model_summary["x1", "Pr(>|z|)"])
  expected_ci <- sprintf(
    "%.3f (%.3f, %.3f)",
    exp(coefficient),
    exp(coefficient - critical_value * std_error),
    exp(coefficient + critical_value * std_error)
  )

  expect_equal(result$`P value [2]`[result$Variable == "x1"], expected_p)
  expect_equal(result$`OR (95% CI) [2]`[result$Variable == "x1"], expected_ci)
})

test_that("run_glm_auto reports model-specific factor reference levels", {
  set.seed(20260814)
  test_data <- data.frame(
    y = rep(c(0, 1), 75),
    group = factor(rep(c("A", "B", "C"), each = 50)),
    z = rnorm(150)
  )
  test_data$z[test_data$group == "A"] <- NA

  result <- run_glm_auto(
    data = test_data,
    vars = "group",
    outcome_var = "y",
    covars = "z",
    family = "binomial"
  )

  expect_equal(result$`OR (95% CI) [1]`[result$Variable == "A"], "1 (Ref)")
  expect_true(is.na(result$`OR (95% CI) [2]`[result$Variable == "A"]))
  expect_equal(result$`OR (95% CI) [2]`[result$Variable == "B"], "1 (Ref)")
})

test_that("run_cox_auto works", {
  skip_if_not_installed("survival")
  lung2 <- survival::lung
  lung2$status2 <- ifelse(lung2$status == 2, 1, 0)
  result <- run_cox_auto(
    data = lung2,
    vars = c("age", "sex"),
    time_var = "time",
    event_var = "status2"
  )
  expect_s3_class(result, "data.frame")
  expect_true("Variable" %in% names(result))
  expect_true(grepl("HR", names(result)[2]))
  expect_equal(
    names(result),
    c(
      "Variable",
      "HR (95% CI) [1]",
      "P value [1]",
      "HR (95% CI) [2]",
      "P value [2]"
    )
  )
})

test_that("longdata_analysis works with 2 groups", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("geepack")
  test_data <- nlme::Orthodont |>
    as.data.frame() |>
    dplyr::mutate(time_str = paste0(age, "岁"))
  result <- longdata_analysis(
    data = test_data,
    id_col = "Subject",
    treatment_col = "Sex",
    time_col = "time_str",
    score_col = "distance"
  )
  expect_s3_class(result, "data.frame")
  expect_true("Sex" %in% names(result))
})
