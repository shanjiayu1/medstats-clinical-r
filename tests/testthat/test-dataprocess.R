test_that("long_to_surv_data produces correct structure", {
  test_data <- data.frame(
    id = rep(1:3, each = 3),
    event = c(0, 0, 1, 0, 0, 0, 0, 1, 0),
    time = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    age = rep(c(50, 60, 70), each = 3)
  )
  result <- long_to_surv_data(
    data = test_data,
    id_var = "id",
    event_flag_var = "event",
    time_var = "time",
    baseline_vars = c("age")
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("status" %in% names(result))
  expect_true("time" %in% names(result))
  # Subject 1 had event at time 3
  expect_equal(result$status[result$id == 1], 1)
  expect_equal(result$time[result$id == 1], 3)
  # Subject 2 was censored, time should be max
  expect_equal(result$status[result$id == 2], 0)
  expect_equal(result$time[result$id == 2], 3)
})

test_that("merge_duplicate_records keeps the first non-missing value", {
  test_data <- data.frame(
    住院号 = c("A001", "A001", "A002", "A002"),
    年龄 = c(NA, 65, 52, 53),
    诊断 = c("高血压", NA, NA, "糖尿病"),
    检查日期 = as.Date(c(NA, NA, "2026-01-02", NA)),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- merge_duplicate_records(test_data, "住院号"),
    "Found 2 duplicate group"
  )

  expect_equal(nrow(result), 2)
  expect_identical(names(result), names(test_data))
  expect_equal(result$年龄, c(65, 52))
  expect_equal(result$诊断, c("高血压", "糖尿病"))
  expect_true(is.na(result$检查日期[1]))
  expect_s3_class(result$检查日期, "Date")
})

test_that("merge_duplicate_records supports multiple grouping variables", {
  test_data <- data.frame(
    id = c(1, 1, 1),
    visit = c(1, 1, 2),
    value = c(NA, 10, 20)
  )

  result <- merge_duplicate_records(
    test_data,
    group_vars = c("id", "visit"),
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(10, 20))
})

test_that("merge_duplicate_records returns unchanged data without duplicates", {
  test_data <- data.frame(id = 1:2, value = c(NA, 2))

  expect_message(
    result <- merge_duplicate_records(test_data, "id"),
    "No duplicate records"
  )
  expect_identical(result, test_data)
})

test_that("merge_duplicate_records validates grouping columns", {
  test_data <- data.frame(id = 1:2)

  expect_error(
    merge_duplicate_records(test_data, "unknown"),
    "Columns not found"
  )
  expect_error(
    merge_duplicate_records(test_data, character()),
    "non-empty character vector"
  )
})

test_that("make_table1 returns gtsummary object", {
  skip_if_not_installed("gtsummary")
  result <- make_table1(
    data = gtsummary::trial,
    vars = c("age", "grade"),
    group_var = "trt"
  )
  expect_s3_class(result, "gtsummary")
})

test_that("make_table1 works without group_var", {
  skip_if_not_installed("gtsummary")
  result <- make_table1(
    data = gtsummary::trial,
    vars = c("age", "grade")
  )
  expect_s3_class(result, "gtsummary")
})

test_that("make_table1 handles a non-normal variable", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("TH.data")
  result <- make_table1(
    data = gtsummary::trial,
    vars = c("age", "marker", "grade"),
    specific_vars = "marker",
    group_var = "trt"
  )
  expect_s3_class(result, "gtsummary")
  expect_true("statistic" %in% names(result$table_body))
})

test_that("make_table1 can override inferred numeric variable types", {
  data <- data.frame(dose = rep(c(0, 5, 10), each = 4))

  default <- make_table1(data, vars = "dose")
  explicit <- make_table1(
    data,
    vars = "dose",
    type = list(dose ~ "continuous")
  )

  expect_identical(unique(default$table_body$var_type), "categorical")
  expect_identical(explicit$table_body$var_type, "continuous")
  expect_identical(explicit$table_body$stat_0, "5.00(4.26)")
})

test_that("make_table1 accepts display formats, precision, and labels", {
  data <- data.frame(
    dose = rep(c(0, 5, 10), each = 4),
    status = rep(c("Yes", "No"), c(4, 8))
  )
  result <- make_table1(
    data,
    vars = c("dose", "status"),
    type = list(dose ~ "continuous", status ~ "categorical"),
    statistic = list(dose ~ "{mean} \u00b1 {sd}", status ~ "{n} ({p}%)"),
    digits = list(dose ~ 1, status ~ c(0, 1)),
    label = list(dose ~ "Dose (mg/day)")
  )

  expect_identical(result$table_body$label[1], "Dose (mg/day)")
  expect_identical(result$table_body$stat_0[c(1, 3, 4)],
                   c("5.0 \u00b1 4.3", "8 (66.7%)", "4 (33.3%)"))
})
