#' Repeated measures analysis with GEE
#'
#' @description
#' Performs a comprehensive repeated measures analysis for longitudinal data, including
#' descriptive statistics, cross-sectional group comparisons (t-test or ANOVA), within-group
#' GEE trend analysis, and a two-factor GEE model with time-by-group interaction.
#'
#' @param data A data frame in long format.
#' @param id_col Character string. Name of the column identifying individual subjects.
#' @param treatment_col Character string. Name of the column indicating treatment group.
#' @param time_col Character string. Name of the column indicating measurement time points.
#'   Must contain extractable numeric values (e.g., "week1", "1").
#' @param score_col Character string. Name of the column containing the continuous outcome measure.
#'
#' @return A data frame summarizing:
#'   - Descriptive statistics (mean +/- SD) by group and time
#'   - Within-group GEE Wald test for time trend
#'   - Cross-sectional group comparison statistics and p-values
#'   - Two-factor GEE interaction results
#'
#' @details
#' Data requirements:
#' 1. Long format (one row per measurement per subject)
#' 2. `id_col` must uniquely identify subjects
#' 3. `treatment_col`: 2 groups -> t-test; >2 groups -> ANOVA
#' 4. `time_col` must contain a parseable numeric component (via `readr::parse_number()`)
#' 5. `score_col` must be continuous numeric
#'
#' @examples
#' \dontrun{
#' library(nlme)
#' my_data <- Orthodont |>
#'   as.data.frame() |>
#'   mutate(time_str = paste0(age, "岁"))
#'
#' longdata_analysis(
#'   data = my_data,
#'   id_col = "Subject",
#'   treatment_col = "Sex",
#'   time_col = "time_str",
#'   score_col = "distance"
#' )
#' }
#'
#' @export
longdata_analysis <- function(data,
                              id_col,
                              treatment_col,
                              time_col,
                              score_col) {

  # Step 1: 规范化数据
  long_data <- data |>
    dplyr::select(
      id = dplyr::all_of(id_col),
      treat = dplyr::all_of(treatment_col),
      time = dplyr::all_of(time_col),
      score = dplyr::all_of(score_col)
    ) |>
    dplyr::filter(!is.na(score)) |>
    dplyr::mutate(
      id = as.factor(id),
      score = as.numeric(score),
      treat = as.factor(treat)
    )

  time_levels <- unique(long_data$time)
  long_data <- long_data |> dplyr::mutate(time = factor(time, levels = time_levels))

  # Step 2: 描述性统计
  mean_values <- long_data |>
    dplyr::group_by(treat, time) |>
    dplyr::summarise(
      mean_score = mean(score, na.rm = TRUE),
      sd_score = sd(score, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(result = paste0(round(mean_score, 2), "(", round(sd_score, 2), ")")) |>
    tidyr::pivot_wider(id_cols = treat, names_from = time, values_from = result)

  # Step 3: 智能组间截面比较
  num_groups <- dplyr::n_distinct(long_data$treat)

  if (num_groups == 2) {
    cross_test <- long_data |>
      tidyr::nest(data = -time) |>
      dplyr::mutate(
        result = purrr::map(data, ~ t.test(score ~ treat, data = .x)),
        stat_value = purrr::map_dbl(result, ~ .x$statistic),
        p_value = purrr::map_dbl(result, ~ .x$p.value)
      )
    stat_label <- "t"
  } else {
    cross_test <- long_data |>
      tidyr::nest(data = -time) |>
      dplyr::mutate(
        result = purrr::map(data, ~ aov(score ~ treat, data = .x)),
        anova_summary = purrr::map(result, ~ summary(.)),
        stat_value = purrr::map_dbl(anova_summary, ~ .[[1]][["F value"]][1]),
        p_value = purrr::map_dbl(anova_summary, ~ .[[1]][["Pr(>F)"]][1])
      )
    stat_label <- "F"
  }

  cross_test <- cross_test |>
    dplyr::mutate(
      stat_value = round(as.numeric(stat_value), 2),
      p_value = format_p(as.numeric(p_value))
    ) |>
    dplyr::select(time, stat_value, p_value)

  t2 <- tibble::tibble(stat_value = cross_test$stat_value, p_value = cross_test$p_value) |>
    t() |> tibble::as_tibble(.name_repair = "minimal")
  colnames(t2) <- as.character(cross_test$time)
  t2 <- t2 |> dplyr::mutate(dplyr::across(everything(), as.character))

  # Step 4: 组内 GEE 趋势分析
  long_data <- long_data |>
    dplyr::mutate(time_num = readr::parse_number(as.character(time))) |>
    dplyr::arrange(treat, id, time_num)

  gee_single <- long_data |>
    tidyr::nest(data = -treat) |>
    dplyr::mutate(
      result = purrr::map(data, ~ geepack::geeglm(score ~ time_num, id = id, family = gaussian, corstr = "ind", data = .x)),
      gee_summary = purrr::map(result, ~ summary(.)),
      wald_value = purrr::map_dbl(gee_summary, ~ .[["coefficients"]][["Wald"]][2]),
      p_value = purrr::map_dbl(gee_summary, ~ .[["coefficients"]][["Pr(>|W|)"]][2])
    ) |>
    dplyr::mutate(
      wald_value = round(as.numeric(wald_value), 2),
      p_value = format_p(as.numeric(p_value))
    ) |>
    dplyr::select(treat, wald_value, p_value) |>
    dplyr::mutate(dplyr::across(everything(), as.character))

  # Step 5: 两因素 GEE
  long_data <- long_data |> dplyr::arrange(id, time_num)

  gee_model <- geepack::geeglm(score ~ treat + time_num + treat * time_num,
                                id = id, family = gaussian, corstr = "ind", data = long_data)

  gee_results <- broom::tidy(gee_model) |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(
      statistic = round(statistic, 2),
      p_str = format_p(p.value),
      result = ifelse(
        p_str == "<0.001",
        paste0("Z=", statistic, ", p<0.001"),
        paste0("Z=", statistic, ", p=", p_str)
      )
    ) |>
    dplyr::select(term, result)

  t6 <- gee_results |> dplyr::rename(treat = term)

  # Step 6: 汇总拼装
  t1 <- tibble::tibble(treat = c(stat_label, "p"))
  t3 <- dplyr::bind_cols(t1, t2)
  t4 <- mean_values |>
    dplyr::mutate(dplyr::across(everything(), as.character)) |>
    dplyr::left_join(gee_single, by = "treat")
  t5 <- dplyr::bind_rows(t4, t3)
  colnames(t6)[2] <- colnames(t5)[2]
  final_results <- dplyr::bind_rows(t5, t6)
  final_results <- final_results |> dplyr::rename(!!treatment_col := treat)

  return(final_results)
}


#' Run automated generalized linear regression (GLM)
#'
#' @description
#' Performs both univariate and multivariate GLM regression for a set of variables,
#' automatically detecting factor variables and adding reference rows.
#' For logistic regression, coefficients are exponentiated to Odds Ratios (OR);
#' for linear regression, raw Beta coefficients are reported.
#'
#' @param data A data frame.
#' @param vars Character vector. Names of predictor variables to regress.
#' @param outcome_var Character string. Name of the outcome variable.
#' @param covars Character vector or NULL. Names of covariates for the multivariate model.
#'   Default is `NULL`.
#' @param family Character or family object. The error distribution for GLM.
#'   Use `"binomial"` for logistic regression, `"gaussian"` for linear regression.
#'   Default is `"binomial"`.
#'
#' @return A data frame with columns:
#'   - `Variable`: variable name and factor levels
#'   - `OR (95% CI) [1]` or `Beta (95% CI) [1]`: univariable estimate
#'   - `P value [1]`: univariable p-value
#'   - `OR (95% CI) [2]` or `Beta (95% CI) [2]`: multivariable estimate
#'   - `P value [2]`: multivariable p-value
#'
#'   Passing the result to [format_flextable()] renders `[1]` and `[2]` as
#'   superscript markers and adds English model footnotes below the table.
#'
#' @examples
#' # Linear regression
#' run_glm_auto(mtcars, vars = c("hp", "wt"), outcome_var = "mpg", family = "gaussian")
#'
#' # Logistic regression
#' \dontrun{
#' library(gtsummary)
#' run_glm_auto(trial, vars = c("age", "stage"), outcome_var = "response", family = "binomial")
#' }
#'
#' @export
run_glm_auto <- function(data, vars, outcome_var, covars = NULL, family = "binomial") {

  fam_name <- if (is.character(family)) family else family$family
  is_linear <- fam_name == "gaussian"

  metric_name <- if (is_linear) "Beta" else "OR"
  ref_text <- if (is_linear) "0 (Ref)" else "1 (Ref)"

  quote_var <- function(var) {
    var_clean <- gsub("`", "", var)
    paste0("`", var_clean, "`")
  }

  run_single <- function(var) {
    var_quoted <- quote_var(var)
    outcome_quoted <- quote_var(outcome_var)
    f <- as.formula(paste0(outcome_quoted, " ~ ", var_quoted))
    stats::glm(f, data = data, family = family)
  }

  run_multi <- function(var) {
    var_quoted <- quote_var(var)
    outcome_quoted <- quote_var(outcome_var)
    if (!is.null(covars)) {
      covars_quoted <- vapply(covars, quote_var, character(1))
      covar_part <- paste(covars_quoted, collapse = " + ")
      f <- as.formula(paste0(outcome_quoted, " ~ ", var_quoted, " + ", covar_part))
    } else {
      f <- as.formula(paste0(outcome_quoted, " ~ ", var_quoted))
    }
    stats::glm(f, data = data, family = family)
  }

  tidy_model <- function(model) {
    # summary.glm() uses normal tests for binomial/Poisson models and t tests
    # when dispersion is estimated. Use the corresponding critical value so
    # that the Wald confidence interval and p-value use the same method.
    uses_normal <- model$family$family %in% c("binomial", "poisson")
    critical_value <- if (uses_normal) {
      stats::qnorm(0.975)
    } else {
      stats::qt(0.975, df = stats::df.residual(model))
    }

    broom::tidy(model, conf.int = FALSE) |>
      dplyr::filter(term != "(Intercept)") |>
      dplyr::mutate(
        conf.low = estimate - critical_value * std.error,
        conf.high = estimate + critical_value * std.error,
        est_val = if (is_linear) estimate else exp(estimate),
        lci_val = if (is_linear) conf.low else exp(conf.low),
        uci_val = if (is_linear) conf.high else exp(conf.high),
        Est = sprintf("%.3f", est_val),
        lci = sprintf("%.3f", lci_val),
        uci = sprintf("%.3f", uci_val),
        Res_CI = paste0(Est, " (", lci, ", ", uci, ")"),
        p.value = dplyr::if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
      ) |>
      dplyr::select(term, Res_CI, p.value)
  }

  variable_terms <- function(model, var) {
    model_matrix <- stats::model.matrix(model)
    term_assign <- attr(model_matrix, "assign")
    term_labels <- attr(stats::terms(model), "term.labels")
    var_clean <- gsub("`", "", var)
    clean_labels <- gsub("`", "", term_labels)
    term_number <- which(clean_labels == var_clean)

    if (length(term_number) == 0L) return(character())
    colnames(model_matrix)[term_assign %in% term_number]
  }

  results_for_variable <- function(model, var) {
    model_terms <- variable_terms(model, var)
    tidy_model(model) |>
      dplyr::filter(term %in% model_terms)
  }

  factor_results <- function(model, var) {
    var_clean <- gsub("`", "", var)
    result <- results_for_variable(model, var)
    quoted_prefix <- paste0("`", var_clean, "`")

    strip_prefix <- function(term) {
      if (startsWith(term, quoted_prefix)) {
        substring(term, nchar(quoted_prefix) + 1L)
      } else if (startsWith(term, var_clean)) {
        substring(term, nchar(var_clean) + 1L)
      } else {
        term
      }
    }

    result <- result |>
      dplyr::mutate(term = vapply(term, strip_prefix, character(1)))

    model_levels <- model$xlevels[[var_clean]]
    if (is.null(model_levels) || length(model_levels) == 0L) return(result)

    reference_row <- tibble::tibble(
      term = model_levels[1],
      Res_CI = ref_text,
      p.value = ""
    )
    dplyr::bind_rows(reference_row, result)
  }

  result_list <- purrr::map(vars, function(var) {
    mod1 <- run_single(var)
    mod2 <- run_multi(var)
    var_clean <- gsub("`", "", var)
    is_categorical <- is.factor(data[[var_clean]]) ||
      is.character(data[[var_clean]]) || is.logical(data[[var_clean]])

    if (is_categorical) {
      res1 <- factor_results(mod1, var)
      res2 <- factor_results(mod2, var)
      merged <- dplyr::full_join(
        res1,
        res2,
        by = "term",
        suffix = c("_m1", "_m2")
      )

      level_order <- unique(c(
        mod1$xlevels[[var_clean]],
        mod2$xlevels[[var_clean]],
        merged$term
      ))
      merged <- merged |>
        dplyr::arrange(match(term, level_order))

      header_row <- tibble::tibble(
        term = var_clean,
        Res_CI_m1 = "",
        p.value_m1 = "",
        Res_CI_m2 = "",
        p.value_m2 = ""
      )
      dplyr::bind_rows(header_row, merged)
    } else {
      res1 <- results_for_variable(mod1, var)
      res2 <- results_for_variable(mod2, var)
      dplyr::full_join(
        res1,
        res2,
        by = "term",
        suffix = c("_m1", "_m2")
      )
    }
  })

  result <- dplyr::bind_rows(result_list) |>
    stats::setNames(c(
      "Variable",
      paste0(metric_name, " (95% CI) [1]"),
      "P value [1]",
      paste0(metric_name, " (95% CI) [2]"),
      "P value [2]"
    ))

  attr(result, "medstats_regression_footnotes") <- c(
    `1` = "Univariable analysis.",
    `2` = "Multivariable analysis."
  )
  result
}


#' Run automated Cox proportional hazards regression
#'
#' @description
#' Performs both univariate and multivariate Cox regression for a set of predictor variables,
#' reporting Hazard Ratios (HR) with 95% confidence intervals. Automatically detects factor
#' variables and adds reference rows.
#'
#' @param data A data frame containing survival data.
#' @param vars Character vector. Names of predictor variables.
#' @param time_var Character string. Name of the survival time variable.
#' @param event_var Character string. Name of the event indicator variable (1 = event).
#' @param covars Character vector or NULL. Names of covariates for the multivariate model.
#'   Default is `NULL`.
#'
#' @return A data frame with columns:
#'   - `Variable`: variable name and factor levels
#'   - `HR (95% CI) [1]`: univariable HR (95% CI)
#'   - `P value [1]`: univariable p-value
#'   - `HR (95% CI) [2]`: multivariable HR (95% CI)
#'   - `P value [2]`: multivariable p-value
#'
#'   Passing the result to [format_flextable()] renders `[1]` and `[2]` as
#'   superscript markers and adds English model footnotes below the table.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' run_cox_auto(lung, vars = c("age", "sex"), time_var = "time", event_var = "status")
#' }
#'
#' @export
run_cox_auto <- function(data, vars, time_var, event_var, covars = NULL) {

  run_single <- function(var) {
    f <- as.formula(paste0("survival::Surv(", time_var, ", ", event_var, ") ~ ", var))
    survival::coxph(f, data = data)
  }

  run_multi <- function(var) {
    if (!is.null(covars)) {
      covar_part <- paste(covars, collapse = " + ")
      f <- as.formula(paste0("survival::Surv(", time_var, ", ", event_var, ") ~ ",
                             var, " + ", covar_part))
    } else {
      f <- as.formula(paste0("survival::Surv(", time_var, ", ", event_var, ") ~ ", var))
    }
    survival::coxph(f, data = data)
  }

  tidy_model <- function(model) {
    broom::tidy(model, conf.int = TRUE) |>
      dplyr::mutate(
        HR = sprintf("%.3f", exp(estimate)),
        lci = sprintf("%.3f", exp(conf.low)),
        uci = sprintf("%.3f", exp(conf.high)),
        HR_CI = paste0(HR, " (", lci, ", ", uci, ")"),
        p.value = dplyr::if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
      ) |>
      dplyr::select(term, HR_CI, p.value)
  }

  result_list <- purrr::map(vars, function(var) {
    mod1 <- run_single(var)
    mod2 <- run_multi(var)
    res1 <- tidy_model(mod1)
    res2 <- tidy_model(mod2)

    merged <- res1 |>
      dplyr::left_join(res2, by = "term", suffix = c("_m1", "_m2"))

    if (is.factor(data[[var]])) {
      merged <- merged |> dplyr::mutate(term = stringr::str_remove(term, var))
      ref <- levels(data[[var]])[1]
      ref_row <- tibble::tibble(
        term = c(var, ref),
        HR_CI_m1 = c("", "1 (Ref)"),
        p.value_m1 = c("", ""),
        HR_CI_m2 = c("", "1 (Ref)"),
        p.value_m2 = c("", "")
      )
      merged <- dplyr::bind_rows(ref_row, merged)
      return(merged)
    } else {
      return(merged)
    }
  })

  result <- dplyr::bind_rows(result_list) |>
    stats::setNames(c(
      "Variable",
      "HR (95% CI) [1]",
      "P value [1]",
      "HR (95% CI) [2]",
      "P value [2]"
    ))

  attr(result, "medstats_regression_footnotes") <- c(
    `1` = "Univariable analysis.",
    `2` = "Multivariable analysis."
  )
  result
}
