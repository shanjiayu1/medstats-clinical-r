#' Convert longitudinal data to survival analysis format
#'
#' @description
#' Transforms long-format longitudinal data into a wide survival dataset suitable for
#' Cox regression. For each subject, it determines whether an event occurred (status = 1)
#' and calculates the time to first event or censoring.
#'
#' @param data A data frame in long format with repeated measurements per subject.
#' @param id_var Character string. Name of the column identifying individual subjects.
#'   Default is `"患者病案号"`.
#' @param event_flag_var Character string. Name of the column indicating event occurrence
#'   (1 = event, 0 = no event). Default is `"是否MSE"`.
#' @param time_var Character string. Name of the column containing follow-up time values.
#'   Default is `"re随访时间"`.
#' @param baseline_vars Character vector. Names of baseline variables to retain (takes the
#'   first value per subject). Default is `c("性别", "胸腺切除")`.
#'
#' @return A data frame with one row per subject, containing:
#'   - All `baseline_vars` columns (first value per subject)
#'   - `status`: 1 if event occurred, 0 otherwise
#'   - `time`: time to first event (if status=1) or last follow-up (if status=0)
#'
#' @details
#' The endpoint event must be coded as numeric 1. Events at time 0 are excluded.
#'
#' @examples
#' \dontrun{
#' surv_data <- long_to_surv_data(
#'   data = my_clinical_data,
#'   id_var = "PatientID",
#'   event_flag_var = "is_event",
#'   time_var = "follow_up_time",
#'   baseline_vars = c("Sex", "Age")
#' )
#' }
#'
#' @export
long_to_surv_data <- function(data,
                              id_var = "患者病案号",
                              event_flag_var = "是否MSE",
                              time_var = "re随访时间",
                              baseline_vars = c("性别", "胸腺切除")) {

  id_sym <- rlang::sym(id_var)
  flag_sym <- rlang::sym(event_flag_var)
  time_sym <- rlang::sym(time_var)

  surv_df <- data |>
    dplyr::arrange(!!id_sym, !!time_sym) |>
    dplyr::group_by(!!id_sym) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(baseline_vars), ~ .x[1]),
      status = ifelse(any((!!flag_sym) == 1 & (!!time_sym) > 0, na.rm = TRUE), 1, 0),
      time = ifelse(
        status == 1,
        min((!!time_sym)[(!!flag_sym) == 1 & (!!time_sym) > 0], na.rm = TRUE),
        max((!!time_sym), na.rm = TRUE)
      ),
      .groups = "drop"
    )

  return(surv_df)
}


#' Merge duplicate records using the first non-missing value
#'
#' @description
#' Checks for records that have the same values in the specified grouping
#' variables and merges each duplicate group into one record. For every
#' non-grouping variable, the first non-missing value in the original row order
#' is retained.
#'
#' @param data A data frame.
#' @param group_vars A non-empty character vector containing the column names
#'   used to identify duplicate records.
#' @param verbose Logical. If `TRUE` (the default), reports whether duplicate
#'   groups were found and how many records were merged.
#'
#' @return A data frame with one row per unique combination of `group_vars`.
#'   The original column order is retained. If every value of a variable is
#'   missing within a group, a typed missing value is returned.
#'
#' @details
#' Row order determines which value is retained when a duplicate group contains
#' more than one different non-missing value. Sort `data` before calling this
#' function if another priority is required.
#'
#' @examples
#' clinical_data <- data.frame(
#'   hospital_id = c("A001", "A001", "A002"),
#'   age = c(NA, 65, 52),
#'   diagnosis = c("hypertension", NA, "diabetes")
#' )
#'
#' merge_duplicate_records(clinical_data, group_vars = "hospital_id")
#'
#' @export
merge_duplicate_records <- function(data, group_vars, verbose = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (
    !is.character(group_vars) ||
      length(group_vars) == 0L ||
      anyNA(group_vars) ||
      any(!nzchar(group_vars))
  ) {
    stop(
      "`group_vars` must be a non-empty character vector of column names.",
      call. = FALSE
    )
  }

  group_vars <- unique(group_vars)
  missing_vars <- setdiff(group_vars, names(data))
  if (length(missing_vars) > 0L) {
    stop(
      "Columns not found in `data`: ",
      paste(missing_vars, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be either TRUE or FALSE.", call. = FALSE)
  }

  duplicate_rows <- duplicated(data[group_vars]) |
    duplicated(data[group_vars], fromLast = TRUE)
  duplicate_keys <- unique(data[duplicate_rows, group_vars, drop = FALSE])
  duplicate_group_count <- nrow(duplicate_keys)
  merged_record_count <- sum(duplicated(data[group_vars]))

  if (verbose) {
    if (duplicate_group_count == 0L) {
      message("No duplicate records found for `group_vars`.")
    } else {
      message(
        "Found ",
        duplicate_group_count,
        " duplicate group(s); merged ",
        merged_record_count,
        " redundant record(s)."
      )
    }
  }

  if (duplicate_group_count == 0L) {
    return(data)
  }

  original_names <- names(data)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::first(
          .x[!is.na(.x)],
          default = .x[NA_integer_][1]
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::select(dplyr::all_of(original_names))
}


#' Create a baseline characteristics table (Table 1)
#'
#' @description
#' Generates a publication-ready baseline table using `gtsummary::tbl_summary()`,
#' with automatic handling of non-normally distributed variables (Wilcoxon rank-sum test
#' with Z statistic) and optional group comparisons.
#'
#' @param data A data frame.
#' @param vars Character vector. Names of variables to include in the table.
#' @param specific_vars Character vector or NULL. Names of continuous variables that are
#'   non-normally distributed (reported as median + IQR). Default is `NULL`.
#' @param group_var Character string or NULL. Name of the grouping variable for comparisons.
#'   If `NULL`, no group comparison or p-values are shown. Default is `NULL`.
#' @param type Optional formula list passed to `gtsummary::tbl_summary()`
#'   to override variable types, for example `list(dose ~ "continuous")`.
#'   When `NULL`, variable types are inferred.
#' @param statistic Optional formula list of summary formats. When provided,
#'   it replaces the default formats, including the `specific_vars` format.
#' @param digits Optional formula list specifying summary precision, for
#'   example `list(gtsummary::all_categorical() ~ c(0, 1))`.
#' @param label Optional formula list of display labels, for example
#'   `list(age ~ "Age (years)")`.
#'
#' @return A `gtsummary` table object.
#'
#' @details
#' - Normally distributed continuous variables: mean (SD)
#' - Non-normally distributed continuous variables (specified via `specific_vars`): median (P25, P75)
#' - Categorical variables: n (p%)
#' - When `group_var` is provided, p-values are computed using t-test for normal
#'   continuous variables and Wilcoxon rank-sum Z-test for `specific_vars`.
#'
#' @examples
#' \dontrun{
#' library(gtsummary)
#' make_table1(
#'   data = trial,
#'   vars = c("age", "marker", "stage", "grade"),
#'   specific_vars = c("marker"),
#'   group_var = "trt"
#' )
#' }
#'
#' @export
make_table1 <- function(data,
                        vars,
                        specific_vars = NULL,
                        group_var = NULL,
                        type = NULL,
                        statistic = NULL,
                        digits = NULL,
                        label = NULL) {

  if (is.null(statistic)) {
    statistic <- list(
      gtsummary::all_continuous() ~ "{mean}({sd})",
      gtsummary::all_categorical() ~ "{n}({p}%)"
    )

    if (length(specific_vars) > 0) {
      statistic <- c(
        statistic,
        list(gtsummary::all_of(specific_vars) ~ "{median}({p25},{p75})")
      )
    }
  }

  if (is.null(digits)) {
    digits <- list(
      gtsummary::all_continuous() ~ 2,
      gtsummary::all_categorical() ~ c(0, 2)
    )
  }

  t <- data |>
    gtsummary::tbl_summary(
      include = gtsummary::all_of(vars),
      by = if (!is.null(group_var)) gtsummary::all_of(group_var) else NULL,
      missing = "no",
      type = type,
      statistic = statistic,
      digits = digits,
      label = label
    )

  if (!is.null(group_var)) {
    t <- t |>
      gtsummary::add_overall() |>
      gtsummary::add_p(
        test = list(
          gtsummary::all_continuous() ~ "t.test",
          specific_vars ~ wilcox_z_test
        ),
        pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
      ) |>
      gtsummary::modify_fmt_fun(
        statistic = gtsummary::label_style_number(digits = 2)
      ) |>
      gtsummary::modify_header(statistic = "**Test Statistic**")
  }

  t
}
