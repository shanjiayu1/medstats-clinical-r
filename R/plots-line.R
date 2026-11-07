#' Plot mean with error bars (line chart)
#'
#' @description
#' Creates a publication-quality line chart showing group means with 95% confidence
#' interval error bars. Supports both single-group and multi-group comparisons.
#'
#' @param data A data frame.
#' @param group_var Character string or NULL. Name of the grouping variable.
#'   If `NULL`, a single-group plot is generated. Default is `NULL`.
#' @param target_var Character string. Name of the continuous outcome variable.
#' @param time_var Character string. Name of the time variable. Must contain parseable
#'   numeric values for proper x-axis ordering.
#' @param legend_title Character string. Title for the legend. Default is `"组别"`.
#' @param xlab Character string. X-axis label. Default is `"随访时间"`.
#' @param ylab Character string. Y-axis label. Default is `"值"`.
#' @param colors Character vector. Colors for multi-group lines. Must have length >= number of
#'   groups. Default is `c("#B5D1E8", "#EB938F", "#A3D9A5", "#F2C68F", "#D1B3DF")`.
#' @param single_color Character string. Color for single-group line. Default is `"#B5D1E8"`.
#' @param test_method Character string or `NULL`. With two groups, `"t"` and
#'   `"wilcox"` compare independent groups at each time point. With one group,
#'   they perform paired t-tests and Wilcoxon signed-rank tests against
#'   `ref_time`. Single-group `"mcnemar"` performs exact McNemar tests.
#'   Default is `NULL`, which requests no tests.
#' @param id_var Character string. Subject ID column, required for single-group
#'   tests. Each subject may have at most one row per observed time point.
#' @param ref_time Single non-missing value matching the original time column.
#'   Reference visit for single-group tests, for example `0` or `"Day 0"`.
#' @param p_adjust_method P-value adjustment method accepted by
#'   [stats::p.adjust()]. Default is `"none"`. Adjustment includes all requested
#'   comparisons in this call. Stars use adjusted p-values.
#' @param event_value Nonzero numeric event code for `"mcnemar"`. Outcomes must
#'   be `0`, `event_value`, or missing. Default is `1`; use `100` for percentages.
#' @param show_ci Logical. Show mean +/- 1.96 SE error bars. Default is `TRUE`.
#' @param max_stars Integer from 1 to 4. Maximum number of significance stars.
#'   Default is `4`; use `3` to combine p < 0.001 into one category.
#' @param label_size Positive number. Size of significance labels in mm.
#'   Default is `6`.
#' @param show_print Logical. Print the plot before returning the result.
#'   Default is `TRUE`; use `FALSE` when saving the returned plot separately.
#' @param font_family Font family for Chinese text. Defaults to the option
#'   `medstats.font_family`, or `NULL` to keep the original theme font.
#' @param latin_family Font family for Latin text, numbers, and units. Defaults
#'   to the option `medstats.latin_family`, or `NULL`. Labels are plain text;
#'   mixed fonts are applied internally. Fonts must be installed locally.
#'   Use a system-font-capable device such as `ragg::agg_png`; base PDF devices
#'   do not support these fonts directly.
#'   Set both font arguments to `NULL` to retain the original theme fonts.
#' @param time_labels Optional character labels in the order of the plotted
#'   numeric time points. Default is `NULL`, using numeric time values.
#'
#' @return Invisibly returns a list with:
#'   - `summary_data`: data frame of computed means, SDs, SEs
#'   - `test_data`: time-specific test results with `p_value`, `p_adjusted`,
#'     `p_adjust_method`, and `significance`, or `NULL` without tests.
#'     Single-group tests also return `paired_n`, `reference_time`, and `status`.
#'   - `plot`: the ggplot object
#'
#' @details
#' Time variable must contain extractable numeric values via `readr::parse_number()`.
#' For multi-group plots, ensure `colors` has enough values for all groups.
#' Significance labels use `*` for p < 0.05, `**` for p < 0.01, `***` for
#' p < 0.001, and `****` for p < 0.0001, capped by `max_stars`.
#' Single-group comparisons match subjects by ID and remove missing outcomes
#' separately for each pair of visits. Plot means use all available outcomes
#' at each time point. Wilcoxon tests use a continuity-corrected normal
#' approximation (`exact = FALSE`). Exact McNemar tests use a two-sided
#' binomial test on discordant pairs, returning p = 1 when all pairs agree.
#' Empty pairs return `NA` with status `"no_pairs"`; t/Wilcoxon tests with fewer
#' than two pairs return `"insufficient_pairs"`. Constant paired differences
#' make the t-test undefined (`"zero_variance"`). Missing IDs, duplicate
#' subject-visits, invalid binary codes, and absent reference visits are errors.
#'
#' @examples
#' \dontrun{
#' plot_meanse(
#'   data = subset(ChickWeight, Diet %in% c(1, 2)),
#'   target_var = "weight",
#'   time_var = "Time",
#'   group_var = "Diet",
#'   test_method = "wilcox"
#' )
#'
#' plot_meanse(
#'   data = ChickWeight,
#'   target_var = "weight", time_var = "Time",
#'   id_var = "Chick", ref_time = 0,
#'   test_method = "wilcox", p_adjust_method = "bonferroni",
#'   show_ci = FALSE
#' )
#' }
#'
#' @importFrom rlang .data
#' @export
plot_meanse <- function(data,
                        group_var = NULL,
                        target_var,
                        time_var,
                        legend_title = "组别",
                        xlab = "随访时间",
                        ylab = "值",
                        colors = c("#B5D1E8", "#EB938F", "#A3D9A5", "#F2C68F", "#D1B3DF"),
                        single_color = "#B5D1E8",
                        test_method = NULL,
                        id_var = NULL,
                        ref_time = NULL,
                        p_adjust_method = "none",
                        event_value = 1,
                        show_ci = TRUE,
                        max_stars = 4L,
                        label_size = 6,
                        show_print = TRUE,
                        font_family = getOption("medstats.font_family", NULL),
                        latin_family = getOption("medstats.latin_family", NULL),
                        time_labels = NULL) {

  is_single <- is.null(group_var)

  if (!is.null(test_method)) {
    if (!is.character(test_method) || length(test_method) != 1L || is.na(test_method)) {
      stop("`test_method` must be NULL, \"t\", \"wilcox\", or \"mcnemar\".", call. = FALSE)
    }
    test_method <- tolower(test_method)
    if (test_method %in% c("t.test", "ttest")) test_method <- "t"
    if (test_method %in% c("wilcox.test", "wilcoxon")) test_method <- "wilcox"
    if (!test_method %in% c("t", "wilcox", "mcnemar")) {
      stop("`test_method` must be NULL, \"t\", \"wilcox\", or \"mcnemar\".", call. = FALSE)
    }
    if (!is_single && (test_method == "mcnemar" || !is.null(id_var) || !is.null(ref_time))) {
      stop("Paired tests and reference visits require `group_var = NULL`.", call. = FALSE)
    }
  }
  if (!is.character(p_adjust_method) || length(p_adjust_method) != 1L ||
      is.na(p_adjust_method) || !p_adjust_method %in% stats::p.adjust.methods) {
    stop("`p_adjust_method` must be a method from stats::p.adjust.methods.", call. = FALSE)
  }
  if (!is.logical(show_ci) || length(show_ci) != 1L || is.na(show_ci)) {
    stop("`show_ci` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_stars) || length(max_stars) != 1L || is.na(max_stars) ||
      !max_stars %in% 1:4) {
    stop("`max_stars` must be an integer from 1 to 4.", call. = FALSE)
  }
  if (!is.numeric(label_size) || length(label_size) != 1L ||
      !is.finite(label_size) || label_size <= 0) {
    stop("`label_size` must be a finite positive number.", call. = FALSE)
  }
  if (!is.logical(show_print) || length(show_print) != 1L || is.na(show_print)) {
    stop("`show_print` must be TRUE or FALSE.", call. = FALSE)
  }

  test_df <- NULL
  if (is_single && !is.null(test_method)) {
    test_df <- meanse_paired_tests(data, target_var, time_var, id_var,
                                 ref_time, test_method, event_value)
  }

  if (is_single) {
    group_var <- make.unique(c(names(data), "dummy_group_var"))[ncol(data) + 1L]
    data[[group_var]] <- "全体患者"
  }

  group_sym <- rlang::sym(group_var)
  target_sym <- rlang::sym(target_var)
  time_sym <- rlang::sym(time_var)

  summary_df <- data |>
    dplyr::group_by(!!group_sym, !!time_sym) |>
    dplyr::summarise(
      n = sum(!is.na(!!target_sym)),
      target_mean = mean(!!target_sym, na.rm = TRUE),
      target_sd = sd(!!target_sym, na.rm = TRUE),
      target_se = target_sd / sqrt(n),
      .groups = "drop"
    ) |>
    dplyr::arrange(!!group_sym, !!time_sym) |>
    dplyr::mutate(
      time_num = readr::parse_number(as.character(!!time_sym))
    )

  annotation_df <- NULL

  lower <- summary_df$target_mean
  upper <- summary_df$target_mean
  if (show_ci) {
    lower <- lower - 1.96 * summary_df$target_se
    upper <- upper + 1.96 * summary_df$target_se
    lower <- dplyr::coalesce(replace(lower, !is.finite(lower), NA_real_), summary_df$target_mean)
    upper <- dplyr::coalesce(replace(upper, !is.finite(upper), NA_real_), summary_df$target_mean)
  }

  if (!is.null(test_method) && !is_single) {
    test_input <- data |>
      dplyr::filter(
        !is.na(!!group_sym),
        !is.na(!!target_sym),
        !is.na(!!time_sym)
      )

    group_levels <- unique(as.character(test_input[[group_var]]))
    if (length(group_levels) != 2L) {
      stop("Time-specific t-tests and Wilcoxon tests require exactly two groups.", call. = FALSE)
    }

    test_df <- test_input |>
      dplyr::group_by(!!time_sym) |>
      dplyr::group_modify(function(.x, .y) {
        group_values <- as.character(.x[[group_var]])
        values_1 <- .x[[target_var]][group_values == group_levels[1]]
        values_2 <- .x[[target_var]][group_values == group_levels[2]]

        p_value <- tryCatch(
          {
            if (test_method == "t") {
              if (length(values_1) < 2L || length(values_2) < 2L) {
                NA_real_
              } else {
                stats::t.test(values_1, values_2)$p.value
              }
            } else if (length(values_1) == 0L || length(values_2) == 0L) {
              NA_real_
            } else {
              suppressWarnings(stats::wilcox.test(values_1, values_2, exact = FALSE)$p.value)
            }
          },
          error = function(e) NA_real_
        )

        data.frame(
          group1 = group_levels[1],
          group2 = group_levels[2],
          n1 = length(values_1),
          n2 = length(values_2),
          method = test_method,
          p_value = p_value,
          stringsAsFactors = FALSE
        )
      }) |>
      dplyr::ungroup()
  }

  if (!is.null(test_method)) {
    test_df <- test_df |>
      dplyr::mutate(
        p_adjusted = stats::p.adjust(p_value, method = p_adjust_method, n = nrow(test_df)),
        p_adjust_method = p_adjust_method,
        significance = dplyr::case_when(
          is.na(.data$p_adjusted) ~ "",
          .data$p_adjusted < 0.0001 ~ "****",
          .data$p_adjusted < 0.001 ~ "***",
          .data$p_adjusted < 0.01 ~ "**",
          .data$p_adjusted < 0.05 ~ "*",
          TRUE ~ ""
        ),
        significance = substr(significance, 1L, max_stars),
        time_num = readr::parse_number(as.character(!!time_sym))
      )

    plotted_values <- c(
      lower,
      upper,
      summary_df$target_mean
    )
    plotted_values <- plotted_values[is.finite(plotted_values)]
    y_span <- if (length(plotted_values)) diff(range(plotted_values)) else 0
    annotation_offset <- if (is.finite(y_span) && y_span > 0) {
      y_span * 0.08
    } else {
      max(abs(plotted_values), 1) * 0.08
    }

    annotation_positions <- summary_df |>
      dplyr::mutate(
        .upper = upper
      ) |>
      dplyr::filter(is.finite(.upper)) |>
      dplyr::group_by(!!time_sym) |>
      dplyr::summarise(y_position = max(.upper, na.rm = TRUE), .groups = "drop")

    annotation_df <- test_df |>
      dplyr::filter(significance != "") |>
      dplyr::left_join(annotation_positions, by = time_var) |>
      dplyr::mutate(y_position = y_position + annotation_offset)
  }

  if (!any(is.finite(lower))) {
    stop("No finite outcomes are available to plot.", call. = FALSE)
  }
  min_val <- min(lower[is.finite(lower)])
  auto_ymin <- ifelse(min_val > 0, min_val - min_val * 0.1, min_val + min_val * 0.1)

  unique_times <- sort(unique(summary_df$time_num))
  if (!is.null(time_labels) && (!is.character(time_labels) || anyNA(time_labels) ||
      length(time_labels) != length(unique_times))) {
    stop("`time_labels` must have one character label per plotted time point.", call. = FALSE)
  }
  min_time <- min(unique_times, na.rm = TRUE)
  max_time <- max(unique_times, na.rm = TRUE)
  x_padding <- max(1, (max_time - min_time) * 0.05)

  p <- summary_df |>
    ggplot2::ggplot(ggplot2::aes(x = time_num, y = target_mean,
                                   group = !!group_sym, color = !!group_sym, shape = !!group_sym))
  if (show_ci) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = target_mean - 1.96 * target_se, ymax = target_mean + 1.96 * target_se),
      width = (max_time - min_time) * 0.02,
      na.rm = TRUE)
  }
  p <- p +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = unique_times,
      labels = if (is.null(time_labels)) ggplot2::waiver() else time_labels,
      expand = if (is.null(time_labels)) c(0, 0) else ggplot2::expansion(mult = 0.08)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(min_time - x_padding / 2, max_time + x_padding),
                              ylim = c(auto_ymin, NA), clip = "off") +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 15, r = 15, b = 15, l = 15)
    )

  if (!is.null(test_method) && nrow(annotation_df) > 0L) {
    p <- p +
      ggplot2::geom_text(
        data = annotation_df,
        ggplot2::aes(x = time_num, y = y_position, label = significance),
        inherit.aes = FALSE,
        color = "black",
        size = label_size,
        family = if (is.null(latin_family)) "" else latin_family,
        fontface = "bold",
        vjust = 0
      )
  }

  if (is_single) {
    p <- p +
      ggplot2::scale_color_manual(values = single_color) +
      ggplot2::theme(legend.position = "none")
    summary_df <- summary_df |> dplyr::select(-dplyr::all_of(group_var))
  } else {
    p <- p +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(color = legend_title, shape = legend_title) +
      ggplot2::guides(
        color = ggplot2::guide_legend(title.position = "left"),
        shape = ggplot2::guide_legend(title.position = "left")
      ) +
      ggplot2::theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = ggplot2::element_text(size = 10, margin = ggplot2::margin(r = 10)),
        legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
        legend.background = ggplot2::element_rect(fill = "transparent", color = NA)
      )
  }

  p <- plot_font_theme(p, font_family, latin_family)
  if (show_print) {
    print(p)
  }
  invisible(list(summary_data = summary_df, test_data = test_df, plot = p))
}


meanse_paired_tests <- function(data, target_var, time_var, id_var,
                                ref_time, test_method, event_value) {
  if (!is.character(id_var) || length(id_var) != 1L || is.na(id_var) ||
      !id_var %in% names(data)) {
    stop("Single-group tests require `id_var` naming a subject ID column.", call. = FALSE)
  }
  if (length(ref_time) != 1L || anyNA(ref_time) ||
      !as.character(ref_time) %in% as.character(data[[time_var]])) {
    stop("`ref_time` must match one observed value in the time column.", call. = FALSE)
  }
  if (anyNA(data[[id_var]])) {
    stop("Subject IDs must not be missing for paired tests.", call. = FALSE)
  }
  outcome <- data[[target_var]]
  if ((!is.numeric(outcome) && !is.logical(outcome)) ||
      any(!is.finite(outcome) & !is.na(outcome))) {
    stop("Paired tests require numeric or logical outcomes with finite observed values.", call. = FALSE)
  }
  if (test_method == "mcnemar") {
    if ((!is.numeric(event_value) && !is.logical(event_value)) ||
        length(event_value) != 1L || !is.finite(event_value) || event_value == 0) {
      stop("`event_value` must be a finite nonzero numeric event code.", call. = FALSE)
    }
    if (any(!is.na(outcome) & !outcome %in% c(0, event_value))) {
      stop("McNemar outcomes must be 0, `event_value`, or missing.", call. = FALSE)
    }
  }

  values <- tibble::tibble(
    .pair_id = data[[id_var]],
    .visit = data[[time_var]],
    .value = outcome
  ) |>
    dplyr::filter(!is.na(.data$.visit))
  if (anyDuplicated(values[c(".pair_id", ".visit")])) {
    stop("Each subject must have at most one row per time point for paired tests.", call. = FALSE)
  }
  visits <- unique(as.character(values$.visit))
  numeric_visits <- suppressWarnings(readr::parse_number(visits))
  if (any(!is.finite(numeric_visits)) || anyDuplicated(numeric_visits)) {
    stop("Paired-test time values must map to distinct finite numeric positions.", call. = FALSE)
  }
  if (length(visits) < 2L) {
    stop("Paired tests require at least one non-reference time point.", call. = FALSE)
  }
  reference <- values |>
    dplyr::filter(as.character(.data$.visit) == as.character(ref_time)) |>
    dplyr::transmute(.pair_id = .data$.pair_id, .reference = .data$.value)

  result <- values |>
    dplyr::filter(as.character(.data$.visit) != as.character(ref_time)) |>
    dplyr::left_join(reference, by = ".pair_id") |>
    dplyr::group_by(.data$.visit) |>
    dplyr::group_modify(function(.x, .y) {
      pairs <- .x[stats::complete.cases(.x[c(".value", ".reference")]), ]
      differences <- pairs$.value - pairs$.reference
      paired_n <- nrow(pairs)
      status <- "ok"
      p_value <- NA_real_
      if (paired_n == 0L) {
        status <- "no_pairs"
      } else if (test_method == "mcnemar") {
        gained <- sum(pairs$.value == event_value & pairs$.reference == 0)
        lost <- sum(pairs$.value == 0 & pairs$.reference == event_value)
        p_value <- if (gained + lost == 0L) 1 else stats::binom.test(gained, gained + lost)$p.value
      } else if (paired_n < 2L) {
        status <- "insufficient_pairs"
      } else if (test_method == "t") {
        if (stats::sd(differences) == 0) {
          status <- "zero_variance"
        } else {
          p_value <- stats::t.test(differences)$p.value
        }
      } else if (all(differences == 0)) {
        p_value <- 1
      } else {
        p_value <- stats::wilcox.test(differences, exact = FALSE)$p.value
      }
      data.frame(
        reference_time = ref_time,
        paired_n = paired_n,
        method = test_method,
        p_value = p_value,
        status = status,
        stringsAsFactors = FALSE
      )
    }) |>
    dplyr::ungroup()
  names(result)[names(result) == ".visit"] <- time_var
  result
}

#' Plot stacked percentage bar chart
#'
#' @description
#' Creates a stacked bar chart showing the percentage distribution of a continuous variable
#' categorized into intervals across time points.
#'
#' @param data A data frame.
#' @param target_var Character string. Name of the continuous variable to categorize.
#' @param time_var Character string. Name of the time variable for the x-axis.
#'   Strongly recommended to be a factor with correct level ordering.
#' @param group_var Character string or `NULL`. Optional grouping variable. When supplied,
#'   one stacked bar is drawn for each group within every time point, and percentages are
#'   calculated separately for each time-by-group combination. Default is `NULL`.
#' @param breaks Numeric vector. Breakpoints for cutting `target_var` into categories.
#'   Must have one more element than `labels`.
#' @param labels Character vector. Labels for the categories.
#' @param colors Character vector. Fill colors for each category. Must equal length of `labels`.
#' @param right Logical. If `TRUE` (default), intervals are (a, b]; if `FALSE`, [a, b).
#' @param legend_title Character string. Legend title. Default is `"Range"`.
#' @param xlab Character string. X-axis label. Default is `"随访时间（月）"`.
#' @param ylab Character string. Y-axis label. Default is `"百分比 (%)"`.
#' @param label_threshold Numeric. Minimum proportion to display percentage labels inside bars.
#'   Default is `0.03`.
#' @param label_size Numeric. Font size of percentage labels inside the bars.
#'   Default is `4`.
#' @param show_print Logical. Automatically print the plot when `TRUE` (default).
#'   Set to `FALSE` to return the result without opening a graphics device.
#' @inheritParams plot_meanse
#'
#' @return Invisibly returns a list with:
#'   - `summary_data`: data frame of computed percentages
#'   - `plot`: the ggplot object
#'
#' @examples
#' \dontrun{
#' plot_stacked(
#'   data = my_data,
#'   target_var = "weight",
#'   time_var = "Time_str",
#'   breaks = c(-Inf, 50, 100, 200, Inf),
#'   labels = c("<=50g", "51-100g", "101-200g", ">200g"),
#'   colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F")
#' )
#'
#' # Grouped stacked bars
#' plot_stacked(
#'   data = ChickWeight,
#'   target_var = "weight",
#'   time_var = "Time",
#'   breaks = c(-Inf, 50, 100, 200, Inf),
#'   labels = c("<=50g", "51-100g", "101-200g", ">200g"),
#'   colors = c("#B5D1E8", "#A3D9A5", "#F2C68F", "#EB938F"),
#'   group_var = "Diet"
#' )
#' }
#'
#' @export
plot_stacked <- function(data,
                         target_var,
                         time_var = "访视标签",
                         breaks,
                         labels,
                         colors,
                         right = TRUE,
                         legend_title = "Range",
                         xlab = "随访时间（月）",
                         ylab = "百分比 (%)",
                         label_threshold = 0.03,
                         group_var = NULL,
                         label_size = 4,
                         show_print = TRUE,
                         font_family = getOption("medstats.font_family", NULL),
                         latin_family = getOption("medstats.latin_family", NULL)) {

  if (!is.logical(show_print) || length(show_print) != 1L || is.na(show_print)) {
    stop("`show_print` must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(breaks) != length(labels) + 1L) {
    stop("`breaks` must contain exactly one more value than `labels`.", call. = FALSE)
  }
  if (length(colors) != length(labels)) {
    stop("`colors` must have the same length as `labels`.", call. = FALSE)
  }
  if (!is.null(group_var) && (!is.character(group_var) || length(group_var) != 1L)) {
    stop("`group_var` must be `NULL` or a single column name.", call. = FALSE)
  }
  if (!is.numeric(label_size) || length(label_size) != 1L ||
      !is.finite(label_size) || label_size <= 0) {
    stop("`label_size` must be a single positive number.", call. = FALSE)
  }

  target_sym <- rlang::sym(target_var)
  time_sym <- rlang::sym(time_var)
  is_grouped <- !is.null(group_var)
  group_sym <- if (is_grouped) rlang::sym(group_var) else NULL
  names(colors) <- labels

  analysis_data <- data |>
    dplyr::filter(!is.na(!!target_sym), !is.na(!!time_sym))

  if (is_grouped) {
    analysis_data <- analysis_data |>
      dplyr::filter(!is.na(!!group_sym))
  }

  analysis_data <- analysis_data |>
    dplyr::mutate(
      category = cut(
        !!target_sym,
        breaks = breaks,
        labels = labels,
        right = right
      ),
      category = factor(category, levels = rev(labels))
    )

  if (is_grouped) {
    plot_data <- analysis_data |>
      dplyr::group_by(!!time_sym, !!group_sym, category) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::group_by(!!time_sym, !!group_sym)
  } else {
    plot_data <- analysis_data |>
      dplyr::group_by(!!time_sym, category) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::group_by(!!time_sym)
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      total = sum(n),
      pct = n / total,
      label_text = ifelse(pct > label_threshold, sprintf("%.1f%%", pct * 100), "")
    ) |>
    dplyr::ungroup()

  if (is_grouped) {
    ordered_values <- function(x) {
      if (is.factor(x)) {
        present <- unique(as.character(x))
        return(levels(x)[levels(x) %in% present])
      }
      as.character(sort(unique(x), na.last = NA))
    }

    time_levels <- ordered_values(analysis_data[[time_var]])
    group_levels <- ordered_values(analysis_data[[group_var]])
    n_groups <- length(group_levels)
    group_step <- 0.8 / n_groups

    axis_data <- data.frame(
      .stack_time = rep(time_levels, each = n_groups),
      .stack_group = rep(group_levels, times = length(time_levels)),
      .time_index = rep(seq_along(time_levels), each = n_groups),
      .group_index = rep(seq_along(group_levels), times = length(time_levels)),
      stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(
        .stack_x = .time_index + (.group_index - (n_groups + 1) / 2) * group_step,
        .stack_label = paste(.stack_time, .stack_group, sep = "\n")
      )

    chart_data <- plot_data |>
      dplyr::mutate(
        .stack_time = as.character(!!time_sym),
        .stack_group = as.character(!!group_sym)
      ) |>
      dplyr::left_join(
        axis_data |>
          dplyr::select(.stack_time, .stack_group, .stack_x),
        by = c(".stack_time", ".stack_group")
      )

    x_mapping <- rlang::sym(".stack_x")
    bar_width <- group_step * 0.9
  } else {
    chart_data <- plot_data
    x_mapping <- time_sym
    bar_width <- 0.75
  }

  p_bar <- ggplot2::ggplot(chart_data, ggplot2::aes(x = !!x_mapping, y = pct, fill = category)) +
    ggplot2::geom_col(width = bar_width, color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_text),
      position = ggplot2::position_stack(vjust = 0.5),
      color = "white",
      size = label_size,
      family = if (is.null(latin_family)) "sans" else latin_family
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 1, by = 0.25),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 11, color = "black"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.5),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.8),
      plot.margin = ggplot2::margin(t = 15, r = 15, b = 15, l = 15)
    )

  if (is_grouped) {
    p_bar <- p_bar +
      ggplot2::scale_x_continuous(
        breaks = axis_data$.stack_x,
        labels = axis_data$.stack_label,
        expand = ggplot2::expansion(mult = c(0.03, 0.03))
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9))
  }

  if (!is_grouped) {
    p_bar <- p_bar + if (is.numeric(analysis_data[[time_var]])) {
      ggplot2::scale_x_continuous()
    } else {
      ggplot2::scale_x_discrete()
    }
  }
  p_bar <- plot_font_theme(p_bar, font_family, latin_family)
  if (show_print) {
    print(p_bar)
  }
  invisible(list(summary_data = plot_data, plot = p_bar))
}
