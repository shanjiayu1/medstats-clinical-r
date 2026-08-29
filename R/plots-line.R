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
#' @param test_method Character string or `NULL`. For a plot with exactly two groups,
#'   use `"t"` for Welch's two-sample t-test or `"wilcox"` for the Wilcoxon rank-sum
#'   test at each time point. Significant results are marked with stars. Default is `NULL`.
#'
#' @return Invisibly returns a list with:
#'   - `summary_data`: data frame of computed means, SDs, SEs
#'   - `test_data`: time-specific test results, or `NULL` when no test is requested
#'   - `plot`: the ggplot object
#'
#' @details
#' Time variable must contain extractable numeric values via `readr::parse_number()`.
#' For multi-group plots, ensure `colors` has enough values for all groups.
#' Significance labels use `*` for p < 0.05, `**` for p < 0.01, `***` for
#' p < 0.001, and `****` for p < 0.0001. Tests require exactly two groups.
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
#' }
#'
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
                        test_method = NULL) {

  is_single <- is.null(group_var)

  if (!is.null(test_method)) {
    if (!is.character(test_method) || length(test_method) != 1L) {
      stop("`test_method` must be `NULL`, \"t\", or \"wilcox\".", call. = FALSE)
    }
    test_method <- tolower(test_method)
    if (test_method %in% c("t.test", "ttest")) test_method <- "t"
    if (test_method %in% c("wilcox.test", "wilcoxon")) test_method <- "wilcox"
    if (!test_method %in% c("t", "wilcox")) {
      stop("`test_method` must be `NULL`, \"t\", or \"wilcox\".", call. = FALSE)
    }
    if (is_single) {
      stop("`test_method` can only be used when `group_var` is supplied.", call. = FALSE)
    }
  }

  if (is_single) {
    group_var <- "dummy_group_var"
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

  test_df <- NULL
  annotation_df <- NULL

  if (!is.null(test_method)) {
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
      dplyr::ungroup() |>
      dplyr::mutate(
        significance = dplyr::case_when(
          is.na(p_value) ~ "",
          p_value < 0.0001 ~ "****",
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*",
          TRUE ~ ""
        ),
        time_num = readr::parse_number(as.character(!!time_sym))
      )

    plotted_values <- c(
      summary_df$target_mean - 1.96 * summary_df$target_se,
      summary_df$target_mean + 1.96 * summary_df$target_se,
      summary_df$target_mean
    )
    plotted_values <- plotted_values[is.finite(plotted_values)]
    y_span <- diff(range(plotted_values))
    annotation_offset <- if (is.finite(y_span) && y_span > 0) {
      y_span * 0.08
    } else {
      max(abs(plotted_values), 1) * 0.08
    }

    annotation_positions <- summary_df |>
      dplyr::mutate(
        .upper = dplyr::if_else(
          is.finite(target_mean + 1.96 * target_se),
          target_mean + 1.96 * target_se,
          target_mean
        )
      ) |>
      dplyr::group_by(!!time_sym) |>
      dplyr::summarise(y_position = max(.upper, na.rm = TRUE), .groups = "drop")

    annotation_df <- test_df |>
      dplyr::filter(significance != "") |>
      dplyr::left_join(annotation_positions, by = time_var) |>
      dplyr::mutate(y_position = y_position + annotation_offset)
  }

  min_val <- min(summary_df$target_mean - 1.96 * summary_df$target_se, na.rm = TRUE)
  auto_ymin <- ifelse(min_val > 0, min_val - min_val * 0.1, min_val + min_val * 0.1)

  unique_times <- sort(unique(summary_df$time_num))
  min_time <- min(unique_times, na.rm = TRUE)
  max_time <- max(unique_times, na.rm = TRUE)
  x_padding <- max(1, (max_time - min_time) * 0.05)

  p <- summary_df |>
    ggplot2::ggplot(ggplot2::aes(x = time_num, y = target_mean,
                                   group = !!group_sym, color = !!group_sym, shape = !!group_sym)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = target_mean - 1.96 * target_se, ymax = target_mean + 1.96 * target_se),
      width = (max_time - min_time) * 0.02,
      na.rm = TRUE) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = unique_times, expand = c(0, 0)) +
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
        size = 6,
        fontface = "bold",
        vjust = 0
      )
  }

  if (is_single) {
    p <- p +
      ggplot2::scale_color_manual(values = single_color) +
      ggplot2::theme(legend.position = "none")
    summary_df <- summary_df |> dplyr::select(-dummy_group_var)
  } else {
    p <- p +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(color = legend_title, shape = legend_title) +
      ggplot2::guides(
        color = ggplot2::guide_legend(title.position = "left", title.vjust = 0.5),
        shape = ggplot2::guide_legend(title.position = "left", title.vjust = 0.5)
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

  print(p)
  invisible(list(summary_data = summary_df, test_data = test_df, plot = p))
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
                         label_size = 4) {

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
      family = "sans"
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

  print(p_bar)
  invisible(list(summary_data = plot_data, plot = p_bar))
}
