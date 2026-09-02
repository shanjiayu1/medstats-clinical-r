#' Plot forest plot with broken axis
#'
#' @description
#' Creates a publication-quality forest plot based on the `forestplot` package, designed
#' for medical statistical results (OR/HR/RR/Beta). Automatically extracts confidence
#' interval values from text strings and implements a "broken axis" effect between the
#' table and the forest plot.
#'
#' @param data A data frame containing text data to display on the left side of the plot.
#' @param ci_column Character or integer. Column name or index containing CI strings like
#'   `"1.25 (1.05-1.55)"`. Default is `"OR (95% CI)"`.
#' @param zero Numeric or numeric vector. The vertical reference line position. If `NULL`
#'   (default), it is auto-detected from the column name (1 for OR/HR/RR, 0 for Beta/MD).
#' @param x_ticks Numeric vector. X-axis tick positions. Default is `c(0, 1, 2, 4, 6)`.
#' @param output_name Character string. Output PNG filename. Default is `"TCM_Forestplot.png"`.
#' @param width Numeric. Plot width in inches. Default is `7.5`.
#' @param xlab Character string. X-axis label. Default is `"Odds Ratio (95% CI)"`.
#' @param graph_ratio Numeric or `NULL`. Proportion of the available plot width used by
#'   the forest graph. Must be strictly between 0 and 1. If `NULL` (default), the width
#'   is determined automatically by `forestplot`.
#' @param boxsize Numeric. Size of the forest plot squares. Default is `0.25`.
#' @param col_box Character string. Color for boxes and lines. Default is `"#1f77b4"`.
#'
#' @return Invisibly returns `NULL`. The plot is saved to `output_name`.
#'
#' @details
#' **Smart CI extraction**: The function uses regex to parse point estimates and CI bounds
#' from strings like `"1.25 (1.05-1.55)"` or `"1.00 (Ref)"`.
#'
#' **Smart zero line**: When `zero = NULL`, the function auto-detects from the column name
#' whether to use 1 (for OR/HR/RR/Ratio) or 0 (for Beta/MD).
#'
#' @note
#' To preserve leading spaces in variable names, use `read_excel(..., trim_ws = FALSE)`.
#'
#' @import forestplot
#' @import grid
#' @export
plot_forest <- function(data,
                        ci_column = "OR (95% CI)",
                        zero = NULL,
                        x_ticks = c(0, 1, 2, 4, 6),
                        output_name = "TCM_Forestplot.png",
                        width = 7.5,
                        xlab = "Odds Ratio (95% CI)",
                        graph_ratio = NULL,
                        boxsize = 0.25,
                        col_box = "#1f77b4") {

  if (is.character(ci_column) && !(ci_column %in% colnames(data))) {
    stop(paste("列名", ci_column, "不存在于数据中！请检查。"))
  }

  if (!is.null(graph_ratio) &&
      (!is.numeric(graph_ratio) || length(graph_ratio) != 1L ||
       is.na(graph_ratio) || !is.finite(graph_ratio) ||
       graph_ratio <= 0 || graph_ratio >= 1)) {
    stop("`graph_ratio` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  graph_width <- if (is.null(graph_ratio)) {
    "auto"
  } else {
    grid::unit(graph_ratio, "npc")
  }

  actual_ci_name <- ifelse(is.numeric(ci_column), colnames(data)[ci_column], ci_column)
  ci_text <- as.character(data[[ci_column]])

  # 自动提取 mean, lower, upper
  extracted_means  <- rep(NA, length(ci_text))
  extracted_lowers <- rep(NA, length(ci_text))
  extracted_uppers <- rep(NA, length(ci_text))

  for (i in seq_along(ci_text)) {
    val <- ci_text[i]
    if (is.na(val) || trimws(val) == "") next
    nums <- as.numeric(unlist(regmatches(val, gregexpr("-?\\d+\\.?\\d*", val))))
    if (length(nums) >= 3) {
      extracted_means[i]  <- nums[1]
      extracted_lowers[i] <- nums[2]
      extracted_uppers[i] <- nums[3]
    } else if (length(nums) %in% c(1, 2)) {
      extracted_means[i]  <- nums[1]
    }
  }

  means  <- c(NA, extracted_means)
  lowers <- c(NA, extracted_lowers)
  uppers <- c(NA, extracted_uppers)

  # 智能判断基准线
  if (is.null(zero)) {
    if (grepl("OR|HR|RR|Ratio", actual_ci_name, ignore.case = TRUE)) {
      plot_zero <- 1
    } else {
      plot_zero <- 0
    }
  } else {
    plot_zero <- zero
  }

  # 文字矩阵
  headers <- colnames(data)
  text_matrix <- as.matrix(data)
  text_matrix[is.na(text_matrix)] <- ""
  label_matrix <- rbind(headers, text_matrix)
  ncol_text <- ncol(data)

  is_summary_vec <- rep(FALSE, nrow(label_matrix))
  total_rows <- nrow(label_matrix)
  align_vec <- c("l", rep("c", ncol_text - 1))

  # 底线设置
  hrzl_lines_list <- list(
    "1" = grid::gpar(lty = 1, lwd = 2),
    "2" = grid::gpar(lty = 1, lwd = 2)
  )
  hrzl_lines_list[[as.character(total_rows + 1)]] <- grid::gpar(lwd = 2, lty = 1, columns = 1:ncol_text)

  # 绘图与导出
  if (!is.null(grDevices::dev.list())) grDevices::dev.off()
  dynamic_height <- 1.05 + total_rows * 0.25

  grDevices::png(output_name, width = width, height = dynamic_height, units = "in", res = 300)
  grid::grid.newpage()

  p <- forestplot::forestplot(
    labeltext  = label_matrix,
    align      = align_vec,
    mean       = means,
    lower      = lowers,
    upper      = uppers,
    zero       = plot_zero,
    lwd.zero   = 1,
    lty.zero   = 2,
    boxsize    = boxsize,
    graph.pos  = ncol_text + 1,
    graphwidth = graph_width,
    mar        = grid::unit(c(0, 2, 3, 2), "mm"),
    xlab       = xlab,
    xticks     = x_ticks,
    clip       = c(min(x_ticks), max(x_ticks)),
    is.summary = is_summary_vec,
    hrzl_lines = hrzl_lines_list,
    txt_gp = forestplot::fpTxtGp(
      label = grid::gpar(cex = 0.95, fontface = "plain"),
      ticks = grid::gpar(cex = 0.9),
      xlab  = grid::gpar(cex = 0.8)
    ),
    lwd.ci      = 1.5,
    lwd.xaxis   = 2,
    lty.ci      = 1,
    ci.vertices = FALSE,
    lineheight  = grid::unit(7, "mm"),
    colgap      = grid::unit(4, "mm"),
    fn.ci_norm  = "fpDrawDiamondCI",
    col = forestplot::fpColors(box = col_box, lines = col_box, zero = "gray40")
  )

  print(p)
  grDevices::dev.off()

  message(paste0("绘图完成！根据列名 ['", actual_ci_name, "']，基准线 (zero) 设定为: ",
                 paste(plot_zero, collapse = ", "), "\n文件保存至: ", output_name))
  invisible(NULL)
}


#' Plot Kaplan-Meier survival curve
#'
#' @description
#' Generates a Kaplan-Meier cumulative incidence curve with optional risk table,
#' supporting both single-group and multi-group analyses.
#'
#' @param data A data frame containing survival data.
#' @param group_var Character string or NULL or `"1"`. Name of the grouping variable.
#'   Use `NULL` or `"1"` for single-group analysis. Default is `"1"`.
#' @param time_var Character string. Name of the survival time variable.
#'   Default is `"time_to_mse"`.
#' @param status_var Character string. Name of the event status variable (1 = event).
#'   Default is `"mse_status"`.
#' @param legend_labs Character vector or NULL. Custom labels for legend. Default is `NULL`.
#' @param legend_title Character string. Legend title. Default is `"组别"`.
#' @param xlab Character string. X-axis label. Default is `"随访时间 (月)"`.
#' @param ylab Character string. Y-axis label. Default is `"累积达成 MSE 的患者比例 (%)"`.
#' @param palette Character vector. Colors for groups. Default is `c("#B5D1E8", "#EB938F", "#A3D9A5")`.
#' @param xlim Numeric vector. X-axis limits. Default is `c(0, 12)`.
#' @param break_time Numeric. Time interval for x-axis breaks. Default is `3`.
#' @param pval_coord Numeric vector. Coordinates for p-value annotation. Default is `c(8, 0.2)`.
#' @param show_risk_table Logical. Whether to show risk table below the plot. Default is `FALSE`.
#' @param save_filename Character string. Output filename for the saved plot.
#'   Default is `"KM累积发生率曲线.png"`.
#' @param plot_width Numeric. Plot width in inches. Default is `8`.
#' @param plot_height Numeric. Plot height in inches. Default is `6`.
#' @param plot_dpi Numeric. Plot resolution (DPI). Default is `600`.
#'
#' @return Invisibly returns `NULL`. The plot is displayed and saved to `save_filename`.
#'
#' @examples
#' \dontrun{
#' library(survival)
#' lung$status2 <- ifelse(lung$status == 2, 1, 0)
#' plot_km(lung, group_var = "sex", time_var = "time", status_var = "status2")
#' }
#'
#' @export
plot_km <- function(data,
                    group_var = "1",
                    time_var = "time_to_mse",
                    status_var = "mse_status",
                    legend_labs = NULL,
                    legend_title = "组别",
                    xlab = "随访时间 (月)",
                    ylab = "累积达成 MSE 的患者比例 (%)",
                    palette = c("#B5D1E8", "#EB938F", "#A3D9A5"),
                    xlim = c(0, 12),
                    break_time = 3,
                    pval_coord = c(8, 0.2),
                    show_risk_table = FALSE,
                    save_filename = "KM累积发生率曲线.png",
                    plot_width = 8,
                    plot_height = 6,
                    plot_dpi = 600) {

  is_single_group <- is.null(group_var) || group_var == "1" || group_var == ""

  if (is_single_group) {
    formula_str <- paste0("survival::Surv(", time_var, ", ", status_var, ") ~ 1")
  } else {
    formula_str <- paste0("survival::Surv(", time_var, ", ", status_var, ") ~ `", group_var, "`")
  }
  surv_formula <- as.formula(formula_str)

  fit <- survminer::surv_fit(surv_formula, data = data)

  show_pval <- ifelse(is_single_group, FALSE, TRUE)
  rt_col <- ifelse(is_single_group, "black", "strata")
  leg_pos <- ifelse(is_single_group, "none", "top")

  final_legend_title <- if (is_single_group) NULL else legend_title
  final_legend_labs <- if (!is_single_group && !is.null(legend_labs)) legend_labs else NULL

  p_km <- survminer::ggsurvplot(
    fit,
    data = data,
    fun = "event",
    pval = show_pval,
    pval.coord = pval_coord,
    conf.int = TRUE,
    risk.table = show_risk_table,
    risk.table.col = rt_col,
    risk.table.height = 0.25,
    xlim = xlim,
    break.time.by = break_time,
    xlab = xlab,
    ylab = ylab,
    palette = palette,
    legend.title = final_legend_title,
    legend.labs = final_legend_labs,
    legend = leg_pos,
    ggtheme = ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        legend.position = leg_pos,
        axis.text = ggplot2::element_text(size = 11, color = "black"),
        axis.title = ggplot2::element_text(face = "bold", size = 12)
      )
  )

  p_km$plot <- suppressMessages(
    p_km$plot +
      ggplot2::scale_y_continuous(
        labels = function(x) x * 100,
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        expand = c(0, 0)
      )
  )

  suppressWarnings(suppressMessages({
    ggplot2::ggsave(filename = save_filename,
                    plot = print(p_km),
                    width = plot_width,
                    height = plot_height,
                    dpi = plot_dpi,
                    bg = "white")
  }))

  suppressWarnings(suppressMessages(print(p_km)))
  invisible(NULL)
}


#' Plot Sankey (alluvial) diagram
#'
#' @description
#' Creates a Sankey/alluvial diagram showing patient state transitions across time points,
#' commonly used in clinical research to visualize longitudinal trajectory patterns.
#'
#' @param data A data frame in long format.
#' @param id_var Character string. Name of the subject identifier column.
#' @param time_var Character string. Name of the time point column. Should be a factor
#'   with correct level ordering.
#' @param state_var Character string. Name of the state/category column.
#' @param colors Character vector. Fill colors for each state. Default includes 8 colors.
#' @param na_strategy Character. How to handle missing data: `"show"` (display as
#'   `missing_label`) or `"drop"` (remove). Default is `"show"`.
#' @param missing_label Character string. Label for missing/drop-out states.
#'   Default is `"Drop-out (失访)"`.
#' @param xlab Character string. X-axis label. Default is `"随访时间点"`.
#' @param ylab Character string. Y-axis label. Default is `"患者频数"`.
#' @param legend_label Character string. Legend title. Default is `"状态"`.
#' @param plot_width Numeric. Plot width in inches. Default is `10`.
#' @param plot_height Numeric. Plot height in inches. Default is `8`.
#' @param dpi Numeric. Plot resolution. Default is `300`.
#'
#' @return Invisibly returns a ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_sankey(
#'   data = my_data,
#'   id_var = "Subject",
#'   time_var = "Visit",
#'   state_var = "Status",
#'   na_strategy = "show"
#' )
#' }
#'
#' @export
plot_sankey <- function(data,
                        id_var,
                        time_var,
                        state_var,
                        colors = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#80B1D3",
                                   "#FDB462", "#FB8072", "#D9D9D9", "#FCCDE5"),
                        na_strategy = "show",
                        missing_label = "Drop-out (失访)",
                        xlab = "随访时间点",
                        ylab = "患者频数",
                        legend_label = "状态",
                        plot_width = 10,
                        plot_height = 8,
                        dpi = 300) {

  tryCatch({
    id_sym    <- rlang::sym(id_var)
    time_sym  <- rlang::sym(time_var)
    state_sym <- rlang::sym(state_var)

    clean_data <- data |> dplyr::filter(!is.na(!!id_sym), !is.na(!!time_sym))
    time_levels <- if (is.factor(clean_data[[time_var]])) levels(clean_data[[time_var]]) else sort(unique(clean_data[[time_var]]))
    clean_state <- clean_data[[state_var]][!is.na(clean_data[[state_var]])]
    state_levels <- if (is.factor(clean_state)) levels(clean_state) else sort(unique(clean_state))

    if (na_strategy == "show" && !(missing_label %in% state_levels)) {
      state_levels <- c(state_levels, missing_label)
    }

    traj_wide <- clean_data |>
      dplyr::mutate(!!time_sym := factor(!!time_sym, levels = time_levels)) |>
      dplyr::select(!!id_sym, !!time_sym, !!state_sym) |>
      dplyr::arrange(!!id_sym, !!time_sym) |>
      tidyr::pivot_wider(names_from = !!time_sym, values_from = !!state_sym)

    if (na_strategy == "drop") {
      traj_wide <- traj_wide |> tidyr::drop_na()
    } else if (na_strategy == "show") {
      traj_wide <- traj_wide |>
        dplyr::mutate(dplyr::across(-!!id_sym, ~ as.character(.))) |>
        dplyr::mutate(dplyr::across(-!!id_sym, ~ ifelse(is.na(.), missing_label, .)))
    }

    plot_data <- traj_wide |>
      dplyr::group_by(dplyr::across(-!!id_sym)) |>
      dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(trajectory_id = as.character(dplyr::row_number())) |>
      tidyr::pivot_longer(cols = -c(trajectory_id, freq), names_to = time_var, values_to = state_var)

    if (na_strategy != "show") {
      plot_data <- plot_data |> dplyr::filter(!is.na(!!state_sym))
    }

    plot_data <- plot_data |>
      dplyr::mutate(
        !!time_sym := factor(!!time_sym, levels = time_levels),
        !!state_sym := factor(!!state_sym, levels = state_levels)
      )

    if (na_strategy == "show") {
      valid_states <- setdiff(state_levels, missing_label)
      color_mapping <- stats::setNames(colors[1:length(valid_states)], valid_states)
      color_mapping[missing_label] <- "#E5E5E5"
      fill_scale <- ggplot2::scale_fill_manual(values = color_mapping)
    } else {
      fill_scale <- ggplot2::scale_fill_manual(values = colors)
    }

    p <- ggplot2::ggplot(plot_data,
                          ggplot2::aes(x = !!time_sym, y = freq, stratum = !!state_sym)) +
      ggalluvial::geom_flow(ggplot2::aes(alluvium = trajectory_id, fill = !!state_sym),
                             width = 4/12, alpha = 0.5, color = "white") +
      ggalluvial::geom_stratum(ggplot2::aes(fill = !!state_sym), width = 4/12, color = "grey") +
      ggalluvial::stat_stratum(
        geom = "label",
        ggplot2::aes(label = ggplot2::after_stat(stratum)),
        size = 3.5,
        fill = "white",
        color = "black"
      ) +
      fill_scale +
      ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::labs(x = xlab, y = ylab, fill = legend_label) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "right",
        legend.title = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 10, color = "black"),
        panel.grid = ggplot2::element_blank()
      )

    print(p)
    invisible(p)

  }, error = function(e) {
    cat("错误: 绘制桑基图时出错:", conditionMessage(e), "\n")
    return(NULL)
  })
}
