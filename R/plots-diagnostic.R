#' Plot restricted cubic spline (RCS) curve
#'
#' @description
#' Generates a restricted cubic spline curve for logistic, linear, or Cox models,
#' displaying the dose-response relationship between an exposure and outcome.
#' Automatically computes ANOVA p-values and nonlinearity p-values.
#'
#' @param data A data frame.
#' @param exposure Character string. Name of the exposure variable.
#' @param outcome Character string. Name of the outcome variable. For Cox models, use
#'   `"Surv(time, status)"` syntax.
#' @param covars Character vector or NULL. Names of covariates to adjust for. Default is `NULL`.
#' @param nk Integer. Number of knots for the RCS. Default is `4`.
#' @param model_type Character. One of `"logistic"`, `"linear"`, or `"cox"`. Default is `"logistic"`.
#' @param xlab Character string or NULL. X-axis label. If `NULL`, uses `exposure`. Default is `NULL`.
#' @param ylab Character string or NULL. Y-axis label. If `NULL`, auto-generated based on
#'   `model_type`. Default is `NULL`.
#' @param xlim Numeric vector or NULL. X-axis limits. Default is `NULL`.
#' @param ylim Numeric vector or NULL. Y-axis limits. Default is `NULL` (auto-computed).
#' @param trim_quantile Numeric vector. Quantiles for trimming extreme exposure values.
#'   Default is `c(0.1, 0.9)`.
#' @param line_color Character string. Color of the main line. Default is `"#1F4E79"`.
#' @param ribbon_color Character string. Color of the confidence ribbon. Default is `"#6FA8DC"`.
#' @param ref_color Character string. Color of the reference line. Default is `"#7F7F7F"`.
#' @param line_size Numeric. Line width. Default is `1.2`.
#' @param ribbon_alpha Numeric. Ribbon transparency (0-1). Default is `0.22`.
#' @param base_size Numeric. Base font size for the theme. Default is `18`.
#'
#' @return Invisibly returns a list with:
#'   - `fit`: the fitted model object
#'   - `anova`: ANOVA results
#'   - `pred`: prediction object
#'   - `plot`: the ggplot object
#'   - `cross_x`: x-values where the curve crosses the reference line
#'
#' @details
#' - Categorical covariates must be pre-converted to factors.
#' - For logistic models, the outcome must be numeric 0/1.
#' - The function uses `rms::lrm()`, `rms::ols()`, or `rms::cph()` depending on `model_type`.
#'
#' @examples
#' \dontrun{
#' # Logistic model
#' plot_rcs(mtcars, exposure = "hp", outcome = "am", model_type = "logistic")
#'
#' # Linear model
#' plot_rcs(mtcars, exposure = "wt", outcome = "mpg", model_type = "linear")
#'
#' # Cox model
#' plot_rcs(lung, exposure = "age", outcome = "Surv(time, status)", model_type = "cox")
#' }
#'
#' @export
plot_rcs <- function(data,
                     exposure,
                     outcome,
                     covars = NULL,
                     nk = 4,
                     model_type = c("logistic", "linear", "cox"),
                     xlab = NULL,
                     ylab = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     trim_quantile = c(0.1, 0.9),
                     line_color = "#1F4E79",
                     ribbon_color = "#6FA8DC",
                     ref_color = "#7F7F7F",
                     line_size = 1.2,
                     ribbon_alpha = 0.22,
                     base_size = 18) {

  model_type <- match.arg(model_type)

  # 标签
  if (is.null(xlab)) xlab <- exposure
  if (is.null(ylab)) {
    ylab <- switch(
      model_type,
      logistic = "OR (95% CI)",
      linear   = "Predicted value (95% CI)",
      cox      = "HR (95% CI)"
    )
  }

  # datadist
  old_options <- options()
  on.exit(options(old_options), add = TRUE)
  dd <- rms::datadist(data)
  options(datadist = dd)

  # 构建公式
  rcs_term <- paste0("rms::rcs(", exposure, ", ", nk, ")")
  rhs <- if (!is.null(covars) && length(covars) > 0) {
    paste(c(rcs_term, covars), collapse = " + ")
  } else {
    rcs_term
  }
  if (model_type == "cox" && grepl("^\\s*Surv\\s*\\(", outcome)) {
    outcome <- sub("Surv\\s*\\(", "survival::Surv(", outcome)
  }
  fml <- as.formula(paste(outcome, "~", rhs))

  # 拟合模型
  fit <- switch(
    model_type,
    logistic = rms::lrm(fml, data = data),
    linear   = rms::ols(fml, data = data),
    cox      = rms::cph(fml, data = data, x = TRUE, y = TRUE)
  )

  # ANOVA
  anova_obj <- anova(fit)

  # Predict
  pred_obj <- switch(
    model_type,
    logistic = eval(parse(text = paste0("rms::Predict(fit, ", exposure, ", fun = exp, ref.zero = TRUE)"))),
    linear   = eval(parse(text = paste0("rms::Predict(fit, ", exposure, ")"))),
    cox      = eval(parse(text = paste0("rms::Predict(fit, ", exposure, ", fun = exp, ref.zero = TRUE)")))
  )

  plot_df <- as.data.frame(pred_obj)

  # 裁剪极端值
  lo_q <- quantile(data[[exposure]], trim_quantile[1], na.rm = TRUE)
  hi_q <- quantile(data[[exposure]], trim_quantile[2], na.rm = TRUE)
  plot_df <- plot_df[plot_df[[exposure]] >= lo_q & plot_df[[exposure]] <= hi_q, ]

  # 参考线
  ref_y <- ifelse(model_type == "linear", 0, 1)

  # 交叉点
  cross_idx <- which(diff(sign(plot_df$yhat - ref_y)) != 0)
  cross_x <- if (length(cross_idx) > 0) {
    sapply(cross_idx, function(i) {
      approx(
        c(plot_df$yhat[i], plot_df$yhat[i + 1]) - ref_y,
        c(plot_df[[exposure]][i], plot_df[[exposure]][i + 1]),
        xout = 0
      )$y
    })
  } else {
    NA
  }

  # 自动Y轴范围
  if (is.null(ylim)) {
    y_max_core <- max(plot_df$yhat, na.rm = TRUE)
    y_min_core <- min(plot_df$yhat, na.rm = TRUE)
    if (model_type == "linear") {
      ylim <- c(y_min_core - abs(y_min_core) * 0.3, y_max_core + abs(y_max_core) * 0.5)
    } else {
      ylim <- c(y_min_core * 0.9, y_max_core * 1.1)
    }
  }

  # 绘图
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[[exposure]], y = yhat)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                          fill = ribbon_color, alpha = ribbon_alpha) +
    ggplot2::geom_line(color = line_color, linewidth = line_size) +
    ggplot2::geom_hline(yintercept = ref_y, linetype = "dashed", color = ref_color)

  if (!all(is.na(cross_x))) {
    p <- p + ggplot2::geom_vline(xintercept = cross_x, linetype = "dotted", color = ref_color)
  }

  p <- p +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  # P值
  p_overall <- tryCatch(anova_obj[exposure, "P"], error = function(e) NA)
  p_nonlin <- tryCatch(anova_obj[" Nonlinear", "P"], error = function(e) NA)

  cross_text <- ""
  if (!all(is.na(cross_x))) {
    cross_text <- paste0("\nRef point: ", paste(sprintf("%.2f", cross_x), collapse = ", "))
  }
  append_text <- if (model_type == "linear") "" else cross_text
  anno_text <- paste0(
    "P overall = ", fmt_p(p_overall),
    "\nP nonlinear = ", fmt_p(p_nonlin),
    append_text
  )

  p <- p +
    ggplot2::annotate("text", x = Inf, y = Inf, label = anno_text,
                       hjust = 1, vjust = 1, size = base_size * 0.3)

  invisible(list(
    fit = fit,
    anova = anova_obj,
    pred = pred_obj,
    plot = p,
    cross_x = cross_x
  ))
}


#' Plot ROC curve with AUC and optimal cutoff
#'
#' @description
#' Generates a ROC curve with AUC, 95% CI, optimal cutoff (Youden Index),
#' and optional Hosmer-Lemeshow goodness-of-fit test.
#'
#' @param data A data frame.
#' @param true_var Character string. Name of the true binary outcome variable (0/1).
#' @param pred_var Character string. Name of the predicted probability variable.
#' @param title Character string. Plot title. Default is `"ROC curve"`.
#' @param line_color Character string. ROC curve color. Default is `"#2E86AB"`.
#' @param show_print Logical. Whether to print statistics to console. Default is `TRUE`.
#' @param xlab Character string. X-axis label. Default is
#'   `"1 - Specificity (False Positive Rate)"`.
#' @param ylab Character string. Y-axis label. Default is
#'   `"Sensitivity (True Positive Rate)"`.
#'
#' @return Invisibly returns a list with:
#'   - `roc_obj`: the pROC roc object
#'   - `auc`: AUC value
#'   - `ci`: 95% CI (lower, upper)
#'   - `best_threshold`: data frame with optimal cutoff, sensitivity, specificity
#'   - `hl_test`: Hosmer-Lemeshow test result
#'   - `plot`: the ggplot object
#'
#' @examples
#' \dontrun{
#' model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial)
#' mtcars$pred <- predict(model, type = "response")
#' plot_roc(mtcars, true_var = "am", pred_var = "pred")
#' }
#'
#' @export
plot_roc <- function(data,
                     true_var,
                     pred_var,
                     title = "ROC curve",
                     line_color = "#2E86AB",
                     show_print = TRUE,
                     xlab = "1 - Specificity (False Positive Rate)",
                     ylab = "Sensitivity (True Positive Rate)") {

  y_true <- data[[true_var]]
  y_pred <- data[[pred_var]]

  if (is.factor(y_true)) {
    y_true_num <- as.numeric(as.character(y_true))
  } else {
    y_true_num <- as.numeric(y_true)
  }

  roc_obj <- pROC::roc(y_true, y_pred, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci_val  <- as.numeric(pROC::ci.auc(roc_obj))

  best_thresh <- pROC::coords(roc_obj, "best", best.method = "youden",
                               ret = c("threshold", "sensitivity", "specificity"),
                               transpose = FALSE)
  if (is.matrix(best_thresh) || is.data.frame(best_thresh)) {
    best_thresh <- best_thresh[1, , drop = FALSE]
  }

  hl_test <- tryCatch({
    ResourceSelection::hoslem.test(y_true_num, y_pred, g = 10)
  }, error = function(e) {
    list(statistic = NA, p.value = NA)
  })

  if (show_print) {
    cat(sprintf("========== %s Performance Evaluation ==========\n", title))
    cat(sprintf("AUC: %.4f\n", auc_val))
    cat(sprintf("95%% CI: %.4f - %.4f\n", ci_val[1], ci_val[3]))
    cat(sprintf("\nOptimal cutoff (Youden index): %.4f\n", best_thresh$threshold))
    cat(sprintf("Sensitivity: %.4f\n", best_thresh$sensitivity))
    cat(sprintf("Specificity: %.4f\n\n", best_thresh$specificity))
    cat("Hosmer-Lemeshow test:\n")
    if (is.na(hl_test$p.value)) {
      cat("  Test failed (possibly due to sample size or predicted probability distribution).\n\n")
    } else {
      cat(sprintf("  Chi-square: %.4f\n", hl_test$statistic))
      cat(sprintf("  P-value: %.4f\n\n", hl_test$p.value))
    }
  }

  roc_data <- data.frame(
    sensitivity = roc_obj$sensitivities,
    specificity = roc_obj$specificities
  )

  hl_p_str <- ifelse(is.na(hl_test$p.value), "NA", sprintf("%.3f", hl_test$p.value))
  anno_text <- sprintf("AUC = %.3f\n95%% CI: %.3f - %.3f\nH-L test p = %s",
                       auc_val, ci_val[1], ci_val[3], hl_p_str)

  p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
    ggplot2::geom_line(color = line_color, linewidth = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::annotate("text", x = 0.55, y = 0.25, label = anno_text,
                       size = 4, hjust = 0, vjust = 1, color = "#333333") +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title = ggplot2::element_text(size = 11),
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3)
    )

  invisible(list(
    roc_obj = roc_obj,
    auc = auc_val,
    ci = c(ci_val[1], ci_val[3]),
    best_threshold = best_thresh,
    hl_test = hl_test,
    plot = p
  ))
}
