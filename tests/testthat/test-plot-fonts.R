test_that("plain labels receive mixed fonts without changing summaries", {
  data <- data.frame(time = rep(0:1, each = 3), value = 1:6)
  result <- plot_meanse(
    data, target_var = "value", time_var = "time", show_print = FALSE,
    font_family = "SimSun", latin_family = "Times New Roman",
    time_labels = c("基线", "周期1"), ylab = "平均ADL评分（分）"
  )
  original <- plot_meanse(
    data, target_var = "value", time_var = "time", show_print = FALSE,
    font_family = NULL, latin_family = NULL
  )
  expect_equal(result$summary_data, original$summary_data)
  expect_match(result$plot$labels$y, "<span.*ADL</span>")
  labels <- result$plot$scales$get_scales("x")$get_labels(c(0, 1))
  expect_match(labels[2], "周期<span.*1</span>")
  expect_equal(result$plot$theme$axis.text.y$family, "Times New Roman")
  expect_s3_class(result$plot$theme$axis.title.y, "element_markdown")
  expect_equal(plot_font_labels("A < 5 & B", "serif"),
               "<span style='font-family:serif'>A &lt; 5 &amp; B</span>")
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  expect_no_error(ggplot2::ggsave(file, result$plot, width = 6.2, height = 4))
})

test_that("stacked axes and legends use mixed fonts", {
  data <- data.frame(time = factor(c("周期1", "周期2")), value = 1:2)
  result <- plot_stacked(
    data, "value", "time", c(0, 1.5, 3), c("Low", "High"), c("blue", "red"),
    show_print = FALSE, font_family = "SimSun", latin_family = "Times New Roman"
  )
  labels <- result$plot$scales$get_scales("x")$labels(levels(data$time))
  expect_match(labels[1], "周期<span.*1</span>")
  expect_match(result$plot$scales$get_scales("fill")$get_labels(c("Low", "High"))[1],
               "<span.*Low</span>")
  expect_s3_class(result$plot$theme$legend.text, "element_markdown")
})

test_that("session font options apply without per-call font arguments", {
  previous <- options(medstats.font_family = "SimSun",
                       medstats.latin_family = "Times New Roman")
  on.exit(options(previous), add = TRUE)
  result <- plot_meanse(
    data.frame(time = rep(1:2, each = 2), value = 1:4),
    target_var = "value", time_var = "time", ylab = "ADL评分", show_print = FALSE
  )
  expect_match(result$plot$labels$y, "<span.*ADL</span>评分")
  expect_equal(result$plot$theme$text$family, "SimSun")
})
