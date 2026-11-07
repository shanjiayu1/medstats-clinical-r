test_that("stacked plots can be returned without touching graphics devices", {
  data <- data.frame(value = c(1, 2, 1, 2), time = rep(1:2, each = 2))
  device <- grDevices::dev.cur()
  result <- withVisible(plot_stacked(
    data, "value", "time", breaks = c(0, 1.5, 3),
    labels = c("Low", "High"), colors = c("blue", "red"),
    xlab = "Time", ylab = "Percent", show_print = FALSE
  ))
  expect_identical(result$visible, FALSE)
  expect_identical(grDevices::dev.cur(), device)
  expect_equal(result$value$summary_data$pct, rep(0.5, 4))
  expect_s3_class(result$value$plot, "ggplot")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  printed <- plot_stacked(
    data, "value", "time", breaks = c(0, 1.5, 3),
    labels = c("Low", "High"), colors = c("blue", "red"),
    xlab = "Time", ylab = "Percent"
  )
  expect_equal(printed$summary_data, result$value$summary_data)
  expect_equal(ggplot2::ggplot_build(printed$plot)$data,
               ggplot2::ggplot_build(result$value$plot)$data)
})
