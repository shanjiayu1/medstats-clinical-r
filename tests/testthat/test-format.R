test_that("format_flextable works with data.frame", {
  ft <- format_flextable(head(mtcars[, 1:4]))
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable works with flextable input", {
  ft_input <- flextable::flextable(head(iris))
  ft <- format_flextable(ft_input)
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable works with gtsummary input", {
  tbl <- gtsummary::tbl_summary(
    gtsummary::trial,
    include = c(age, grade, trt),
    by = trt
  )
  ft <- format_flextable(tbl)
  expect_s3_class(ft, "flextable")
})

test_that("format_flextable adds regression superscripts and footnotes", {
  result <- run_glm_auto(
    data = mtcars,
    vars = "hp",
    outcome_var = "mpg",
    covars = "wt",
    family = "gaussian"
  )

  ft <- format_flextable(result)
  estimate_header <- ft$header$content$data[
    1,
    "Beta (95% CI) [1]"
  ][[1]]
  first_footer <- ft$footer$content$data[1, "Variable"][[1]]
  second_footer <- ft$footer$content$data[2, "Variable"][[1]]

  expect_equal(estimate_header$txt, c("Beta (95% CI)", "1"))
  expect_equal(estimate_header$vertical.align[2], "superscript")
  expect_equal(first_footer$txt, c("1", " Univariable analysis."))
  expect_equal(first_footer$vertical.align[1], "superscript")
  expect_equal(second_footer$txt, c("2", " Multivariable analysis."))
  expect_equal(second_footer$vertical.align[1], "superscript")
  expect_equal(ft$footer$styles$pars$text.align$data[, 1], c("left", "left"))
})

test_that("export_word validates inputs", {
  expect_error(export_word("not_a_list", c("t1")), "list")
  expect_error(export_word(list(head(mtcars), head(iris)), c("only_one")), "do not match")
  expect_error(export_word(list(1), c("t1"), output_file = "test.txt"), ".docx")
  expect_error(export_word(list(1), "t1"), "Unsupported object")
  expect_error(
    export_word(list(head(mtcars)), "t1", figure_width = 0),
    "positive numbers"
  )
})

test_that("export_word accepts two objects and generates default table titles", {
  output_file <- tempfile(fileext = ".docx")
  extracted <- tempfile()
  on.exit(unlink(c(output_file, extracted), recursive = TRUE), add = TRUE)

  expect_no_error(export_word(head(mtcars), head(iris), output_file))
  expect_true(file.exists(output_file))

  utils::unzip(output_file, exdir = extracted)
  document_xml <- paste(
    readLines(file.path(extracted, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )
  expect_match(document_xml, "Table 1", fixed = TRUE)
  expect_match(document_xml, "Table 2", fixed = TRUE)
  expect_lt(regexpr("Table 1", document_xml, fixed = TRUE)[1],
            regexpr("Table 2", document_xml, fixed = TRUE)[1])
})

test_that("export_word puts figure titles below images", {
  output_file <- tempfile(fileext = ".docx")
  extracted <- tempfile()
  on.exit(unlink(c(output_file, extracted), recursive = TRUE), add = TRUE)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_no_error(export_word(
    data_list = list(head(mtcars), p),
    output_file = output_file,
    figure_res = 72
  ))

  utils::unzip(output_file, exdir = extracted)
  document_xml <- paste(
    readLines(file.path(extracted, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )
  image_position <- regexpr("<w:drawing>", document_xml, fixed = TRUE)[1]
  figure_title_position <- regexpr("Figure 1", document_xml, fixed = TRUE)[1]
  expect_gt(image_position, 0)
  expect_gt(figure_title_position, image_position)
})

test_that("export_word renders large and scales down inside Word", {
  output_file <- tempfile(fileext = ".docx")
  extracted <- tempfile()
  on.exit(unlink(c(output_file, extracted), recursive = TRUE), add = TRUE)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_no_error(export_word(
    data_list = list(p),
    table_titles = "Figure 1",
    output_file = output_file,
    figure_width = 9,
    figure_height = 7,
    figure_res = 100
  ))

  utils::unzip(output_file, exdir = extracted)
  media_file <- list.files(
    file.path(extracted, "word", "media"),
    pattern = "\\.png$",
    full.names = TRUE
  )
  expect_length(media_file, 1L)

  con <- file(media_file, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, where = 16L)
  pixel_width <- readBin(con, integer(), n = 1L, size = 4L, endian = "big")
  pixel_height <- readBin(con, integer(), n = 1L, size = 4L, endian = "big")
  expect_equal(c(pixel_width, pixel_height), c(900L, 700L))

  document_xml <- paste(
    readLines(file.path(extracted, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )
  expected_width_emu <- round((8.263889 - 2 * 0.9840278) * 914400)
  expected_height_emu <- round(expected_width_emu * 7 / 9)
  expect_match(document_xml, paste0("cx=\"", expected_width_emu, "\""))
  expect_match(document_xml, paste0("cy=\"", expected_height_emu, "\""))
})

test_that("export_word accepts package plot result lists", {
  output_file <- tempfile(fileext = ".docx")
  on.exit(unlink(output_file), add = TRUE)
  result <- list(
    plot = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point(),
    summary_data = head(mtcars)
  )

  expect_no_error(export_word(list(result), "Figure 1", output_file))
  expect_true(file.exists(output_file))
})

test_that("export_word preserves the original PNG aspect ratio", {
  image_file <- tempfile(fileext = ".png")
  output_file <- tempfile(fileext = ".docx")
  extracted <- tempfile()
  on.exit(
    unlink(c(image_file, output_file, extracted), recursive = TRUE),
    add = TRUE
  )
  grDevices::png(image_file, width = 900, height = 700)
  graphics::plot(mtcars$wt, mtcars$mpg)
  grDevices::dev.off()

  expect_no_error(export_word(
    data_list = list(image_file),
    table_titles = "Figure 1",
    output_file = output_file
  ))
  utils::unzip(output_file, exdir = extracted)
  document_xml <- paste(
    readLines(file.path(extracted, "word", "document.xml"), warn = FALSE),
    collapse = ""
  )
  usable_width <- 8.263889 - 2 * 0.9840278
  expected_width_emu <- round(usable_width * 914400)
  expected_height_emu <- round(usable_width * 7 / 9 * 914400)
  expect_match(document_xml, paste0("cx=\"", expected_width_emu, "\""))
  expect_match(document_xml, paste0("cy=\"", expected_height_emu, "\""))
})
