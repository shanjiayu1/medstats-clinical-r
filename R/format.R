#' Format a flextable with publication-ready three-line table style
#'
#' @description
#' Applies a unified formatting scheme to a flextable, gtsummary, or data.frame object,
#' producing a clean three-line table (三线表) suitable for academic publications.
#' Regression results returned by [run_glm_auto()] and [run_cox_auto()] receive
#' superscript model markers in their headers and English footnotes below the table.
#'
#' @param ft_data A `flextable`, `gtsummary`, or `data.frame` object to format.
#'
#' @return A formatted `flextable` object with Times New Roman font, three-line borders,
#'   centered alignment, and auto-fitted column widths.
#'
#' @examples
#' # Format a data.frame
#' format_flextable(head(mtcars[, 1:4]))
#'
#' # Format a gtsummary object
#' \dontrun{
#' library(gtsummary)
#' tbl <- tbl_summary(trial, include = c(age, grade))
#' format_flextable(tbl)
#' }
#'
#' @export
format_flextable <- function(ft_data) {
  regression_footnotes <- attr(
    ft_data,
    "medstats_regression_footnotes",
    exact = TRUE
  )

  # 判断输入类型
  if (inherits(ft_data, "flextable")) {
    ft <- ft_data
    n_cols <- length(ft$body$col_keys)
  } else if (inherits(ft_data, "gtsummary")) {
    ft <- gtsummary::as_flex_table(ft_data)
    n_cols <- length(ft$body$col_keys)
  } else {
    ft <- flextable::flextable(ft_data)
    n_cols <- ncol(ft_data)
  }

  ft <- ft |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 10, part = "header") |>
    flextable::bold(part = "header") |>
    flextable::align(align = "left", part = "all", j = 1) |>
    flextable::align(align = "center", part = "all", j = 2:n_cols) |>
    flextable::border_remove() |>
    flextable::hline_top(part = "header", border = officer::fp_border(width = 1.5)) |>
    flextable::hline_bottom(part = "header", border = officer::fp_border(width = 1)) |>
    flextable::hline_bottom(part = "body", border = officer::fp_border(width = 1.5)) |>
    flextable::autofit() |>
    flextable::set_table_properties(width = 1, layout = "autofit") |>
    flextable::line_spacing(space = 1, part = "all")

  if (!is.null(regression_footnotes)) {
    header_pattern <- "^(.*) \\[([0-9]+)\\]$"
    marked_columns <- grep(header_pattern, ft$body$col_keys, value = TRUE)

    for (column in marked_columns) {
      header_label <- sub(header_pattern, "\\1", column)
      marker <- sub(header_pattern, "\\2", column)
      ft <- flextable::compose(
        ft,
        j = column,
        part = "header",
        value = flextable::as_paragraph(
          flextable::as_chunk(header_label),
          flextable::as_sup(marker)
        )
      )
    }

    footer_values <- paste(
      names(regression_footnotes),
      unname(regression_footnotes)
    )
    ft <- flextable::add_footer_lines(ft, values = footer_values)

    for (i in seq_along(regression_footnotes)) {
      ft <- flextable::compose(
        ft,
        i = i,
        j = 1,
        part = "footer",
        value = flextable::as_paragraph(
          flextable::as_sup(names(regression_footnotes)[i]),
          flextable::as_chunk(paste0(" ", unname(regression_footnotes[i])))
        )
      )
    }

    ft <- ft |>
      flextable::align(align = "left", part = "footer") |>
      flextable::fontsize(size = 8, part = "footer") |>
      flextable::font(fontname = "Times New Roman", part = "footer")
  }

  ft
}


#' Export tables and plots to a Word document
#'
#' @description
#' Exports tables and plots into one `.docx` file. Plot rendering size is
#' independent of its displayed size in Word: by default, a plot is rendered
#' at 9 by 7 inches and then proportionally scaled to the usable page width.
#' This preserves the original plot layout and prevents labels from overlapping
#' when the image is fitted to a Word page.
#'
#' @param data_list A table or plot object, or a list of such objects. Tables can
#'   be `data.frame`, `gtsummary`, or `flextable` objects. Plots can be `ggplot`
#'   objects, result lists containing a `ggplot` in `$plot`,
#'   `officer::plot_instr` objects, or paths to PNG, JPEG, BMP, GIF, or TIFF
#'   files.
#' @param table_titles Either a character vector of item titles or a second
#'   table/plot object. When omitted, titles are generated automatically as
#'   `"Table 1"`, `"Table 2"`, `"Figure 1"`, and so on. Table titles are placed
#'   above tables and figure titles below figures.
#' @param output_file Output `.docx` path. Default is `"Tables_Output.docx"`.
#' @param figure_width Width in inches used to render plots. Default is `9`.
#' @param figure_height Height in inches used to render plots. Default is `7`.
#' @param figure_res Plot resolution in pixels per inch. Default is `300`.
#' @param word_width Display width in inches inside Word. When `NULL`, the usable
#'   page width is calculated from the document page size and margins.
#' @param word_height Display height in inches inside Word. When `NULL`, it is
#'   calculated from `word_width` while preserving the rendering aspect ratio.
#'   For PNG files, the original pixel dimensions are read automatically.
#'
#' @return Invisibly returns the `officer::rdocx` document object.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#'
#' export_word(
#'   data_list = list(head(mtcars), p),
#'   table_titles = c("Table 1: mtcars", "Figure 1: MPG and weight"),
#'   output_file = "tables_and_plots.docx"
#' )
#'
#' # Two objects can also be passed directly; titles are generated automatically.
#' export_word(head(mtcars), head(iris), "two_tables.docx")
#' }
#'
#' @export
export_word <- function(data_list, table_titles = NULL,
                        output_file = "Tables_Output.docx",
                        figure_width = 9, figure_height = 7, figure_res = 300,
                        word_width = NULL, word_height = NULL) {
  image_extensions <- c("png", "jpg", "jpeg", "bmp", "gif", "tif", "tiff")

  item_type <- function(item) {
    if (inherits(item, "ggplot") || inherits(item, "plot_instr") ||
        (is.list(item) && !inherits(item, "data.frame") &&
         inherits(item$plot, "ggplot")) ||
        (is.character(item) && length(item) == 1L && file.exists(item) &&
         tolower(tools::file_ext(item)) %in% image_extensions)) {
      return("figure")
    }
    if (inherits(item, "data.frame") || inherits(item, "gtsummary") ||
        inherits(item, "flextable")) {
      return("table")
    }
    NA_character_
  }

  if (is.character(table_titles) && length(table_titles) == 1L &&
      grepl("\\.docx$", table_titles, ignore.case = TRUE) &&
      identical(output_file, "Tables_Output.docx")) {
    output_file <- table_titles
    table_titles <- NULL
  }

  first_type <- item_type(data_list)
  second_type <- if (is.null(table_titles)) NA_character_ else item_type(table_titles)

  if (!is.na(first_type) && !is.na(second_type)) {
    items <- list(data_list, table_titles)
    titles <- NULL
  } else if (!is.na(first_type)) {
    items <- list(data_list)
    titles <- table_titles
  } else {
    if (!is.list(data_list)) {
      stop(
        "'data_list' must be an exportable object or a list of exportable objects.",
        call. = FALSE
      )
    }
    items <- data_list
    titles <- table_titles
  }

  if (length(items) == 0L) {
    stop("At least one table or figure must be supplied.", call. = FALSE)
  }

  if (!is.character(output_file) || length(output_file) != 1L ||
      !grepl("\\.docx$", output_file, ignore.case = TRUE)) {
    stop("'output_file' must end with .docx.", call. = FALSE)
  }

  item_types <- vapply(items, item_type, character(1))
  unsupported <- which(is.na(item_types))
  if (length(unsupported) > 0L) {
    stop(sprintf(
      paste0(
        "Unsupported object at item %d. Use a table, ggplot, result list ",
        "with $plot, plot_instr, or image path."
      ),
      unsupported[1]
    ), call. = FALSE)
  }

  if (is.null(titles)) {
    table_number <- cumsum(item_types == "table")
    figure_number <- cumsum(item_types == "figure")
    titles <- ifelse(
      item_types == "table",
      paste("Table", table_number),
      paste("Figure", figure_number)
    )
  } else {
    if (!is.character(titles) || anyNA(titles)) {
      stop(
        "'table_titles' must be a character vector without missing values.",
        call. = FALSE
      )
    }
    if (length(items) != length(titles)) {
      stop(
        "The numbers of exported items and titles do not match.",
        call. = FALSE
      )
    }
  }

  size_args <- list(figure_width, figure_height, figure_res)
  valid_sizes <- vapply(
    size_args,
    function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0,
    logical(1)
  )
  if (!all(valid_sizes)) {
    stop("'figure_width', 'figure_height', and 'figure_res' must be positive numbers.")
  }

  doc <- officer::read_docx()
  doc_dim <- officer::docx_dim(doc)
  usable_width <- unname(
    doc_dim$page["width"] - doc_dim$margins["left"] - doc_dim$margins["right"]
  )

  if (is.null(word_width)) {
    word_width <- usable_width
  }
  if (!is.numeric(word_width) || length(word_width) != 1L ||
      !is.finite(word_width) || word_width <= 0) {
    stop("'word_width' must be NULL or a positive number.")
  }
  if (word_width > usable_width) {
    warning(sprintf(
      "'word_width' (%.2f in) exceeds the usable page width (%.2f in).",
      word_width, usable_width
    ))
  }
  if (!is.null(word_height) &&
      (!is.numeric(word_height) || length(word_height) != 1L ||
       !is.finite(word_height) || word_height <= 0)) {
    stop("'word_height' must be NULL or a positive number.")
  }

  rendered_files <- character()
  on.exit(unlink(rendered_files), add = TRUE)

  for (i in seq_along(items)) {
    item <- items[[i]]
    if (item_types[i] == "table") {
      doc <- officer::body_add_par(doc, value = titles[i], style = "Normal")
    }
    plot_item <- if (inherits(item, "ggplot")) {
      item
    } else if (is.list(item) && !inherits(item, "data.frame") &&
               inherits(item$plot, "ggplot")) {
      item$plot
    } else {
      NULL
    }

    if (!is.null(plot_item)) {
      display_height <- if (is.null(word_height)) {
        word_width * figure_height / figure_width
      } else {
        word_height
      }
      image_file <- tempfile(fileext = ".png")
      rendered_files <- c(rendered_files, image_file)
      ggplot2::ggsave(
        filename = image_file,
        plot = plot_item,
        width = figure_width,
        height = figure_height,
        units = "in",
        dpi = figure_res,
        bg = "white"
      )
      doc <- officer::body_add_img(
        doc, src = image_file, width = word_width, height = display_height
      )
    } else if (inherits(item, "plot_instr")) {
      display_height <- if (is.null(word_height)) {
        word_width * figure_height / figure_width
      } else {
        word_height
      }
      image_file <- tempfile(fileext = ".png")
      rendered_files <- c(rendered_files, image_file)
      grDevices::png(
        filename = image_file,
        width = figure_width,
        height = figure_height,
        units = "in",
        res = figure_res
      )
      tryCatch(eval(item$code), finally = grDevices::dev.off())
      doc <- officer::body_add_img(
        doc, src = image_file, width = word_width, height = display_height
      )
    } else if (is.character(item) && length(item) == 1L && file.exists(item) &&
               tolower(tools::file_ext(item)) %in% image_extensions) {
      display_height <- word_height
      if (is.null(display_height)) {
        if (tolower(tools::file_ext(item)) == "png") {
          con <- file(item, open = "rb")
          png_dimensions <- tryCatch({
            signature <- readBin(con, what = "raw", n = 24L)
            if (length(signature) < 24L ||
                !identical(signature[1:8],
                           as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))) {
              stop("invalid PNG header")
            }
            c(
              width = sum(as.numeric(signature[17:20]) * 256^(3:0)),
              height = sum(as.numeric(signature[21:24]) * 256^(3:0))
            )
          }, finally = close(con))
          display_height <- word_width * png_dimensions["height"] /
            png_dimensions["width"]
        } else {
          display_height <- word_width * figure_height / figure_width
        }
      }
      doc <- officer::body_add_img(
        doc, src = item, width = word_width, height = display_height
      )
    } else {
      formatted_ft <- format_flextable(item)
      doc <- flextable::body_add_flextable(doc, value = formatted_ft)
    }

    if (item_types[i] == "figure") {
      doc <- officer::body_add_par(doc, value = titles[i], style = "Normal")
    }

    doc <- officer::body_add_par(doc, value = "", style = "Normal")
  }

  print(doc, target = output_file)
  message(sprintf("Successfully exported %d item(s) to: %s", length(items), output_file))
  invisible(doc)
}
