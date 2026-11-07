plot_font_theme <- function(plot, font_family, latin_family) {
  for (font in list(font_family, latin_family)) {
    if (!is.null(font) && (!is.character(font) || length(font) != 1L ||
        is.na(font) || !nzchar(trimws(font)))) {
      stop("Font families must be NULL or a single nonempty string.", call. = FALSE)
    }
  }
  if (is.null(font_family) && is.null(latin_family)) return(plot)
  if (is.null(latin_family)) {
    return(plot + ggplot2::theme(text = ggplot2::element_text(family = font_family)))
  }
  # gridtext's CSS parser needs a font alias without spaces.
  alias <- paste0("medstats", paste(as.hexmode(utf8ToInt(latin_family)), collapse = ""))
  systemfonts::register_variant(alias, family = latin_family)
  if (.Platform$OS.type == "windows") {
    families <- unique(c(font_family, latin_family, alias))
    mappings <- lapply(families, function(font) {
      grDevices::windowsFont(if (identical(font, alias)) latin_family else font)
    })
    do.call(grDevices::windowsFonts, setNames(mappings, families))
  }
  element <- ggtext::element_markdown(family = font_family, face = "plain")
  elements <- c(
    "axis.text.x", "axis.text.x.bottom", "axis.text.x.top",
    "axis.text.y", "axis.text.y.left", "axis.text.y.right",
    "axis.title.x", "axis.title.x.bottom", "axis.title.x.top",
    "axis.title.y", "axis.title.y.left", "axis.title.y.right",
    "legend.text", "legend.title", "plot.title", "plot.subtitle", "plot.caption",
    "strip.text", "strip.text.x", "strip.text.y"
  )
  settings <- setNames(rep(list(element), length(elements)), elements)
  settings$text <- ggplot2::element_text(family = font_family)
  settings$axis.text.y <- ggplot2::element_text(family = latin_family)
  settings$axis.text.y.left <- ggplot2::element_text(family = latin_family)
  settings$axis.text.y.right <- ggplot2::element_text(family = latin_family)
  for (name in c("x", "y", "title", "subtitle", "caption", "colour", "shape", "fill")) {
    if (is.character(plot$labels[[name]])) {
      plot$labels[[name]] <- plot_font_labels(plot$labels[[name]], alias)
    }
  }
  for (scale in plot$scales$scales) {
    if ("y" %in% scale$aesthetics) next
    previous <- scale$labels
    scale$labels <- local({
      original <- previous
      font_alias <- alias
      function(x) {
        text <- if (is.function(original)) original(x) else if (inherits(original, "waiver")) {
          as.character(x)
        } else original
        if (is.expression(text) || is.null(text)) return(text)
        plot_font_labels(text, font_alias)
      }
    })
  }
  plot + do.call(ggplot2::theme, settings)
}

plot_font_labels <- function(label, alias) {
  label <- as.character(label)
  label <- gsub("&", "&amp;", label, fixed = TRUE)
  label <- gsub("<", "&lt;", label, fixed = TRUE)
  label <- gsub(
    "([[:ascii:]]+)",
    paste0("<span style='font-family:", alias, "'>\\1</span>"), label,
    perl = TRUE
  )
  gsub("\n", "<br>", label, fixed = TRUE)
}
