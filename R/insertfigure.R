#' Function to insert a figure and format the caption when using pandoc-crossref.
#'
#' Formats the caption according to the output format.
#' For the short caption, you need this Lua Filter: https://github.com/pandoc/lua-filters/tree/master/short-captions
#' The function `EZfigure()` is a wrapper function for `insertfigure(..., label = knitr::opts_current$get("label"), path = knitr::fig_chunk(knitr::opts_current$get("label"), ext = "png"), unlisted = FALSE, ext = "png", auto_pdf = TRUE)`. By default, the file path from the previous chunk is used.
#'
#' @param path Full path to the image file (with extension), default is the path to the figure of the last chunk
#' @param caption A character vector containing the (long) caption of the figure.
#' @param label An optional character vector containing the label used for referencing.
#' @param shortcaption An optional character vector containing the short caption.
#' @param unlisted Either `TRUE` or `FALSE`, to determine if the figure should be added to the list of figures
#' @param ext The file extension, defaults to `"png"`
#' @param auto_pdf Whether to use PDF images automatically when the output format is LaTeX. If `TRUE`, then e.g. `foo/bar.png` will be replaced by `foo/bar.pdf` if the latter exists. This can be useful since normally PDF images are of higher quality than raster images like PNG, when the output is LaTeX/PDF.
#'
#' @return  Command to insert a figure in Markdown
#' @importFrom knitr is_latex_output
#' @importFrom stringr str_starts str_replace
#' @importFrom xfun native_encode is_web_path with_ext
#'
#' @export
insertfigure <- function(
  path,
  caption,
  label = NULL,
  shortcaption = NULL,
  unlisted = FALSE,
  ext = "png",
  auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE)) {
  # ext must be a character vector
  if (!is.null(ext)) {
    if (!is.character(ext) | !is.vector(ext)  | nchar(ext) == 0) {
      stop("ext must be a character vector.")
    }
  }
  # Check if file exists
  # Copied from knitr::include_graphics
  path = xfun::native_encode(path)
  path = xfun::with_ext(path, ext)
  if (auto_pdf && knitr::is_latex_output()) {
    path2 = xfun::with_ext(path, "pdf")
    i = file.exists(path2)
    path[i] = path2[i]
  }
  # if (child_mode()) {
  #   error = FALSE
  #   }
  if (!(child_mode()) && length(p <- path[!xfun::is_web_path(path) & !file.exists(path)])) {
    stop("Cannot find the file(s): ", paste0("\"", p, "\"", collapse = "; "))
  }
  # Check, if caption is a character vector
  if (!is.character(caption) | !is.vector(caption) | nchar(caption) == 0) {
    stop("caption must be a character vector.")
  }
  # Check, if shortcaption is a character vector and not NULL
  if (!is.null(shortcaption)) {
    if (!is.character(shortcaption) | !is.vector(shortcaption)  | nchar(shortcaption) == 0) {
      stop("shortcaption must be a character vector.")
    }
  }
  # Check, if label is a character vector if not NULL
  if (!is.null(label)) {
    if (!is.character(label) | !is.vector(label)) {
      stop("label must be a character vector.")
    }
  }
  # Label must start with #fig:
  if (!is.null(label)) {
    label <- ifelse(stringr::str_starts(label, "#fig:"), label,
                    ifelse(stringr::str_starts(label, "fig:"), paste0("#", label),
                           paste0("#fig:", label)))
  }
  # unlisted must be TRUE or FALSE
  if (!is.logical(unlisted)) stop("unlisted must be TRUE or FALSE.")
  # Craft the Insert Figure Command
  # if (knitr::is_latex_output()) {
  if (TRUE) {
    figure_str <- paste0(
      "![",
      caption,
      "](",
      path,
      "){",
      ifelse(!is.null(label), label, ""),
      ifelse(unlisted & !is.null(label), " .unlisted ", ""),
      ifelse(!is.null(shortcaption) & !is.null(label),
             paste0(' short-caption="', shortcaption, '"'),""),
      "}")
    figure_str <- stringr::str_replace(figure_str, "\\{\\}\\Z", "")
  } else {
    figure_str <- paste0("![",
                          caption,
                          "](",
                          path,
                          ")",
                          ifelse(!is.null(label), paste0("{", label, "}"), ""))
  }
  cat(figure_str)
}

child_mode <- utils::getFromNamespace("child_mode", "knitr")

#' @rdname insertfigure
#' @export
EZfigure <- function(caption, shortcaption = NULL) { insertfigure(caption = caption, shortcaption = shortcaption, label = knitr::opts_current$get("label"),
  path = knitr::fig_chunk(knitr::opts_current$get("label"), "png"), unlisted = FALSE, ext = "png", auto_pdf = TRUE) }
