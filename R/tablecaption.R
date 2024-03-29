#' Function to format captions for table when using pandoc-crossref and table-short-captions Lua filter.
#'
#' Formats the caption according to the output format.
#' For the short caption, you need this Lua Filter: https://github.com/pandoc/lua-filters/tree/master/table-short-captions
#'
#' @param caption A character vector containing the (long) caption of the table.
#' @param label An optional character vector containing the label used for referencing.
#' @param shortcaption An optional character vector containing the short caption.
#' @param unlisted Either `TRUE` or `FALSE`, to determine if the table should be added to the list of tables.
#'
#' @return  Formatted table caption
#' @importFrom knitr is_latex_output
#' @importFrom stringr str_starts str_replace
#'
#' @examples
#' tablecaption("Caption of the Table")
#'
#' @export
tablecaption <- function(
  caption,
  label = NULL,
  shortcaption = NULL,
  unlisted = FALSE) {
  # Check, if caption is a character vector
  if (!is.character(caption) | !is.vector(caption) | nchar(caption) == 0) {
    stop("caption must be a character vector.")
  }
  # Check, if shortcaption is a character vector if not NULL
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
  # Label must start with #tbl
  if (!is.null(label)) {
    label <- ifelse(stringr::str_starts(label, "#tbl:"), label,
                    ifelse(stringr::str_starts(label, "tbl:"), paste0("#", label),
                           paste0("#tbl:", label)))
  }
  # unlisted must be TRUE or FALSE
  if (!is.logical(unlisted)) stop("unlisted must be TRUE or FALSE.")
  # Craft the Caption
  if (knitr::is_latex_output()) {
    caption_str <- paste0(
      "Table: ",
      caption,
      " []{",
      ifelse(!is.null(label), label, ""),
      ifelse(unlisted & !is.null(label), " .unlisted ", ""),
      ifelse(!is.null(shortcaption) & !is.null(label),
             paste0(' short-caption="', shortcaption, '"'),""),
      "}")
    caption_str <- stringr::str_replace(caption_str, "\\h*\\[\\]\\{\\}\\Z", "")
  } else {
    caption_str <- paste0("Table: ",
                          caption,
                          ifelse(!is.null(label), paste0(" {", label, "}"), ""))
  }
  return(caption_str)
}

#' @rdname tablecaption
#' @export
EZtable <- function(caption, shortcaption = NULL, label = knitr::opts_current$get("label")) { tablecaption(caption = caption, shortcaption = shortcaption, unlisted = FALSE, label = label) }
