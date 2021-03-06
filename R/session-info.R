#' Print session information
#'
#' This is [sessioninfo::session_info()] rewritten to export the version of
#' Pandoc, pandoc-citeproc and \href{https://github.com/lierdakil/pandoc-crossref}{pandoc-crossref}.
#' By default it also hides the library paths. Apart from that, it's (nearly) identical to the original function.
#' It also prints the system time in addition to the date.
#'
#' @details
#' Columns in the *printed* package list:
#' * `package`: package name
#' * `*`: whether the package is attached to the search path
#' * `version`: package version. If the version is marked with `(!)` that
#'   means that the loaded and the on-disk version of the package are
#'   different.
#' * `date`: when the package was built, if this information is available.
#'   This is the `Date/Publication` or the `Built` field from
#'   `DESCRIPTION`. (These are usually added automatically by R.)
#'   Sometimes this data is not available, then it is `NA`.
#' * `source`: where the package was built or installed from, if available.
#'   Examples: `CRAN (R 3.3.2)`, `Github (r-lib/pkgbuild@8aab60b)`,
#'   `Bioconductor`, `local`.
#'
#' If the Pandoc version used to build the document is different from the version which was used
#' to compile pandoc-crossref, this is noted in the output if `check_versions = TRUE`. This may be helpful
#' because mixing versions is not supported and "strange things may (and likely will) happen silently"
#' due to internal changes of Pandoc between versions.
#'
#' See [sessioninfo::package_info()] for the list of columns in the data frame that
#' is *returned* (as opposed to *printed*).
#' @param include_libs Include the library path of each package in the output. Disabled by default
#' @param check_versions Compares the used version of Pandoc with the Pandoc version used to build pandoc-crossref. For more information, see Details
#'
#'
#' @inheritParams sessioninfo::session_info
#' @importFrom sessioninfo session_info
#' @importFrom rmarkdown pandoc_version
#' @importFrom withr local_options
#' @export
#' @examples
#' Session_Info()
#' Session_Info("jRmisc")

Session_Info <- function(pkgs = NULL, include_base = FALSE, include_libs = FALSE, check_versions = TRUE) {
  info <- sessioninfo::session_info()
  info[["platform"]][["time"]] <-  format(Sys.time(), "%H:%M:%S")
  if (!is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
    info$platform$pandoc <- paste("Version", as.character(rmarkdown:::pandoc_version()))
    if (!is.null(citeproc_version()) && citeproc_version() > 0) {
      info$platform$`pandoc-citeproc` <- paste("Version", as.character(citeproc_version()))
    }
    if (!is.null(crossref_version()) && crossref_version() > 0 && !is.null(crossref_pandoc_version())) {
      info$platform$`pandoc-crossref` <- paste0("Version ", as.character(crossref_version()), " (Pandoc ", as.character(crossref_pandoc_version()), ")")
    }
  }
  if (check_versions == TRUE) {
    if (!is.null(crossref_version()) && rmarkdown::pandoc_version() != crossref_pandoc_version()) {
    info$platform$` `   <- "**********************************************"
    info$platform$`  `  <- "Versions Mismatch: pandoc-crossref and Pandoc"
    info$platform$`   ` <- "**********************************************"
    }
  }
  if (include_libs == FALSE) {
    class(info$packages) <- "Packages_Info"
  }
  return(info)
}

#' @export

print.Packages_Info <- function(x, ...) {

  unloaded <- is.na(x$loadedversion)
  flib <- function(x) ifelse(is.na(x), "?", as.integer(x))

  px <- data.frame(
    package = x$package,
    "*"     = ifelse(x$attached, "*", ""),
    version = ifelse(unloaded, x$ondiskversion, x$loadedversion),
    date    = x$date,
    # lib     = paste0("[", flib(x$library), "]"),
    source  = x$source,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  badloaded <- package_version(x$loadedversion, strict = FALSE) !=
    package_version(x$ondiskversion, strict = FALSE)
  badloaded <- !is.na(badloaded) & badloaded

  badmd5 <- !is.na(x$md5ok) & !x$md5ok

  badpath <- !is.na(x$loadedpath) & x$loadedpath != x$path

  baddel <- is.na(x$ondiskversion)
  badpath[baddel] <- FALSE

  if (any(badloaded) || any(badmd5) || any(badpath) ||  any(baddel)) {
    prob <- paste0(
      ifelse(badloaded, "V", ""),
      ifelse(badpath, "P", ""),
      ifelse(badmd5, "D", ""),
      ifelse(baddel, "R", ""))
    px <- cbind("!" = prob, px)
  }

  withr::local_options(list(max.print = 99999))
  pr <- print.data.frame(px, right = FALSE, row.names = FALSE)

  # cat("\n")
  # lapply(
  #   seq_along(levels(x$library)),
  #   function(i) cat_ln(paste0("[", i, "] ", levels(x$library)[i])))
  #
  # if ("!" %in% names(px)) cat("\n")
  # if (any(badloaded)) {
  #   cat_ln(" V ", dash(2), " Loaded and on-disk version mismatch.")
  # }
  # if (any(badpath))  {
  #   cat_ln(" P ", dash(2), " Loaded and on-disk path mismatch.")
  # }
  # if (any(badmd5)) {
  #   cat_ln(" D ", dash(2), " DLL MD5 mismatch, broken installation.")
  # }
  # if (any(baddel)) {
  #   cat_ln(" R ", dash(2), " Package was removed from disk.")
  # }

  invisible(x)
}

#' @export
#' @importFrom utils capture.output

as.character.Packages_Info <- function(x, ...) {
  capture.output(print(x))
}
