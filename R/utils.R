#' @importFrom utils file_test
.citeproc <- new.env()
.citeproc$dir <- NULL
.citeproc$version <- NULL

citeproc_version <- function() {
  find_citeproc()
  .citeproc$version
}

find_citeproc <- function(cache = TRUE) {

  if (!is.null(.citeproc$dir) && cache) return(invisible(as.list(.citeproc)))

  # define potential sources
  sys_citeproc <- rmarkdown:::find_program("pandoc-citeproc")
  sources <- c(Sys.getenv("RSTUDIO_PANDOC"), if (nzchar(sys_citeproc)) dirname(sys_citeproc))
  if (!rmarkdown:::is_windows()) sources <- c(sources, path.expand("~/opt/pandoc"))

  # determine the versions of the sources
  versions <- lapply(sources, function(src) {
    if (rmarkdown:::dir_exists(src)) get_citeproc_version(src) else numeric_version("0")
  })

  # find the maximum version
  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if (ver > found_ver) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }

  # did we find a version?
  if (!is.null(found_src)) {
    .citeproc$dir <- found_src
    .citeproc$version <- found_ver
  }

  invisible(as.list(.citeproc))
}

# Get an S3 numeric_version for the pandoc-citeproc utility at the specified path
get_citeproc_version <- function(citeproc_dir) {
  path <- file.path(citeproc_dir, "pandoc-citeproc")
  if (rmarkdown:::is_windows()) path <- paste0(path, ".exe")
  if (!utils::file_test("-x", path)) return(numeric_version("0"))
  info <- rmarkdown:::with_pandoc_safe_environment(
    system(paste(shQuote(path), "--version"), intern = TRUE)
  )
  version <- strsplit(info, "\n")[[1]][1]
  version <- strsplit(version, " ")[[1]][2]
  numeric_version(version)
}

.crossref <- new.env()
.crossref$dir <- NULL
.crossref$version <- NULL

crossref_version <- function() {
  find_crossref()
  .crossref$version
}

crossref_pandoc_version <- function() {
  find_crossref()
  .crossref$pandoc
}

find_crossref <- function(cache = TRUE) {

  if (!is.null(.crossref$dir) && cache) return(invisible(as.list(.crossref)))

  # define potential sources
  sys_crossref <- rmarkdown:::find_program("pandoc-crossref")
  sources <- c(Sys.getenv("RSTUDIO_PANDOC"), if (nzchar(sys_crossref)) dirname(sys_crossref))
  if (!rmarkdown:::is_windows()) sources <- c(sources, path.expand("~/opt/pandoc"))

  # determine the versions of the sources
  versions <- lapply(sources, function(src) {
    if (rmarkdown:::dir_exists(src)) get_crossref_version(src) else numeric_version("0")
  })

  # find the maximum version
  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if (ver > found_ver) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }

  # did we find a version?
  if (!is.null(found_src)) {
    .crossref$dir <- found_src
    .crossref$version <- found_ver
    .crossref$pandoc <- get_crossref_pandoc_version(.crossref$dir)
  }

  invisible(as.list(.crossref))
}

# Get an S3 numeric_version for the pandoc-crossref utility at the specified path
get_crossref_version <- function(crossref_dir) {
  path <- file.path(crossref_dir, "pandoc-crossref")
  if (rmarkdown:::is_windows()) path <- paste0(path, ".exe")
  if (!utils::file_test("-x", path)) return(numeric_version("0"))
  info <- rmarkdown:::with_pandoc_safe_environment(
    system(paste(shQuote(path), "--version"), intern = TRUE)
  )
  version <- strsplit(info, " ")[[1]][2]
  version <- sub("v", "", version)
  numeric_version(version)
}

# Extract Pandoc Version used for building pandoc-crossref
get_crossref_pandoc_version <- function(crossref_dir) {
  path <- file.path(crossref_dir, "pandoc-crossref")
  if (rmarkdown:::is_windows()) path <- paste0(path, ".exe")
  if (!utils::file_test("-x", path)) return(numeric_version("0"))
  info <- rmarkdown:::with_pandoc_safe_environment(
    system(paste(shQuote(path), "--version"), intern = TRUE)
  )
  built_pandoc <- regmatches(info[[1]], regexpr("built with Pandoc [v0-9.]{4,20}", info[[1]]))
  if (nzchar(built_pandoc)) {
    built_pandoc <- regmatches(built_pandoc, regexpr("[0-9.]{4,20}", built_pandoc))
  } else {
    built_pandoc <- "unknown"
  }
  numeric_version(built_pandoc)
}
