#' Print the table on an isolated landscape page in PDF
#' like the function inn the `kableExtra` package
#'
#' @description This function will put the table on an single landscape page after the next page break.
#' It's useful for wide tables that can't be printed on a portrait page.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param margin Customizable page margin for special needs. Values can be
#' "1cm", "1in" or similar.
#' @param clearpage If `TRUE` the landscape page is inserted after the page break. Requires the LaTeX package `afterpage`.
#'
#' @examples
#' \dontrun{
#' landscape_after(knitr::kable(head(mtcars), "latex"), clearpage = TRUE)
#' }
#'
#' @export
landscape_after <-
  function(kable_input, margin = NULL, clearpage = FALSE) {
    ls_input <- kableExtra::landscape(kable_input, margin)
    if (clearpage == TRUE && attr(ls_input, "landscape") == TRUE) {
      kable_attrs <- attributes(ls_input)
      out <- paste0(
        "\n\\afterpage{", kableExtra:::solve_enc(ls_input),
        "\n}"
      )
      out <- structure(out, format = "latex", class = "knitr_kable")
      attributes(out) <- kable_attrs
      return(out)
    }
    return(ls_input)
  }
