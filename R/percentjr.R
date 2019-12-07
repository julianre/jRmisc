#' Label percentages with thin space between number and percent sign (2.5 %, 50 %, etc.)
#'
#' @param x A numeric vector to format.
#' @param accuracy A number to round to. Use (e.g.) `0.01` to show 2 decimal places of precision. If `NULL`, the default, uses a heuristic that should ensure breaks have the minimum number of digits needed to show the difference between adjacent values.
#' Applied to rescaled data.
#' @param scale A scaling factor: x will be multiplied by `scale` before formating. This is useful if the underlying data is very small or very large.
#' @param prefix Symbols to display before and after value.
#' @param suffix Symbols to display before and after value. If `suffix=="thinspace"` returns " %", else if `suffix` is a character, returns `suffix`. If it is not a character, returns "%".
#'
#' @param big.mark Character used between every 3 digits to separate thousands.
#' @param decimal.mark The character to be used to indicate the numeric decimal point.
#' @param trim Logical, if `FALSE`, values are right-justified to a common width (see [base::format()]).
#' @param ... Other arguments passed on to [base::format()].
#'
#' @return  Formated percentages
#' @importFrom scales number
#'
#' @examples
#' percentjr(0.01)
#'
#' @export
percentjr <- function(x, accuracy = NULL, scale = 100, prefix = "",
                      suffix = "thinspace", big.mark = " ", decimal.mark = ".", trim = TRUE, ...) {
  suffix <- ifelse(identical(suffix, "thinspace"), "\u202f%", ifelse(is.character(suffix), suffix, "%"))
  scales::number(x = x, accuracy = accuracy, scale = scale, prefix = prefix,
                 suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
                 trim = trim, ...)
}
