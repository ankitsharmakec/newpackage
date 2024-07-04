#' Round a Number
#'
#' Rounds a number to the specified number of decimal places and returns it as a string.
#'
#' @param x A numeric vector to be rounded.
#' @param digits An integer specifying the number of decimal places to round to. Default is 0.
#'
#' @return A character vector with the rounded numbers as strings.
#' @export
#'
#' @examples
#' round0(3.14159)
#' round0(3.14159, 2)
round0 <- function(x, digits = 0) {
  fmt <- paste0("%.", digits, "f")
  x0 <- sprintf(fmt, x)

  return(x0)
}
