#' Reproduce round function from MS Excel
#'
#' @description See: https://stackoverflow.com/questions/59392686/r-how-to-define-round-function-like-in-excel
#'
#'
#' @param .num number to be rounded
#' @param .digits Number of digits to be passed to round()
#'
#' @return a rounded numerical value
#'
#' @export
fct_round <- function(.num, .digits){

  round(.num + 2 * sign(.num) * .Machine$double.eps, digits = .digits)

}
