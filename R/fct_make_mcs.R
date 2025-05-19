#' Generate Monte Carlo Simulations for a given PDF
#'
#' @description TBD
#'
#' @param .n_iter Number of iterations of the Monte Carlo simulations
#' @param .pdf Probability Distribution Function name, supported distributions so far: "normal", "beta".
#' @param .mean Mean value if PDF parameters are mean and sd
#' @param .se Standard deviation of the mean value if PDF parameters are mean and sd
#' @param .params Vector pf parameters for other PDFs (NOT IMPLEMENTED YET)
#' @param .trunc TRUE or FALSE, should simulations be truncated to allow only values
#'                above 0 where negative values are impossible
#'
#' @return a vector of N simulation with N, the number of iterations (.n_iter)
#'
#' @examples
#' library(mocaredd)
#'
#' tt <- fct_make_mcs(.pdf = "normal", .mean = 0, .se = 1)
#' hist(tt)
#'
#' @export
fct_make_mcs <- function(.n_iter = 10000, .pdf, .mean = NA, .se = NA, .params = NULL, .trunc = FALSE){

  pdf_mean <- substitute(.mean)
  pdf_se <- substitute(.se)

  if (.pdf == "normal") {
    SIMS <- stats::rnorm(n = .n_iter, mean = .mean, sd = .se)
    if (.trunc) SIMS[SIMS < 0] <- 0
  } else if (.pdf == "beta") {
    SIMS <- stats::rbeta(n = .n_iter, shape1 = .params[1], shape2 = .params[2])
  }

  round(SIMS, 3)

}


