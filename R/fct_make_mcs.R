
## TO BE CHECK IF WORTH MAKING THIS CODE INTO A FUNCTION

# if (ad_x$trans_pdf == "normal") {
#   sim_mean <- ad_x$trans_area
#   sim_se   <- ad_x$trans_se
#   SIMS <- rnorm(n = usr$n_iter, mean = sim_mean, sd = sim_se)
#   min(SIMS)
#   median(SIMS)
#   mean(SIMS)
#   if (usr$trunc_pdf) SIMS[SIMS < 0] <- 0
#   median(SIMS)
#   mean(SIMS)
#   AD <- SIMS
# }

fct_make_mcs <- function(.n_iter = 10000, .pdf, .mean, .se, .a = NULL, .b = NULL, .c = NULL, .d = NULL, .trunc = FALSE){

  pdf_mean <- substitute(.mean)
  pdf_se <- substitute(.se)

  if (.pdf == "normal") {
    SIMS <- rnorm(n = usr$n_iter, mean = .mean, sd = .se)
    if (.trunc) SIMS[SIMS < 0] <- 0
    SIMS
  }

}


tt <- fct_make_mcs(.n_iter = usr$n_iter, .pdf = 'normal', .mean = ad_x$trans_area, .se = ad_x$trans_se, .a = NULL, .b = NULL, .c = NULL, .d = NULL, .trunc = FALSE)
min(tt)
median(tt)
mean(tt)
