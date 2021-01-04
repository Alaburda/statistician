#' Format a p-value according to APA standards
#'
#' @param pvalues The p-values
#' @param decimals The number of decimals to be printed
#' @param numbers_only Logical, indicates whether the p-values
#'                     should be printed whithout the accompanying p.
#'                     Defaults to \code{FALSE}.
#'
#' @return A string representation of the p value to be used in Rmarkdown
#'   documents.
#'
#' @export
#'
#' @examples
#'
#' # Format a p-value, default is 3 decimals
#' format_p(0.03123)
#' format_p(0.000001231)
#' format_p(0.000001231, decimals = 2)
#' format_p(0.3123, decimals = 2)
#' # Format several p-values with one function call
#' format_p(c(0.3123, 0.001, 0.00001, 0.19))
#' format_p(c(.999, .9999, 1))
#' format_p(c(0.3123, 0.001, 0.00001, 0.19, .99999), numbers_only = TRUE)
#'


print_ttest_text <- function(data, x, y, var1_name, var2_name) {
  chi.obj <- t.test(table(data[,x],data[,y]))
  chi.statistic <- gsub("\\.",",",force_decimals(chi.obj$statistic))

  chi_rs <- glue::glue("$\\chi^2({chi.obj$parameter[1]}) = {chi.statistic}$, {format_p(chi.obj$p.value)}")


  if (chi.obj$p.value < 0.05) {
    glue::glue("Atlikus Chi-kvadrato testą rasta statistiškai reikšminga asociacija tarp {var1_name} ir {var2_name} ({chi_rs}).")
  } else {
    glue::glue("Atlikus Chi-kvadrato testą statistiškai reikšminga asociacija tarp {var1_name} ir {var2_name} nebuvo nustatyta ({chi_rs}).")
  }
}

