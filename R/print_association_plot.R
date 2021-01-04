#' Print the results of a chi-square test
#'
#' @param x A contingency table (passed as \code{table} or \code{matrix}) or
#'     an object of type "htest" returned by \code{\link{chisq.test}}. Can
#'     also handle objects returned by \code{\link[spgs]{chisq.unif.test}}
#'     from the \code{spgs} package.
#' @param es Boolean. Should the phi coefficient be printed as a
#'     measure of effect size. See details.
#' @param correct Boolean. Apply a continuity correction? See
#'     \code{\link{chisq.test}}. Only has an effect if the chi-square-test
#'     is computed by this function, i.e., if \code{x} is a contingency
#'     table. The default value is \code{FALSE}.
#' @param decimals How many decimals should be printed
#' @param decimals_p How many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the results of the chi-square test to be
#'     printed in Rmarkdown documents.
#'
#' @details
#'
#' The argument \code{es} only has an effect if \code{x} is passed as a 2x2
#' contingency table. In this case, the phi coefficient is computed as
#' a measure of effect size (see Cohen, 1988, page 223).
#'
#' @references
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'     (2nd ed.). Hillsale, NJ: Lawrence Erlbaum.
#'
#' @examples
#'
#' # Pass a matrix
#' x <- matrix(c(12, 5, 7, 7), ncol = 2)
#' print_chi2(x) # does not use continuity correction by default
#' print_chi2(x, correct = TRUE) # uses continuity correction
#'
#' # Pass a table
#' tab <- table(rbinom(150, 1, 0.5), rbinom(150, 1, 0.1))
#' print_chi2(tab, correct = FALSE)
#'
#' # Pass a chi-squared test object
#' print_chi2(chisq.test(tab, correct = FALSE))
#'
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @importFrom stats chisq.test
#'
#' @export
#'

print_association_plot <- function(data, x, y, var_x, var_y, var_n = "vnt.", flip = FALSE) {
  p <- data %>%
    group_by(x_ = as.factor(!!rlang::sym(x)),
             y_ = as.factor(!!rlang::sym(y)),
             .drop = FALSE) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = x_,
               fill = y_,
               y = n,
               label = n)) +
    geom_bar(stat = "identity",
             position = "dodge",
             color = "black") +
    theme_bw() +
    labs(x = var_x,
         fill = var_y,
         y = var_n) +
    theme(text = element_text(family="serif"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))

  if (flip == FALSE) {
    p <- p + geom_text(colour = "black",
                       family = "serif",
                       size = 4.5,
                       position = position_dodge(width = 0.9),
                       vjust = -0.4)
    return(p)
  } else {
    p <- p + coord_flip() + geom_text(colour = "black",
                                      family = "serif",
                                      size = 4.5,
                                      position = position_dodge(width = 0.9),
                                      hjust = -0.4)
    return(p)
  }
}
