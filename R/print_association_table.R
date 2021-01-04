print_association_table <- function(data, x, y, caption = "Lentelė") {
  data %>%
    janitor::tabyl(!!rlang::sym(x), !!rlang::sym(y)) %>%
    adorn_totals(name = "Iš viso") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>%
    knitr::kable(caption = caption)
}
