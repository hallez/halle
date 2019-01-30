#' Bonferroni-Holm correction
#' for published example, see: https://www.pnas.org/content/pnas/111/40/E4264.full.pdf
#' online tutorial: https://www.statisticshowto.datasciencecentral.com/holm-bonferroni-method/
#' theoretically, should be able to use `p.adjust`, but results are non-sensical
#' based on: https://rcompanion.org/rcompanion/f_01.html
#' 1. rank order the p-values
#' 2. divide the largest tolerated p-value (alpha value) by the rank - this is the crux of the Holm correction
#' 3. return whether or not the observed p-value is smaller (and, thus, significant) than the Holm corrected p-value
#' TODO: Figure out how to dynamically set name of computed column so that contains name of `pvalue_column` (think: the way that `summarise` works)
#' @export

compute_holm_bonferroni <- function(input_data, pvalue_column, alpha_threshold){
  holm_corr <<- NULL
  holm_corr <<- input_data %>%
    dplyr::mutate(rank_id = min_rank(desc(pvalue_column)),
                pval_holm_adj = (alpha_threshold / rank_id),
                pval_holm_signif = ifelse(pvalue_column < pval_holm_adj, "signif", "n/s"))
}

