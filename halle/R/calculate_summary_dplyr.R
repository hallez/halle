#' Calculate summary function
#'
#' This function calculates mean, SD, SEM, and limits (mean +/- SEM) on a variable (value) within a dataframe (input_dataframe).
#' Returns global variable `summarized_data`
#' @param input_dataframe: grouped dataframe

#' @export

calculate_summary_dplyr <- function(input_dataframe) {
  summarized_data <- NULL

  input_dataframe %>%
    dplyr::summarise(mean_PS = mean(value, na.rm = TRUE),
                     sd_PS = sd(value, na.rm = TRUE),
                     N = length(unique(subj)),
                     se_PS = sd_PS / sqrt(unique(N)),
                     upper = (mean_PS + se_PS),
                     lower = (mean_PS - se_PS)) ->> summarized_data
}
