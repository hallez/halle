#' Cohen's d function
#'
#' Compute an effect size (Cohen's d) for paired samples t-tests.
#' To use, your dataframe must have just 2 columns called `var1` and `var2` that correspond to the two conditions of interest.
#' These columns must be paired data of the same length such that each row represents observations from a single subject.
#' To call function:
#' compute_cohens_d(data_frame_with_var1_var2_columns)
#'
#' It will return three versions of Cohen's d
#' Standard: mean / sd
#'
#' Morris DeShon: Computes effect size while correcting for the correlation between condition means
#' Generally takes the form of `mean_difference / (mean_sd * (sqrt(2*(1-correlation))))`
#' This can be found in their paper under Equation 8.
#' Morris, S. B., & DeShon, R. P. (2002). Combining effect size estimates in meta-analysis with repeated measures and independent-groups designs. Psychological Methods, 7(1), 105–125. http://doi.org/10.1037//1082-989X.7.1.105
#'
#' Dunlap: Computes effect size while correcting for the correlation between condition means. Uses the t-value rather than computing the difference between the means as is done with Morris DeShon.
#' Generally takes the form of `t_statistic * (((2 *(1 - correlation)) / (number_of_subjects) ^ 0.5)`
#' This can be found in their paper under Equation 3.
#' Dunlap, W. P., Cortina, J. M., Vaslow, J. B., & Burke, M. J. (1996). Meta-analysis of experiments with matched groups or repeated measures designs. Psychological Methods, 1(2), 170–177. http://doi.org/10.1037/1082-989X.1.2.170
#'---
#' To compute Cohen's d for a single group t-test against 0:
#' compute_cohens_d_vs_0(data_frame_with_var1_column)
#' This will return the "standard" Cohen's d (described above).
#' ---
#' To compute Cohen's d for two different (ie, independent) samples:
#' compute_cohens_d_independent_samples(input_df)
#' Here, the input_df should have a column of data (`var1`) and a grouping variable (`group`)
#' ---
#' To change the names of dataframe columns, here's one option:
#' original_dataframe %>%
#'   dplyr::select(long_var_name1, long_var_name2) %>%
#'   dplyr::rename(var1 = long_var_name1,
#'                 var2 = long_var_name2) ->
#'   renamed_dataframe
#' halle::compute_cohens_d(renamed_dataframe)
#' ---
#' To verify this function works with a set of test data, try one of the following.
#' Test data courtesy of Tanya Jonker (tanyarjonker <at> gmail.com) and results based on calculations from her Excel workbook.
#' Example 1 test data
#' e1_condA <- c(0.50,0.88,0.56,0.69,1.00,0.56,0.69,0.56,0.81,0.69,0.69,0.63,0.56,0.69,0.81,0.81,0.63,0.75,0.88,0.69,0.75,0.69,0.69,0.69,0.56,0.81,0.56,0.69,0.75,0.63)
#' e1_condB <- c(0.75,1.00,0.75,0.75,0.94,0.69,0.44,0.69,0.81,0.56,0.75,0.69,0.56,0.88,0.94,0.50,0.69,0.75,1.00,0.69,0.94,0.50,0.63,0.75,0.31,0.81,0.94,0.81,0.81,0.75)
#' e1_df <- data.frame(e1_condA, e1_condB)
#' TJ would estimate the effect size as
#' standard: -0.259328434
#' Morris DeShon: -0.271365019
#' Dunlap: -0.268558307
#'
#' example 2 test data
#' e2_condA <- c(0.38,0.63,0.69,0.50,0.88,0.50,0.75,0.81,0.56,0.31,0.69,0.56,0.38,0.69,0.44,0.38,0.50,0.69,0.88,0.75,0.75,0.56,0.94,0.63,0.44,0.69,0.81,0.69,0.69,0.69)
#' e2_condB <- c(0.56,0.94,0.88,0.56,0.81,0.50,0.50,0.75,0.69,0.38,0.81,0.31,0.69,0.69,0.63,0.56,0.63,0.75,0.94,0.75,0.75,0.75,0.88,0.44,0.50,1.00,0.75,0.75,0.88,0.94)
#' e2_df <- data.frame(e2_condA, e2_condB)
#' TJ would estimate the effect size as
#' standard: -0.468829054
#' Morris DeShon: -0.470087261
#' Dunlap: -0.413884462
#'
#' To create test data for independent samples t-test:
#' indepen_df <- data.frame(var1 = c(e1_condA, e1_condB, e2_condA, e2_condB), group = sample(LETTERS[1:2], 120, replace = TRUE))
#' # check that groups are of unequal size. otherwise, re-run to generate new random groups
#' table(indepen_df$group)
#' @export

compute_cohens_d <- function(input_df) {

  # have option to turn on/off printing out intermediate steps
  # eventually, this will be an option in the function #126253201
  verbose <- 0

  # clear out all the variables we're going to use in the function just to be safe
  diff_df <- NULL
  mean_diff_df <- NULL
  sd_diff_df <- NULL
  sd_df <- NULL
  mean_sd_df <- NULL
  corr_df <- NULL
  tt <- NULL
  cohens_d_standard <- NULL
  cohens_d_morris_deshon <- NULL
  cohens_d_dunlap <- NULL

  # compute the variables needed for each effect size measure
  input_df %>%
    dplyr::mutate(diff = var1 - var2) -> diff_df
  if(verbose == 1){
    print(cat("Difference between the conditions, by subject (first 6 subjects): "))
    print(head(diff_df))
  }

  diff_df %>%
    dplyr::summarise(mean_diff = mean(diff, na.rm = TRUE)) -> mean_diff_df
  if(verbose == 1){
    print(cat("Difference between the condition means: "))
    print(mean_diff_df)
  }

  input_df %>%
    dplyr::summarise_each(funs(sd)) -> sd_df
  if(verbose == 1){
    print(cat("Standard deviations of each condition: "))
    print(sd_df)
  }

  diff_df %>%
    dplyr::summarise(sd_diff = sd(diff, na.rm = TRUE)) -> sd_diff_df
  if(verbose == 1){
    print(cat("Standard deviation of the difference between conditions: "))
    print(sd_diff_df)
  }

  sd_df %>%
    dplyr::summarise(mean_sd = mean(c(var1, var2))) -> mean_sd_df
  if(verbose == 1){
    print(cat("Mean of the standard deviations for each condition: "))
    print(mean_sd_df)
  }

  input_df %>%
    dplyr::summarise(corr = cor(var1, var2)) -> corr_df
  if(verbose == 1){
    print(cat("Correlation between the conditions: "))
    print(corr_df)
  }

  with(input_df, t.test(var1, var2, paired = TRUE)) ->  tt
  print(cat("Paired t-test value: "))
  print(tt$statistic)

  cohens_d_standard <- mean_diff_df / sd_diff_df

  cohens_d_morris_deshon <- mean_diff_df / (mean_sd_df * (sqrt(2*(1-corr_df))))

  cohens_d_dunlap <- tt$statistic * (((2 *(1 - corr_df)) / (dim(input_df)[1])) ^ 0.5)

  print(cat("Cohen's d effect size ('standard') is: "))
  print(cohens_d_standard)

  print(cat("Cohen's d effect size ('Morris Deshon') is: "))
  print(cohens_d_morris_deshon)

  print(cat("Cohen's d effect size ('Dunlap') is: "))
  print(cohens_d_dunlap)
}

#' @export
compute_cohens_d_vs_0 <- function(input_df){
  # clear out all the variables we're going to use in the function just to be safe
  cohens_d <- NULL
  tt <- NULL

  with(input_df, t.test(var1, mu = 0, na.rm = TRUE)) ->  tt
  print(cat("T-test vs. 0 value: "))
  print(tt$statistic)

  input_df %>%
    dplyr::summarise_each(funs(mean(., na.rm = TRUE), sd)) %>%
    dplyr::summarise(cohens_d = mean/sd) -> cohens_d

  print(cat("Cohen's d ('standard') for condition is: "))
  print(cohens_d)
}

#' @export
compute_cohens_d_independent_samples <- function(input_df){
  # clear out the variables we're going to use in the function
  summarized_df <- NULL

  # compute mean, sd, and length of each group
  indepen_df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise_each(funs(mean, sd, length)) -> summarized_df

  # computed pooled SD
  # based on: http://stackoverflow.com/questions/16974389/how-to-calculate-a-pooled-standard-deviation-in-r
  # sqrt( sum(df$sd^2 * (df$n - 1)) / (sum(df$n - 1)) )
  pooled_sd <- sqrt(sum(summarized_df$sd^2 * (summarized_df$length - 1)) / (sum(summarized_df$length - 1)))

  # subtract means
  # based on: http://stackoverflow.com/questions/22589851/build-difference-between-groups-with-dplyr-in-r
  # http://stackoverflow.com/questions/28225473/how-to-subtract-rows-where-column-factor-matches-using-dplyr
  summarized_df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(d = mean[1] - mean[2])

}
