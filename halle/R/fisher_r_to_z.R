#' Fisher's r to z transform
#' //TODO investigate using fisherz function instead
#' @param data: column from a dataframe to do fishers transform on

#' @export

fisher_r_to_z <- function(data){
  0.5 * (log(1+data) - log(1-data))
}
