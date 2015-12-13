#' Format subject IDs to s### format
#'
#' @param sub Subject number
#' @return formatted subject number

format_subject_id <- function(sub) {
  if(sub < 10) {
    return(paste0('s00', sub))
  } else if(sub < 100) {
    return(paste0('s0', sub))
  } else {
    return(paste0('s',sub))
  }
}
