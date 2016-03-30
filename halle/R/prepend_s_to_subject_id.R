#' Format subject IDs by appending s to subjectID so that format is s#
#'
#' @param sub Subject number
#' @return formatted subject number
#'
#' @export

format_subject_id <- function(sub) {
    return(paste0('s',sub))
}
