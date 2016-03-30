#' Format subject IDs by appending s to subjectID so that format is s#
#'
#' @param sub Subject number
#' @return formatted subject number
#'
#' @export

prepend_s_to_subject_id <- function(sub) {
    return(paste0('s',sub))
}
