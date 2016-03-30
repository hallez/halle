#' Function to ensure path ends in a trailing slash
#'
#' @param path Input string to check for a trailing slash
#' @return path Input string with a trailing slash (if had at slash at time of input, will not duplicate)
#'
#' @export

ensure_trailing_slash <- function(path) {
  numFound <- regexpr('/$',path)
  if(numFound == -1){
    path <- paste0(path, "/")
  }
  return(path)
}
