#' FML function
#'
#' based on:
#' http://stackoverflow.com/questions/21718711/writing-a-shortcut-function-in-r-for-rmlist-ls
#' http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r
#'
#' Clears the environment (like rm(list=ls()))
#' Clears the screen (like ctrl+L)
#'
#' @export

fml <- function() {
  ENV <- globalenv()
  ll <- ls(envir = ENV)
  ll <- ll[ll != "clr"]
  rm(list = ll, envir = ENV)
  cat("\014")
}
