# Functions --------------------------------------------------------------------

#' Print system information about R
#' 
#' @export
print_r_info <- function() {
  system("type R"); file.path(R.home("bin"), "R")
}

#' Say hello to someone (test function)
#' 
#' @param name Name of a person
#' @param exclamation Whether to include an exclamation mark
#' @export 
say_hello <- function(name, exclamation = TRUE) {
  paste0("Hello ", name, ifelse(exclamation, "!", "."))
}