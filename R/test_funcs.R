# Functions --------------------------------------------------------------------

#' Print system information about R
#' 
#' @return Information about the R installation
#' @examples
#' print_r_info()
#' @export
print_r_info <- function() {
  cat("R version:", R.version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  r_path <- file.path(R.home("bin"), "R")
  cat("R executable path:", r_path, "\n")
  invisible(list(
    version = R.version.string,
    platform = R.version$platform,
    path = r_path
  ))
}