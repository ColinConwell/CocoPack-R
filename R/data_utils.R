#' Slice a Vector
#'
#' Returns a subset of a vector from start to stop position
#'
#' @param vector The input vector to slice
#' @param start Starting position of the slice
#' @param stop Optional ending position of the slice (defaults to vector length)
#' @return A vector containing the selected slice
#' @export
vslice <- function(vector, start, stop=NULL) {
  if (is.null(stop)) {stop = length(vector)}
  return(vector[start:stop])
}

#' Read CSV and Add Filename Column
#'
#' Reads a CSV file and adds the filename as a column in the resulting dataframe
#'
#' @param file Path to the CSV file
#' @return A tibble with an additional filename column
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
read_csv_add_name <- function(file) {
  read_csv(file) %>% mutate(filename = file)
}

#' Read Parquet and Add Filename Column
#'
#' Reads a Parquet file and adds the filename as a column in the resulting dataframe
#'
#' @param file Path to the Parquet file
#' @return A tibble with an additional filename column
#' @export
#' @importFrom arrow read_parquet
#' @importFrom dplyr mutate
read_parquet_add_name <- function(file) {
  read_csv(file) %>% mutate(filename = file)
}

#' Get Package Citation in BibTeX Format
#'
#' @param x Package name
#' @return Prints BibTeX citation
#' @export
get_package_bibtex <- function(x) {
  print(x, bibtex = TRUE)
}



#' Flip Keys and Values in a Named Vector or List
#'
#' Takes a named vector or list and swaps the names with their values
#'
#' @param mapping A named vector or list where names will be swapped with values
#' @return A named vector or list with keys and values flipped, preserving the original class
#' @importFrom dplyr mutate select
#' @importFrom tibble tibble deframe
#' @export
flip_names <- function(mapping) {
  original_class <- class(mapping)
  
  new_vec <- 
    tibble(keys = names(mapping), 
           values = mapping) %>%
    mutate(temp_keys = keys, 
           keys = values,
           values = temp_keys) %>%
    select(-temp_keys) %>% deframe()
  
  if (original_class == 'list') {
    new_vec <- as.list(new_vec)
  } else {
    return(new_vec)
  }
}

#' Convert Names to Title Case
#'
#' Converts variable names to clean title case format using janitor package
#'
#' @param x Character string to convert to title case
#' @return Character string in clean title case format
#' @importFrom janitor make_clean_names
#' @export
name_to_title <- function(x) {
  janitor::make_clean_names(x, 'title', allow_dupes=1)
}

#' Convert Names to Upper Case
#'
#' Converts variable names to clean upper case format using janitor package
#'
#' @param x Character string to convert to upper case
#' @return Character string in clean upper case format
#' @importFrom janitor make_clean_names
#' @export
name_to_upper <- function(x) {
  janitor::make_clean_names(x, 'all_caps', allow_dupes=1)
}

#' Add Log() to Variable Names
#'
#' Adds log transformation notation to variable names, removing existing 'log' or 'Log' 
#' tags if present
#'
#' @param x Character string representing the variable name
#' @param check Logical, if TRUE only adds log notation if 'log' or 'Log' not already present
#' @return Character string with log transformation notation
#' @importFrom stringr str_detect str_remove str_trim
#' @export
add_log_to_name <- function(x, check=TRUE) {
  tags <- 'log|Log'
  if (check & !str_detect(x, tags)) {
    return(x)
  } else {
    x <- str_trim(str_remove(x, tags))
    return(paste0('log(', x, ')'))
  }
}

#' Remove Log Labels from Column Names
#'
#' Removes 'log_', 'log(', and ')' from column names in a dataframe
#'
#' @param data A dataframe containing columns with log labels
#' @return A dataframe with log labels removed from column names
#' @importFrom dplyr rename_at starts_with
#' @importFrom stringr str_remove
#' @export
remove_log_label <- function(data) {
  remove_label <- function(x) {
    x %>% str_remove('log_') %>%
      str_remove('log(') %>% str_remove(')')
  }
  
  data %>% 
    rename_at(starts_with('log'), remove_log)
}