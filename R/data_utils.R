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
  
  if (original_class != 'list') {
    return(new_vec)
  } else {
    return(as.list(new_vec))
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
  remove_log <- function(x) {
    x %>% str_remove('log_') %>%
      str_remove('log(') %>% str_remove(')')
  }
  
  data %>% 
    rename_at(starts_with('log'), remove_log)
}

#' Load a Dataframe from any File Format
#'
#' Loads data from various file formats including CSV, Excel, Parquet, and RDS files.
#' Can handle a single file path or multiple file paths.
#'
#' @param file_path Character string or character vector with path(s) to file(s)
#' @param file_type Optional character string specifying file type. If NULL (default),
#'                 the function will detect the file type from the file extension.
#' @param sheet Optional sheet name or number for Excel files (default is the first sheet)
#' @param add_path_col Logical, if TRUE adds a column with the file path (default: FALSE)
#' @param path_col_name Character string specifying the name of the path column (default: "file_path")
#' @param ... Additional parameters passed to the underlying read function
#' @return A single dataframe when one file path is provided, or a combined dataframe when multiple paths are provided
#' @importFrom readr read_csv read_rds read_tsv
#' @importFrom arrow read_parquet
#' @importFrom readxl read_excel
#' @importFrom purrr map2_df
#' @importFrom tools file_ext
#' @importFrom dplyr mutate
#' @export
load_data <- function(file_path, file_type = NULL, sheet = NULL, 
                      add_path_col = FALSE, path_col_name = "file_path", ...) {
  
  # Function to determine file type from extension
  get_file_type <- function(path) {
    ext <- tolower(tools::file_ext(path))
    switch(ext,
           "csv" = "csv",
           "tsv" = "tsv",
           "xls" = "excel",
           "xlsx" = "excel",
           "parquet" = "parquet",
           "rds" = "rds",
           stop("Unsupported file extension: ", ext))
  }
  
  # Function to read a single file
  read_single_file <- function(path, type = NULL, ...) {
    if (is.null(type)) {
      type <- get_file_type(path)
    }
    
    data <- switch(type,
           "csv" = readr::read_csv(path, ...),
           "tsv" = readr::read_tsv(path, ...),
           "excel" = readxl::read_excel(path, sheet = sheet, ...),
           "parquet" = arrow::read_parquet(path, ...),
           "rds" = readr::read_rds(path, ...),
           stop("Unsupported file type: ", type))
    
    if (add_path_col) {
      data <- dplyr::mutate(data, !!path_col_name := path)
    }
    
    return(data)
  }
  
  # Handle single or multiple file paths
  if (length(file_path) == 1) {
    return(read_single_file(file_path, file_type, ...))
  } else {
    # For multiple files, use the same file_type for all if specified
    if (!is.null(file_type) && length(file_type) == 1) {
      file_type <- rep(file_type, length(file_path))
    }
    
    # Read all files and combine them
    result <- purrr::map2_df(
      file_path, 
      if (is.null(file_type)) rep(list(NULL), length(file_path)) else file_type,
      ~ read_single_file(.x, .y, ...)
    )
    
    return(result)
  }
}

#' Read a Dataframe from any File Format
#'
#' Alias for load_data(). Reads data from various file formats including CSV, Excel, Parquet, and RDS files.
#' Can handle a single file path or multiple file paths.
#'
#' @inheritParams load_data
#' @return A single dataframe when one file path is provided, or a combined dataframe when multiple paths are provided
#' @export
read_data <- function(file_path, file_type = NULL, sheet = NULL, 
                      add_path_col = FALSE, path_col_name = "file_path", ...) {
  load_data(file_path, file_type, sheet, add_path_col, path_col_name, ...)
}