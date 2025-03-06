#' Run Bootstrap Analysis
#'
#' Performs bootstrap analysis on grouped data with progress bar
#'
#' @param data_frame Input data frame
#' @param group_vars Vector of grouping variables
#' @param boot_fn Bootstrap function to apply
#' @param times Number of bootstrap iterations
#' @param hypothesis Null hypothesis value
#' @param parse Whether to parse results into summary statistics
#' @param simplify Whether to return simplified results
#' @return Data frame with bootstrap results
#' @export
#' @importFrom purrr map map_dbl
#' @importFrom progress progress_bar
#' @importFrom boot boot boot.ci
#' @importFrom dplyr mutate select
run_bootstrap <- function(data_frame, group_vars, boot_fn, times=1000, 
                         hypothesis=0, parse=TRUE, simplify=TRUE) {
  
  nested_data <- data_frame %>% ungroup() %>%
    group_by(across(all_of(group_vars))) %>% nest()
  
  total_groups <- nrow(nested_data)
  pb <- progress_bar$new(format = "Bootstrap: [:bar] :percent% eta: :eta", 
                        total = total_groups, clear = FALSE, width = 60)
  
  result <- nested_data %>%
    mutate(bootstrap_results = map(data, ~{
      on.exit(pb$tick())
      boot(data = .x, statistic = boot_fn, R = times)
    }))
  
  if (parse) {
    result <- result %>%
      mutate(bootstrap_ci = map(bootstrap_results, ~boot.ci(.x, type = "basic")),
             boot_stat = map_dbl(bootstrap_results, ~.x$t0),
             boot_lower_ci = map_dbl(bootstrap_ci, ~.x$basic[4]),
             boot_upper_ci = map_dbl(bootstrap_ci, ~.x$basic[5]),
             count_above_zero = map_dbl(bootstrap_results, ~sum(.x$t > 0)),
             count_below_zero = map_dbl(bootstrap_results, ~sum(.x$t < 0)),
             prop_above_zero = map_dbl(bootstrap_results, ~mean(.x$t > 0)),
             prop_below_zero = map_dbl(bootstrap_results, ~mean(.x$t < 0)),
             p_value = map_dbl(bootstrap_results, ~{
               obs_stat = hypothesis %||% .x$t0
               1 - (sum(abs(.x$t) >= abs(obs_stat)) / (times+1))
             }), # two_tailed test
             n_bootstraps = times,
             signif_alpha = case_when(
               p_value < 0.001 ~ 0.001,
               p_value < 0.01 ~ 0.01,
               p_value < 0.05 ~ 0.05,
               TRUE ~ NA_real_,
             ),
             signif_label = case_when(
               p_value < 0.05 ~ "*",
               p_value < 0.01 ~ "**",
               p_value < 0.001 ~ "***",
               TRUE ~ "NS"
             )) %>%
      select(-bootstrap_results, -bootstrap_ci, -data)
    
    if (simplify) {
      result <- result %>% select(-signif_alpha, signif_label) %>%
        select(-prop_below_zero, -prop_above_zero) %>%
        select(-count_above_zero, -count_below_zero, -n_bootstraps)
    }
    
  } else {
    result <- result %>% unnest(bootstrap_results) %>%
      mutate(boot_index = row_number()) %>% select(-data)
  }
  
  return(result)
}

#' Label Statistical Significance
#'
#' @param results Results data frame
#' @param p_col Column name containing p-values
#' @param alpha Significance threshold
#' @param label_values Vector of p-value cutpoints to label
#' @param ns_label Label for non-significant results
#' @return Data frame with added significance labels
#' @export
#' @importFrom purrr map keep
#' @importFrom glue glue
#' @importFrom rstatix add_significance
#' @importFrom dplyr rename_at
#' @importFrom stringr str_replace_all str_replace fixed
label_significance <- function(results, p_col='p', alpha=0.05, label_values=c(0.001, .01, 0.05), ns_label='NS') {
  p_values = label_values %>% keep(function(x) {x <= alpha})

  labels = c(map(p_values, function(x) {glue('p > {x}')}), ns_label) %>%
    str_replace_all(fixed('0.'), '.')
  
  results %>% 
    add_significance(p.col = p_col, symbols = labels,
                     cutpoints = c(0, p_values, 1)) %>%
    rename_at(vars(ends_with('.signif')), str_replace, '.signif', '_signif')
}

#' Spearman Correlation Helper Function
#'
#' @param data Data frame
#' @param indices Bootstrap indices
#' @param x_var X variable name
#' @param y_var Y variable name
#' @return Correlation coefficient
#' @keywords internal
#' @importFrom stats cor.test
spearman_corr <- function(data, indices=NULL, x_var, y_var) {
  if (is.null(indices)) {
    x <- data[[x_var]]
    y <- data[[x_var]]
  } else {
    x <- data[indices, ][[x_var]]
    y <- data[indices, ][[y_var]]
  }
  cor_test_result <- cor.test(x, y, method = "spearman")
  c(correlation = cor_test_result$estimate)
}

#' Spearman Correlation with Bootstrap
#'
#' @param df Input data frame
#' @param x_var X variable name
#' @param y_var Y variable name
#' @param group_vars Grouping variables
#' @param R Number of bootstrap iterations
#' @param parse Whether to parse results
#' @return Bootstrap results for Spearman correlation
#' @export
#' @importFrom dplyr mutate
spearman_boot <- function(df, x_var, y_var, group_vars, R=1000, parse=TRUE) {

  # suppress warnings from spearman_corr
  boot_fn <- function(data, indices) {
    suppressWarnings(spearman_corr(data, indices, x_var, y_var))
  }
  
  boot_results <- run_bootstrap(df, group_vars, boot_fn, 
                              times=R, parse=parse)
  
  boot_results <- mutate(boot_results, method='spearman')
  
  if (parse) {
    boot_results <- boot_results %>%
      rename(cor = boot_stat,
             cor_lower = boot_lower_ci,
             cor_upper = boot_upper_ci)
  }
  
  return(boot_results)
}