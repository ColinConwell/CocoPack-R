#' Create a Custom ggplot2 Theme
#'
#' CocoPack themes for ggplot2.
#'
#' The default theme is a minimalist theme based on theme_bw() with gridlines removed.
#' 
#' @param which Character string specifying the theme variant. Currently only 'default' is supported.
#' @param text_size Numeric value for the base text size in the theme. Default is 16.
#' @return A ggplot2 theme object
#' @export
#' @importFrom ggplot2 theme element_text theme_bw
cocopack_theme <- function(which='default', text_size=16) {
  if (which == 'default') {
    theme_bw() + theme(
      text = element_text(size = text_size),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  } else {
    stop("Invalid CocoPack theme; choose from c('default')")
  }
}

#' Add a Closer Legend to ggplot
#'
#' Modifies the legend position and margins to place it closer to the plot
#' area than the default ggplot2 spacing.
#'
#' @param position Character string specifying the legend position ('bottom', 'top', etc.)
#' @return A ggplot2 theme object that modifies legend positioning
#' @export
#' @importFrom ggplot2 theme margin
closer_legend <- function(position='bottom') {
  theme(legend.justification="center",
        legend.position=position, 
        legend.box.margin=margin(-12,0,0,0))
}

#' Display Available ggplot2 Shape Palettes
#'
#' Creates a visualization showing all available shape options in ggplot2 (0-24)
#' with their corresponding numerical IDs.
#'
#' @return A ggplot object showing the shape palette
#' @export
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual labs theme_minimal coord_fixed element_blank
view_ggplot2_shapes <- function() {
  data.frame(x = 1:25, y = rep(1, 25), shape = 0:24) %>%
  ggplot(aes(x = x, y = y)) +
    geom_point(aes(shape = factor(shape)), 
              size = 5, fill = "lightblue") +
    scale_shape_manual(values = 0:24) +
    labs(title = "ggplot2 Shape Palette", 
        shape = "Shape ID") +
    theme_minimal() + coord_fixed(ratio = 25) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}