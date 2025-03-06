# CocoPack-R <img src="man/figures/logo.png" align="right" width="224px" height="224px" />

Miscellaneous, tidy-style quality-of-life and convenience functions for development in R, inspired by packages like [`Hmisc`](https://hbiostat.org/r/hmisc/).

*Caveat Emptor*: Much of the documentation for this package is the product of a custom auto-documentation tool (coming soon!) I've written with ample help from Claude Sonnet 3.5. The code itself is code I've used and tested, but the documentation may not always be fully accurate.

## Installation

You can install the development version of CocoPack from GitHub with:

```r
if (!require(devtools)) {install.packages("devtools")}
devtools::install_github("colinconwell/CocoPack-R")
```

...or (CocoPack's preferred method), with [`pacman`](https://github.com/trinker/pacman):

```r
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load_gh("colinconwell/CocoPack-R")
```

Note: `pacman` simultaneously installs the package and loads it.

## Features

### Bootstrap Analysis Tools

```r
# Bootstrap analysis with stats parsing
results <- run_bootstrap(
  data_frame = your_data,
  group_vars = c("group1", "group2"),
  boot_fn = your_function,
  times = 1000
)

# Spearman correlation with bootstrap
cor_results <- spearman_boot(
  df = your_data,
  x_var = "variable1",
  y_var = "variable2",
  group_vars = c("group"),
  R = 1000
)

# Add significance labels
results <- label_significance(results, p_col = "p", alpha = 0.05)
```

### Data Utilities

Helpful functions for data manipulation:

```r
# flip mapping keys and values
mapping <- list(a=1, b=2, c=3, d=4, e=5)
flip_names(mapping)

# Slice a vector like dplyr::slice()
# with python-style indexing:
vector <- c('a', 'b', 'c', 'd', 'e')
vslice(vector, start, stop=NULL)

# Read CSV and add filename as a column
df <- read_csv_add_name("path/to/your/file.csv")
```

### Plot Utilities

Helper functions for plotting with ggplot2:

```r
# View all ggplot2 shapes
view_ggplot2_shapes()

# Legends with less margin
plot + closer_legend()
```

## DIY Development Tools

The `diy/` directory contains helpful resources for package development:

- `README.md`: Comprehensive guide for R package development pipeline
- `build.qmd`: Step-by-step Quarto document for building the package
- `hexicon.R`: Script for generating the package's hexagonal logo / sticker

### Using the Build Tools

The build process is documented in `diy/build.qmd` and includes:

1. Documentation generation
2. Package integrity checks
3. Description file management
4. Remote installation options

Example usage of the hex sticker generator: `diy/hexicon.R`

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## Bug Reports

Please report any bugs or issues on the [GitHub issues page](https://github.com/colinconwell/CocoPack-R/issues).
