# DIY Guides + Cool Tools for Building R Packages

## Basic Development Pipeline

(Below is a verbatim description given by GPT4o of a basic development pipeline for building a GitHub-based R package.)

This pipeline outlines the steps to create and manage an R package on GitHub, specifying which files are programmatically generated and which require manual editing.

---

A step-by-step script for building this package is available in [build.R](build.R).<br>
A narrative quarto version of this same script is available in [build.qmd](build.qdm).

## 1. Set Up the Package Structure
- Use `usethis::create_package("path/to/package")` to initialize the package structure.
- Initialize a Git repository with `usethis::use_git()` and link to GitHub with `usethis::use_github()`.

## 2. Edit Core Package Files
- **DESCRIPTION**: Contains metadata about the package.
  - Programmatically generated by `usethis::create_package()`, but requires manual edits for fields like `Title`, `Description`, `Authors`, and `Version`.
  
- **NAMESPACE**: Defines functions and datasets exported by the package and imports from other packages.
  - Managed by `roxygen2` via `@export` and `@import` tags in function documentation.
  - Programmatically generated with `devtools::document()`; avoid manual edits.

## 3. Write Functions and Documentation
- Write R functions in the `R/` folder.
- Document each function with `roxygen2` comments (e.g., `@param`, `@return`, `@export`).
- Run `devtools::document()` to automatically generate:
  - **NAMESPACE** (as described above).
  - **man/** folder: Contains `.Rd` files (function manuals), generated by `roxygen2` based on the documentation comments.

## 4. Add Package Dependencies
- Use `usethis::use_package("pkg")` to add packages to the `Imports` field in `DESCRIPTION`.
- `roxygen2` will handle imports within `NAMESPACE`.

## 5. Testing
- Use `usethis::use_testthat()` to set up the `tests/testthat/` folder for unit tests.
- Write test files manually in `tests/testthat/` and run tests with `devtools::test()`.

## 6. Build and Check Package
- Use `devtools::check()` to ensure everything is correctly structured and compliant with CRAN standards. This command checks for issues in all package files.

## 7. Vignettes and Readme
- **README.md**: Use `usethis::use_readme_rmd()` to generate a starter README.
- **Vignettes**: Use `usethis::use_vignette("vignette-name")` to add a folder under `vignettes/` and a `.Rmd` file for longer examples and documentation.

## 8. GitHub Actions for CI/CD (Optional)
- `usethis::use_github_action_check_standard()` sets up GitHub Actions workflows for automated checks.

---

## Summary of File Management

Here is a table summarizing which files are programmatically generated and which require manual editing:

| File/Folder      | Programmatic | Manual               |
|------------------|--------------|----------------------|
| DESCRIPTION      | Partially    | Title, Description   |
| NAMESPACE        | Yes          | No                   |
| R/               | No           | Function code        |
| man/             | Yes          | No                   |
| tests/           | Partially    | Test code            |
| vignettes/       | Partially    | Main content         |
| README.md        | Partially    | Content              |

---