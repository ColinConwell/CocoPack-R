if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('this.path', 'usethis', 'devtools', 
               'roxygen2', 'remotes', 'knitr', 'pkgdown')

# set working directory:
setwd(dirname(this.dir()))

list.files() # check contents

# generate man pages
devtools::document()

# check integrity:
devtools::check()

# install the package:
devtools::install('.')

# clean pkgdown site:
pkgdown::clean_site()

# build pkgdown site:
pkgdown::build_site()

# Rebuild with development mode
options(pkgdown.internet = FALSE)
pkgdown::build_site(preview = TRUE)

# preview pkgdown site:
pkgdown::preview_site()

# test package import:
pacman::p_load('cocopack')
cocopack::say_hello("Coco")

# uninstall the package:
remove.packages("cocopack")
