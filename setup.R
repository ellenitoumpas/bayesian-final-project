if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

if (!requireNamespace("here"))
  install.packages("here")

library('here')
source(here('functions', 'install_packages.R'))

# TODO: "lintr"
