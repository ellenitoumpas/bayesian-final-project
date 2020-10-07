# if (!requireNamespace('remotes'))
#   install.packages('remotes')

# remotes::install_github('rstudio/renv')

if (!requireNamespace('renv'))
  install.packages('renv')
library('renv')

if (!requireNamespace('here'))
  install.packages('here')
library('here')

# renv::init()

packages <- scan('requirements.txt', what="", sep='\n')
source(here('functions', 'functions_packages.R'))
lock_n_load_libraries(packages)

# TODO: "lintr"
