#     version='0.1.0',
#     description='',
#     author='Phil Steinke',
#     license='',

if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

# Reference packages from categorical assignment
# packages = c('askpass', 'bookdown', 'base64enc', 'curl', 'docstring',  'dplyr', 'ggplot2', 'here', 'htmltools', 'httr', 'janitor', 
#              'jsonlite', 'kableExtra', 'lubridate', 'openssl', 'packrat', 'purrr', 'rmarkdown', 'rvest', 'selectr', 
#              'snakecase', 'sys', 'testthat', 'tidyr', 'tinytex', 'vroom', 'webshot', 'xml2', 'tidygeocoder', 'geosphere', 
#              'tidyverse', 'backport', 'gridExtra', 'GGally', 'caret')
packages = c('rticles','vroom','dplyr','lubridate','purrr','janitor','ggplot2','gridExtra','rjags','ks','hrbrthemes','here')

lapply(packages, install.packages)

# TODO: "lintr"
# TODO:
# install.packages('devtools')
# devtools::install_git('https://gitlab.com/botbotdotdotcom/packagr')
# library(packagr)
# packagr(packages) # alpha package to check, install and load packages

# TODO: update packrat to remotes. I currently have a bug with this
# if (!requireNamespace("remotes"))
#   install.packages("remotes")

# from setuptools import find_packages, setup
