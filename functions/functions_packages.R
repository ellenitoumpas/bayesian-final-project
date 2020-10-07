packages <- scan("requirements.txt", what = "", sep = "\n")

# Reference packages we might use
# askpass, bookdown, base64enc, curl, docstring, dplyr, ggplot2, here, htmltools, httr, janitor, jsonlite, kableExtra, lubridate, openssl, purrr, rmarkdown, rvest, selectr, snakecase, sys, testthat, tidyr, tinytex, vroom, webshot, xml2, tidygeocoder, geosphere, tidyverse, backport, gridExtra, GGally, caret, 
# install unloaded packages then library the lot
lock_n_load_libraries <- function(required_packages) {
  
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])] 

  if (length(missing_packages)) {
      install.packages(missing_packages)
  }

  print(sapply(required_packages, require, character.only = TRUE))

  # take a renv snapshot:
  library('renv')
  renv::snapshot()
  # don't forget to commit this!

}
