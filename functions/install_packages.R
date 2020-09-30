packages <- scan("requirements.txt",
 what="",
 sep="\n")

install_packages <- function() {
  lapply(packages,
   install.packages)
}

# Reference packages from categorical assignment
# askpass, bookdown, base64enc, curl, docstring, dplyr, ggplot2, here, htmltools, httr, janitor, jsonlite, kableExtra, lubridate, openssl, purrr, rmarkdown, rvest, selectr, snakecase, sys, testthat, tidyr, tinytex, vroom, webshot, xml2, tidygeocoder, geosphere, tidyverse, backport, gridExtra, GGally, caret, 