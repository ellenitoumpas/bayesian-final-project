if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

if (!requireNamespace("here"))
  install.packages("here")

packages <- scan("requirements.txt", what="", sep="\n")
lock_n_load_libraries(packages)

# TODO: "lintr"
