packages <- scan("requirements.txt", what="", sep="\n")

load_packages <- function() {
  # this allows us to install packages as we go, kind of like a cheap docker image
  lock_n_load_libraries(packages)
}
