loadLibraries <- function(libs) {
  for (lib in libs) {
    if(!requireNamespace(lib, quietly=TRUE)) install.packages(lib, quiet=TRUE)
    library(lib,character.only = TRUE)
  }
}
loadBMLibraries <- function(libs) {
  for (lib in libs) {
    BiocManager::install(lib, quiet=TRUE, force=FALSE, update=FALSE, ask=FALSE)
  }
}
