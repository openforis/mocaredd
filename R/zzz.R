

## Make path to images from inst/assets available for the shiny app
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "assets",
    directoryPath = system.file(
      "assets",
      package = "mocaredd"
    )
  )
}

## Remove path to images for the shiny app when package is not loaded
.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("assets")
}
