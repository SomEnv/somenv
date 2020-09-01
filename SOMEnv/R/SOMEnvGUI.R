#' @export
SOMEnvGUI <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "SOMEnv")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `SOMEnv`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

library(rlist)
library(kohonen)
library(shiny)
library(plyr)
library(colourpicker)
library(openair)
library(shinycssloaders)
library(shinycustomloader)


