.onAttach <- function(libname, pkgname){
  packageStartupMessage("######################################################################
Welcome to 'SOMEnv' package
For further information use help(SOMEnv)
Find citation details with citation(\"SOMEnv\")
Use SomEnvGUI() to start the Graphical User Interface in the default browser.
######################################################################")
}
.onLoad <- function(libname, pkgname){
  library(shinycssloaders)
  library(shinycustomloader)
  library(openair)
  library(kohonen)
  library(rlist)
  library(colourpicker)
  library(plyr)
}