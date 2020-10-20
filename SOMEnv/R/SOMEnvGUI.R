#' The function starts the SOMEnv GUI
#'
#' @author Sabina Licen, Marco Franzon, Tommaso Rodani
#'
#' @return This function starts the graphical user interface with the default system browser.
#'The main help suggestion for using the tool are embedded in the GUI
#'
#' @references {Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan
#'             McPherson (2019). shiny: Web Application Framework for R. R package
#'             version 1.4.0. https://CRAN.R-project.org/package=shiny
#' }
#'
#'seealso shiny
#'
#' @export
#' @examples
#' \dontrun{
#' SomEnvGUI()
#' }



SomEnvGUI <- function() {
  shiny::runApp(system.file('shiny-examples/myapp', package='SOMEnv'))
}


