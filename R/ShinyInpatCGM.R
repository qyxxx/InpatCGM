#' @title Launch the Inpatient CGM Shiny App
#' @description
#' This function launches the Shiny application bundled within the \code{InpatCGM} package.
#' It is intended to provide a user-friendly interface for exploring and analyzing
#' continuous glucose monitoring (CGM) data in inpatient settings.
#'
#' The app UI and server logic are located in the \code{inst/shiny/} directory of the package.
#'
#' @return Launches a Shiny application in the default web browser. Returns \code{NULL} invisibly.
#' @examples
#' \dontrun{
#'   runInpatCGM()
#' }
#' @export
runInpatCGM <- function() {
  appDir <- system.file("shiny", package = "InpatCGM")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing `InpatCGM`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
