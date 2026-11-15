#' Run Task 5 Shiny Application
#'
#' Launches the interactive Shiny application for Task 5.
#' @export
run_app5 <- function() {
  appPath <- system.file("app_task5.R", package = "AURATHOMALAB12")
  if (appPath == "") {
    stop("Could not find app file. Try re-installing `AURATHOMALAB12`.", call. = FALSE)
  }
  shiny::runApp(appPath)
}
