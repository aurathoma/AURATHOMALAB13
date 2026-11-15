#' Run Task 3 Shiny Application
#'
#' Launches the interactive Shiny application for Task 3.
#' @export
run_app3 <- function() {
  appPath <- system.file("app_task3.R", package = "AURATHOMALAB12")
  if (appPath == "") {
    stop("Could not find app file. Try re-installing `AURATHOMALAB12`.", call. = FALSE)
  }
  shiny::runApp(appPath)
}
