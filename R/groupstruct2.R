#' Launch the GroupStruct2 Shiny app
#'
#' This function launches the Shiny app from the package.
#' @export
groupstruct2 <- function() {
  app_dir <- system.file("app", package = "GroupStruct2")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::shinyAppDir(appDir = app_dir)
}
