# inst/app/app.R

cat("\n")
cat("  +==========================+\n")
cat("  |   GroupStruct2  v1.3.1  |\n")
cat("  +==========================+\n")
cat("\n  Check for the latest version at:\n")
cat("  https://github.com/chankinonn/GroupStruct2\n")
cat("\n  Loading packages...\n\n")

source("global.R")
source("app_ui.R", local = TRUE)
source("app_server.R", local = TRUE)

shiny::shinyApp(ui = app_ui, server = app_server)
