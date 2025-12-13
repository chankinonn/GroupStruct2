# inst/app/app.R

source("global.R")
source("app_ui.R", local = TRUE)
source("app_server.R", local = TRUE)

shiny::shinyApp(ui = app_ui, server = app_server)
