library(shiny)

if (file.exists(".Renviron")) readRenviron(".Renviron")

source("ui.R", encoding = "UTF-8")
source("server.R", encoding = "UTF-8")

shinyApp(ui = ui, server = server)