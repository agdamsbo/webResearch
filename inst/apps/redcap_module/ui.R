library(REDCapCAST)
library(REDCapR)
library(shiny)

ui <- shiny::fluidPage(
  # shiny::helpText("Submit URL and API token to browse download options"),
  shiny::textInput(
    inputId = "uri",
    label = "URI",
    value = "https://redcap.your.institution/api/"
  ),
  shiny::textInput(
    inputId = "api",
    label = "API token",
    value = ""
  ),
  shiny::tableOutput("table"),
  shiny::uiOutput("fields"),
  shiny::uiOutput("instruments"),
  shiny::uiOutput("arms"),
  shiny::actionButton("submit", "Submit"),
  DT::DTOutput("export")
)
