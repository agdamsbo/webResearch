library(REDCapCAST)
library(REDCapR)
library(shiny)

# ns <- shiny::NS(id)




server <- function(input, output, session) {
  # ns <- NS(id)

  instr <- shiny::reactive({
    shiny::req(input$api)
    shiny::req(input$uri)
    REDCapR::redcap_instruments(redcap_uri = input$uri, token = input$api)
  })

  output$instruments <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "instruments",
      selected = NULL,
      label = "Instruments to include",
      choices = instr()[["data"]][[1]],
      multiple = TRUE
    )
  })

  dd <- shiny::reactive({
    shiny::req(input$api)
    shiny::req(input$uri)
    REDCapR::redcap_metadata_read(redcap_uri = input$uri, token = input$api)
  })

  output$fields <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "fields",
      selected = NULL,
      label = "Fields/variables to include",
      choices = dd()[["data"]][[1]],
      multiple = TRUE
    )
  })

  arms <- shiny::reactive({
    shiny::req(input$api)
    shiny::req(input$uri)
    REDCapR::redcap_event_read(redcap_uri = input$uri, token = input$api)
  })

  output$arms <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "arms",
      selected = NULL,
      label = "Arms/events to include",
      choices = arms()[["data"]][[3]],
      multiple = TRUE
    )
  })

  output$table <- shiny::renderTable({
    dd()[["data"]]
  })

  data <- shiny::eventReactive(input$submit, {
    browser()
    shiny::req(input$api)
    data <- REDCapCAST::read_redcap_tables(
      uri = input$uri,
      token = input$api,
      fields = unique(c(dd()[["data"]][[1]][1], input$fields)),
      forms = input$instruments,
      events = input$arms,
      raw_or_label = "both"
    )

    info <- REDCapR::redcap_project_info_read(redcap_uri = input$uri, token = input$api)
    filename <- info$data$project_title

    data |>
      REDCapCAST::redcap_wider() |>
      REDCapCAST::suffix2label() |>
      REDCapCAST::as_factor() |>
      dplyr::select(-dplyr::ends_with("_complete")) |>
      dplyr::select(-dplyr::any_of(dd()[["data"]][[1]][1]))
  })

  output$export <- DT::renderDT({
    data()
  })
}
