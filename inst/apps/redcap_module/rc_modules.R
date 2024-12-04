m_redcap_readUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    shiny::tableOutput(outputId = ns("table")),
    shiny::uiOutput(outputId = ns("fields")),
    shiny::uiOutput(outputId = ns("instruments")),
    shiny::uiOutput(outputId = ns("arms")),
    shiny::actionButton(inputId = ns("submit"), "Submit")
  )
}

m_redcap_readServer <- function(id) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      instr <- shiny::reactive({
        shiny::req(input$api)
        shiny::req(input$uri)
        REDCapR::redcap_instruments(redcap_uri = input$uri, token = input$api)
      })

      output$instruments <- shiny::renderUI({
        shiny::selectizeInput(
          inputId = ns("instruments"),
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
          inputId = ns("fields"),
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
          inputId = ns("arms"),
          selected = NULL,
          label = "Arms/events to include",
          choices = arms()[["data"]][[3]],
          multiple = TRUE
        )
      })

      output$table <- shiny::renderTable({
        dd()[["data"]]
      })

      shiny::eventReactive(input$submit, {
        shiny::req(input$api)
        data <- REDCapCAST::read_redcap_tables(
          uri=input$uri,
          token = input$api,
          fields = unique(c(dd()[["data"]][[1]][1],input$fields)),
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
    }
  )
}
