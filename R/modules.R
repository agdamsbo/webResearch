#' Shiny UI module to load a data file
#'
#' @param id id
#'
#' @return shiny UI
#' @export
#'
m_datafileUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fileInput(
      inputId = ns("file"),
      label = "Upload a file",
      multiple = FALSE,
      accept = c(
        ".csv",
        ".xlsx",
        ".xls",
        ".dta",
        ".ods",
        ".rds"
      )
    ),
    shiny::h4("Parameter specifications"),
    shiny::helpText(em("Select the desired variables and press 'Submit'")),
    shiny::uiOutput(ns("include_vars")),
    DT::DTOutput(ns("data_input")),
    shiny::actionButton(ns("submit"), "Submit")
  )
}

m_datafileServer <- function(id, output.format = "df") {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session, ...) {
    ns <- shiny::NS(id)
    ds <- shiny::reactive({
      REDCapCAST::read_input(input$file$datapath) |> REDCapCAST::parse_data()
    })

    output$include_vars <- shiny::renderUI({
      shiny::req(input$file)
      selectizeInput(
        inputId = ns("include_vars"),
        selected = NULL,
        label = "Covariables to include",
        choices = colnames(ds()),
        multiple = TRUE
      )
    })

    base_vars <- shiny::reactive({
      if (is.null(input$include_vars)) {
        out <- colnames(ds())
      } else {
        out <- input$include_vars
      }
      return(out)
    })

    output$data_input <-
      DT::renderDT({
        shiny::req(input$file)
        ds()[base_vars()]
      })

    shiny::eventReactive(input$submit, {
      shiny::req(input$file)

      file_export(
        data = ds()[base_vars()] |> REDCapCAST::numchar2fct(),
        output.format = output.format,
        filename = tools::file_path_sans_ext(input$file$name)
      )
    })
  })
}


m_redcap_readUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(
      inputId = ns("uri"),
      label = "URI",
      value = "https://redcap.your.institution/api/"
    ),
    shiny::textInput(
      inputId = ns("api"),
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

m_redcap_readServer <- function(id, output.format="df") {
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session,...) {
      ns <- shiny::NS(id)
      instr <- shiny::reactive({
        shiny::req(input$api)
        shiny::req(input$uri)
        REDCapR::redcap_instruments(redcap_uri = input$uri, token = input$api)
      })

      output$instruments <- shiny::renderUI({
        shiny::selectizeInput(
          inputId = ns("instruments"),
          # inputId = "instruments",
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
          # inputId = "fields",
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
          # inputId = "arms",
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
          uri = input$uri,
          token = input$api,
          fields = unique(c(dd()[["data"]][[1]][1], input$fields)),
          forms = input$instruments,
          events = input$arms,
          raw_or_label = "both"
        )

        info <- REDCapR::redcap_project_info_read(redcap_uri = input$uri,
                                                  token = input$api)

        data |>
          REDCapCAST::redcap_wider() |>
          REDCapCAST::suffix2label() |>
          REDCapCAST::as_factor() |>
          dplyr::select(-dplyr::ends_with("_complete")) |>
          dplyr::select(-dplyr::any_of(dd()[["data"]][[1]][1])) |>
          file_export(
            output.format = output.format,
            filename = info$data$project_title
          )
      })
    }
  )
}
