#' Shiny UI module to load a data file
#'
#' @param id id
#'
#' @return shiny UI
#' @export
#'
m_datafileUI <- function(id) {
  ns <- shiny::NS(id)
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
    shiny::helpText(shiny::em("Select the desired variables and press 'Submit'")),
    shiny::uiOutput(ns("include_vars")),
    DT::DTOutput(ns("data_input")),
    shiny::actionButton(ns("submit"), "Submit")
  )
}

m_datafileServer <- function(id, output.format = "df") {
  shiny::moduleServer(id, function(input, output, session, ...) {
    ns <- shiny::NS(id)
    ds <- shiny::reactive({
      REDCapCAST::read_input(input$file$datapath) |> REDCapCAST::parse_data()
    })

    output$include_vars <- shiny::renderUI({
      shiny::req(input$file)
      shiny::selectizeInput(
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
      out
    })

    output$data_input <-
      DT::renderDT({
        shiny::req(input$file)
        ds()[base_vars()]
      })

    shiny::eventReactive(input$submit, {
      # shiny::req(input$file)

      data <- shiny::isolate({
        ds()[base_vars()]
      })

      file_export(data,
        output.format = output.format,
        tools::file_path_sans_ext(input$file$name)
      )
    })
  })
}





file_app <- function() {
  ui <- shiny::fluidPage(
    m_datafileUI("data"),
    # DT::DTOutput(outputId = "redcap_prev")
    toastui::datagridOutput2(outputId = "redcap_prev")
  )
  server <- function(input, output, session) {
    m_datafileServer("data", output.format = "list")
  }
  shiny::shinyApp(ui, server)
}

file_app()

tdm_data_upload <- teal::teal_data_module(
  ui <- function(id) {
    shiny::fluidPage(
      m_datafileUI(id)
    )
  },
  server = function(id) {
    m_datafileServer(id, output.format = "teal")
  }
)

tdm_data_read <- teal::teal_data_module(
  ui <- function(id) {
    shiny::fluidPage(
      m_redcap_readUI(id = "redcap")
    )
  },
  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns

        m_redcap_readServer(id = "redcap", output.format = "teal")
      }
    )
  }
)
