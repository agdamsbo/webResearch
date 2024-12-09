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


#' Shiny module to browser and export REDCap data
#'
#' @param id Namespace id
#' @rdname redcap_read_shiny_module
#'
#' @return shiny ui element
#' @export
m_redcap_readUI <- function(id) {
  ns <- shiny::NS(id)

  server_ui <- fluidRow(
    column(
      width = 6,
      shiny::textInput(
        inputId = ns("uri"),
        label = "URI",
        value = "https://redcap.your.institution/api/"
      ),
      shiny::textInput(
        inputId = ns("api"),
        label = "API token",
        value = ""
      )
    )
  )

  params_ui <- fluidRow(
    column(
      width = 6,
      shiny::uiOutput(outputId = ns("fields")),
      shinyWidgets::switchInput(
        inputId = "do_filter",
        label = "Apply filter?",
        value = FALSE,
        inline = TRUE
      ),
      # shiny::radioButtons(
      #   inputId = "do_filter",
      #   label = "Filter export?",
      #   selected = "no",
      #   inline = TRUE,
      #   choices = list(
      #     "No" = "no",
      #     "Yes" = "yes"
      #   )
      # ),
      shiny::conditionalPanel(
        condition = "input.do_filter",
        shiny::uiOutput(outputId = ns("arms")),
        shiny::textInput(
          inputId = ns("filter"),
          label = "Optional filter logic (e.g., ⁠[gender] = 'female')"
        )
      )
    )
  )

  shiny::fluidPage(
    server_ui,
    params_ui,
    shiny::actionButton(inputId = ns("import"), label = "Import"),
    shiny::br(),
    DT::DTOutput(outputId = ns("table"))
    # toastui::datagridOutput2(outputId = ns("table")),
    # toastui::datagridOutput2(outputId = ns("data")),
    # shiny::actionButton(inputId = ns("submit"), label = "Submit"),
    # DT::DTOutput(outputId = ns("data_prev"))
  )
}

#' @param output.format data.frame ("df") or teal data object ("teal")
#' @rdname redcap_read_shiny_module
#'
#' @return shiny server module
#' @export
#'
m_redcap_readServer <- function(id, output.format = c("df", "teal", "list")) {
  output.format <- match.arg(output.format)

  module <- function(input, output, session) {
    # ns <- shiny::NS(id)
    ns <- session$ns

    dd <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(input$uri)

      REDCapR::redcap_metadata_read(
          redcap_uri = input$uri,
          token = input$api
        )$data
    })

    arms <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(input$uri)

      REDCapR::redcap_event_read(
          redcap_uri = input$uri,
          token = input$api
        )$data
    })

    output$fields <- shiny::renderUI({
      shinyWidgets::virtualSelectInput(
        inputId = ns("fields"),
        label = "Multiple select:",
        choices = dd() |>
          dplyr::select(field_name, form_name) |>
          (\(.x){
            split(.x$field_name, .x$form_name)
          })() # |>
        # stats::setNames(instr()[["data"]][[2]])
        ,
        updateOn = "close",
        multiple = TRUE
      )
    })

    output$arms <- shiny::renderUI({
      shiny::selectizeInput(
        # inputId = "arms",
        inputId = ns("arms"),
        selected = NULL,
        label = "Filter by events/arms",
        choices = arms()[[3]],
        multiple = TRUE
      )
    })

    output$table <- DT::renderDT(
      {
        shiny::req(input$api)
        shiny::req(input$uri)
        # dd()[["data"]][c(1,2,4,5,6,8)]
        data.df <- dd()[c(1, 2, 4, 5, 6, 8)]
        DT::datatable(data.df,
          caption = "Subset of data dictionary"
        )
      },
      server = TRUE
    )

    name <- reactive({
      shiny::req(input$api)
      REDCapR::redcap_project_info_read(
        redcap_uri = input$uri,
        token = input$api
      )$data$project_title
    })

    shiny::eventReactive(input$import, {
      shiny::req(input$api)
      record_id <- dd()[[1]][1]

      redcap_data <- REDCapCAST::read_redcap_tables(
        uri = input$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        # forms = input$instruments,
        events = input$arms,
        raw_or_label = "both",
        filter_logic = input$filter
      ) |>
        REDCapCAST::redcap_wider() |>
        dplyr::select(-dplyr::ends_with("_complete")) |>
        dplyr::select(-dplyr::any_of(record_id)) |>
        REDCapCAST::suffix2label()

      out_object <- file_export(redcap_data,
        output.format = output.format,
        filename = name()
      )

      if (output.format == "list") {
        out <- list(
            data = shiny::reactive(redcap_data),
            meta = dd()[["dd"]],
            name = name,
            filter = input$filter
          )

      } else {
        out <- out_object
      }

      return(out)
    })
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}


tdm_redcap_read <- teal::teal_data_module(
  ui <- function(id) {
    shiny::fluidPage(
      m_redcap_readUI(id)
    )
  },
  server = function(id) {
    m_redcap_readServer(id, output.format = "teal")
  }
)

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


redcap_app <- function() {
  ui <- fluidPage(
    m_redcap_readUI("data"),
    DT::DTOutput(outputId = "redcap_prev")
  )
  server <- function(input, output, session) {
    ds <- m_redcap_readServer("data")
    output$redcap_prev <- DT::renderDT(
      {

        # df <- shiny::isolate(data_redcap())
        # browser()
        #
        DT::datatable(ds(),
                      caption = "Observations"
        )
      },
      server = TRUE
    )
  }
  shinyApp(ui, server)
}

redcap_app()
