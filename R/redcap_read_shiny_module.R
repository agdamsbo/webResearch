#' Shiny module to browser and export REDCap data
#'
#' @param id Namespace id
#' @param include_title logical to include title
#'
#' @rdname redcap_read_shiny_module
#'
#' @return shiny ui element
#' @export
m_redcap_readUI <- function(id, include_title = TRUE) {
  ns <- shiny::NS(id)

  server_ui <- shiny::column(
    width = 6,
    shiny::tags$h4("REDCap server information"),
    shiny::textInput(
      inputId = ns("uri"),
      label = "URI/Address",
      value = "https://redcap.your.institution/api/"
    ),
    shiny::textInput(
      inputId = ns("api"),
      label = "API token",
      value = ""
    )
  )


  params_ui <-
    shiny::column(
      width = 6,
      shiny::tags$h4("Data import parameters"),
      shiny::helpText("Options here will show, when API and uri are typed"),
      shiny::uiOutput(outputId = ns("fields")),
      shinyWidgets::switchInput(
        inputId = "do_filter",
        label = "Apply filter?",
        value = FALSE,
        inline = FALSE,
        onLabel = "YES",
        offLabel = "NO"
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


  shiny::fluidPage(
    if (include_title) shiny::tags$h3("Import data from REDCap"),
    fluidRow(
      server_ui,
      params_ui),
    shiny::column(
      width = 12,
      # shiny::actionButton(inputId = ns("import"), label = "Import"),
      bslib::input_task_button(
        id = ns("import"),
        label = "Import",
        icon = shiny::icon("download", lib = "glyphicon"),
        label_busy = "Just a minute...",
        icon_busy = fontawesome::fa_i("arrows-rotate",
                                      class = "fa-spin",
                                      "aria-hidden" = "true"
        ),
        type = "primary",
        auto_reset = TRUE
      ),
      shiny::helpText("Press 'Import' after having specified API token and URI to export data from the REDCap server. A preview will show below the DataDictionary."),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      DT::DTOutput(outputId = ns("table"))
      # toastui::datagridOutput2(outputId = ns("table"))
    )
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

    # data_list <- shiny::reactiveValues(
    #   dict = NULL,
    #   stat = NULL,
    #   arms = NULL,
    #   data = NULL,
    #   name = NULL
    # )

    dd <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(input$uri)


      REDCapR::redcap_metadata_read(
        redcap_uri = input$uri,
        token = input$api
      )$data
    })

    # dd <- shiny::reactive({
    #   shiny::req(input$api)
    #   shiny::req(input$uri)
    #
    #
    #   out <- REDCapR::redcap_metadata_read(
    #     redcap_uri = input$uri,
    #     token = input$api
    #   )
    #
    #   data_list$dict <- out$data
    #   data_list$stat <- out$success
    #
    #   out$data
    # })

    arms <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(input$uri)

      REDCapR::redcap_event_read(
        redcap_uri = input$uri,
        token = input$api
      )$data

      # data_list$arms <- out
      # out
    })

    output$fields <- shiny::renderUI({
      shinyWidgets::virtualSelectInput(
        inputId = ns("fields"),
        label = "Select fields/variables to import:",
        choices = dd() |>
          dplyr::select(field_name, form_name) |>
          (\(.x){
            split(.x$field_name, .x$form_name)
          })() # |>
        # stats::setNames(instr()[["data"]][[2]])
        ,
        updateOn = "close",
        multiple = TRUE,
        search = TRUE,
        showValueAsTags = TRUE
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
        # shiny::req(data_list$dict)
        # dd()[["data"]][c(1,2,4,5,6,8)]
        # browser()
        data.df <- dd()[, c(1, 2, 4, 5, 6, 8)]
        DT::datatable(data.df,
                      caption = "Subset of data dictionary"
        )
      },
      server = TRUE
    )

    # Messes up the overlay of other objects. JS thing?
    # output$table <- toastui::renderDatagrid2(
    #   {
    #     shiny::req(input$api)
    #     shiny::req(input$uri)
    #     # shiny::req(data_list$dict)
    #     # dd()[["data"]][c(1,2,4,5,6,8)]
    #     # browser()
    #     toastui::datagrid(dd()[,c(1, 2, 4, 5, 6, 8)]
    #     )
    #   }
    # )

    name <- shiny::reactive({
      shiny::req(input$api)
      REDCapR::redcap_project_info_read(
        redcap_uri = input$uri,
        token = input$api
      )$data$project_title
    })

    shiny::eventReactive(input$import, {
      shiny::req(input$api)
      shiny::req(input$fields)
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
          meta = dd(),
          name = name(),
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

#' REDCap import teal data module
#'
#' @rdname redcap_read_shiny_module
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


#' Test app for the redcap_read_shiny_module
#'
#' @rdname redcap_read_shiny_module
#'
#' @examples
#' \dontrun{
#' redcap_app()
#' }
redcap_app <- function() {
  ui <- shiny::fluidPage(
    m_redcap_readUI("data"),
    # DT::DTOutput(outputId = "redcap_prev")
    toastui::datagridOutput2(outputId = "redcap_prev"),
    shiny::fluidRow(
      shiny::column(
        8,
        # verbatimTextOutput("data_filter_code"),
        DT::DTOutput("data_summary")
      ),
      shiny::column(4, IDEAFilter::IDEAFilter_ui("data_filter"))
    )
  )
  server <- function(input, output, session) {
    data_val <- shiny::reactiveValues(data=NULL)

    ds <- m_redcap_readServer("data", output.format = "df")
    # output$redcap_prev <- DT::renderDT(
    #   {
    #     DT::datatable(purrr::pluck(ds(), "data")(),
    #       caption = "Observations"
    #     )
    #   },
    #   server = TRUE
    # )

    # shiny::reactive({
    #   data_val$data <- purrr::pluck(ds(), "data")()
    # })

    output$redcap_prev <- toastui::renderDatagrid2({
      # toastui::datagrid(purrr::pluck(ds(), "data")())
      # toastui::datagrid(data_val$data)
      toastui::datagrid(ds())
    })

    filtered_data <- IDEAFilter::IDEAFilter("data_filter",
                                            data = ds,
                                            verbose = FALSE)

    # filtered_data <- shiny::reactive({
    #   IDEAFilter::IDEAFilter("data_filter",
    #                          data = purrr::pluck(ds(), "data")(),
    #                          verbose = FALSE)
    # })

    # output$data_filter_code <- renderPrint({
    #   cat(gsub(
    #     "%>%", "%>% \n ",
    #     gsub(
    #       "\\s{2,}", " ",
    #       paste0(
    #         capture.output(attr(filtered_data(), "code")),
    #         collapse = " "
    #       )
    #     )
    #   ))
    # })

    output$data_summary <- DT::renderDataTable(
      {
        filtered_data()
      },
      options = list(
        scrollX = TRUE,
        pageLength = 5
      )
    )
  }
  shiny::shinyApp(ui, server)
}
