# project.aid::merge_scripts(list.files("R/",full.names = TRUE),dest = here::here("app/functions.R"))
# source(here::here("app/functions.R"))

# source("https://raw.githubusercontent.com/agdamsbo/webResearch/refs/heads/main/app/functions.R")

library(readr)
library(MASS)
library(stats)
library(gtsummary)
library(gt)
library(openxlsx2)
library(haven)
library(readODS)
require(shiny)
library(bslib)
library(assertthat)
library(dplyr)
library(quarto)
library(here)
library(broom)
library(broom.helpers)
library(REDCapCAST)
library(easystats)
library(patchwork)
library(DHARMa)
library(datamods)
library(toastui)
library(IDEAFilter)
library(shinyWidgets)
library(DT)
# if (!requireNamespace("webResearch")) {
#   devtools::install_github("agdamsbo/webResearch", quiet = TRUE, upgrade = "never")
# }
# library(webResearch)

source(here::here("functions.R"))

server <- function(input, output, session) {
  ## Listing files in www in session start to keep when ending and removing
  ## everything else.
  files.to.keep <- list.files("www/")

  output$docs_file <- renderUI({
    # shiny::includeHTML("www/docs.html")
    HTML(readLines("www/docs.html"))
  })


  rv <- shiny::reactiveValues(
    list = NULL,
    ds = NULL,
    input = exists("webResearch_data"),
    local_temp = NULL,
    quarto = NULL,
    test = "no",
    data_original = NULL,
    data = NULL,
    data_filtered = NULL
  )

  ##############################################################################
  #########
  #########  Data import section
  #########
  ##############################################################################

  data_file <- datamods::import_file_server(
    id = "file_import",
    show_data_in = "popup",
    trigger_return = "change",
    return_class = "data.frame",
    read_fns = list(
      ods = function(file) {
        readODS::read_ods(path = file)
      },
      dta = function(file) {
        haven::read_dta(file = file)
      }
    )
  )

  shiny::observeEvent(data_file$data(), {
    shiny::req(data_file$data())
    rv$data_original <- data_file$data()
  })

  data_redcap <- m_redcap_readServer(
    id = "redcap_import",
    output.format = "list"
  )

  shiny::observeEvent(data_redcap(), {
    rv$data_original <- purrr::pluck(data_redcap(), "data")()
  })

  output$redcap_prev <- DT::renderDT(
    {
      DT::datatable(head(purrr::pluck(data_redcap(), "data")(), 5),
        caption = "First 5 observations"
      )
    },
    server = TRUE
  )

  from_env <- import_globalenv_server(
    id = "env",
    trigger_return = "change",
    btn_show_data = FALSE,
    reset = reactive(input$hidden)
  )

  shiny::observeEvent(from_env$data(), {
    shiny::req(from_env$data())
    rv$data_original <- from_env$data()
  })

  ds <- shiny::reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    # if (v$input) {
    #   out <- webResearch_data
    # } else if (input$source == "file") {
    #   req(data_file$data())
    #   out <- data_file$data()
    # } else if (input$source == "redcap") {
    #   req(purrr::pluck(data_redcap(), "data")())
    #   out <- purrr::pluck(data_redcap(), "data")()
    # }

    req(rv$data_original)
    rv$data_original <- rv$data_original |>
      REDCapCAST::parse_data() |>
      REDCapCAST::as_factor() |>
      REDCapCAST::numchar2fct()

    rv$ds <- "loaded"

    rv$data <- rv$data_original

    rv$data_original
  })

  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  #########  Modifications

  shiny::observeEvent(rv$data_original, rv$data <- rv$data_original)
  shiny::observeEvent(input$data_reset, rv$data <- rv$data_original)

  ## Using modified version of the datamods::cut_variable_server function
  ## Further modifications are needed to have cut/bin options based on class of variable
  ## Could be defined server-side
  shiny::observeEvent(input$modal_cut, modal_cut_variable("modal_cut"))
  data_modal_cut <- cut_variable_server(
    id = "modal_cut",
    data_r = shiny::reactive(rv$data)
  )
  shiny::observeEvent(data_modal_cut(), rv$data <- data_modal_cut())


  shiny::observeEvent(input$modal_update, datamods::modal_update_factor("modal_update"))
  data_modal_update <- datamods::update_factor_server(
    id = "modal_update",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(data_modal_update(), {
    shiny::removeModal()
    rv$data <- data_modal_update()
  })



  # Show result
  output$table_mod <- toastui::renderDatagrid({
    shiny::req(rv$data)
    # data <- rv$data
    toastui::datagrid(
      # data = rv$data # ,
      data = data_filter()
      # bordered = TRUE,
      # compact = TRUE,
      # striped = TRUE
    )
  })

  output$code <- renderPrint({
    attr(rv$data, "code")
  })

  updated_data <- datamods::update_variables_server(
    id = "vars_update",
    data = reactive(rv$data),
    return_data_on_init = FALSE
  )

  output$original_str <- renderPrint({
    str(rv$data_original)
  })

  output$modified_str <- renderPrint({
    str(rv$data)
  })

  observeEvent(updated_data(), {
    rv$data <- updated_data()
  })

  # IDEAFilter has the least cluttered UI, but might have a License issue
  data_filter <- IDEAFilter::IDEAFilter("data_filter", data = reactive(rv$data), verbose = TRUE)

  # shiny::observeEvent(data_filter(), {
  #   rv$data_filtered <- data_filter()
  # })

  output$filtered_code <- shiny::renderPrint({
    cat(gsub(
      "%>%", "|> \n ",
      gsub(
        "\\s{2,}", " ",
        gsub(
          "reactive(rv$data)", "data",
          paste0(
            capture.output(attr(data_filter(), "code")),
            collapse = " "
          )
        )
      )
    ))
  })



  ##############################################################################
  #########
  #########  Data analyses section
  #########
  ##############################################################################

  ## Keep these "old" selection options as a simple alternative to the modification pane

  output$include_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "include_vars",
      selected = NULL,
      label = "Covariables to include",
      choices = colnames(data_filter()),
      multiple = TRUE
    )
  })

  output$outcome_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "outcome_var",
      selected = NULL,
      label = "Select outcome variable",
      choices = colnames(data_filter()),
      multiple = FALSE
    )
  })


  output$factor_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "factor_vars",
      selected = colnames(data_filter())[sapply(data_filter(), is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(data_filter()),
      multiple = TRUE
    )
  })

  base_vars <- shiny::reactive({
    if (is.null(input$include_vars)) {
      out <- colnames(data_filter())
    } else {
      out <- unique(c(input$include_vars, input$outcome_var))
    }
    return(out)
  })

  output$strat_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "strat_var",
      selected = "none",
      label = "Select variable to stratify baseline",
      choices = c("none", colnames(data_filter()[base_vars()])),
      multiple = FALSE
    )
  })

  ## Have a look at column filters at some point
  ## There should be a way to use the filtering the filter data for further analyses
  ## Disabled for now, as the JS is apparently not isolated
  # output$data_table <-
  #   DT::renderDT(
  #     {
  #       DT::datatable(ds()[base_vars()])
  #     },
  #     server = FALSE
  #   )
  #
  # output$data.classes <- gt::render_gt({
  #   shiny::req(input$file)
  #   data.frame(matrix(sapply(ds(), \(.x){
  #     class(.x)[1]
  #   }), nrow = 1)) |>
  #     stats::setNames(names(ds())) |>
  #     gt::gt()
  # })

  shiny::observeEvent(input$act_start, {
    bslib::nav_select(id = "main_panel", selected = "Modifications")
  })

  shiny::observeEvent(
    {
      input$load
    },
    {
      shiny::req(input$outcome_var)
      # browser()
      # Assumes all character variables can be formatted as factors
      # data <- data_filter$filtered() |>
      data <- data_filter() |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
        REDCapCAST::fct_drop.data.frame() |>
        factorize(vars = input$factor_vars)

      if (input$strat_var == "none") {
        by.var <- NULL
      } else {
        by.var <- input$strat_var
      }

      data <- data[base_vars()]

      # model <- data |>
      #   regression_model(
      #     outcome.str = input$outcome_var,
      #     auto.mode = input$regression_auto == 1,
      #     formula.str = input$regression_formula,
      #     fun = input$regression_fun,
      #     args.list = eval(parse(text = paste0("list(", input$regression_args, ")")))
      #   )

      models <- list(
        "Univariable" = regression_model_uv,
        "Multivariable" = regression_model
      ) |>
        lapply(\(.fun){
          do.call(
            .fun,
            c(
              list(data = data),
              list(outcome.str = input$outcome_var),
              list(formula.str = input$regression_formula),
              list(fun = input$regression_fun),
              list(args.list = eval(parse(text = paste0("list(", input$regression_args, ")"))))
            )
          )
        })

      check <- purrr::pluck(models, "Multivariable") |>
        performance::check_model()

      rv$list <- list(
        data = data,
        check = check,
        table1 = data |>
          baseline_table(
            fun.args =
              list(
                by = by.var
              )
          ) |>
          (\(.x){
            if (!is.null(by.var)) {
              .x |> gtsummary::add_overall()
            } else {
              .x
            }
          })() |>
          (\(.x){
            if (input$add_p == "yes") {
              .x |>
                gtsummary::add_p() |>
                gtsummary::bold_p()
            } else {
              .x
            }
          })(),
        table2 = models |>
          purrr::map(regression_table) |>
          tbl_merge(),
        input = input
      )

      output$table1 <- gt::render_gt(
        rv$list$table1 |>
          gtsummary::as_gt()
      )

      output$table2 <- gt::render_gt(
        rv$list$table2 |>
          gtsummary::as_gt()
      )

      output$check <- shiny::renderPlot({
        p <- plot(check) +
          patchwork::plot_annotation(title = "Multivariable regression model checks")
        p
        # Generate checks in one column
        # layout <- sapply(seq_len(length(p)), \(.x){
        #   patchwork::area(.x, 1)
        # })
        #
        # p + patchwork::plot_layout(design = Reduce(c, layout))

        # patchwork::wrap_plots(ncol=1) +
        # patchwork::plot_annotation(title = 'Multivariable regression model checks')
      })
    }
  )


  shiny::conditionalPanel(
    condition = "output.uploaded == 'yes'",
  )

  # observeEvent(input$act_start, {
  #   nav_show(id = "overview",target = "Import"
  #   )
  # })



  output$uploaded <- shiny::reactive({
    if (is.null(rv$ds)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)


  # Reimplement from environment at later time
  # output$has_input <- shiny::reactive({
  #   if (rv$input) {
  #     "yes"
  #   } else {
  #     "no"
  #   }
  # })

  # shiny::outputOptions(output, "has_input", suspendWhenHidden = FALSE)

  # Could be rendered with other tables or should show progress
  # Investigate quarto render problems
  # On temp file handling: https://github.com/quarto-dev/quarto-cli/issues/3992
  output$report <- downloadHandler(
    filename = shiny::reactive({
      paste0("report.", input$output_type)
    }),
    content = function(file, type = input$output_type) {
      ## Notification is not progressing
      ## Presumably due to missing
      shiny::withProgress(message = "Generating report. Hold on for a moment..", {
        rv$list |>
          write_quarto(
            output_format = type,
            input = file.path(getwd(), "www/report.qmd")
          )
      })
      file.rename(paste0("www/report.", type), file)
    }
  )

  session$onSessionEnded(function() {
    cat("Session Ended\n")
    files <- list.files("www/")
    lapply(files[!files %in% files.to.keep], \(.x){
      unlink(paste0("www/", .x), recursive = FALSE)
      print(paste(.x, "deleted"))
    })
  })
}
