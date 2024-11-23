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
library(shiny)
library(bslib)
library(assertthat)
library(dplyr)
library(quarto)
library(here)
library(broom)
library(broom.helpers)
if (!requireNamespace("REDCapCAST")) {
  devtools::install_github("agdamsbo/REDCapCAST", quiet = TRUE, upgrade = "never")
}
library(REDCapCAST)
if (!requireNamespace("webResearch")) {
  devtools::install_github("agdamsbo/webResearch", quiet = TRUE, upgrade = "never")
}
library(webResearch)

server <- function(input, output, session) {
  ## Listing files in www in session start to keep when ending and removing
  ## everything else.
  files.to.keep <- list.files("www/")

  v <- shiny::reactiveValues(
    list = NULL,
    ds = NULL,
    input = exists("webResearch_data"),
    local_temp = NULL,
    quarto = NULL,
    test = "no"
  )

  test_data <- shiny::eventReactive(input$test_data, {
    v$test <- "test"
  })

  ds <- shiny::reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if (v$input) {
      out <- webResearch_data
    } else if (v$test=="test") {
      out <- gtsummary::trial
    } else {
      shiny::req(input$file)
      out <- read_input(input$file$datapath)
    }

    v$ds <- "present"
    return(out)
  })

  output$include_vars <- shiny::renderUI({
    selectizeInput(
      inputId = "include_vars",
      selected = NULL,
      label = "Covariables to include",
      choices = colnames(ds())[-match(input$outcome_var, colnames(ds()))],
      multiple = TRUE
    )
  })

  output$outcome_var <- shiny::renderUI({
    selectInput(
      inputId = "outcome_var",
      selected = NULL,
      label = "Select outcome variable",
      choices = colnames(ds()),
      multiple = FALSE
    )
  })

  output$factor_vars <- shiny::renderUI({
    selectizeInput(
      inputId = "factor_vars",
      selected = colnames(ds())[sapply(ds(), is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(ds())[sapply(ds(), is.character)],
      multiple = TRUE
    )
  })

  output$data.input <- shiny::renderTable({
    utils::head(ds(),20)
  })

  output$data.classes <- shiny::renderTable({
    shiny::req(input$file)
    data.frame(matrix(sapply(ds(),\(.x){class(.x)[1]}),nrow=1)) |>
      stats::setNames(names(ds()))
  })

  shiny::observeEvent(
    {
      input$load
    },
    {
      shiny::req(input$outcome_var)

      # Assumes all character variables can be formatted as factors
      data <- ds() |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

      data <- data |> factorize(vars = input$factor_vars)

      if (is.factor(data[[input$outcome_var]])) {
        by.var <- input$outcome_var
      } else {
        by.var <- NULL
      }

      if (is.null(input$include_vars)) {
        base_vars <- colnames(data)
      } else {
        base_vars <- c(input$include_vars, input$outcome_var)
      }

      data <- dplyr::select(data, dplyr::all_of(base_vars))

      model <- data |>
        regression_model(
          outcome.str = input$outcome_var,
          auto.mode = input$regression_auto == 1,
          formula.str = input$regression_formula,
          fun = input$regression_fun,
          args.list = eval(parse(text = paste0("list(", input$regression_args, ")")))
        )


      v$list <- list(
        data = data,
        table1 = data |>
          baseline_table(
            fun.args =
              list(
                by = by.var
              )
          ),
        table2 = model |>
          regression_table()
      )

      output$table1 <- gt::render_gt(
        v$list$table1 |>
          gtsummary::as_gt()
      )

      output$table2 <- gt::render_gt(
        v$list$table2 |>
          gtsummary::as_gt()
      )



    }
  )

  # renderUI({
  #   tags$iframe(seamless="seamless",
  #               src= "Hub_Infographic.html",
  #               width=800,
  #               height=800)
  # })
  #
  #
  # getPage<-shiny::reactive({
  #   shiny::req(file.exists(file.path(getwd(), "www/report_format.html")))
  #   return(shiny::includeHTML(file.path(getwd(), "www/report_format.html")))
  # })

  output$uploaded <- shiny::reactive({
    if (is.null(v$ds)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)

  output$has_input <- shiny::reactive({
    if (v$input) {
      "yes"
    } else {
      "no"
    }
  })

  shiny::outputOptions(output, "has_input", suspendWhenHidden = FALSE)

  # Could be rendered with other tables or should show progress
  # Investigate quarto render problems
  # On temp file handling: https://github.com/quarto-dev/quarto-cli/issues/3992
  output$report <- downloadHandler(
    filename = shiny::reactive({
      paste0("report.", input$output_type)
    }),
    content = function(file, type = input$output_type) {
      v$list |>
        write_quarto(
          output_format = type,
          input = file.path(getwd(), "www/report.qmd")
        )
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
