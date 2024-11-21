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
if (!requireNamespace("webResearch")) {
  devtools::install_github("agdamsbo/webResearch", quiet = TRUE, upgrade = "never")
}
library(webResearch)

server <- function(input, output, session) {
  v <- shiny::reactiveValues(
    list = NULL,
    ds = NULL,
    input = exists("webResearch_data"),
    local_temp = NULL
  )

  ds <- shiny::reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if (v$input) {
      out <- webResearch_data
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

  output$data.input <- shiny::renderTable({
    ds()
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

      if (is.factor(data[[input$outcome_var]])) {
        by.var <- input$outcome_var
      } else {
        by.var <- NULL
      }

      if (is.null(input$include_vars)) {
        base_vars <- NULL
      } else {
        base_vars <- c(input$include_vars, input$outcome_var)
      }


      v$list <- list(
        data = data,
        table1 = data |>
          baseline_table(
            vars = base_vars,
            fun.args =
              list(
                by = by.var
              )
          ),
        table2 = data |>
          regression_model(
            outcome.str = input$outcome_var,
            auto.mode = input$regression_auto == 1,
            formula.str = input$regression_formula,
            fun = input$regression_fun,
            args.list = eval(parse(text = paste0("list(", input$regression_args, ")"))),
            vars = input$include_vars
          ) |>
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

  #####
  #### Generating output
  #####

  # Downloadable csv of selected dataset ----
  # output$downloadData <- shiny::downloadHandler(
  #   filename = "index_lookup.csv",
  #   content = function(file) {
  #     write.csv(v$index, file, row.names = FALSE)
  #   }
  # )


  # Could be rendered with other tables or should show progress
  # Investigate quarto render problems
  # On temp file handling: https://github.com/quarto-dev/quarto-cli/issues/3992
  output$report <- downloadHandler(
    filename = "analyses.html",
    content = function(file) {
      local.temp <- paste0("temp.", tools::file_ext(file))
      v$list |>
        write_quarto(
          file = local.temp,
          qmd.file = file.path(getwd(), "www/analyses.qmd")
        )
      v$local_temp <- local.temp
      file.rename(v$local_temp, file)
    }
  )

  #
}
