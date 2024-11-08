# project.aid::merge_scripts(list.files("R/",full.names = TRUE),dest = here::here("app/functions.R"))
# source(here::here("functions.R"))

source("https://raw.githubusercontent.com/agdamsbo/webResearch/refs/heads/main/app/functions.R")

library(readr)
library(MASS)
library(stats)
library(gt)
library(gtsummary)
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


server <- function(input, output, session) {
  v <- shiny::reactiveValues(
    list = NULL,
    ds = NULL
  )

  ds <- shiny::reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    shiny::req(input$file)

    v$ds <- "present"
    return(read_input(input$file$datapath))
  })

  output$include_vars <- shiny::renderUI({
    selectizeInput(
      inputId = "include_vars",
      selected = NULL,
      label = "Covariables to include",
      choices = colnames(ds()),
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
    shiny::req(input$file)
    ds()
  })

  shiny::observeEvent(
    {
      input$load
    },
    {
      shiny::req(input$outcome_var)

      v$list <- ds() |>
        (\(data){
          # browser()
          list(
            data = data,
            table1 = data |> baseline_table(),
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
        })()

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

  output$report <- downloadHandler(
    filename = "analyses.html",
    content = function(file) {
      v$list |>
        write_quarto(file = file,
                     qmd.file = "www/analyses.qmd")
    }
  )

  #
}
