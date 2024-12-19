
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
# library(REDCapCAST)
library(easystats)
library(patchwork)
library(DHARMa)
# if (!requireNamespace("webResearch")) {
#   devtools::install_github("agdamsbo/webResearch", quiet = TRUE, upgrade = "never")
# }
# library(webResearch)

if (file.exists(here::here("functions.R"))) {
  source(here::here("functions.R"))
}

#' freesearcheR server
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @returns server
#' @export
#' @importFrom REDCapCAST numchar2fct
#'
#' @examples
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
    } else if (v$test == "test") {
      out <- gtsummary::trial
    } else {
      shiny::req(input$file)
      out <- read_input(input$file$datapath)
    }

    v$ds <- "present"
    if (input$factorize == "yes") {
      out <- out |>
        (\(.x){
          suppressWarnings(
            numchar2fct(.x)
          )
        })()
    }
    return(out)
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

  output$strat_var <- shiny::renderUI({
    selectInput(
      inputId = "strat_var",
      selected = "none",
      label = "Select variable to stratify baseline",
      choices = c("none", colnames(ds()[base_vars()])),
      multiple = FALSE
    )
  })

  output$factor_vars <- shiny::renderUI({
    selectizeInput(
      inputId = "factor_vars",
      selected = colnames(ds())[sapply(ds(), is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(ds()),
      multiple = TRUE
    )
  })

  base_vars <- shiny::reactive({
    if (is.null(input$include_vars)) {
      out <- colnames(ds())
    } else {
      out <- unique(c(input$include_vars, input$outcome_var))
    }
    return(out)
  })

  output$data.input <-
    DT::renderDT({
      shiny::req(input$file)
      ds()[base_vars()]
    })

  output$data.classes <- gt::render_gt({
    shiny::req(input$file)
    data.frame(matrix(sapply(ds(), \(.x){
      class(.x)[1]
    }), nrow = 1)) |>
      stats::setNames(names(ds())) |>
      gt::gt()
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

      # if (is.factor(data[[input$strat_var]])) {
      #   by.var <- input$strat_var
      # } else {
      #   by.var <- NULL
      # }

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

      # browser()
      # check <- performance::check_model(purrr::pluck(models,"Multivariable") |>
      #                                     (\(x){
      #                                       class(x) <- class(x)[class(x) != "webresearch_model"]
      #                                       return(x)
      #                                     })())

      check <- purrr::pluck(models, "Multivariable") |>
        performance::check_model()


      v$list <- list(
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
        v$list$table1 |>
          gtsummary::as_gt()
      )

      output$table2 <- gt::render_gt(
        v$list$table2 |>
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
      ## Notification is not progressing
      ## Presumably due to missing
      shiny::withProgress(message = "Generating report. Hold on for a moment..", {
        v$list |>
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
