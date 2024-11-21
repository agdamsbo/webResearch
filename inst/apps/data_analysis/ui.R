library(shiny)
library(bslib)
requireNamespace("gt")
# require(ggplot2)
# source("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/R/index_from_raw.R")
# source("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/R/plot_index.R")
# source(here::here("R/index_from_raw.R"))
# source(here::here("R/plot_index.R"))

# ui <- fluidPage(

cards <- list(
  bslib::card(
    max_height = "200px",
    full_screen = TRUE,
    bslib::card_header("Data overview"),
    shiny::uiOutput("data.input")
  ),
  bslib::card(
    # max_height = "200px",
    full_screen = TRUE,
    bslib::card_header("Baseline characteristics"),
    gt::gt_output(outputId = "table1")
  ),
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Multivariable regression table"),
    gt::gt_output(outputId = "table2")
  )
)

panels <- list(
  bslib::nav_panel(
    title = "Data overview",
    shiny::uiOutput("data.input")
  ),
  bslib::nav_panel(
    title = "Baseline characteristics",
    gt::gt_output(outputId = "table1")
  ),
  bslib::nav_panel(
    title = "Multivariable regression table",
    gt::gt_output(outputId = "table2")
  )
)


ui <- bslib::page(
  theme = bslib::bs_theme(bootswatch = "minty"),
  title = "webResearcher for easy data analysis",
  bslib::page_navbar(
    title = "webResearcher",
    header = h6("Welcome to the webResearcher tool. This is an early alpha version to act as a proof-of-concept and in no way intended for wider public use."),

    # sidebarPanel(
    sidebar = bslib::sidebar(
      open = "open",
      shiny::h4("Upload your dataset"),
      shiny::conditionalPanel(
        condition = "output.has_input=='yes'",
        # Input: Select a file ----
        shiny::helpText("Analyses are performed on provided data")
      ),
      shiny::conditionalPanel(
        condition = "output.has_input=='no'",
        # Input: Select a file ----
        shiny::fileInput(
          inputId = "file",
          label = "Choose data file",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsx",
            ".xls",
            ".dta",
            ".ods",
            ".rds"
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output.uploaded=='yes'",
        shiny::h4("Parameter specifications"),
        shiny::radioButtons(
          inputId = "regression_auto",
          label = "Automatically choose function",
          inline = TRUE,
          choiceNames = c(
            "Yes",
            "No"
          ),
          choiceValues = c(1, 2)
        ),
        shiny::conditionalPanel(
          condition = "input.regression_auto==2",
          shiny::textInput(
            inputId = "regression_formula",
            label = "Formula string to render with 'glue::glue'",
            value = NULL
          ),
          shiny::textInput(
            inputId = "regression_fun",
            label = "Function to use for analysis (needs pasckage and name)",
            value = "stats::lm"
          ),
          shiny::textInput(
            inputId = "regression_args",
            label = "Arguments to pass to the function (provided as a string)",
            value = ""
          )
        ),
        shiny::helpText(em("Please specify relevant columns from your data, and press 'Load data'")),
        shiny::uiOutput("outcome_var"),
        shiny::radioButtons(
          inputId = "all",
          label = "Specify covariables",
          inline = TRUE, selected = 2,
          choiceNames = c(
            "Yes",
            "No"
          ),
          choiceValues = c(1, 2)
        ),
        shiny::conditionalPanel(
          condition = "input.all==1",
          shiny::uiOutput("include_vars")
        ),
        shiny::actionButton("load", "Analyse", class = "btn-primary"),
        #
        # # Horizontal line ----
        tags$hr(),
        h4("Download results"),

        shiny::helpText("The download currently works, but the output is not correctly formatted. Work in progress!"),

        # Button
        downloadButton(
          outputId = "report",
          label = "Download",
          icon = shiny::icon("download")
        )

      )
    ),
    bslib::nav_spacer(),
    panels[[1]],
    panels[[2]],
    panels[[3]]

    # layout_columns(
    #   cards[[1]]
    # ),
    # layout_columns(
    #   cards[[2]], cards[[3]]
    # )
  )
)
