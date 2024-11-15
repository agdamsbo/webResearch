require(shiny)
require(bslib)
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
  bslib::nav_panel(title="Data overview",
                   shiny::uiOutput("data.input")),
  bslib::nav_panel(title="Baseline characteristics",
                   gt::gt_output(outputId = "table1")),
  bslib::nav_panel(title="Multivariable regression table",
                   gt::gt_output(outputId = "table2"))
)


ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(bootswatch = "minty"),
  title = "webResearcher for easy data analysis",
  window_title = "webResearcher",
  header = h6("Welcome to the webResearcher tool. This is an early alpha version to act as a proof-of-concept and in no way intended for wider public use."),

  # sidebarPanel(
  sidebar = bslib::sidebar(
    open = "open",
    h4("Upload your dataset"),

    # Input: Select a file ----
    fileInput(
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
        ".ods"
      )
    ),
    conditionalPanel(
      condition = "output.uploaded=='yes'",
      h4("Parameter specifications"),
      radioButtons(
        inputId = "regression_auto",
        label = "Automatically choose function",
        inline = TRUE,
        choiceNames = c(
          "Yes",
          "No"
        ),
        choiceValues = c(1, 2)
      ),
      conditionalPanel(
        condition = "input.regression_auto==2",
        textInput(
          inputId = "regression_formula",
          label = "Formula string to render with 'glue::glue'",
          value = NULL
        ),
        textInput(
          inputId = "regression_fun",
          label = "Function to use for analysis (needs pasckage and name)",
          value = "stats::lm"
        ),
        textInput(
          inputId = "regression_args",
          label = "Arguments to pass to the function (provided as a string)",
          value = ""
        )
      ),
      helpText(em("Please specify relevant columns from your data, and press 'Load data'")),
      uiOutput("outcome_var"),
      radioButtons(
        inputId = "all",
        label = "Specify covariables",
        inline = TRUE, selected = 2,
        choiceNames = c(
          "Yes",
          "No"
        ),
        choiceValues = c(1, 2)
      ),
      conditionalPanel(
        condition = "input.all==1",
        uiOutput("include_vars")
      ),
      actionButton("load", "Analyse", class = "btn-primary")
    )
     # ,
     #
     # # Horizontal line ----
     # tags$hr(),
     # h4("Download results"),
     #
     # # Button
     # downloadButton(outputId="report",
     #                label= "Download",
     #                icon = shiny::icon("download"))
  ),
  bslib::navset_card_underline(
    title="Data and results",
    panels[[1]],
    panels[[2]],
    panels[[3]]
  )

  # layout_columns(
  #   cards[[1]]
  # ),
  # layout_columns(
  #   cards[[2]], cards[[3]]
  # )
)
