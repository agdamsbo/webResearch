library(shiny)
library(bslib)
library(datamods)
library(shinyWidgets)
library(DT)
requireNamespace("gt")

# ns <- NS(id)

ui_elements <- list(
  # bslib::nav_panel(
  #   title = "Data overview",
  #   # shiny::uiOutput("data.classes"),
  #   # shiny::uiOutput("data.input"),
  #   # shiny::p("Classes of uploaded data"),
  #   # gt::gt_output("data.classes"),
  #   shiny::p("Subset data"),
  #   DT::DTOutput(outputId = "data.input")
  #     ),
  # bslib::nav_panel(
  #   title = "Baseline characteristics",
  #   gt::gt_output(outputId = "table1")
  # ),
  # bslib::nav_panel(
  #   title = "Regression table",
  #   gt::gt_output(outputId = "table2")
  # ),
  # bslib::nav_panel(
  #   title = "Regression checks",
  #   shiny::plotOutput(outputId = "check")
  # ),
  ##############################################################################
  #########
  #########  Import panel
  #########
  ##############################################################################
  "import" = bslib::nav_panel(
    title = "Data import",
    shiny::h4("Upload your dataset"),
    shiny::conditionalPanel(
      condition = "output.has_input=='yes'",
      # Input: Select a file ----
      shiny::helpText("Analyses are performed on provided data")
    ),
    shiny::conditionalPanel(
      condition = "output.has_input=='no'",
      # Input: Select a file ----
      shiny::radioButtons(
        inputId = "source",
        label = "Upload file or export from REDCap?",
        selected = "file",
        inline = TRUE,
        choices = list(
          "File" = "file",
          "REDCap" = "redcap"
        )
      ),
      shiny::conditionalPanel(
        condition = "input.source=='file'",
        datamods::import_file_ui("file_import",
                                 title = "Choose a datafile to upload",
          file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav", ".ods", ".dta")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        m_redcap_readUI("redcap_import"),
        DT::DTOutput(outputId = "redcap_prev")
      )
    ),
    shiny::br(),
    shiny::actionButton(inputId = "act_start",label = "Start")
  ),
  ##############################################################################
  #########
  #########  Data analyses panel
  #########
  ##############################################################################
  "analyze" = bslib::nav_panel(
    title = "Data analysis",
    bslib::page_navbar(
      title = "",
      # bslib::layout_sidebar(
      #   fillable = TRUE,
        sidebar = bslib::sidebar(
          shiny::helpText(em("Please specify relevant settings for your data, and press 'Analyse'")),
          shiny::uiOutput("outcome_var"),
          shiny::uiOutput("strat_var"),
          shiny::conditionalPanel(
            condition = "input.strat_var!='none'",
            shiny::radioButtons(
              inputId = "add_p",
              label = "Compare strata?",
              selected = "no",
              inline = TRUE,
              choices = list(
                "No" = "no",
                "Yes" = "yes"
              )
            ),
            shiny::helpText("Option to perform statistical comparisons between strata in baseline table.")
          ),
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
          shiny::radioButtons(
            inputId = "specify_factors",
            label = "Specify categorical variables?",
            selected = "no",
            inline = TRUE,
            choices = list(
              "Yes" = "yes",
              "No" = "no"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.specify_factors=='yes'",
            shiny::uiOutput("factor_vars")
          ),
          bslib::input_task_button(
            id = "load",
            label = "Analyse",
            icon = shiny::icon("pencil", lib = "glyphicon"),
            label_busy = "Working...",
            icon_busy = fontawesome::fa_i("arrows-rotate",
              class = "fa-spin",
              "aria-hidden" = "true"
            ),
            type = "primary",
            auto_reset = TRUE
          ),
          shiny::helpText("If you change the parameters, press 'Analyse' again to update the tables")
        # )
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "Data overview",
        DT::DTOutput(outputId = "data_table")
      ),
      bslib::nav_panel(
        title = "Baseline characteristics",
        gt::gt_output(outputId = "table1")
      ),
      bslib::nav_panel(
        title = "Regression table",
        gt::gt_output(outputId = "table2")
      ),
      bslib::nav_panel(
        title = "Regression checks",
        shiny::plotOutput(outputId = "check")
      )
    )
  ),
  ##############################################################################
  #########
  #########  Documentation panel
  #########
  ##############################################################################
  "docs" = bslib::nav_panel(
    title = "Intro",
    shiny::markdown(readLines("www/intro.md")),
    shiny::br()
  )
)

# cards <- list(
  # "overview"=bslib::card(
  #   title = "Data overview",
  #   # shiny::uiOutput("data.classes"),
  #   # shiny::uiOutput("data.input"),
  #   # shiny::p("Classes of uploaded data"),
  #   # gt::gt_output("data.classes"),
  #   shiny::p("Subset data"),
  #   DT::DTOutput(outputId = "data_table")
  # ),
#   "baseline"=bslib::card(
#     title = "Baseline characteristics",
#     gt::gt_output(outputId = "table1")
#   ),
#   "regression"= bslib::card(
#     title = "Regression table",
#     gt::gt_output(outputId = "table2")
#   ),
#   "checks" =bslib::card(
#     title = "Regression checks",
#     shiny::plotOutput(outputId = "check")
#   )
# )

ui <- bslib::page(
  title = "freesearcheR",
  theme = bslib::bs_theme(
    primary = "#1E4A8F",
    secondary = "#FF6F61",
    bootswatch = "minty",
    base_font = bslib::font_google("Montserrat"),
    code_font = bslib::font_google("Open Sans")
  ),
  bslib::page_navbar(
    id = "main_panel",
    ui_elements$import,
    ui_elements$analyze,
    ui_elements$docs
  )
)







# ui <- bslib::page(
#   theme = bslib::bs_theme(
#     bootswatch = "minty",
#     base_font = font_google("Inter"),
#     code_font = font_google("JetBrains Mono")
#   ),
#   title = "fresearcheR - free, web-based research analyses",
#   bslib::page_navbar(
#     title = "fresearcheR - free, web-based research analyses",
#     header = h6("Welcome to the fresearcheR tool. This is an early alpha version to act as a proof-of-concept and in no way intended for wider public use."),
#     sidebar = bslib::sidebar(
#       width = 300,
#       open = "open",
#       shiny::h4("Upload your dataset"),
#       shiny::conditionalPanel(
#         condition = "output.has_input=='yes'",
#         # Input: Select a file ----
#         shiny::helpText("Analyses are performed on provided data")
#       ),
#       shiny::conditionalPanel(
#         condition = "output.has_input=='no'",
#         # Input: Select a file ----
#         shiny::radioButtons(
#           inputId = "source",
#           label = "Upload file or export from REDCap?",
#           selected = "file",
#           inline = TRUE,
#           choices = list(
#             "File" = "file",
#             "REDCap" = "redcap"
#           )
#         ),
#         shiny::conditionalPanel(
#           condition = "input.source=='file'",
#         datamods::import_file_ui("file_import",
#                                  file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav",".ods",".dta"))
#         )
#       ,
#         shiny::conditionalPanel(
#           condition = "input.source=='redcap'",
#           m_redcap_readUI("redcap_import")
#         ),
#         # Does not work??
#         #   shiny::actionButton(inputId = "test_data",
#         #                       label = "Load test data", class = "btn-primary")
#       ),
#       shiny::conditionalPanel(
#         condition = "output.uploaded=='yes'",
#         shiny::h4("Parameter specifications"),
#         shiny::radioButtons(
#           inputId = "factorize",
#           label = "Factorize variables with few levels?",
#           selected = "yes",
#           inline = TRUE,
#           choices = list(
#             "Yes" = "yes",
#             "No" = "no"
#           )
#         ),
#         shiny::radioButtons(
#           inputId = "regression_auto",
#           label = "Automatically choose function",
#           inline = TRUE,
#           choiceNames = c(
#             "Yes",
#             "No"
#           ),
#           choiceValues = c(1, 2)
#         ),
#         shiny::conditionalPanel(
#           condition = "input.regression_auto==2",
#           shiny::textInput(
#             inputId = "regression_formula",
#             label = "Formula string to render with 'glue::glue'",
#             value = NULL
#           ),
#           shiny::textInput(
#             inputId = "regression_fun",
#             label = "Function to use for analysis (needs pasckage and name)",
#             value = "stats::lm"
#           ),
#           shiny::textInput(
#             inputId = "regression_args",
#             label = "Arguments to pass to the function (provided as a string)",
#             value = ""
#           )
#         ),
#         shiny::helpText(em("Please specify relevant settings for your data, and press 'Analyse'")),
#         shiny::uiOutput("outcome_var"),
#         shiny::uiOutput("strat_var"),
#         shiny::conditionalPanel(
#           condition = "input.strat_var!='none'",
#           shiny::radioButtons(
#             inputId = "add_p",
#             label = "Compare strata?",
#             selected = "no",
#             inline = TRUE,
#             choices = list(
#               "No" = "no",
#               "Yes" = "yes"
#             )
#           ),
#           shiny::helpText("Option to perform statistical comparisons between strata in baseline table.")
#         ),
#         shiny::radioButtons(
#           inputId = "all",
#           label = "Specify covariables",
#           inline = TRUE, selected = 2,
#           choiceNames = c(
#             "Yes",
#             "No"
#           ),
#           choiceValues = c(1, 2)
#         ),
#         shiny::conditionalPanel(
#           condition = "input.all==1",
#           shiny::uiOutput("include_vars")
#         ),
#         shiny::radioButtons(
#           inputId = "specify_factors",
#           label = "Specify categorical variables?",
#           selected = "no",
#           inline = TRUE,
#           choices = list(
#             "Yes" = "yes",
#             "No" = "no"
#           )
#         ),
#         shiny::conditionalPanel(
#           condition = "input.specify_factors=='yes'",
#           shiny::uiOutput("factor_vars")
#         ),
#         bslib::input_task_button(
#           id = "load",
#           label = "Analyse",
#           icon = shiny::icon("pencil", lib = "glyphicon"),
#           label_busy = "Working...",
#           icon_busy = fontawesome::fa_i("arrows-rotate",
#             class = "fa-spin",
#             "aria-hidden" = "true"
#           ),
#           type = "primary",
#           auto_reset = TRUE
#         ),
#         shiny::helpText("If you change the parameters, press 'Analyse' again to update the tables"),
#         # shiny::actionButton("load", "Analyse", class = "btn-primary"),
#         #
#         # # Horizontal line ----
#         tags$hr(),
#         shiny::conditionalPanel(
#           condition = "input.load",
#           h4("Download results"),
#           shiny::helpText("Choose your favourite output file format for further work."),
#           shiny::selectInput(
#             inputId = "output_type",
#             label = "Choose your desired output format",
#             selected = NULL,
#             choices = list(
#               "Word" = "docx",
#               "LibreOffice" = "odt"
#               # ,
#               # "PDF" = "pdf",
#               # "All the above" = "all"
#             )
#           ),
#
#           # Button
#           downloadButton(
#             outputId = "report",
#             label = "Download",
#             icon = shiny::icon("download")
#           )
#         )
#       )
#     ),
#     bslib::nav_spacer(),
#     panels[[1]],
#     panels[[2]],
#     panels[[3]],
#     panels[[4]]
#
#     # layout_columns(
#     #   cards[[1]]
#     # ),
#     # layout_columns(
#     #   cards[[2]], cards[[3]]
#     # )
#   )
# )
