library(datamods)
library(toastui)
library(phosphoricons)
library(rlang)

# x <- lubridate::as_datetime(seq(1,1000000,2000), origin = "2000-12-31")
# class(x)
#
# lubridate::hms(c("01:00:20"))
#
# int_x <- classInt::classIntervals(lubridate::as_datetime(seq(1,1000000,2000), origin = "2000-12-31"), 4, style = "quantile")
# classInt::classIntervals(readr::parse_time(c("01:00:20","03:00:20","01:20:20","03:02:20")), 2, style = "quantile")
# int_x|> dput()
#
# library(hms)
#
# ?cut.POSIXt
#
# x <- readr::parse_time(c("01:00:20","03:00:20","01:20:20","03:02:20"))
# cut(x)

#' Title
#'
#' @param x an object inheriting from class "hms"
#' @param breaks Can be "hour" or "dn"
#' @param ... passed on
#'
#' @return
#' @export
#'
#' @examples
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |>
#'   cut() |>
#'   dput()
cut.hms <- function(x, breaks = "hour", ...) {
  browser()
  # For now, this function will allways try to cut to hours
  # This limits time cutting to only do hour-binning, no matter the
  if (length(breaks) != 1) {
    if ("hms" %in% class(breaks)) {

    } else {
      breaks <- "hour"
    }
  }
  if (!breaks %in% c("hour", "dn")) {
    if (is.numeric(breaks)) {
      breaks_n <- quantile(x, probs = seq(0, 1, 1 / breaks))
      ## Use lapply or similar to go through levels two at a time

    } else {
      breaks <- "hour"
    }
  }

  ch <- strsplit(as.character(x), ":") |>
    lapply(\(.x).x[[1]]) |>
    unlist()

  num <- as.numeric(ch)

  if (breaks == "hour") {
    splitter <- match(
      num,
      levels(factor(num))
    )
  } else if (breaks == "dn") {
    splitter <- num %in% 8:20 + 1
  } else {
    stop("No other methods than hour cut is implemented.")
  }

  labs <- split(x, splitter) |>
    purrr::imap(\(.x, .i){
      if (breaks == "dn" && .i == 1) {
        h <- hms::as_hms(hms::hms(hours = 24) - abs(.x - hms::hms(hours = 8)))

        paste0("[", .x[match(sort(h)[1], h)], ",", .x[match(sort(h)[length(h)], h)], "]")
      } else {
        .x <- sort(.x)
        paste0("[", .x[1], ",", .x[length(.x)], "]")
      }
    }) |>
    unlist()

  structure(match(num, l), levels = labs, class = "factor")
}

#' Title
#'
#' @param data data
#' @param class.vec vector of class names to test
#'
#' @return
#' @export
#'
#' @examples
#' vapply(REDCapCAST::redcapcast_data, \(.x){
#'   is_any_class(.x, c("hms", "Date", "POSIXct", "POSIXt"))
#' }, logical(1))
is_any_class <- function(data, class.vec) {
  any(class(data) %in% class.vec)
}

#' Title
#'
#' @param data data
#'
#' @return
#' @export
#'
#' @examples
#' vapply(REDCapCAST::redcapcast_data, is_datetime, logical(1))
is_datetime <- function(data) {
  is_any_class(data, class.vec = c("hms", "Date", "POSIXct", "POSIXt"))
}

#' @title Module to Convert Numeric to Factor
#'
#' @description
#' This module contain an interface to cut a numeric into several intervals.
#'
#'
#' @param id Module ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#' @export
#'
#' @importFrom shiny NS fluidRow column numericInput checkboxInput checkboxInput plotOutput uiOutput
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom toastui datagridOutput2
#'
#' @name cut-variable
#'
#' @example examples/cut_variable.R
cut_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        virtualSelectInput(
          inputId = ns("variable"),
          label = i18n("Variable to cut:"),
          choices = NULL,
          width = "100%"
        )
      ),
      column(
        width = 3,
        virtualSelectInput(
          inputId = ns("method"),
          label = i18n("Method:"),
          choices = c(
            "fixed",
            # "sd",
            # "equal",
            # "pretty",
            "quantile",
            # "kmeans",
            # "hclust",
            # "bclust",
            # "fisher",
            # "jenks",
            "headtails",
            # "maximum",
            # "box",
            "hour",
            "day",
            "week",
            "month",
            "quarter",
            "year"
          ),
          selected = "quantile",
          width = "100%"
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("n_breaks"),
          label = i18n("Number of breaks:"),
          value = 5,
          min = 2,
          max = 12,
          width = "100%"
        )
      ),
      column(
        width = 3,
        checkboxInput(
          inputId = ns("right"),
          label = i18n("Close intervals on the right"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("include_lowest"),
          label = i18n("Include lowest value"),
          value = TRUE
        )
      )
    ),
    conditionalPanel(
      condition = "input.method == 'fixed'",
      ns = ns,
      uiOutput(outputId = ns("slider_fixed"))
    ),
    plotOutput(outputId = ns("plot"), width = "100%", height = "270px"),
    datagridOutput2(outputId = ns("count")),
    actionButton(
      inputId = ns("create"),
      label = tagList(ph("scissors"), i18n("Create factor variable")),
      class = "btn-outline-primary float-end"
    ),
    tags$div(class = "clearfix")
  )
}

#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#'
#' @export
#'
#' @importFrom shiny moduleServer observeEvent reactive req bindEvent renderPlot
#' @importFrom shinyWidgets updateVirtualSelect noUiSliderInput
#' @importFrom toastui renderDatagrid2 datagrid grid_colorbar
#' @importFrom rlang %||% call2 set_names expr syms
#' @importFrom classInt classIntervals
#'
#' @rdname cut-variable
cut_variable_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL)

      bindEvent(observe({
        data <- data_r()
        rv$data <- data
        vars_num <- vapply(data, \(.x){
          is.numeric(.x) || is_datetime(.x)
        }, logical(1))
        vars_num <- names(vars_num)[vars_num]
        updateVirtualSelect(
          inputId = "variable",
          choices = vars_num,
          selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        )
      }), data_r(), input$hidden)

      output$slider_fixed <- renderUI({
        data <- req(data_r())
        variable <- req(input$variable)
        req(hasName(data, variable))
        noUiSliderInput(
          inputId = session$ns("fixed_brks"),
          label = i18n("Fixed breaks:"),
          min = floor(min(data[[variable]], na.rm = TRUE)),
          max = ceiling(max(data[[variable]], na.rm = TRUE)),
          value = classInt::classIntervals(
            var = as.numeric(data[[variable]]),
            n = input$n_breaks,
            style = "quantile"
          )$brks,
          color = datamods:::get_primary_color(),
          width = "100%"
        )
      })

      breaks_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        req(hasName(data, variable))
        req(input$n_breaks, input$method)
        if (input$method == "fixed") {
          req(input$fixed_brks)
          classInt::classIntervals(
            var = as.numeric(data[[variable]]),
            n = input$n_breaks,
            style = "fixed",
            fixedBreaks = input$fixed_brks
          )
        } else if (input$method %in% c(
          "day",
          "week",
          "month",
          "quarter",
          "year"
        )) {
          # To enable datetime cutting
          cut.POSIXct <- cut.POSIXt
          f <- cut(data[[variable]], breaks = input$method)
          list(var = f, brks = levels(f))
        } else if (input$method %in% c("hour")) {
          # To enable datetime cutting
          cut.POSIXct <- cut.POSIXt
          f <- cut(data[[variable]], breaks = "hour")
          list(var = f, brks = levels(f))
        } else {
          classInt::classIntervals(
            var = as.numeric(data[[variable]]),
            n = input$n_breaks,
            style = input$method
          )
        }
      })

      output$plot <- renderPlot({
        data <- req(data_r())
        variable <- req(input$variable)
        plot_histogram(data, variable, breaks = breaks_r()$brks, color = datamods:::get_primary_color())
      })


      data_cutted_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        data[[paste0(variable, "_cut")]] <- cut(
          x = data[[variable]],
          breaks = if (input$method %in% c("day","week","month","quarter","year","hour")) input$method else breaks_r()$brks,
          include.lowest = input$include_lowest,
          right = input$right
        )
        code <- call2(
          "mutate",
          !!!set_names(
            list(
              expr(cut(
                !!!syms(list(x = variable)),
                !!!list(breaks = breaks_r()$brks, include.lowest = input$include_lowest, right = input$right)
              ))
            ),
            paste0(variable, "_cut")
          )
        )
        attr(data, "code") <- Reduce(
          f = function(x, y) expr(!!x %>% !!y),
          x = c(attr(data, "code"), code)
        )
        data
      })

      output$count <- renderDatagrid2({
        data <- req(data_cutted_r())
        variable <- req(input$variable)
        count_data <- as.data.frame(
          table(
            breaks = data[[paste0(variable, "_cut")]],
            useNA = "ifany"
          ),
          responseName = "count"
        )
        gridTheme <- getOption("datagrid.theme")
        if (length(gridTheme) < 1) {
          datamods:::apply_grid_theme()
        }
        on.exit(toastui::reset_grid_theme())
        grid <- datagrid(
          data = count_data,
          colwidths = "guess",
          theme = "default",
          bodyHeight = "auto"
        )
        grid <- toastui::grid_columns(grid, className = "font-monospace")
        grid_colorbar(
          grid,
          column = "count",
          label_outside = TRUE,
          label_width = "40px",
          bar_bg = datamods:::get_primary_color(),
          from = c(0, max(count_data$count) + 1)
        )
      })

      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_cutted_r()
      })
      return(reactive(rv$data))
    }
  )
}



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname cut-variable
modal_cut_variable <- function(id,
                               title = i18n("Convert Numeric to Factor"),
                               easyClose = TRUE,
                               size = "l",
                               footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    cut_variable_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


#' @inheritParams shinyWidgets::WinBox
#' @export
#'
#' @importFrom shinyWidgets WinBox wbOptions wbControls
#' @importFrom htmltools tagList
#' @rdname cut-variable
winbox_cut_variable <- function(id,
                                title = i18n("Convert Numeric to Factor"),
                                options = shinyWidgets::wbOptions(),
                                controls = shinyWidgets::wbControls()) {
  ns <- NS(id)
  WinBox(
    title = title,
    ui = tagList(
      cut_variable_ui(id),
      tags$div(
        style = "display: none;",
        textInput(inputId = ns("hidden"), label = NULL, value = genId())
      )
    ),
    options = modifyList(
      shinyWidgets::wbOptions(height = "750px", modal = TRUE),
      options
    ),
    controls = controls,
    auto_height = FALSE
  )
}


#' @importFrom graphics abline axis hist par plot.new plot.window
plot_histogram <- function(data, column, bins = 30, breaks = NULL, color = "#112466") {
  x <- data[[column]]
  x <- as.numeric(x)
  op <- par(mar = rep(1.5, 4))
  on.exit(par(op))
  plot.new()
  plot.window(xlim = range(pretty(x)), ylim = range(pretty(hist(x, breaks = bins, plot = FALSE)$counts)))
  abline(v = pretty(x), col = "#D8D8D8")
  abline(h = pretty(hist(x, breaks = bins, plot = FALSE)$counts), col = "#D8D8D8")
  hist(x, breaks = bins, xlim = range(pretty(x)), xaxs = "i", yaxs = "i", col = color, add = TRUE)
  axis(side = 1, at = pretty(x), pos = 0)
  axis(side = 2, at = pretty(hist(x, breaks = bins, plot = FALSE)$counts), pos = min(pretty(x)))
  abline(v = breaks, col = "#FFFFFF", lty = 1, lwd = 1.5)
  abline(v = breaks, col = "#2E2E2E", lty = 2, lwd = 1.5)
}
