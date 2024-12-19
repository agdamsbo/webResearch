library(datamods)
library(toastui)
library(phosphoricons)
library(rlang)
library(shiny)


# old_deprecated_cut.hms <- function(x, breaks = "hour", ...) {
#   # For now, this function will allways try to cut to hours
#   # This limits time cutting to only do hour-binning, no matter the
#
#   breaks_o <- breaks
#
#   if (identical(breaks, "hour")) {
#     # splitter <- match(
#     #   num,
#     #   levels(factor(num))
#     # )
#     breaks <- hms::as_hms(paste0(1:23, ":00:00"))
#   }
#
#   # if (identical(breaks, "daynight")) {
#   #   # splitter <- num %in% 8:20 + 1
#   #   breaks <- hms::as_hms(c("08:00:00","20:00:00"))
#   # }
#
#   if (length(breaks) != 1) {
#     if ("hms" %in% class(breaks)) {
#       splitter <- seq_along(breaks) |>
#         purrr::map(\(.x){
#           # browser()
#           out <- x %in% x[x >= breaks[.x] & x < breaks[.x + 1]]
#           if (.x == length(breaks)) {
#             out[match(breaks[length(breaks)], x)] <- TRUE
#           }
#           ifelse(out, .x, 0)
#         }) |>
#         dplyr::bind_cols(.name_repair = "unique_quiet") |>
#         rowSums()
#       splitter[splitter == 0] <- NA
#     } else {
#       breaks <- "hour"
#     }
#   }
#
#   if (is.numeric(breaks)) {
#     breaks_n <- quantile(x, probs = seq(0, 1, 1 / breaks))
#     ## Use lapply or similar to go through levels two at a time
#     splitter <- seq(breaks) |>
#       purrr::map(\(.x){
#         # browser()
#         out <- x %in% x[x >= breaks_n[.x] & x < breaks_n[.x + 1]]
#         if (.x == breaks) {
#           out[match(breaks_n[length(breaks_n)], x)] <- TRUE
#         }
#         ifelse(out, .x, 0)
#       }) |>
#       dplyr::bind_cols(.name_repair = "unique_quiet") |>
#       rowSums()
#   }
#
#   # browser()
#
#   num <- strsplit(as.character(x), ":") |>
#     lapply(\(.x).x[[1]]) |>
#     unlist() |>
#     as.numeric()
#
#   # browser()
#   labs <- split(x, splitter) |>
#     purrr::imap(\(.x, .i){
#       # if (identical(breaks_o, "daynight") && .i == 1) {
#       #   h <- hms::as_hms(hms::hms(hours = 24) - abs(.x - hms::hms(hours = 8)))
#       #
#       #   paste0("[", .x[match(sort(h)[1], h)], ",", .x[match(sort(h)[length(h)], h)], "]")
#       # } else {
#       .x <- sort(.x)
#       paste0("[", .x[1], ",", .x[length(.x)], "]")
#       # }
#     }) |>
#     unlist()
#
#   structure(match(splitter, names(labs)), levels = labs, class = "factor")
# }

#' Title
#'
#' @param x an object inheriting from class "hms"
#' @param ... passed on
#'
#' @rdname cut
#'
#' @return factor
#' @export
#'
#' @examples
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut("min")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(breaks = "hour")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(breaks = hms::as_hms(c("01:00:00", "03:01:20", "9:20:20")))
#' d_t <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA))
#' f <- d_t |> cut(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |> cut(breaks = lubridate::as_datetime(c(hms::as_hms(levels(f)), hms::as_hms(max(d_t, na.rm = TRUE) + 1))), right = FALSE)
cut.hms <- function(x, breaks, ...) {
  if (hms::is_hms(breaks)) {
    breaks <- lubridate::as_datetime(breaks, tz = "UTC")
  }
  x <- lubridate::as_datetime(x, tz = "UTC")
  out <- cut.POSIXt(x, breaks = breaks, ...)
  attr(out, which = "brks") <- hms::as_hms(lubridate::as_datetime(attr(out, which = "brks")))
  attr(out, which = "levels") <- as.character(hms::as_hms(lubridate::as_datetime(attr(out, which = "levels"))))
  out
}

#' @rdname cut
#' @param x an object inheriting from class "POSIXt" or "Date"
#'
#' @examples
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(2)
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(breaks="weekday")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(breaks="month_only")
cut.POSIXt <- function(x, breaks, right = FALSE, include.lowest = TRUE, start.on.monday=TRUE, ...) {
  breaks_o <- breaks
  # browser()
  if (is.numeric(breaks)) {
    breaks <- quantile(
      x,
      probs = seq(0, 1, 1 / breaks),
      right = right,
      include.lowest = include.lowest,
      na.rm=TRUE
    )
  }

  if(identical(breaks,"weekday")){
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
              "Sunday")
    if (!start.on.monday){
      days <- days[c(7,1:6)]
    }
    out <- factor(weekdays(x),levels=days) |> forcats::fct_drop()
  } else if (identical(breaks,"month_only")){
    ms <- paste0("1970-",1:12,"-01") |> as.Date() |> months()

    out <- factor(months(x),levels=ms) |> forcats::fct_drop()
  } else {
  ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
  out <- base::cut.POSIXt(x, breaks=breaks,right=right,...) |> forcats::fct_drop()
  # browser()
}
  l <- levels(out)
  if (is.numeric(breaks_o)) {
    l <- breaks
  } else if (is.character(breaks) && length(breaks) == 1 && !(identical(breaks,"weekday") | identical(breaks,"month_only"))) {
    if (include.lowest) {
      if (right) {
        l <- c(l, min(as.character(x)))
      } else {
        l <- c(l, max(as.character(x)))
      }
    }
  } else if (length(l) < length(breaks_o)) {
    l <- breaks_o
  }

  attr(out, which = "brks") <- l
  out
}

#' @rdname cut
#' @param x an object inheriting from class "POSIXct"
cut.POSIXct <- cut.POSIXt

#' @rdname cut
#' @param x an object inheriting from class "POSIXct"
#'
#' @examples
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(2)
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(breaks="weekday")
cut.Date <- function(x,breaks,start.on.monday=TRUE,...){
  if(identical(breaks,"weekday")){
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
              "Sunday")
    if (!start.on.monday){
      days <- days[c(7,1:6)]
    }
    out <- factor(weekdays(x),levels=days) |> forcats::fct_drop()
  } else if (identical(breaks,"month_only")){
    ms <- paste0("1970-",1:12,"-01") |> as.Date() |> months()

    out <- factor(months(x),levels=ms) |> forcats::fct_drop()
  } else {
    ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
    out <- base::cut.Date(x, breaks=breaks,...) |> forcats::fct_drop()
    # browser()
  }
  out
}

#' Test class
#'
#' @param data data
#' @param class.vec vector of class names to test
#'
#' @return factor
#' @export
#'
#' @examples
#' vapply(REDCapCAST::redcapcast_data, \(.x){
#'   is_any_class(.x, c("hms", "Date", "POSIXct", "POSIXt"))
#' }, logical(1))
is_any_class <- function(data, class.vec) {
  any(class(data) %in% class.vec)
}

#' Test is date/datetime/time
#'
#' @param data data
#'
#' @return factor
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
cut_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
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
        shiny::uiOutput(ns("cut_method"))
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("n_breaks"),
          label = i18n("Number of breaks:"),
          value = 3,
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

        if (is_datetime(data[[variable]])) {
          brks <- cut(data[[variable]],
            breaks = input$n_breaks
          )$brks
        } else {
          brks <- classInt::classIntervals(
            var = data[[variable]],
            n = input$n_breaks,
            style = "quantile"
          )$brks
        }

        if (is_datetime(data[[variable]])) {
          lower <- min(data[[variable]], na.rm = TRUE)
        } else {
          lower <- floor(min(data[[variable]], na.rm = TRUE))
        }

        if (is_datetime(data[[variable]])) {
          upper <- max(data[[variable]], na.rm = TRUE)
        } else {
          upper <- ceiling(max(data[[variable]], na.rm = TRUE))
        }


        noUiSliderInput(
          inputId = session$ns("fixed_brks"),
          label = i18n("Fixed breaks:"),
          min = lower,
          max = upper,
          value = brks,
          color = datamods:::get_primary_color(),
          width = "100%"
        )
      })

      output$cut_method <- renderUI({
        data <- req(data_r())
        variable <- req(input$variable)

        choices <- c(
          # "quantile"
          )

        if ("hms" %in% class(data[[variable]])) {
          choices <- c(choices, "hour")
        } else if (any(c("POSIXt","Date") %in% class(data[[variable]]))) {
          choices <- c(
            choices,
            "day",
            "weekday",
            "week",
            "month",
            "month_only",
            "quarter",
            "year"
          )
        } else {
          choices <- c(
            choices,
            "fixed",
            "quantile",
            # "sd",
            # "equal",
            # "pretty",
            # "kmeans",
            # "hclust",
            # "bclust",
            # "fisher",
            # "jenks",
            "headtails" # ,
            # "maximum",
            # "box"
          )
        }

        shinyWidgets::virtualSelectInput(
          inputId = session$ns("method"),
          label = i18n("Method:"),
          choices = choices,
          selected = NULL,
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
          if (any(c("hms", "POSIXt") %in% class(data[[variable]]))) {
            cut.POSIXct <- cut.POSIXt
            f <- cut(data[[variable]], breaks = input$fixed_brks)
            list(var = f, brks = levels(f))
          } else {
            classInt::classIntervals(
              var = as.numeric(data[[variable]]),
              n = input$n_breaks,
              style = "fixed",
              fixedBreaks = input$fixed_brks
            )
          }
        } else if (input$method == "quantile") {
          req(input$fixed_brks)
          if (any(c("hms", "POSIXt") %in% class(data[[variable]]))) {
            cut.POSIXct <- cut.POSIXt
            f <- cut(data[[variable]], breaks = input$n_breaks)
            list(var = f, brks = levels(f))
          } else {
            classInt::classIntervals(
              var = as.numeric(data[[variable]]),
              n = input$n_breaks,
              style = "quantile"
            )
          }
        } else if (input$method %in% c(
          "day",
          "weekday",
          "week",
          "month",
          "month_only",
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
          breaks = if (input$method %in% c("day", "weekday", "week", "month", "month_only", "quarter", "year", "hour")) input$method else breaks_r()$brks,
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
