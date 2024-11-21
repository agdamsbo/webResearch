

########
#### Current file: /Users/au301842/webResearch/R//app.R 
########














shiny_webResearch <-  function(data=NULL,...){
  appDir <- system.file("apps", "data_analysis", package = "webResearch")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `webResearch`.", call. = FALSE)
  }

  G <- .GlobalEnv
  assign("webResearch_data", data, envir=G)
  a=shiny::runApp(appDir = appDir, ...)
  return(invisible(a))
}









page_panels <- function(data){
bslib::navset_card_underline(
  title="Data and results",
  data[[1]],
  data[[2]],
  data[[3]]
)
}


########
#### Current file: /Users/au301842/webResearch/R//baseline_table.R 
########














baseline_table <- function(data, fun.args = NULL, fun = gtsummary::tbl_summary, vars = NULL) {
  if (!is.null(vars)) {
    data <- data |> dplyr::select(dplyr::all_of(vars))
  }

  out <- do.call(fun, c(list(data = data), fun.args))
  return(out)
}



########
#### Current file: /Users/au301842/webResearch/R//helpers.R 
########












getfun <- function(x) {
  if("character" %in% class(x)){
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      requireNamespace(parts[1])
      getExportedValue(parts[1], parts[2])
    }
  }else {
    x
  }
}












write_quarto <- function(data,fileformat,qmd.file=here::here("analyses.qmd"),file=NULL,...){
  if (is.null(file)){
    file <- paste0("analyses.",fileformat)
  }
  temp <- tempfile(fileext = ".Rds")
  # write_rds(mtcars, temp)
  # read_rds(temp)
  web_data <- data
  saveRDS(web_data,file=temp)

  quarto::quarto_render(qmd.file,
                        output_file = file,
                        execute_params = list(data.file=temp)
  )
}











read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- tools::file_ext(file)

  if (ext == "csv") {
    df <- readr::read_csv(file = file, na = consider.na)
  } else if (ext %in% c("xls", "xlsx")) {
    df <- openxlsx2::read_xlsx(file = file, na.strings = consider.na)
  } else if (ext == "dta") {
    df <- haven::read_dta(file = file)
  } else if (ext == "ods") {
    df <- readODS::read_ods(path = file)
  } else {
    stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta' or '.ods'")
  }

  df
}











argsstring2list <- function(string){
  eval(parse(text = paste0("list(", string, ")")))
}


########
#### Current file: /Users/au301842/webResearch/R//regression_model.R 
########

































regression_model <- function(data,
                             outcome.str,
                             auto.mode = TRUE,
                             formula.str = NULL,
                             args.list = NULL,
                             fun = NULL,
                             vars = NULL) {
  if (!is.null(formula.str)) {
    if (formula.str == "") {
      formula.str <- NULL
    }
  }

  if (!is.null(formula.str)) {
    formula.str <- glue::glue(formula.str)
  } else {
    assertthat::assert_that(outcome.str %in% names(data),
      msg = "Outcome variable is not present in the provided dataset"
    )
    formula.str <- glue::glue("{outcome.str}~.")

    if (!is.null(vars)) {
      if (outcome.str %in% vars) {
        vars <- vars[vars %in% outcome.str]
      }
      data <- data |> dplyr::select(dplyr::all_of(c(vars, outcome.str)))
    }
  }

  # Formatting character variables as factor
  # Improvement should add a missing vector to format as NA
  data <- data |> dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  # browser()
  if (auto.mode) {
    if (is.numeric(data[[outcome.str]])) {
      fun <- "stats::lm"
    } else if (is.factor(data[[outcome.str]])) {
      if (length(levels(data[[outcome.str]])) == 2) {
        fun <- "stats::glm"
        args.list <- list(family = stats::binomial(link = "logit"))
      } else if (length(levels(data[[outcome.str]])) > 2) {
        fun <- "MASS::polr"
        args.list <- list(
          Hess = TRUE,
          method = "logistic"
        )
      } else {
        stop("The provided output variable only has one level")
      }
    } else {
      stop("Output variable should be either numeric or factor for auto.mode")
    }
  }

  assertthat::assert_that("character" %in% class(fun),
    msg = "Please provide the function as a character vector."
  )

  out <- do.call(
    getfun(fun),
    c(
      list(data = data),
      list(formula = as.formula(formula.str)),
      args.list
    )
  )

  # Recreating the call
  # out$call <-  match.call(definition=eval(parse(text=fun)), call(fun, data = 'data',formula = as.formula(formula.str),args.list))

  return(out)
}


########
#### Current file: /Users/au301842/webResearch/R//regression_table.R 
########
































regression_table <- function(data, args.list = NULL, fun = "gtsummary::tbl_regression") {

  if (any(c(length(class(data))!=1, class(data)!="lm"))){
    if (!"exponentiate" %in% names(args.list)){
      args.list <- c(args.list,list(exponentiate=TRUE))
    }
  }

  out <- do.call(getfun(fun), c(list(x = data), args.list))
  return(out)
}
