#' Create a regression model programatically
#'
#' @param data data set
#' @param fun Name of function as character vector or function to use for model creation.
#' @param vars character vector of variables to include
#' @param outcome.str Name of outcome variable. Character vector.
#' @param auto.mode Make assumptions on function dependent on outcome data format. Overwrites other arguments.
#' @param formula.str Formula as string. Passed through 'glue::glue'. If given, 'outcome.str' and 'vars' are ignored. Optional.
#' @param args.list List of arguments passed to 'fun' with 'do.call'.
#' @param ... ignored for now
#'
#' @importFrom stats as.formula
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
#' gtsummary::trial |>
#'   regression_model(outcome.str = "age")
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     auto.mode = FALSE,
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   )
#' gtsummary::trial |> regression_model(
#'   outcome.str = "trt",
#'   auto.mode = FALSE,
#'   fun = "stats::glm",
#'   args.list = list(family = binomial(link = "logit"))
#' )
regression_model <- function(data,
                             outcome.str,
                             auto.mode = TRUE,
                             formula.str = NULL,
                             args.list = NULL,
                             fun = NULL,
                             vars = NULL,
                             ...) {
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
  data <- data |>
    purrr::map(\(.x){
      if (is.character(.x)) {
        suppressWarnings(REDCapCAST::as_factor(.x))
      } else {
        .x
      }
    }) |>
    dplyr::bind_cols()

  if (is.null(fun)) auto.mode <- TRUE

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

  class(out) <- c("webresearch_model", class(out))
  return(out)
}

#' Create a regression model programatically
#'
#' @param data data set
#' @param fun Name of function as character vector or function to use for model creation.
#' @param vars character vector of variables to include
#' @param outcome.str Name of outcome variable. Character vector.
#' @param args.list List of arguments passed to 'fun' with 'do.call'.
#' @param ... ignored for now
#'
#' @importFrom stats as.formula
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model_uv(outcome.str = "age")
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     args.list = NULL
#'   )
#' gtsummary::trial |> regression_model_uv(
#'   outcome.str = "trt",
#'   fun = "stats::glm",
#'   args.list = list(family = stats::binomial(link = "logit"))
#' )
#' }
regression_model_uv <- function(data,
                                outcome.str,
                                args.list = NULL,
                                fun = NULL,
                                vars = NULL,
                                ...) {
  if (!is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(
        unique(c(outcome.str, vars))
      ))
  }

  if (is.null(args.list)) {
    args.list <- list()
  }

  if (is.null(fun)) {
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

  out <- names(data)[!names(data) %in% outcome.str] |>
    purrr::map(\(.var){
      do.call(
        regression_model,
        c(
          list(data = data[match(c(outcome.str,.var),names(data))]),
          list(outcome.str=outcome.str),
          list(args.list=args.list)
        )
      )
    })

  return(out)
}
