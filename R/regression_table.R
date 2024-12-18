#' Create table of regression model
#'
#' @param x regression model
#' @param args.list list of arguments passed to 'fun'.
#' @param fun function to use for table creation. Default is "gtsummary::tbl_regression".
#' @param ... passed to methods
#'
#' @return object of standard class for fun
#' @export
#' @name regression_table
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "stage",
#'     fun = "MASS::polr"
#'   ) |>
#'   regression_table(args.list = list("exponentiate" = TRUE))
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   ) |>
#'   regression_table()
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     args.list = list(family = binomial(link = "logit"))
#'   ) |>
#'   regression_table()
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     args.list = list(family = stats::binomial(link = "logit"))
#'   ) |>
#'   regression_table()
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "stage",
#'     args.list = list(family = stats::binomial(link = "logit"))
#'   ) |>
#'   regression_table()
#'
#' list(
#'   "Univariable" = regression_model_uv,
#'   "Multivariable" = regression_model
#' ) |>
#'   lapply(\(.fun){
#'     do.call(
#'       .fun,
#'       c(
#'         list(data = gtsummary::trial),
#'         list(outcome.str = "stage")
#'       )
#'     )
#'   }) |>
#'   purrr::map(regression_table) |>
#'   tbl_merge()
#'   }
#' regression_table <- function(x, ...) {
#'   UseMethod("regression_table")
#' }
#'
#' #' @rdname regression_table
#' #' @export
#' regression_table.list <- function(x, ...) {
#'   x |>
#'     purrr::map(\(.m){
#'       regression_table(x = .m, ...) |>
#'         gtsummary::add_n()
#'     }) |>
#'     gtsummary::tbl_stack()
#' }
#'
#' #' @rdname regression_table
#' #' @export
#' regression_table.default <- function(x, ..., args.list = NULL, fun = "gtsummary::tbl_regression") {
#'   # Stripping custom class
#'   class(x) <- class(x)[class(x) != "webresearch_model"]
#'
#'   if (any(c(length(class(x)) != 1, class(x) != "lm"))) {
#'     if (!"exponentiate" %in% names(args.list)) {
#'       args.list <- c(args.list, list(exponentiate = TRUE))
#'     }
#'   }
#'
#'   out <- do.call(getfun(fun), c(list(x = x), args.list))
#'   out |>
#'     gtsummary::add_glance_source_note() # |>
#'   # gtsummary::bold_p()
#' }

regression_table <- function(x, ...) {
  if ("list" %in% class(x)){
    x |>
      purrr::map(\(.m){
        regression_table_create(x = .m, ...) |>
          gtsummary::add_n()
      }) |>
      gtsummary::tbl_stack()
  } else {
    regression_table_create(x,...)
  }
}

regression_table_create <- function(x, ..., args.list = NULL, fun = "gtsummary::tbl_regression") {
  # Stripping custom class
  class(x) <- class(x)[class(x) != "webresearch_model"]

  if (any(c(length(class(x)) != 1, class(x) != "lm"))) {
    if (!"exponentiate" %in% names(args.list)) {
      args.list <- c(args.list, list(exponentiate = TRUE))
    }
  }

  out <- do.call(getfun(fun), c(list(x = x), args.list))
  out |>
    gtsummary::add_glance_source_note() # |>
  # gtsummary::bold_p()
}


#' A substitue to gtsummary::tbl_merge, that will use list names for the tab
#' spanner names.
#'
#' @param data gtsummary list object
#'
#' @return gt summary list object
#' @export
#'
tbl_merge <- function(data) {
  if (is.null(names(data))) {
    data |> gtsummary::tbl_merge()
  } else {
    data |> gtsummary::tbl_merge(tab_spanner = names(data))
  }
}
