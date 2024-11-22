#' Create table of regression model
#'
#' @param data regression model
#' @param args.list list of arguments passed to 'fun'.
#' @param fun function to use for table creation. Default is "gtsummary::tbl_regression".
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
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
regression_table <- function(data, args.list = NULL, fun = "gtsummary::tbl_regression") {

  if (any(c(length(class(data))!=1, class(data)!="lm"))){
    if (!"exponentiate" %in% names(args.list)){
      args.list <- c(args.list,list(exponentiate=TRUE))
    }
  }

  out <- do.call(getfun(fun), c(list(x = data), args.list))
  out |> gtsummary::add_glance_source_note()
}
