#' Test version of the shiny_cast function to launch the app with a data set in
#' the environment.
#'
#' @param data optional data set to provide for analysis
#' @param ... arguments passed on to `shiny::runApp()`
#'
#' @return shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars |> shiny_webResearch(launch.browser = TRUE)
#' }
shiny_webResearch <- function(data = NULL, ...) {
  appDir <- system.file("apps", "data_analysis_modules", package = "webResearch")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `webResearch`.", call. = FALSE)
  }

  G <- .GlobalEnv

  if (!is.null(data) && is.data.frame(data)) {
    assign("webResearch_data", data, envir = G)
  }
  a <- shiny::runApp(appDir = appDir, ...)
  return(invisible(a))
}
