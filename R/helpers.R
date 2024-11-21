#' Wrapper function to get function from character vector referring to function from namespace. Passed to 'do.call()'
#'
#' @description
#' This function follows the idea from this comment: https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
#' @param x function or function name
#'
#' @return function or character vector
#' @export
#'
#' @examples
#' getfun("stats::lm")
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

#' Wrapper to save data in RDS, load into specified qmd and render
#'
#' @param data list to pass to qmd
#' @param fileformat output format. Ignored if file!=NULL
#' @param qmd.file qmd file to render. Default is 'here::here("analyses.qmd")'
#' @param file exact filename (Optional)
#' @param ... Ignored for now
#'
#' @return none
#' @export
#'
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

#' Flexible file import based on extension
#'
#' @param file file name
#' @param consider.na character vector of strings to consider as NAs
#'
#' @return tibble
#' @export
#'
#' @examples
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
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
  } else if (ext == "rds") {
    df <- readr::read_rds(file = file)
  } else {
    stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta', '.ods' or '.rds'")
  }

  df
}

#' Convert string of arguments to list of arguments
#'
#' @description
#' Idea from the answer: https://stackoverflow.com/a/62979238
#'
#' @param string string to convert to list to use with do.call
#'
#' @return list
#' @export
#'
argsstring2list <- function(string){
  eval(parse(text = paste0("list(", string, ")")))
}
