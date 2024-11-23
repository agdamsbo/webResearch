#' Split vector by an index and embed addition
#'
#' @param data vector
#' @param index split index
#' @param add addition
#'
#' @return vector
#' @export
#'
index_embed <- function(data, index, add = NULL) {
  start <- seq_len(index)
  end <- seq_along(data)[-start]
  c(
    data[start],
    add,
    data[end]
  )
}

#' Specify format arguments to include in qmd header/frontmatter
#'
#' @param data vector
#' @param fileformat format to include
#'
#' @return vector
#' @export
#'
specify_qmd_format <- function(data, fileformat = c("docx", "odt", "pdf", "all")) {
  fileformat <- match.arg(fileformat)
  args_list <- default_format_arguments() |> purrr::imap(format_writer)

  if (fileformat == "all") {
    out <- data |> index_embed(index = 4, add = Reduce(c, args_list))
  } else {
    out <- data |> index_embed(index = 4, add = args_list[[fileformat]])
  }
  out
}

#' Merges list of named arguments for qmd header generation
#'
#' @param data vector
#' @param name name
#'
#' @return vector
#' @export
#'
format_writer <- function(data, name) {
  if (data == "default") {
    glue::glue("  {name}: {data}")
  } else {
    warning("Not implemented")
  }
}

#' Defaults qmd formats
#'
#' @return list
#' @export
#'
default_format_arguments <- function() {
  list(
    docx = list("default"),
    odt = list("default"),
    pdf = list("default")
  )
}

#' Wrapper to modify quarto file to render specific formats
#'
#' @param file filename
#' @param format desired output
#'
#' @return none
#' @export
#'
modify_qmd <- function(file, format) {
  readLines(file) |>
    specify_qmd_format(fileformat = "all") |>
    writeLines(paste0(tools::file_path_sans_ext(file), "_format.", tools::file_ext(file)))
}
