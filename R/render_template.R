#' @importFrom jinjar jinjar_config
#' @importFrom jinjar package_loader
config <- jinjar::jinjar_config(
  loader = jinjar::package_loader("SeverityEstimate")
)


#' @title
#' Find And Render A Template
#'
#' @param template The name of the template to render, should match a file under
#' the `inst/` directory.
#' @param data The data to use when rendering the template.
#'
#' @returns
#' A rendered template as a single length character.
#'
#' @importFrom jinjar render
#' @keywords internal
render_template <- function(template, data = list()) {
  if (any(c(".x", ".config") %in% names(data))) {
    stop("The names '.x' and '.config' are not allowed in `data`.")
  }
  data[[".x"]] <- template
  data[[".config"]] <- config
  do.call(jinjar::render, data)
}
