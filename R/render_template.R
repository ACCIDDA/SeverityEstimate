#' @importFrom jinjar jinjar_config
#' @importFrom jinjar package_loader
config <- jinjar::jinjar_config(
  loader = jinjar::package_loader("SeverityEstimate", "models")
)


#' @title
#' Find And Render A Template
#'
#' @param template The name of the template to render, should match a file under
#' the `inst/` directory.
#' @param template_data The data to use when rendering the template.
#'
#' @returns
#' A rendered template as a single length character.
#'
#' @importFrom fs path
#' @importFrom jinjar render
#' @keywords internal
render_template <- function(template, template_data = list()) {
  if (any(c(".x", ".config") %in% names(template_data))) {
    stop("The names '.x' and '.config' are not allowed in `template_data`.")
  }
  if (!inherits(template, "fs_path")) {
    template <- fs::path(template)
  }
  template_data[[".x"]] <- template
  template_data[[".config"]] <- config
  do.call(jinjar::render, template_data)
}
