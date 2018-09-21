#' Generate a geovis output
#'
#' @param width width of output
#' @param height height of output
#'
#' @import htmlwidgets
#'
#' @export
geovis <- function(width = NULL, height = NULL) {

  id <- digest::digest(Sys.time())

  x <- list(
    id = id,
    in_knitr = FALSE
  )

  # create widget
  htmlwidgets::createWidget(
    name = "geovis",
    x,
    width = width,
    height = height,
    package = "geovis",
    elementId = id
  )
}

#' Shiny bindings for geovis
#'
#' Output and render functions for using geovis within Shiny
#' applications and interactive Rmd documents.
#'
#' @param output_id output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a geovis
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name geovis-shiny
#'
#' @export
geovisOutput <- function(output_id, width = "100%", height = "400px"){
  htmlwidgets::shinyWidgetOutput(output_id, "geovis", width, height,
  package = "geovis")
}

#' @rdname geovis-shiny
#' @export
renderGeovis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, geovisOutput, env, quoted = TRUE)
}
