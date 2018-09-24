#' Generate a geovis widget output
#'
#' @param width width of output
#' @param height height of output
#' @param www_dir directory where widget will be placed
#'
#' @import htmlwidgets
# geovis_widget(www_dir = "~/Desktop/geowidget")
geovis_widget <- function(width = NULL, height = NULL, www_dir) {

  id <- paste(sample(letters, 10, replace = FALSE),
    collapse = "")

  x <- list(
    id = id,
    in_knitr = FALSE
  )

  # create widget
  wdgt <- htmlwidgets::createWidget(
    name = "geovis_widget",
    x,
    width = width,
    height = height,
    package = "geovis",
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 0,
      browser.fill = TRUE,
      knitr.defaultWidth = 900,
      knitr.defaultHeight = 550,
      knitr.figure = FALSE,
      viewer.defaultWidth = "100%",
      viewer.defaultHeight = "100%",
      viewer.padding = 0,
      viewer.fill = TRUE,
      browser.defaultWidth = "100%",
      browser.defaultHeight = "100%",
      browser.padding = 0
    ),
    elementId = id
  )

  attr(wdgt, "geovis_pars") <- list(
    www_dir = www_dir
  )

  return(wdgt)
}

# Override print.htmlwidget for geovisjs_widget so we can control the output location
#' @export
print.geovis_widget <- function(x, ..., view = interactive()) {

  # if we have a viewer then forward viewer pane height (if any)
  viewer <- getOption("viewer")
  if (!is.null(viewer)) {
    viewer_func <- function(url) {

      # get the requested pane height (it defaults to NULL)
      pane_height <- x$sizingPolicy$viewer$pane_height

      # convert maximize to -1 for compatibility with older versions of rstudio
      # (newer versions convert 'maximize' to -1 interally, older versions
      # will simply ignore the height if it's less than zero)
      if (identical(pane_height, "maximize"))
        pane_height <- -1

      # call the viewer
      viewer(url, height = pane_height)
    }
  } else {
    viewer_func <- utils::browseURL
  }

  www_dir <- attr(x, "geovis_pars")$www_dir

  # TODO: check if www_dir is in tempdir() or not and handle / warn appropriately

  # call html_print with the viewer
  el_tags <- htmltools::as.tags(x, standalone = FALSE)
  geovis_html_print(el_tags, www_dir = www_dir,
    viewer = if (view) viewer_func)

  # return value
  invisible(x)
}

geovis_html_print <- function(html, www_dir = NULL, background = "white",
  viewer = getOption("viewer", utils::browseURL)) {

  if (is.null(www_dir))
    www_dir <- tempfile("viewhtml")
  if (!dir.exists(www_dir))
    dir.create(www_dir)
  www_dir <- normalizePath(www_dir)
  index_html <- file.path(www_dir, "index.html")

  htmltools::save_html(html, file = index_html, background = background,
    libdir = "lib")
  if (!is.null(viewer))
    viewer(index_html)

  invisible(index_html)
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
