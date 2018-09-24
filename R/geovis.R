#' Create a geovis visualization
#'
#' @param geo an object returned from \code{\link{get_geo_data}}
#' @param path where to place the widget
#' @param country_data data to use at the country level
#' @param state_data data to use at the state level
#' @param muni_data data to use at the municipality level
#' @param var_info information about variables (TODO)
#' @importFrom sp CRS
#' @importFrom rgdal writeOGR
#' @importFrom jsonlite toJSON
#' @export
geovis <- function(geo, path = NULL,
  country_data = NULL, state_data = NULL, muni_data = NULL,
  var_info = NULL
) {
  if (!inherits(geo, "geovis_geo"))
    stop("'geo' argument must be generated from get_geo_data().",
      call. = FALSE)

  if (is.null(path))
    path <- tempfile()

  # TODO: check data for being sorted by year and unique year

  check_path(path)

  if (!is.null(geo$country)) {
    message("writing country geojson...")

    pth <- check_path(path, "data")
    # out <- as.character(geojson::as.geojson(geo$country))
    out <- getgeojson(geo$country)

    cat(paste0("__geovis_country__({\"geo\":", out, ","),
      file = file.path(pth, "countries.jsonp"))
  }

  if (!is.null(geo$state)) {
    message("writing state geojson...")

    pth <- check_path(path, "data", "states")
    idxs <- split(seq_along(geo$state), geo$state@data$country_code)
    for (ctry in names(idxs)) {
      # out <- as.character(geojson::as.geojson(geo$state[idxs[[ctry]], ]))
      out <- getgeojson(geo$state[idxs[[ctry]], ])
      cat(paste0("__geovis_state__({\"geo\":", out, ","),
        file = file.path(pth, paste0(ctry, ".jsonp")))
    }
  }

  if (!is.null(geo$muni)) {
    message("writing muni geojson...")

    pth <- check_path(path, "data", "munis")
    idxs <- split(seq_along(geo$muni), geo$muni@data$country_code)
    for (ctry in names(idxs)) {
      pth2 <- check_path(pth, ctry)
      tmp <- geo$muni[idxs[[ctry]], ]
      idxs2 <- split(seq_along(tmp), tmp@data$state_code)
      for (st in names(idxs2)) {
        # out <- as.character(geojson::as.geojson(tmp[idxs2[[st]], ]))
        out <- getgeojson(tmp[idxs2[[st]], ])
        cat(paste0("__geovis_muni__({\"geo\":", out, ","),
          file = file.path(pth2, paste0(st, ".jsonp")))
      }
    }
  }

  if (!is.null(country_data)) {
    message("writing country data...")

    pth <- check_path(path, "data")

    country_data <- check_data(country_data,
      "country_data", "country_code", geo$country)

    tmp <- split(country_data, country_data$country_code)
    tmp <- lapply(tmp, function(x) {
      x$country_code <- NULL
      as.list(x)
    })

    out <- jsonlite::toJSON(tmp)
    cat(paste0("\"data\":", out, "})"),
      file = file.path(pth, "countries.jsonp"),
      append = TRUE)
  } else {
    cat("\"data\":{}})",
      file = file.path(pth, "countries.jsonp"),
      append = TRUE)
  }

  if (!is.null(state_data)) {
    message("writing state data...")

    pth <- check_path(path, "data", "states")

    state_data <- check_data(state_data, "state_data",
      c("country_code", "state_code"),
      geo$state)

    tmp <- split(state_data, state_data$country_code)
    ff <- gsub("\\.jsonp$", "", list.files(pth))
    for (ctry in ff) {
      if (is.null(tmp[[ctry]])) {
        cat("\"data\":{})",
          file = file.path(pth, paste0(ctry, ".jsonp")),
          append = TRUE)
      } else {
        tmp2 <- lapply(split(tmp[[ctry]], tmp[[ctry]]$state_code), function(a) {
          a$country_code <- NULL
          a$state_code <- NULL
          as.list(a)
        })
        out <- jsonlite::toJSON(tmp2)
        cat(paste0("\"data\":", out, "})"),
          file = file.path(pth, paste0(ctry, ".jsonp")),
          append = TRUE)
      }
    }
  } else {
    pth <- check_path(path, "data", "states")
    ff <- list.files(pth, full.names = TRUE)
    for (f in ff)
      cat("\"data\":{})", file = f, append = TRUE)
  }

  if (!is.null(muni_data)) {
    message("writing muni data...")

    pth <- check_path(path, "data", "munis")

    muni_data <- check_data(muni_data, "muni_data",
      c("country_code", "state_code", "muni_code"),
      geo$muni)

    tmp <- split(muni_data, muni_data$country_code)
    ff1 <- gsub("\\.jsonp$", "", list.files(pth))
    for (ctry in ff1) {
      pth2 <- check_path(pth, ctry)
      tmp2 <- split(tmp[[ctry]], tmp[[ctry]]$state_code)
      ff2 <- gsub("\\.jsonp$", "", list.files(pth2))
      for (st in ff2) {
        if (is.null(tmp2[[st]])) {
          cat(paste0("\"data\":{})"),
            file = file.path(pth2, paste0(st, ".jsonp")),
            append = TRUE)
        } else {
          tmp3 <- lapply(split(tmp2[[st]], tmp2[[st]]$muni_code), function(a) {
            a$country_code <- NULL
            a$state_code <- NULL
            a$muni_code <- NULL
            as.list(a)
          })
          out <- jsonlite::toJSON(tmp3)
          cat(paste0("\"data\":", out, "})"),
            file = file.path(pth2, paste0(st, ".jsonp")),
            append = TRUE)
        }
      }
    }
  } else {
    pth <- check_path(path, "data", "munis")
    ff <- list.files(pth2, full.names = TRUE, recursive = TRUE,
      pattern = "jsonp$")
    for (f in ff)
      cat("\"data\":{})", file = f, append = TRUE)
  }
}

check_data <- function(dat, datname, varnames, geo) {
  nms <- names(dat)
  if (! "year" %in% nms)
    stop("The variable 'year' must exist in ", datname, ".",
      call. = FALSE)

  for (varname in varnames) {
    if (! varname %in% nms)
      stop("The variable '", varname, "' must exist in ", datname, ".",
        call. = FALSE)

    uc <- unique(dat[[varname]])
    if (! all(uc %in% geo[[varname]])) 
      stop("There are '", varname, "' values in country_data that ",
        "aren't found in the geographic data.",
        call. = FALSE)
  }

  # remove factors
  idx <- which(sapply(dat, is.factor))
  for (i in idx)
    dat[[i]] <- as.character(dat[[i]])

  dat
}

getgeojson <- function(x) {
  tf <- tempfile()
  x@proj4string <- sp::CRS()
  rgdal::writeOGR(
    x,
    dsn = tf,
    layer = "",
    driver = "GeoJSON",
    layer_options = "COORDINATE_PRECISION=4",
    encoding = "latin1"
  )
  paste(readLines(tf), collapse = "")
}

check_path <- function(...) {
  pth <- file.path(...)
  if (!dir.exists(pth)) {
    dir.create(pth, recursive = TRUE)
  }
  pth
}
