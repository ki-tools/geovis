#' Get geographic data for a geovis visualization
#'
#' @param countries a vector of country names (see \code{\link{country_names}}) for a valid list
#' @param get_states should state boundaries within each \code{country} also be retrieved?
#' @param get_munis should municipality (county/province) boundaries be retrieved?
#' @param scale scale to use when retrieving
#'
#' @details This function uses rnaturalearth to obtain country and state boundary data for a given vector of country names or codes
#' @export
get_geo_data <- function(countries,
  get_states = TRUE, get_munis = FALSE,
  scale = 110
) {
  res <- list()
  res$country <- rnaturalearth::ne_countries(
    country = countries, scale = scale)
  res$country <- res$country[, c("name", "adm0_a3")]
  names(res$country) <- c("country_name", "country_code")

  if (get_states) {
    res$state <- rnaturalearth::ne_states(
      country = countries)
    res$state <- res$state[, c("name", "adm0_a3", "adm1_code")]
    names(res$state) <- c("state_name", "country_code", "state_code")
  }

  if (get_munis) {
    mdat <- lapply(countries, function(x) {
      if (tolower(x) == "brazil") {
        if (!requireNamespace("brazilgeo", quietly = TRUE))
          stop("Package 'brazilgeo' is needed for Brazil municipality data. ",
          "Please install it.",
          call. = FALSE)
        return (brazilgeo::br_muni_geo)
      } else if (tolower(x) == 'united states of america') {
        if (!requireNamespace("USAboundaries", quietly = TRUE))
          stop("Package 'USAboundaries' is needed for USA municipality data. ",
               "Please install it.",
               call. = FALSE)
        return (sf:::as_Spatial(USAboundaries::us_counties()))
      } else {
        message("No municipality geo data for ", x)
      }
    })
    res$muni <- do.call(rbind, mdat)
  }

  class(res) <- c("geovis_geo", "list")

  res
}

print.geovis_geo <- function(x, ...) {
  message("geovis geographic boundaries object")
  if (!is.null(x$country))
  message("  ", nrow(x$country), " countries")
  if (!is.null(x$state))
    message("  ", nrow(x$state), " states")
  if (!is.null(x$muni))
    message("  ", nrow(x$muni), " municipalities")
}

#' Get list of available country names from rnaturalearth
#'
#' @param scale of map to use, one of 110, 50, 10
#' @note At \code{scale = 110}, 177 countries are available. At \code{scale=50}, 241 countries are available. At \code{scale=10}, 255 countries are available.
#'
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @export
country_names <- function(scale = 110) {
  if (scale == 110) {
    data.frame(
      country = rnaturalearth::countries110$name,
      code = rnaturalearth::countries110$adm0_a3,
      continent = rnaturalearth::countries110$continent
    )
  } else if (scale == 50) {
    data.frame(
      country = rnaturalearthdata::countries50$name,
      code = rnaturalearthdata::countries50$adm0_a3,
      continent = rnaturalearthdata::countries50$continent
    )
  } else if (scale == 10) {
    data.frame(
      country = rnaturalearthhires::countries10$name,
      code = rnaturalearthhires::countries10$adm0_a3,
      continent = rnaturalearthhires::countries10$continent
    )
  } else {
    stop("scale must be 10, 50, or 110")
  }
}
