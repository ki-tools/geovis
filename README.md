## geovis

- Visualize geographic data as a choropleth


## Example

```r
geo <- get_geo_data("brazil", get_munis = TRUE, scale = 10)
geo$muni$state_code <- geo$muni$state_code2
geo$muni$state_code2 <- NULL

country_data <- data.frame(
  year = 2001:2010,
  country_code = "BRA",
  dat = runif(10),
  stringsAsFactors = FALSE
)

state_data <- data.frame(
  year = rep(2001:2010, nrow(geo$state)),
  country_code = "BRA",
  state_code = rep(geo$state$state_code, each = 10),
  dat = runif(10 * nrow(geo$state)),
  stringsAsFactors = FALSE
)

muni_data <- data.frame(
  year = rep(2001:2010, nrow(geo$muni)),
  country_code = "BRA",
  state_code = rep(geo$muni$state_code, each = 10),
  muni_code = rep(geo$muni$muni_code, each = 10),
  dat = runif(10 * nrow(geo$muni)),
  stringsAsFactors = FALSE
)

geovis(geo, path = "~/Desktop/geowidget3",
  name = "Brazil SINASC Explorer",
  view_level = "country",
  view_country_code = "BRA",
  default_var = "dat",
  country_data = country_data,
  state_data = state_data,
  muni_data = muni_data
)
```


## TODO

- Support geo grid morhping to view all time series simultaneously while preserving some notion of geographic location (as is done [here]()). This will not always be effective, especially when the number of geographic entities is very large.
- Make setting height / width work (currently always full page)
- Broader testing with multiple countries, etc.
- More robust checking of config to ensure a bad spec isn't sent to the JS library

