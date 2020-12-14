#' parse output from rweatherlink objects
#' @param x weatherlink object
#' @param ... other arguments to methods (currently unused)
#' @export
#' @examples
#' \dontrun{
#' (stations <- wl_stations() %>% wl_parse())
#' wl_nodes() %>% wl_parse()
#' wl_sensors() %>% wl_parse()
#' wl_sensor_activity() %>% wl_parse()
#' wl_current(station_id = stations$station_id[[1]]) %>% wl_parse()
#' wl_historic(station_id = stations$station_id[[1]],
#'   start = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
#'   end = as.integer(as.POSIXct("2020-06-16 20:59:39 CET"))
#'  ) %>% wl_parse()
#' }
wl_parse <- function(x, ...) {
  UseMethod("wl_parse", object = x)
}

#' @export
wl_parse.default <- function(x, ...) {
  warning("No matching parsing method for object of class '",
          class(x)[[1]], "' found. Returning object."
          )
  x
}

#' @export
#' @importFrom data.table setDF rbindlist
wl_parse.wl_stations <- function(x, ...) {
  suppressWarnings({
    out <- data.table::setDF(data.table::rbindlist(x$stations, fill = TRUE))
  })

  if (nrow(out) == 0)
    return(NULL)

  out <- out %>%
    mutate(elevation_m = .data$elevation / 3.2808,
           registered_date_utc = as.POSIXct(.data$registered_date,
            origin = "1970-01-01",  tz = "UTC"))
  return(out)
}

#' @export
#' @importFrom data.table setDF rbindlist
wl_parse.wl_nodes <- function(x, ...) {
  suppressWarnings({
    out <- data.table::setDF(data.table::rbindlist(x$nodes, fill = TRUE))
  })
  if (nrow(out) == 0)
    return(NULL)
  return(out)
}

#' @export
#' @importFrom data.table setDF rbindlist
wl_parse.wl_sensors <- function(x, ...) {
  suppressWarnings({
    out <- data.table::setDF(data.table::rbindlist(x$sensors, fill = TRUE))
  })
  if (nrow(out) == 0)
    return(NULL)

  out <- out %>%
    mutate(elevation_m = .data$elevation / 3.2808,
      created_date_utc = as.POSIXct(.data$created_date,
        origin = "1970-01-01",  tz = "UTC"),
      modified_date_utc = as.POSIXct(.data$modified_date,
        origin = "1970-01-01",  tz = "UTC"))

  return(out)
}

#' @importFrom data.table setDF rbindlist
parse_sensor <- function(sensor) {
  suppressWarnings({
    data <- data.table::setDF(data.table::rbindlist(sensor$data, fill = TRUE))
  })
  out <- data.frame(lsid = sensor$lsid, sensor_type = sensor$sensor_type, data)
  return(out)
}

#' @export
#' @importFrom purrr map_df
#' @import dplyr
wl_parse.wl_current <- function(x, ...) {
  out <- purrr::map_df(x$sensors, parse_sensor)
  out <- out %>%
    mutate(ts_utc = as.POSIXct(.data$ts, origin = "1970-01-01",  tz = "UTC"))
  return(out)
}

#' @export
#' @importFrom purrr map_df
#' @import dplyr
wl_parse.wl_historic <- function(x, ...) {
  out <- purrr::map_df(x$sensors, parse_sensor)
  out <- out %>%
    mutate(ts_utc = as.POSIXct(.data$ts, origin = "1970-01-01",  tz = "UTC"))
  return(out)
}
