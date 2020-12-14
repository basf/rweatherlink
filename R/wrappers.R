#' Query /stations & stations/{station-ids} endpoint
#' @rdname wl_wrappers
#' @param station_ids vector of stations ids to query. If NULL (default) all
#'   will be returned.
#' @param ... other arguments passed to [wl_request()]
#' @references https://weatherlink.github.io/v2-api/api-reference
#' @export
#' @examples
#' \dontrun{
#' (stations <- wl_stations())
#' wl_stations(station_ids = stations$stations[[1]]$station_id)
#' wl_stations(station_ids = c(stations$stations[[1]]$station_id,
#'     stations$stations[[2]]$station_id))
#' }
wl_stations <- function(station_ids = NULL, ...) {

  if (is.null(station_ids)) {
    out <- wl_request(endpoint = "stations", ...)
  } else {
    out <- wl_request(endpoint = "stations",
      path = list(`station-ids` = station_ids), ...)
  }
  class(out) <- append("wl_stations", class(out))
  return(out)
}

#' Query /nodes & nodes/{nodes-ids} endpoint
#' @rdname wl_wrappers
#' @param node_ids vector of stations ids to query. If NULL (default) all will
#'   be returned.
#' @export
#' @examples
#' \dontrun{
#' wl_nodes()
#' }
wl_nodes <- function(node_ids = NULL, ...) {

  if (is.null(node_ids)) {
    out <- wl_request(endpoint = "nodes", ...)
  } else {
    out <- wl_request(endpoint = "nodes",
                      path = list(`node-ids` = node_ids), ...)
  }
  class(out) <- append("wl_nodes", class(out))
  return(out)
}

#' Query /sensors & sensors/{sensor-ids} endpoint
#' @rdname wl_wrappers
#' @param sensor_ids vector of stations ids to query. If NULL (default) all
#'   will be returned.
#' @export
#' @examples
#' \dontrun{
#' (sensors <- wl_sensors())
#' wl_sensors(sensor_ids = sensors$sensors[[1]]$lsid)
#' wl_sensors(sensor_ids = c(sensors$sensors[[1]]$lsid,
#'   sensors$sensors[[2]]$lsid))
#' }
wl_sensors <- function(sensor_ids = NULL, ...) {

  if (is.null(sensor_ids)) {
    out <- wl_request(endpoint = "sensors", ...)
  } else {
    out <- wl_request(endpoint = "sensors",
                      path = list(`sensor-ids` = sensor_ids), ...)
  }
  class(out) <- append("wl_sensors", class(out))
  return(out)
}

#' Query /sensor-activity & sensor-activity/{sensor-ids} endpoint
#' @rdname wl_wrappers
#' @export
#' @examples
#' \dontrun{
#' wl_sensor_activity()
#' wl_sensor_activity(sensor_ids = sensors$sensors[[1]]$lsid)
#' wl_sensor_activity(sensor_ids = c(sensors$sensors[[1]]$lsid,
#'   sensors$sensors[[2]]$lsid))
#' }
wl_sensor_activity <- function(sensor_ids = NULL, ...) {

  if (is.null(sensor_ids)) {
    out <- wl_request(endpoint = "sensor-activity", ...)
  } else {
    out <- wl_request(endpoint = "sensor-activity",
                      path = list(`sensor-ids` = sensor_ids), ...)
  }
  class(out) <- append("wl_sensor_activity", class(out))
  return(out)
}

#' Query /sensor-catalog
#' @rdname wl_wrappers
#' @export
#' @examples
#' \dontrun{
#' wl_sensor_catalog()
#' }
wl_sensor_catalog <- function(...) {
  out <- wl_request(endpoint = "sensor-catalog", ...)
  class(out) <- append("wl_sensor_catalog", class(out))
  return(out)
}

#' Query /current/{station-id} endpoint
#' @rdname wl_wrappers
#' @param station_id station_id for which to query data.
#' @export
#' @examples
#' \dontrun{
#' (stations <- wl_stations())
#' wl_current(station_id = stations$stations[[1]]$station_id)
#' }
wl_current <- function(station_id, ...) {

  stopifnot(!missing(station_id))
  stopifnot(length(station_id) == 1)
  out <- wl_request(endpoint = "current",
                    path = list(`station-id` = station_id),
                    ...)
  class(out) <- append("wl_current", class(out))
  return(out)
}


#' Query /historic/{station-id} endpoint
#' @rdname wl_wrappers
#' @param start integer; Unix timestamp. Current timestamp can be generated
#'   with as.integer(Sys.time())
#' @param end integer; Unix timestamp. Current timestamp can be generated
#'   with as.integer(Sys.time())
#' @export
#' @examples
#' \dontrun{
#' (stations <- wl_stations())
#' wl_historic(station_id = stations$stations[[1]]$station_id,
#'   start = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
#'   end = as.integer(as.POSIXct("2020-06-16 20:59:39 CET"))
#'  )
#' }
wl_historic <- function(station_id, start, end, ...) {

  stopifnot(!missing(station_id))
  stopifnot(length(station_id) == 1)
  stopifnot(!missing(start))
  stopifnot(is.integer(start))
  stopifnot(!missing(end))
  stopifnot(is.integer(end))

  out <- wl_request(endpoint = "historic",
    path = list(`station-id` = station_id),
    query = list(
      `start-timestamp` = start,
      `end-timestamp` = end),
    ...
  )
  class(out) <- append("wl_historic", class(out))
  return(out)
}
