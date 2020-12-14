#' General request function for weatherlink api
#' @rdname wl_request
#' @param endpoint api endpoint to query
#' @param path named list; path parameter to pass
#' @param query named list of length 2; Only "start-timestamp", "end_timestamp"
#'   are allowed
#' @param api_key API key. Read by default from env variable `WL_API_KEY`
#' @param api_secret API secret. Read by default from env variable
#'   `WL_API_SECRET`
#' @param ignore_ratelimit ignore wait time to not hit rate limit of 10/second?
#' @param verbose logical, should the request be printed?
#' @description authentication is done via hmac, see [wl_params()].
#' @importFrom httr modify_url GET content http_error status_code
#' @importFrom jsonlite fromJSON
#' @importFrom purrr compact
#' @export
#' @examples
#' \dontrun{
#' stations <- wl_request(endpoint = "stations")
#' wl_request(endpoint = "stations",
#'   path = list(`station-ids` = stations$stations[[1]]$station_id))
#' wl_request(endpoint = "stations",
#'   path = list(`station-ids` = c(stations$stations[[1]]$station_id,
#'     stations$stations[[2]]$station_id)))
#' wl_request(endpoint = "current",
#'   path = list(`station-id` = stations$stations[[1]]$station_id))
#' wl_request(endpoint = "historic",
#'   path = list(`station-id` = stations$stations[[1]]$station_id),
#'   query = list(
#'     `start-timestamp` = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
#'     `end-timestamp` = as.integer(as.POSIXct("2020-06-16 20:59:39 CET"))))
#' }
wl_request <- function(endpoint = NULL,
                       path = NULL,
                       query = NULL,
                       api_key = Sys.getenv("WL_API_KEY"),
                       api_secret = Sys.getenv("WL_API_SECRET"),
                       ignore_ratelimit = FALSE,
                       verbose = FALSE) {

  # check inputs
  stopifnot(!is.null(endpoint))
  if (!is.null(path)) {
    stopifnot(is.list(path))
    stopifnot(names(path) %in% path_params)
    stopifnot(length(path) == 1)
    path[[1]] <-  paste(path[[1]], collapse = ",")
  }
  if (!is.null(query)) {
    stopifnot(is.list(query))
    stopifnot(names(query) %in% query_params)
    stopifnot(length(query) == 2)
  }

  if (nchar(api_key) == 0)
    stop("api_key is empty. Is environment variable 'WL_API_KEY' set?")

  if (nchar(api_secret) == 0)
    stop("api_secret is empty. Is environment variable 'WL_API_SECRET' set?")

  fullpath <- purrr::compact(list("v2", endpoint, path[[1]]))
  qurl <- httr::modify_url("https://api.weatherlink.com", path = fullpath)
  params <- wl_params(api_key = api_key, path = path, query = query)


  if (verbose)
    message("GET: ", qurl)

  if (!ignore_ratelimit)
    Sys.sleep(0.1)

  resp <- httr::GET(url = qurl, query = params)

  parsed <- resp %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "weatherlink API request failed [%s]\n%s",
        httr::status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }

  return(parsed)
}


#' Create paramesters for weatherlink api calls
#' @rdname wl_request
#' @importFrom digest hmac
#' @export
#' @examples
#' \dontrun{
#' wl_params()
#' wl_params(path = list(`station-ids` = 123456))
#' }
wl_params <- function(path = NULL,
                      query = NULL,
                      api_key = Sys.getenv("WL_API_KEY"),
                      api_secret = Sys.getenv("WL_API_SECRET")) {

  if (is.null(path)) {
    params <- list(
      `api-key` = api_key,
      `t` = as.character(as.integer(Sys.time()))
    )
  } else {
    stopifnot(is.list(path))
    stopifnot(names(path) %in% path_params)
    stopifnot(length(path) == 1)
    if (is.null(query)) {
      params <- list(
        `api-key` = api_key,
        p = path[[1]],
        `t` = as.character(as.integer(Sys.time()))
      )
      names(params)[2] <- names(path)
    } else {
      stopifnot(is.list(query))
      stopifnot(names(query) %in% query_params)
      stopifnot(length(query) == 2)
      params <- list(
        `api-key` = api_key,
        `end-timestamp` = query[["end-timestamp"]],
        `start-timestamp` = query[["start-timestamp"]],
        p = path[[1]],
        `t` = as.character(as.integer(Sys.time()))
      )
      names(params)[4] <- names(path)
    }
  }


  signature <- digest::hmac(
    key = api_secret,
    object = paste(names(params),
                   as.character(params), sep = "", collapse = ""),
    algo = "sha256"
  )

  if (is.null(query)) {
    out <- list(`api-key` = params$`api-key`,
                t = params$t,
                `api-signature` = signature)
  } else {
    out <- list(`api-key` = params$`api-key`,
                t = params$t,
                `start-timestamp` = query[["start-timestamp"]],
                `end-timestamp` = query[["end-timestamp"]],
                `api-signature` = signature)
  }

  return(out)
}

#' list of allowed path parameter names
path_params <- c("station-id", "station-ids", "sensor-id", "sensor-ids",
                 "node-ids", "start-timestamp", "end_timestamp")
#' list of allowed query parameter names
query_params <- c("start-timestamp", "end-timestamp")
