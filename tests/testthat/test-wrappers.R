context("wrappers")
skip_if(Sys.getenv("WL_API_KEY") == "" | Sys.getenv("WL_API_SECRET") == "")


# wl_stations -------------------------------------------------------------
stations <- wl_stations()
skip_if(length(stations$stations) < 1)

test_that("wl_stations() works without args", {
  res <- stations
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "stations")
})
test_that("wl_stations() works with on station", {
  res <- wl_stations(station_ids = stations$stations[[1]]$station_id)
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "stations")
  expect_length(res[[1]], 1)
})

test_that("wl_stations() works with two stations", {
  skip_if(length(stations$stations) < 2)
  res <- wl_stations(station_ids =
    c(stations$stations[[1]]$station_id, stations$stations[[2]]$station_id))
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "stations")
  expect_length(res[[1]], 2)
})


# wl_nodes ----------------------------------------------------------------
test_that("wl_nodes() works without args", {
  res <- wl_nodes()
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "nodes")
})
test_that("wl_nodes() works with args", {
  expect_error(wl_nodes(node_ids = "invalid"))
})


# wl_sensors --------------------------------------------------------------
sensors <- wl_sensors()
test_that("wl_sensors() works without args", {
  res <- sensors
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensors")
})
test_that("wl_sensors() works with on station", {
  res <- wl_sensors(sensor_ids = sensors$sensors[[1]]$lsid)
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensors")
  expect_length(res[[1]], 1)
})
test_that("wl_sensors() works with two stations", {
  skip_if(length(sensors$sensors) < 2)
  res <- wl_sensors(sensor_ids = c(sensors$sensors[[1]]$lsid,
                                   sensors$sensors[[2]]$lsid))
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensors")
  expect_length(res[[1]], 2)
})


# wl_sensor_activity ------------------------------------------------------
test_that("wl_sensor_activity() works without args", {
  res <- wl_sensor_activity()
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensor_activity")
})
test_that("wl_sensor_activity() works with on station", {
  res <- wl_sensor_activity(sensor_ids = sensors$sensors[[1]]$lsid)
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensor_activity")
})
test_that("wl_sensor_activity() works with two stations", {
  skip_if(length(sensors$sensors) < 2)
  res <- wl_sensor_activity(sensor_ids = c(sensors$sensors[[1]]$lsid,
                                           sensors$sensors[[2]]$lsid))
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensor_activity")
})


# wl_sensor_catalog -------------------------------------------------------
test_that("wl_sensor_catalog() works", {
  res <- wl_sensor_catalog()
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensor_types")
})


# wl_current --------------------------------------------------------------
test_that("wl_current() works", {
  res <- wl_current(station_id = stations$stations[[1]]$station_id)
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "station_id")
  expect_error(wl_current(), regexp = "\\!missing\\(station_id\\) is not TRUE")
})


# wl_historic -------------------------------------------------------------
test_that("wl_historic() works", {
  res <- wl_historic(station_id = stations$stations[[1]]$station_id,
                     start = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
                     end = as.integer(as.POSIXct("2020-06-16 20:59:39 CET")))
  expect_is(res, "list")
  expect_true(names(res)[[1]] == "sensors")
  expect_error(wl_historic(station_id = stations$stations[[1]]$station_id),
               regexp = "\\!missing\\(start\\) is not TRUE")
  expect_error(
    wl_historic(station_id = stations$stations[[1]]$station_id,
      start = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
      end = as.integer(as.POSIXct("2020-06-16 12:59:39 CET"))
    ),
    regexp = "End timestamp can not be before the start timestamp"
  )
  expect_error(
    wl_historic(station_id = stations$stations[[1]]$station_id,
                start = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
                end = as.integer(as.POSIXct("2020-06-18 12:59:39 CET"))
    ),
    regexp = "Time range requested exceeds maximum"
  )
})
