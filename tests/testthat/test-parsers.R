context("parsers")
skip_if(Sys.getenv("WL_API_KEY") == "" | Sys.getenv("WL_API_SECRET") == "")

stations <- wl_stations()
skip_if(length(stations$stations) < 1)


# wl_stations ----------------------------------------------------------------
test_that("correctly parse stations w/o agrs", {
  res <- stations %>% wl_parse()
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 24)
  expect_is(res$registered_date_utc, "POSIXct")
})
test_that("correctly parse stations one arg", {
  res <- wl_stations(station_ids = stations$stations[[1]]$station_id) %>%
    wl_parse()
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 24)
  expect_true(nrow(res) == 1)
  expect_is(res$registered_date_utc, "POSIXct")
  expect_equal(attr(res$registered_date_utc, "tzone"), "UTC")
})
test_that("correctly parse stations two args", {
  skip_if(length(stations$stations) < 2)
  res <- wl_stations(station_ids = c(stations$stations[[1]]$station_id,
                                     stations$stations[[2]]$station_id)) %>%
    wl_parse()
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 24)
  expect_true(nrow(res) == 2)
  expect_is(res$registered_date_utc, "POSIXct")
  expect_equal(attr(res$registered_date_utc, "tzone"), "UTC")
})


# wl_nodes -------------------------------------------------------------------
nodes <- wl_nodes()
test_that("correctly parse nodes w/o agrs", {
  res <- nodes %>% wl_parse()
  expect_equal(res, NULL)
})


# wl_sensors -----------------------------------------------------------------
sensors <- wl_sensors()
test_that("correctly parse sensors w/o agrs", {
  res <- sensors %>% wl_parse()
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 24)
  expect_is(res$created_date_utc, "POSIXct")
})
test_that("correctly parse sensors one arg", {
  res <- wl_sensors(sensor_ids = sensors$sensors[[1]]$lsid) %>%
    wl_parse()
  expect_is(res, "data.frame")
  expect_true(nrow(res) == 1)
  expect_true(ncol(res) == 24)
  expect_is(res$created_date_utc, "POSIXct")
  expect_equal(attr(res$created_date_utc, "tzone"), "UTC")
})
test_that("correctly parse sensors with 2 args", {
  skip_if(length(sensors$sensors) < 2)
  res <- wl_sensors(sensor_ids = c(sensors$sensors[[1]]$lsid,
                                   sensors$sensors[[2]]$lsid)) %>%
    wl_parse()
  expect_is(res, "data.frame")
  expect_true(nrow(res) == 2)
  expect_true(ncol(res) == 24)
  expect_is(res$created_date_utc, "POSIXct")
  expect_equal(attr(res$created_date_utc, "tzone"), "UTC")
})


# wl_sensor_activity ---------------------------------------------------------
activity <- wl_sensor_activity()
test_that("correctly parse sensor_activity w/o agrs", {
  expect_warning({
    res <- activity %>% wl_parse()
    },
    regexp = "No matching parsing method for object of class")
  expect_is(res, "wl_sensor_activity")
  expect_equal(res, activity)
})

# wl_sensor_activity ---------------------------------------------------------
catalog <- wl_sensor_catalog()
test_that("correctly parse sensor_activity w/o agrs", {
  expect_warning({
    res <- catalog %>% wl_parse()
  },
  regexp = "No matching parsing method for object of class")
  expect_is(res, "wl_sensor_catalog")
  expect_equal(res, catalog)
})


# wl_current --------------------------------------------------------------
current <- wl_current(station_id = stations$stations[[1]]$station_id)
test_that("correctly parse current w/o agrs", {
  res <- current %>% wl_parse()
  expect_is(res, "data.frame")
  expect_true(nrow(res) == 1)
  expect_is(res$ts_utc, "POSIXct")
  expect_equal(attr(res$ts_utc, "tzone"), "UTC")
})


# wl_historic -------------------------------------------------------------

historic <- wl_historic(station_id = stations$stations[[1]]$station_id,
                   start = as.integer(as.POSIXct("2020-06-16 13:59:39 CET")),
                   end = as.integer(as.POSIXct("2020-06-16 20:59:39 CET")))
test_that("correctly parse historic w/o agrs", {
  res <- historic %>% wl_parse()
  expect_is(res, "data.frame")
  expect_is(res$ts_utc, "POSIXct")
  expect_equal(attr(res$ts_utc, "tzone"), "UTC")
})
