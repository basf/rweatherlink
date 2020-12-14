context("wl_request")

test_that("wl_request fails with invalid credentials", {
  expect_error(
    wl_request(endpoint = "stations",
               api_key = "invalid",
               api_secret = "invalid"),
    regexp = "Invalid authentication credentials")
})

test_that("wl_request fails with missing credentials", {
  skip_if(Sys.getenv("WL_API_KEY") == "" | Sys.getenv("WL_API_SECRET") == "")

  expect_error(wl_request(endpoint = "stations", api_key = ""),
               regexp = "api_key is empty")
  expect_error(wl_request(endpoint = "stations", api_secret = ""),
               regexp = "api_secret is empty")
})

test_that("wl_request works", {
  skip_if(Sys.getenv("WL_API_KEY") == "" | Sys.getenv("WL_API_SECRET") == "")

  expect_message({
    stations <- wl_request(endpoint = "stations", verbose = TRUE)
    },
    "GET: https://api.weatherlink.com/v2/stations")
  expect_is(stations, "list")
  expect_true(names(stations)[[1]] == "stations")
})
