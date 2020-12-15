
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rweatherlink

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/rweatherlink)](https://CRAN.R-project.org/package=rweatherlink)
[![R build
status](https://github.com/basf/rweatherlink/workflows/R-CMD-check/badge.svg)](https://github.com/basf/rweatherlink/actions)
[![Codecov test
coverage](https://codecov.io/gh/basf/rweatherlink/branch/master/graph/badge.svg?token=3OZ8Y9VVWN)](https://codecov.io/gh/basf/rweatherlink?branch=master)
<!-- badges: end -->

An R client for [Weatherlink v2
API](https://weatherlink.github.io/v2-api/)

## Usage

``` r
library("rweatherlink")
```

### Authentication

The api key and api secret are read by default from environmental
variables `WL_API_KEY` and `WL_API_SECRET`, but you can provide them
also in every function call using the `api_key=` and `api_secret=`
arguments.

### Basic usage

For each endpoint we provide convenience wrappers, like

``` r
wl_stations()
```

The full list of endpoints wrappers is:

  - `wl_stations()` for `/stations` & `stations/{station-ids}` endpoints
  - `wl_nodes()` for `/nodes` & `nodes/{nodes-ids}` endpoints
  - `wl_sensors()` for `/sensors` & `sensors/{sensor-ids}` endpoints
  - `wl_sensor_activity()` for `/sensor-activity` &
    `sensor-activity/{sensor-ids}` endpoints
  - `wl_sensor_catalog()` for `/sensor-catalog` endpoint
  - `wl_current()` for `/current/{station-id}` endpoint
  - `wl_historic()` for `/historic/{station-id}` endpoint

### Parser

For each of the listed functions (except `wl_sensor_catalog()`) we
provide (opinionated) convenience parsers, that parse the return value
into a nice `data.frame`. Columns with UTC-dates for timestamps are
added, as well as elevation in meters.

The generic function for all objects is `wl_parse()`

``` r
wl_stations() %>% wl_parse()
wl_sensors() %>% wl_parse()
```

These parsers are in an experimental stage.

### Timestamps

`wl_historic()` needs unix-timestamps as input for start and end. These
can be easily generated from `POSIXct` objects, e.g.

``` r
as.integer(Sys.time())
#> [1] 1608050438
```

gives the timestamp of the current time.

## Under the hood

`wl_request()` is the workhorse of this package.

With it you can query every API endpoint, e.g.

``` r
wl_request(endpoint = "stations")
```

It takes also care of creating the correct api-signature via HMAC, which
us handled by `wl_params()`.

## Unit tests

To run the full test suite set valid environmental variables
`WL_API_KEY` and `WL_API_SECRET` and run `devtools::test()`.

Note, that for some tests more than one stations is needed.

Currently, code coverage is very high. wl\_nodes is not well covered by
the unit tests.
