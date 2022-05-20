## Base API url
#' Helper function for base CoinCap API URL
#'
#' @param endpoint character
#'
#' @return character API URL
cg_api_url <- function(endpoint) {
  base_api <- "https://api.coingecko.com/api/v3"
  if (missing(endpoint)) return(base_api)
  file.path(base_api, endpoint)
}

## import from thelpers package
#' Helper functions for converting between millisecond numerics and datetime objects
#'
#' @name datetime_millisec
#'
#' @param x numeric/datetime
#'
#' @return numeric/datetime
NULL

#' @rdname datetime_millisec
#'
#' @export
millisec_to_datetime <- function(x) {
  require(lubridate)
  stopifnot(is.numeric(x))
  lubridate::as_datetime(x / 1000)
}

#' @rdname datetime_millisec
#'
#' @export
datetime_to_millisec <- function(x) {
  as.numeric(x) * 1000
}
