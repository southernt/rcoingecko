## supported vs currencies
#' Query for supported vs currencies
#'
#' @return vector
#'
#' @export
cg_supported_vs_currencies <- function() {
  require(httr)
  require(jsonlite)

  res <- httr::GET(cg_api_url("simple/supported_vs_currencies"))
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  jsonlite::fromJSON(rawToChar(res$content))
}
# cg_supported_vs_currencies()


## asset platforms
#' Query for asset platforms
#'
#' @return data frame
#'
#' @export
cg_asset_platforms <- function() {
  require(httr)
  require(jsonlite)
  require(dplyr)

  res <- httr::GET(cg_api_url("asset_platforms"))
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  if (!inherits(dat, "data.frame")) return(dat$error)
  dat |>
    dplyr::mutate_if(is.character, ~ ifelse(.x == "", NA_character_, .x)) |>
    dplyr::as_tibble()
}
# cg_asset_platforms()

## supported coins
#' Query for supported coins
#'
#' @return data frame
#'
#' @export
cg_supported_coins <- function(){
  require(httr)
  require(jsonlite)
  require(dplyr)

  cg_api_url("coins/list")
  res <- httr::GET(cg_api_url("coins/list"))
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  if (!inherits(dat, "data.frame")) return(dat$error)
  dat |>
    dplyr::filter(
      nchar(id) <= 8
    ) |>
    dplyr::as_tibble()
}
# cg_supported_coins()


## supported exchanges
#' Query for supported exchanges
#'
#' @return data frame
#'
#' @export
cg_exchanges <- function() {
  require(httr)
  require(jsonlite)
  require(dplyr)

  res <- httr::GET(cg_api_url("exchanges"))
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  dplyr::as_tibble(jsonlite::fromJSON(rawToChar(res$content)))
}


## coin categories
#' Query for coin categories
#'
#' @return data frame
#'
#' @export
cg_categories <- function() {
  require(httr)
  require(jsonlite)
  require(dplyr)

  res <- httr::GET(cg_api_url("coins/categories"))
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  jsonlite::fromJSON(rawToChar(res$content)) |>
    dplyr::mutate_if(is.character, ~ ifelse(.x == "", NA_character_, .x)) |>
    dplyr::as_tibble()
}
# cg_categories()


## market chart range
#' Query for high-level market data by time range
#'
#' @param coin coin to query for
#' @param vs_currency quote currency
#' @param start_tm min time for desired time range
#' @param end_tm max time for desired time range
#'
#' @return `data.frame`
#'
#' @export
cg_market_chart_range <- function(
  coin = "bitcoin",
  vs_currency = "usd",
  start_tm = Sys.time() - lubridate::days(30),
  end_tm = Sys.time()
) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(lubridate)
  require(purrr)

  stopifnot(inherits(coin, "character"))
  q_ls <- as.list(match.call())[-1]
  q_ls <- q_ls[setdiff(names(q_ls), "coin")]
  if (missing(vs_currency)) q_ls$vs_currency <- vs_currency
  if (missing(start_tm)) q_ls$start_tm <- start_tm
  if (missing(end_tm)) q_ls$end_tm <- end_tm
  names(q_ls) <- dplyr::case_when(
    names(q_ls) == "start_tm" ~ "from",
    names(q_ls) == "end_tm" ~ "to",
    TRUE ~ names(q_ls)
  )
  q_ls <- lapply(q_ls, function(x) {
    if (inherits(x, "name")) return(eval(x))
    x
  })
  for (i in c("from","to")) {
    if (is.character(q_ls[[i]])) {
      q_ls[[i]] <- as.numeric(lubridate::as_datetime(q_ls[[i]]))
    } else if (lubridate::is.Date(q_ls[[i]]) | inherits(q_ls[[i]], "POSIXct")) {
      q_ls[[i]] <- as.numeric(q_ls[[i]])
    }
  }

  api_url <- cg_api_url(file.path("coins", coin, "market_chart/range"))
  res <- httr::GET(api_url, query = q_ls)
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content), simplifyDataFrame = FALSE)
  setNames(dat, c("price","market_cap","volume")) |>
    purrr::imap(~ setNames(as.data.frame(.x), c("tm", .y))) |>
    purrr::reduce(dplyr::left_join, by = "tm") |>
    dplyr::mutate(tm = millisec_to_datetime(tm)) |>
    dplyr::as_tibble()
}
# cg_market_chart_range()


# "coins/bitcoin/history?date=30-12-2017"
#' Query for OHLC coin data
#'
#' @param coin coin to query for
#' @param date date of snapshot to query for
#'
#' @return data frame
#'
#' @export
cg_coin_snapshot <- function(
  coin = "bitcoin",
  date = format(Sys.Date(), "%d-%m-%Y")
) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(tidyr)
  require(purrr)

  stopifnot(inherits(coin, "character"))
  q_ls <- as.list(match.call())[-1]
  q_ls <- q_ls[setdiff(names(q_ls), "coin")]
  if (missing(date)) q_ls$date <- date
  q_ls <- lapply(q_ls, function(x) {
    if (inherits(x, "name")) return(eval(x))
    x
  })

  api_url <- cg_api_url(file.path("coins", coin, "history"))
  res <- httr::GET(api_url, query = q_ls)
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  ## process data
  df_community_data <- dat[["community_data"]] |>
    lapply(function(x) as.numeric(ifelse(is.null(x), NA_real_, x))) |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(!is.na(value))|>
    dplyr::mutate(ctgy = "community_data")
  df_developer_data <- dat[["developer_data"]] |>
    lapply(function(x) as.numeric(ifelse(is.null(x), NA_real_, x))) |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(!is.na(value))|>
    dplyr::mutate(ctgy = "developer_data")
  df_public_interest_stats <- dat[["public_interest_stats"]] |>
    lapply(function(x) as.numeric(ifelse(is.null(x), NA_real_, x))) |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(!is.na(value))|>
    dplyr::mutate(ctgy = "public_interest_stats")
  list(df_community_data, df_developer_data, df_public_interest_stats) |>
    dplyr::bind_rows() |>
    dplyr::transmute(coin = coin, date = lubridate::dmy(date), ctgy, name, value)
}


#' Query for OHLC coin data
#'
#' @param coin coin to query for
#' @param vs_currency quote currency
#' @param day_ct count of days of history to query for
#'
#' @return `data.frame`
#'
#' @details Candle's body:
#' * 1 - 2 days: 30 minutes
#' * 3 - 30 days: 4 hours
#' * 31 and before: 4 days
#'
#' @export
cg_ohlc <- function(
  coin = "bitcoin",
  vs_currency = "usd",
  day_ct = 30
) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(purrr)

  stopifnot(inherits(coin, "character"))
  q_ls <- as.list(match.call())[-1]
  q_ls <- q_ls[setdiff(names(q_ls), "coin")]
  if (missing(vs_currency)) q_ls$vs_currency <- vs_currency
  if (missing(day_ct)) q_ls$day_ct <- day_ct
  names(q_ls) <- dplyr::case_when(
    names(q_ls) == "day_ct" ~ "days",
    TRUE ~ names(q_ls)
  )
  q_ls <- lapply(q_ls, function(x) {
    if (inherits(x, "name")) return(eval(x))
    x
  })

  api_url <- cg_api_url(file.path("coins", coin, "ohlc"))
  res <- httr::GET(api_url, query = q_ls)
  if (res$status_code != 200) {
    return(jsonlite::fromJSON(rawToChar(res$content))$error)
  }
  dat <- jsonlite::fromJSON(rawToChar(res$content))
  as.data.frame(dat) |>
    setNames(c("tm","open","high","low","close")) |>
    dplyr::mutate(tm = millisec_to_datetime(tm)) |>
    dplyr::as_tibble()
}


#' Query for market summary data
#'
#' @param coin_ct the number of rows to return
#' @param vs_currency quote currency
#' @param category coin category to query for
#' @param order the desired row order of the return data frame
#' @param price_change_pct time ranges to include returns for
#'
#' @return `data.frame`
#'
#' @export
cg_coin_markets_summ <- function(
  coin_ct = 250,
  vs_currency = "usd",
  category,
  order = "market_cap_desc",
  price_change_pct = c("1h","24h","7d","14d","30d","200d","1y")
) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(lubridate)
  require(purrr)

  q_ls <- as.list(match.call())[-1]
  if (missing(coin_ct)) q_ls$coin_ct <- 100
  q_ls$per_page <- ifelse(q_ls$coin_ct < 250, q_ls$coin_ct, 250)
  q_ls <- q_ls[setdiff(names(q_ls), "coin_ct")]
  if (missing(vs_currency)) q_ls$vs_currency <- vs_currency
  # if (missing(category)) q_ls$category <- category
  # if (missing(order)) q_ls$order <- order
  # if (missing(page)) q_ls$page <- page
  if (missing(price_change_pct)) q_ls$price_change_pct <- price_change_pct
  q_ls$price_change_pct <- match.arg(
    arg = q_ls$price_change_pct,
    choices = c("1h","24h","7d","14d","30d","200d","1y"),
    several.ok = TRUE
  )
  if ("order" %in% names(q_ls)) {
    q_ls$order <- match.arg(
      arg = q_ls$order,
      choices = c(
        "gecko_desc","gecko_asc","market_cap_desc","market_cap_asc","volume_desc",
        "volume_asc","id_asc","id_desc"
      ),
      several.ok = FALSE
    )
  }
  q_ls$price_change_pct <- paste(
    q_ls$price_change_pct,
    collapse = "%2C"
  )
  names(q_ls) <- dplyr::case_when(
    names(q_ls) == "price_change_pct" ~ "price_change_percentage",
    TRUE ~ names(q_ls)
  )
  q_ls <- lapply(q_ls, function(x) {
    if (inherits(x, "name")) return(eval(x))
    x
  })

  cg_coin_markets_helper <- function(q_ls) {
    res <- httr::GET(cg_api_url("coins/markets"), query = q_ls)
    if (res$status_code != 200) {
      return(jsonlite::fromJSON(rawToChar(res$content))$error)
    }
    rawToChar(res$content) |>
      jsonlite::fromJSON(simplifyDataFrame = FALSE, simplifyMatrix = FALSE) |>
      purrr::map_dfr(~ {
        .x[setdiff(names(.x), "roi")] |>
          lapply(function(x) ifelse(is.null(x), NA, x))
      })
  }

  if (coin_ct > 250) {
    iter_ct <- ceiling(coin_ct / 250)
    dat_ls <- list()
    for (i in 1:iter_ct) {
      q_ls$page <- i
      dat_ls[[i]] <- cg_coin_markets_helper(q_ls)
    }
    dat <- dplyr::bind_rows(dat_ls) |> dplyr::slice_head(n = coin_ct)
  } else {
    dat <- cg_coin_markets_helper(q_ls)
  }
  dat <- dplyr::as_tibble(dat) |>
    dplyr::mutate_at(
      dplyr::vars(dplyr::ends_with("_date"), last_updated),
      lubridate::as_datetime
    )
  if (!missing(category)) return(dat |> dplyr::mutate(ctgy = category))
  dat
}

