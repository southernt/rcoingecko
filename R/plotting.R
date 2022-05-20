
#' Query for stablecoin data and plot pie chart of market dominance
#'
#' @return ggplot
#'
#' @export
gg_stablecoins_pie <- function() {
  require(dplyr)
  require(ggplot2)

  cg_coin_markets_summ(category = "stablecoins") |>
    # filter(market_cap > 1E9) |>
    dplyr::mutate(
      name = case_when(
        market_cap < 1E9 ~ "Coins w/ <$1B Market Cap", # c("usdt","udsc","busd","dai")
        TRUE ~ name
      )
    ) |>
    dplyr::group_by(name) |>
    dplyr::summarise(market_cap = sum(market_cap, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      market_cap_prop = market_cap / sum(market_cap),
      name = reorder(factor(name), desc(market_cap))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = "", y = market_cap_prop, fill = name)) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_viridis_d(direction = -1, guide = ggplot2::guide_legend()) +
    ggplot2::labs(
      title = "Stablecoin Market Cap Dominance",
      fill = "Coin"
      ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "top")
}

#' Query for stablecoin data and plot pie chart of market dominance
#'
#' @return ggplot
#'
#' @export
gg_crypto_platform_prop <- function() {
  require(dplyr)
  require(stringr)
  require(ggplot2)

  cg_categories() |>
    dplyr::filter(stringr::str_detect(id, "ecosystem"), market_cap > 2E9) |>
    dplyr::mutate(
      market_cap_prop = market_cap / sum(market_cap),
      name = factor(substr(name, 1, nchar(name) - 10L)),
      name = reorder(name, market_cap)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = name, y = market_cap_prop, fill = market_cap_prop)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_viridis_c(guide = "none") +
    ggplot2::labs(
      x = "", y = "Proportion (%)",
      title = "Blockchain Ecosystem Market Cap Proportion"
      ) +
    ggplot2::theme_minimal()
}
