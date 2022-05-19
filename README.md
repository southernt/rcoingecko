rcoingecko <img src="man/figures/logo.png" align="right" height="139" />
========

R Package for Querying Cryptocurrency Data from the [CoinGecko API](https://www.coingecko.com/api/documentations/v3)

--------

## Existing Packages

- [CoinGeckoR](https://github.com/SamBuckberry/CoinGeckoR)
- [geckor](https://github.com/next-game-solutions/geckor)

While these packages provide well-made functions & good coverage for the 
available endpoints, they inherit some of the limitations when querying for 
high-granularity historical data (e.g. 5 minute OHLC buckets).  

Example Limitations:  

- querying for historical granularity smaller than daily requires loops when 
  querying for large day counts  
- endpoints with pages require loops to pull larger amounts of data  
