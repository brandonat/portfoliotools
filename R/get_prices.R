## Get price data

get_prices <- function(symbols, ...) {

  ## Default start date is 2007-01-01
  ## To change that, add an argument: from = "YYYY-MM-DD"
  
  ## Download OHLC data
  prices_env <- new.env()
  quantmod::getSymbols(symbols, env = prices_env,  ...)
  
  ## Get Adjsuted Close* price from environment with price data
  close_prices <- function(sym, envir) {
    out <- get(sym, envir = envir)
    out <- quantmod::Ad(out)
    names(out) <- sym
    out
  }
  x <- do.call(
    xts::cbind.xts, 
    lapply(symbols, close_prices, envir = prices_env)
  )
  na.omit(x)
}

daily_returns <- function(x) {
  
  out <- vapply(x, quantmod::dailyReturn, numeric(nrow(x)))
  xts::xts(out, order.by = zoo::index(x))
}



## * Adjusted close takes into account splits