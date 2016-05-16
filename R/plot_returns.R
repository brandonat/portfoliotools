## Plot returns

plot_returns <- function(rets) {
  
  ## Plot cumulative returns of an xts object with multiple stock returns
  
  x <- rets + 1
  x <- vapply(x, cumprod, numeric(nrow(x)))
  x <- data.frame(date = zoo::index(rets), x)
  x <- tidyr::gather(x, "tkr", "return", 2:ncol(x))
  
  ggplot(x) + 
    geom_line(aes(x = date, y = return, color = tkr)) +
    theme_minimal()
}