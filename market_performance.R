market_performance <- function(asset_names, start_date, rolling_window = 30) {
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
  
  # Fetch asset prices and align data
  price_data <- lapply(asset_names, function(asset) {
    data <- suppressWarnings(getSymbols(asset, from = start_date, to = Sys.Date(), src = "yahoo", auto.assign = TRUE))
    Cl(get(data))
  })
  price_data <- do.call(merge, price_data)
  colnames(price_data) <- asset_names
  
  # Calculate daily returns for each asset
  daily_returns <- na.omit(Return.calculate(price_data, method = "log"))
  
  # Compute price weights each day
  price_weights <- price_data / rowSums(price_data, na.rm = TRUE)
  
  # Calculate price-weighted returns
  price_weighted_returns <- rowSums(price_weights[-1, ] * daily_returns)
  price_weighted_returns <- xts(price_weighted_returns, order.by = index(daily_returns))
  colnames(price_weighted_returns) <- "Price-Weighted Portfolio"
  
  # Plot performance summary
  tryCatch({
    charts.PerformanceSummary(price_weighted_returns, 
                              main = "Price-Weighted Portfolio Performance Summary",
                              legend.loc = "topright")
  }, error = function(e) {
    cat("Error in plotting performance summary:", conditionMessage(e), "\n")
  })
  
  # Calculate rolling realized volatility
  rolling_volatility <- rollapply(
    price_weighted_returns,
    width = rolling_window,
    FUN = function(x) sd(x, na.rm = TRUE) * sqrt(365),  # Annualized volatility
    fill = NA,
    align = "right"
  )
  
  # Convert rolling volatility to a data frame
  rolling_vol_df <- data.frame(
    Date = index(price_weighted_returns)[-(1:(rolling_window - 1))],  # Exclude NA periods
    RealizedVolatility = na.omit(coredata(rolling_volatility) * 100)  # Convert to percentage
  )
  
  # Ensure column names match for plotting
  colnames(rolling_vol_df) <- c("Date", "RealizedVolatility")
  
  # Plot rolling realized volatility
  rolling_vol_plot <- ggplot(rolling_vol_df, aes(x = Date, y = RealizedVolatility)) +
    geom_line(color = "blue", linewidth = 1) +
    ggtitle(paste("Rolling Realized Volatility (", rolling_window, "-Day)", sep = "")) +
    xlab("Date") +
    ylab("Volatility (Annualized, %)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Display the plot
  print(rolling_vol_plot)
  
  # Return the portfolio returns and rolling volatility for further use
  return(list(
    PortfolioReturns = price_weighted_returns,
    RollingVolatility = rolling_volatility
  ))
}
