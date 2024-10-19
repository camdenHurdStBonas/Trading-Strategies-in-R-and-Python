# Load necessary libraries
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(ggplot2)
library(parallel)
library(plotly)

# Define the function
macd_strategy <- function(asset_name, start_date, risk_free_rate, transaction_cost, borrowing_cost, nFast_values, nSlow_values, nSig_values, short) {
  
  suppressWarnings({
    # Download stock data
    getSymbols(asset_name, from = start_date, to = Sys.Date(), src = "yahoo")
  })
  
  # Get closing prices using the asset name
  stock_data <- na.omit(Cl(get(asset_name)))  # Closing price of stock
  stock_returns <- dailyReturn(stock_data)
  
  if(short == -1) {
    short_text <- "Buy and Short"
  } else if(short == 1) {
    short_text <- "Only Short"
  } else {
    short_text <- "Only Buy"
  }
  
  # Function to evaluate MACD with given parameters
  evaluate_macd <- function(nFast, nSlow, nSig, rf_rate, transaction_cost, borrow_cost) {
    # Calculate MACD
    macd_values <- MACD(stock_data, nFast = nFast, nSlow = nSlow, nSig = nSig, maType = "EMA")
    
    # Generate trading signals based on the 'short' variable
    if (short == -1) {
      signal <- Lag(ifelse(macd_values$macd > macd_values$signal, 1, -1))
    } else if (short == 1) {
      signal <- Lag(ifelse(macd_values$macd < macd_values$signal, -1, 0))
    } else {
      signal <- Lag(ifelse(macd_values$macd > macd_values$signal, 1, 0))
    }
    
    # Handle NA values and fill forward
    signal[is.na(signal)] <- 0
    signal <- na.locf(signal)
    
    # Detect changes in position to account for transaction costs
    signal_shift <- Lag(signal)
    signal_shift[is.na(signal_shift)] <- 0
    trade_occurred <- signal != signal_shift
    
    # Calculate returns from the strategy based on signals
    strategy_returns <- ifelse(signal == 1, stock_returns, 
                               ifelse(signal == -1, -stock_returns - borrow_cost / 252, rf_rate / 252))
    strategy_returns <- strategy_returns - transaction_cost * trade_occurred
    
    # Calculate Sharpe Ratio for the strategy (subtract risk-free rate)
    excess_returns <- log(1 + strategy_returns) - (rf_rate / 252)
    sharpe_ratio <- SharpeRatio.annualized(excess_returns, scale = 252)
    
    return(sharpe_ratio)
  }
  
  # Data frame to store results
  results <- expand.grid(nFast = nFast_values, nSlow = nSlow_values, nSig = nSig_values)
  results$SharpeRatio <- NA
  
  # Remove invalid parameter combinations
  results <- subset(results, nFast < nSlow & nSig <= nFast)
  
  # Parallel processing to evaluate all parameter combinations
  results$SharpeRatio <- mclapply(1:nrow(results), function(i) {
    nFast <- results$nFast[i]
    nSlow <- results$nSlow[i]
    nSig <- results$nSig[i]
    evaluate_macd(nFast, nSlow, nSig, risk_free_rate, transaction_cost, borrowing_cost)
  }, mc.cores = detectCores() - 1)  # Use available cores minus one
  
  # Ensure SharpeRatio is numeric
  results$SharpeRatio <- as.numeric(results$SharpeRatio)
  
  # 3D Visualization of MACD Parameter Combinations
  three_dim_plot <- plot_ly(data = results, 
                            x = ~nFast, 
                            y = ~nSlow, 
                            z = ~SharpeRatio, 
                            type = 'scatter3d', 
                            mode = 'markers', 
                            marker = list(size = 3), 
                            color = ~SharpeRatio,  # Color based on Sharpe Ratio
                            colors = colorRamp(c("red", "green"))) %>%
    layout(title = paste(short_text, "MACD Strategy on", asset_name, "3D Plot by Sharpe Ratio by nFast and nSlow"),
           scene = list(xaxis = list(title = "nFast"),
                        yaxis = list(title = "nSlow"),
                        zaxis = list(title = "Sharpe Ratio")),
           legend = list(title = list(text = "Sharpe Ratio")))
  
  print(three_dim_plot)
  
  # Identify the best parameters
  best_result <- results[which.max(results$SharpeRatio), ]
  cat("Best Parameters:\n")
  print(best_result)
  
  # Best parameters
  best_nFast <- best_result$nFast
  best_nSlow <- best_result$nSlow
  best_nSig <- best_result$nSig
  
  # Calculate MACD using best parameters
  macd_values <- MACD(stock_data, nFast = best_nFast, nSlow = best_nSlow, nSig = best_nSig, maType = "EMA")
  
  # Create a trading strategy based on MACD crossovers
  if (short == -1) {
    signal <- Lag(ifelse(macd_values$macd > macd_values$signal, 1, -1))
  } else if (short == 1) {
    signal <- Lag(ifelse(macd_values$macd < macd_values$signal, -1, 0))
  } else {
    signal <- Lag(ifelse(macd_values$macd > macd_values$signal, 1, 0))
  }
  
  signal[is.na(signal)] <- 0
  signal <- na.locf(signal)
  
  # Add transaction cost for signal changes
  signal_shift <- Lag(signal)
  signal_shift[is.na(signal_shift)] <- 0
  trade_occurred <- signal != signal_shift
  
  # Calculate returns from the strategy
  strategy_returns <- ifelse(signal == 1, stock_returns, 
                             ifelse(signal == -1, -stock_returns - borrowing_cost / 252, risk_free_rate / 252))
  strategy_returns <- strategy_returns - transaction_cost * trade_occurred
  
  # Buy-and-hold strategy returns
  buy_hold_returns <- stock_returns  # Buy and hold returns are just holding stock
  
  # Plot MACD and Signal with the Price
  chartSeries(stock_data, name = paste(asset_name,"Closing Price"), TA = paste("addMACD(", best_nFast, ", ", best_nSlow, ", ", best_nSig, ")"))
  
  # Compare MACD Strategy vs Buy-and-Hold Strategy
  comparison_returns <- cbind(buy_hold_returns, strategy_returns)
  colnames(comparison_returns) <- c("Buy and Hold", "MACD Strategy")
  
  # Plot performance of both strategies
  charts.PerformanceSummary(comparison_returns, 
                            main = paste(short_text, "MACD Strategy vs. Buy and Hold"), 
                            legend.loc = "topleft", 
                            colorset = c("black", "blue"))
  
  # Output basic performance metrics for both strategies
  annualized_returns <- table.AnnualizedReturns(comparison_returns, scale = 252)
  
  # Calculate the annualized standard deviation for both strategies
  stddev_bh <- StdDev.annualized(comparison_returns[, "Buy and Hold"], scale = 252)
  stddev_macd <- StdDev.annualized(comparison_returns[, "MACD Strategy"], scale = 252)
  
  # Subtract risk-free rate for Sharpe Ratio calculation
  excess_buy_hold <- log(1 + comparison_returns[, "Buy and Hold"]) - (risk_free_rate / 252)
  excess_macd <- log(1 + comparison_returns[, "MACD Strategy"]) - (risk_free_rate / 252)
  
  # Calculate Sharpe Ratios
  sharpe_bh <- SharpeRatio.annualized(excess_buy_hold, scale = 252)
  sharpe_macd <- SharpeRatio.annualized(excess_macd, scale = 252)
  
  # Calculate Max Drawdown for each strategy
  max_drawdown_bh <- maxDrawdown(comparison_returns[,"Buy and Hold"])
  max_drawdown_macd <- maxDrawdown(comparison_returns[,"MACD Strategy"])
  
  # Combine all metrics into a data frame
  performance_metrics <- data.frame(
    Metric = c("Annualized Return", "Annualized Standard Deviation", "Annualized Sharpe Ratio", "Max Drawdown"),
    Buy_and_Hold = c(annualized_returns[1, "Buy and Hold"], stddev_bh, sharpe_bh, max_drawdown_bh),
    MACD_Strategy = c(annualized_returns[1, "MACD Strategy"], stddev_macd, sharpe_macd, max_drawdown_macd)
  )
  
  print(performance_metrics)
  
  # Return the performance metrics and store key variables in the environment
  assign("best_nFast", best_nFast)
  assign("best_nSlow", best_nSlow)
  assign("best_nSig", best_nSig)
  assign("comparison_returns", comparison_returns)
  
  return(performance_metrics)
}

# Example of calling the function with parameters
# Replace with your desired inputs
# macd_strategy("AAPL", "2018-01-01", 0.03, 0.05, 0.005, seq(8,12,1), seq(20,30,1), seq(7,9,1), short = -1)
