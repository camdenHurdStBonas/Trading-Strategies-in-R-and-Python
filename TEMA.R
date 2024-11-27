tema_strategy <- function(asset_name, start_date, tema_window_values, risk_free_rate, transaction_cost) {
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
  library(plotly)
  library(TTR)
  
  # Fetch asset data
  suppressWarnings(getSymbols(asset_name, from = start_date, to = Sys.Date(), src = "yahoo"))
  
  if (!exists(asset_name)) {
    stop("Error: Asset data could not be retrieved. Please check the asset name or internet connection.")
  }
  
  stock_data <- na.omit(Cl(get(asset_name)))
  
  # Create full date index from start_date to the last available date
  full_index <- seq(from = as.Date(start_date), to = as.Date(Sys.Date()), by = "days")
  
  # Create a parameter grid for optimization
  param_grid <- expand.grid(TEMA_Window = tema_window_values)
  param_grid$SharpeRatio <- NA
  strategy_data <- vector("list", length = nrow(param_grid))
  
  # Optimization loop
  for (i in 1:nrow(param_grid)) {
    tema_window <- param_grid$TEMA_Window[i]
    
    # Calculate TEMA manually
    ema1 <- EMA(stock_data, n = tema_window)
    ema2 <- EMA(ema1, n = tema_window)
    ema3 <- EMA(ema2, n = tema_window)
    tema_values <- 3 * (ema1 - ema2) + ema3
    
    # Ensure TEMA has enough non-NA values
    if (all(is.na(tema_values)) || length(na.omit(tema_values)) < 2) {
      next
    }
    
    # Align stock_data and tema_values by removing NA values
    combined_data <- na.omit(merge(stock_data, tema_values))
    stock_data_aligned <- combined_data[, 1]
    tema_values_aligned <- combined_data[, 2]
    
    # Generate buy and sell signals
    signal <- numeric(length(stock_data_aligned))
    signal[1] <- 0  # Start with no position
    
    for (j in 2:length(stock_data_aligned)) {
      if (stock_data_aligned[j - 1] < tema_values_aligned[j - 1] && stock_data_aligned[j] > tema_values_aligned[j]) {
        # Buy signal when the price crosses above TEMA from below
        signal[j] <- 1
      } else if (stock_data_aligned[j - 1] > tema_values_aligned[j - 1] && stock_data_aligned[j] < tema_values_aligned[j]) {
        # Sell signal when the price crosses below TEMA from above
        signal[j] <- -1
      } else {
        signal[j] <- 0
      }
    }
    
    # Lag the signal to apply it the next day
    signal <- Lag(signal, k = 1)  # Move the signal one day forward
    signal[is.na(signal)] <- 0  # Replace NA values with 0 (no action)
    
    # Track position (1 for holding the asset, 0 for holding cash)
    position <- numeric(length(signal))
    position[1] <- 0  # Start with no position (all cash)
    for (j in 2:length(signal)) {
      if (signal[j] == 1) {
        position[j] <- 1  # Buy
      } else if (signal[j] == -1) {
        position[j] <- 0  # Sell
      } else {
        position[j] <- position[j - 1]  # Hold the same position
      }
    }
    
    # Calculate returns: account for transaction costs and risk-free rate
    daily_returns <- dailyReturn(stock_data_aligned)
    strategy_returns <- numeric(length(position))
    
    for (j in 2:length(position)) {
      if (position[j] == 1 && position[j - 1] == 0) {
        # Buy at today's close and hold until the next day's close (apply transaction cost)
        strategy_returns[j] <- daily_returns[j] - transaction_cost
      } else if (position[j] == 0 && position[j - 1] == 1) {
        # Sell at today's close (transaction cost applies)
        strategy_returns[j] <- -transaction_cost
      } else if (position[j] == 1) {
        # Hold the position from today's close to the next day's close
        strategy_returns[j] <- daily_returns[j]
      } else {
        # Hold cash at risk-free rate
        strategy_returns[j] <- risk_free_rate / 365
      }
    }
    
    # Convert to time series format using xts
    strategy_returns_xts <- xts(strategy_returns, order.by = index(stock_data_aligned))
    signal_xts <- xts(signal, order.by = index(stock_data_aligned))
    
    # Align strategy_returns and signal with full date index starting from start_date
    strategy_returns_xts <- merge(xts(, full_index), strategy_returns_xts, fill = 0)
    signal_xts <- merge(xts(, full_index), signal_xts, fill = 0)
    
    # Calculate Sharpe Ratio
    excess_returns <- log(strategy_returns_xts+1) - (risk_free_rate / 365)
    sharpe_ratio <- ifelse(any(!is.na(excess_returns)), 
                           (mean(excess_returns,na.rm=TRUE) - risk_free_rate/365 )/sd(excess_returns,na.rm=TRUE) * sqrt(365), 
                           NA)
    
    # Store result only if valid Sharpe Ratio is found
    if (!is.na(sharpe_ratio)) {
      param_grid$SharpeRatio[i] <- as.numeric(sharpe_ratio)
      strategy_data[[i]] <- list(returns = strategy_returns_xts, signal = signal_xts)
    }
  }
  
  # Remove rows with NA Sharpe Ratios from param_grid and strategy_data
  valid_indices <- which(!is.na(param_grid$SharpeRatio))
  param_grid <- param_grid[valid_indices, ]
  strategy_data <- strategy_data[valid_indices]
  
  # Find the best parameter combination
  if (nrow(param_grid) == 0) {
    stop("Error: No valid Sharpe Ratio found. Please check your data or parameter range.")
  }
  
  best_index <- which.max(param_grid$SharpeRatio)
  cat("Best TEMA Window:", param_grid$TEMA_Window[best_index], "\n")
  cat("Best Sharpe Ratio:", param_grid$SharpeRatio[best_index], "\n")
  
  # Retrieve the best TEMA window
  best_tema_window <- param_grid$TEMA_Window[best_index]
  
  # Retrieve the best strategy returns and signal for plotting and analysis
  best_strategy <- strategy_data[[best_index]]
  
  # Plot asset price, TEMA, and Buy/Sell signals
  buy_signals <- index(best_strategy$signal)[which(best_strategy$signal == 1)]
  sell_signals <- index(best_strategy$signal)[which(best_strategy$signal == -1)]
  
  # Filter only the dates that exist in stock_data_aligned
  buy_signals <- buy_signals[buy_signals %in% index(stock_data_aligned)]
  sell_signals <- sell_signals[sell_signals %in% index(stock_data_aligned)]
  
  # Prepare data for plotting
  stock_df <- data.frame(
    Date = index(stock_data_aligned),
    Price = as.numeric(stock_data_aligned),
    TEMA = as.numeric(tema_values_aligned)
  )
  
  buy_df <- data.frame(Date = buy_signals, Price = as.numeric(stock_data_aligned[buy_signals]))
  sell_df <- data.frame(Date = sell_signals, Price = as.numeric(stock_data_aligned[sell_signals]))
  
  # Plot with ggplot2
  p <- ggplot(stock_df, aes(x = Date)) +
    geom_line(aes(y = Price), color = "blue", linewidth = 0.8) +
    geom_line(aes(y = TEMA), color = "red", linetype = "dashed", linewidth = 0.6) +
    geom_point(data = buy_df, aes(x = Date, y = Price), color = "green", size = 2, shape = 17) +
    geom_point(data = sell_df, aes(x = Date, y = Price), color = "red", size = 2, shape = 17) +
    ggtitle(paste(asset_name, "Price with TEMA Buy/Sell Signals")) +
    xlab("Date") + ylab("Price") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to interactive plotly chart
  interactive_plot <- ggplotly(p)
  print(interactive_plot)
  
  # Plot performance comparison between Buy and Hold and TEMA Strategy
  if (!is.null(best_strategy$returns)) {
    tryCatch({
      comparison_returns <- cbind(dailyReturn(stock_data), best_strategy$returns)
      colnames(comparison_returns) <- c("Buy and Hold", "TEMA Strategy")
      charts.PerformanceSummary(
        comparison_returns, 
        main = paste("TEMA Strategy vs. Buy and Hold on", asset_name),
        legend.loc = "topleft"
      )
    }, error = function(e) {
      cat("Error during plotting performance summary:", conditionMessage(e), "\n")
    })
  } else {
    cat("Warning: Best strategy returns are not available for plotting.\n")
  }
  
  return(list(BestTEMAWindow = best_tema_window, BestSharpeRatio = param_grid$SharpeRatio[best_index], returns = best_strategy$returns, signal = best_strategy$signal))
}
