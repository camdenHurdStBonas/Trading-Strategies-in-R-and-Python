vwap_strategy <- function(asset_name, start_date, vwap_window_values, risk_free_rate, transaction_cost) {
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
  library(plotly)
  library(zoo)  # For rollapply
  
  # Fetch asset data
  suppressWarnings(getSymbols(asset_name, from = start_date, to = Sys.Date(), src = "yahoo"))
  
  if (!exists(asset_name)) {
    stop("Error: Asset data could not be retrieved. Please check the asset name or internet connection.")
  }
  
  stock_data <- na.omit(Cl(get(asset_name)))
  volume_data <- na.omit(Vo(get(asset_name)))
  high_data <- na.omit(Hi(get(asset_name)))
  low_data <- na.omit(Lo(get(asset_name)))
  
  # Create full date index from start_date to the last available date
  full_index <- seq(from = as.Date(start_date), to = as.Date(Sys.Date()), by = "days")
  
  # Create a parameter grid for optimization
  param_grid <- expand.grid(VWAP_Window = vwap_window_values)
  param_grid$SharpeRatio <- NA
  strategy_data <- vector("list", length = nrow(param_grid))
  
  # Optimization loop
  for (i in 1:nrow(param_grid)) {
    vwap_window <- param_grid$VWAP_Window[i]
    
    # Calculate the typical price
    typical_price <- (high_data + low_data + stock_data) / 3
    
    typical_price_volume <- typical_price * volume_data
    
    # Calculate rolling VWAP over the specified window
    rolling_cumulative_price_volume <- rollapply(typical_price_volume, vwap_window, sum, fill = NA, align = "right")
    rolling_cumulative_volume <- rollapply(volume_data, vwap_window, sum, fill = NA, align = "right")
    
    vwap_values <- rolling_cumulative_price_volume / rolling_cumulative_volume
    
    # Ensure VWAP has enough non-NA values
    if (all(is.na(vwap_values)) || length(na.omit(vwap_values)) < 2) {
      next
    }
    
    # Align stock_data and vwap_values by removing NA values
    combined_data <- na.omit(cbind(stock_data, vwap_values))
    stock_data_aligned <- combined_data[, 1]
    vwap_values_aligned <- combined_data[, 2]
    
    # Generate buy and sell signals
    signal <- rep(0, length(stock_data_aligned))
    in_position <- FALSE  # Track whether we are currently in a trade
    
    for (j in 2:length(stock_data_aligned)) {
      if (!in_position && stock_data_aligned[j - 1] < vwap_values_aligned[j - 1] && stock_data_aligned[j] > vwap_values_aligned[j]) {
        # Buy signal when the price crosses above VWAP from below and we are not in a position
        signal[j] <- 1
        in_position <- TRUE
      } else if (in_position && stock_data_aligned[j - 1] > vwap_values_aligned[j - 1] && stock_data_aligned[j] < vwap_values_aligned[j]) {
        # Sell signal when the price crosses below VWAP from above and we are in a position
        signal[j] <- -1
        in_position <- FALSE
      } else {
        signal[j] <- 0  # No action
      }
    }
    
    # Convert the signal to xts format and lag the signal to apply on the next trading day
    signal_xts <- xts(signal, order.by = index(stock_data_aligned))
    signal_xts <- Lag(signal_xts, k = 1)
    signal_xts[is.na(signal_xts)] <- 0  # Replace NA values with 0 (no action)
    
    # Track position (1 for holding the asset, 0 for holding cash)
    position <- numeric(length(signal))
    position[1] <- 0  # Start with no position (all cash)
    
    in_position <- FALSE  # Reinitialize to track the position correctly
    for (j in 2:length(signal)) {
      if (signal_xts[j] == 1 && !in_position) {
        # Enter position (Buy)
        position[j] <- 1
        in_position <- TRUE
      } else if (signal_xts[j] == -1 && in_position) {
        # Exit position (Sell)
        position[j] <- 0
        in_position <- FALSE
      } else {
        # Maintain the previous position
        position[j] <- position[j - 1]
      }
    }
    
    # Convert position to xts format
    position_xts <- xts(position, order.by = index(stock_data_aligned))
    
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
    signal_xts <- merge(xts(, full_index), signal_xts, fill = 0)
    position_xts <- merge(xts(, full_index), position_xts, fill = 0)
    strategy_returns_xts <- merge(xts(, full_index), strategy_returns_xts, fill = 0)
    
    # Calculate Sharpe Ratio
    excess_returns <- log(strategy_returns_xts + 1) - (risk_free_rate / 365)
    sharpe_ratio <- ifelse(any(!is.na(excess_returns)), 
                           (mean(excess_returns, na.rm = TRUE) - risk_free_rate / 365) / 
                             sd(excess_returns, na.rm = TRUE) * sqrt(365), 
                           NA)
    
    # Store result only if valid Sharpe Ratio is found
    if (!is.na(sharpe_ratio)) {
      param_grid$SharpeRatio[i] <- as.numeric(sharpe_ratio)
      strategy_data[[i]] <- list(returns = strategy_returns_xts, signal = signal_xts, position = position_xts)
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
  cat("Best VWAP Window:", param_grid$VWAP_Window[best_index], "\n")
  cat("Best Sharpe Ratio:", param_grid$SharpeRatio[best_index], "\n")
  
  # Retrieve the best VWAP window
  best_vwap_window <- param_grid$VWAP_Window[best_index]
  
  # Retrieve the best strategy returns, signal, and position for plotting and analysis
  best_strategy <- strategy_data[[best_index]]
  
  # Plot asset price, VWAP, and Buy/Sell signals
  vwap_values <- TTR::VWAP(price = stock_data, volume = volume_data, n = best_vwap_window)
  combined_data <- na.omit(cbind(stock_data, vwap_values))
  stock_data_aligned <- combined_data[, 1]
  vwap_values_aligned <- combined_data[, 2]
  
  buy_signals <- index(best_strategy$signal)[which(best_strategy$signal == 1)]
  sell_signals <- index(best_strategy$signal)[which(best_strategy$signal == -1)]
  
  # Prepare data for plotting
  stock_df <- data.frame(
    Date = index(stock_data_aligned),
    Price = as.numeric(stock_data_aligned),
    VWAP = as.numeric(vwap_values_aligned)
  )
  
  buy_df <- data.frame(Date = buy_signals, Price = as.numeric(stock_data_aligned[buy_signals]))
  sell_df <- data.frame(Date = sell_signals, Price = as.numeric(stock_data_aligned[sell_signals]))
  
  # Plot with ggplot2
  p <- ggplot(stock_df, aes(x = Date)) +
    geom_line(aes(y = Price), color = "blue", linewidth = 0.8) +
    geom_line(aes(y = VWAP), color = "red", linetype = "dashed", linewidth = 0.6) +
    geom_point(data = buy_df, aes(x = Date, y = Price), color = "green", size = 2, shape = 17) +
    geom_point(data = sell_df, aes(x = Date, y = Price), color = "red", size = 2, shape = 17) +
    ggtitle(paste(asset_name, "Price with VWAP Buy/Sell Signals")) +
    xlab("Date") + ylab("Price") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to interactive plotly chart
  interactive_plot <- ggplotly(p)
  print(interactive_plot)
  
  # Plot performance comparison between Buy and Hold and VWAP Strategy
  if (!is.null(best_strategy$returns)) {
    tryCatch({
      comparison_returns <- cbind(dailyReturn(stock_data), best_strategy$returns)
      colnames(comparison_returns) <- c("Buy and Hold", "VWAP Strategy")
      charts.PerformanceSummary(
        comparison_returns, 
        main = paste("VWAP Strategy vs. Buy and Hold on", asset_name),
        legend.loc = "topleft"
      )
    }, error = function(e) {
      cat("Error during plotting performance summary:", conditionMessage(e), "\n")
    })
  } else {
    cat("Warning: Best strategy returns are not available for plotting.\n")
  }
  
  return(list(BestVWAPWindow = best_vwap_window, BestSharpeRatio = param_grid$SharpeRatio[best_index], returns = best_strategy$returns, signal = best_strategy$signal, position = best_strategy$position))
}
