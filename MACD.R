macd_strategy <- function(asset_name, start_date, risk_free_rate, transaction_cost, nFast_values, nSlow_values, nSig_values) {
  # Load necessary libraries
  library(quantmod)
  library(PerformanceAnalytics)
  library(zoo)
  library(ggplot2)
  library(plotly)
  
  # Fetch asset data
  suppressWarnings(getSymbols(asset_name, from = start_date, to = Sys.Date(), src = "yahoo"))
  
  if (!exists(asset_name)) {
    stop("Error: Asset data could not be retrieved. Please check the asset name or internet connection.")
  }
  
  stock_data <- na.omit(Cl(get(asset_name)))  # Closing prices
  stock_returns <- dailyReturn(stock_data)
  
  # Function to evaluate MACD with given parameters
  evaluate_macd <- function(nFast, nSlow, nSig) {
    # Calculate standard deviations for fast and slow periods
    fast_sd <- rollapply(stock_returns, width = nFast, FUN = mean, fill = NA, align = "right")
    slow_sd <- rollapply(stock_returns, width = nSlow, FUN = mean, fill = NA, align = "right")
    
    if (is.null(fast_sd) || is.null(slow_sd) || length(fast_sd) != length(slow_sd)) {
      return(list(returns = NA, SharpeRatio = NA, position = NA, signal = NA))
    }
    
    # Calculate the difference between the fast and slow standard deviations
    macd_line <- fast_sd - slow_sd
    
    # Calculate the signal line (EMA of the macd_line)
    signal_line <-rollapply(stock_returns, width = nSig, FUN = mean, fill = NA, align = "right")
    
    macd_values <- list(macd= macd_line, signal= signal_line)
    
    # Generate buy (1) and sell (0) signals based on MACD crossing above/below the signal line
    signal <- ifelse(macd_values$macd > macd_values$signal, 1, 0)
    
    # Lag the signal by 1 day to reflect next-day execution
    signal <- Lag(signal, k = 1)
    signal[is.na(signal)] <- 0  # Replace NA values with 0 (no action)
    
    # Track position (1 for holding the asset, 0 for holding cash)
    position <- numeric(length(signal))
    position[1] <- 0  # Start with no position (all cash)
    for (i in 2:length(signal)) {
      if (signal[i] == 1) {
        position[i] <- 1  # Buy
      } else if (signal[i] == 0) {
        position[i] <- 0  # Sell
      } else {
        position[i] <- position[i - 1]  # Hold the same position
      }
    }
    
    # Calculate returns: account for transaction costs and risk-free rate
    strategy_returns <- numeric(length(position))
    for (i in 2:length(position)) {
      if (position[i] == 1 && position[i - 1] == 0) {
        # Buy at today's close and hold until the next day's close (apply transaction cost)
        strategy_returns[i] <- stock_returns[i] - transaction_cost
      } else if (position[i] == 0 && position[i - 1] == 1) {
        # Sell at today's close (transaction cost applies)
        strategy_returns[i] <- -transaction_cost
      } else if (position[i] == 1) {
        # Hold the position from today's close to the next day's close
        strategy_returns[i] <- stock_returns[i]
      } else {
        # Hold cash at risk-free rate
        strategy_returns[i] <- risk_free_rate / 365
      }
    }
    
    # Convert to time series format using xts
    strategy_returns_xts <- xts(strategy_returns, order.by = index(stock_data))
    
    # Calculate Sharpe Ratio
    excess_returns <- log(strategy_returns_xts+1) - (risk_free_rate / 365)
    sharpe_ratio <- ifelse(any(!is.na(excess_returns)), 
                           (mean(excess_returns,na.rm=TRUE)-risk_free_rate/365)/sd(excess_returns,na.rm=TRUE) * sqrt(365), 
                           NA)
    
    return(list(returns = strategy_returns_xts, SharpeRatio = sharpe_ratio, position = position, signal = signal))
  }
  
  # Generate parameter combinations
  results <- expand.grid(Fast = nFast_values, Slow = nSlow_values, Signal = nSig_values)
  results$SharpeRatio <- NA
  strategy_data <- vector("list", length = nrow(results))  # Initialize strategy data list
  
  # Optimize parameters
  for (i in 1:nrow(results)) {
    # Ensure nFast < nSlow for valid MACD settings
    if (results$Fast[i] < results$Slow[i]) {
      res <- evaluate_macd(results$Fast[i], results$Slow[i], results$Signal[i])
      strategy_data[[i]] <- res  # Save strategy data
      results$SharpeRatio[i] <- res$SharpeRatio
    }
  }
  
  # Ensure SharpeRatio is numeric
  results$SharpeRatio <- as.numeric(results$SharpeRatio)
  results$SharpeRatio[is.na(results$SharpeRatio)] <- min(results$SharpeRatio, na.rm = TRUE, default = -1)
  
  # Identify the best parameters
  best_index <- which.max(results$SharpeRatio)
  
  # Check if there is a valid best index
  if (length(best_index) == 0 || is.na(best_index)) {
    stop("Error: No valid Sharpe Ratio found. Please check your data or parameter range.")
  }
  
  # Retrieve the best parameters
  best_result <- results[best_index, ]
  best_nFast <- best_result$Fast
  best_nSlow <- best_result$Slow
  best_nSig <- best_result$Signal
  
  # Display the best parameters
  cat("Best Fast EMA:", best_nFast, "\n")
  cat("Best Slow EMA:", best_nSlow, "\n")
  cat("Best Signal EMA:", best_nSig, "\n")
  cat("Best Sharpe Ratio:", results$SharpeRatio[best_index], "\n")
  
  # Retrieve the best strategy data
  best_strategy <- strategy_data[[best_index]]
  
  # Plot MACD, Price Data, Buy and Sell Signals using ggplot2 and plotly
  tryCatch({
    macd_values <- MACD(stock_data, nFast = best_nFast, nSlow = best_nSlow, nSig = best_nSig, maType = "EMA")
    buy_signals <- index(stock_data)[which(diff(best_strategy$signal) == 1)]
    sell_signals <- index(stock_data)[which(diff(best_strategy$signal) == -1)]
    
    # Prepare data for plotting
    stock_df <- data.frame(
      Date = index(stock_data),
      Price = as.numeric(stock_data),
      MACD = as.numeric(macd_values$macd),
      Signal = as.numeric(macd_values$signal)
    )
    
    buy_df <- data.frame(Date = buy_signals, Price = as.numeric(stock_data[buy_signals]))
    sell_df <- data.frame(Date = sell_signals, Price = as.numeric(stock_data[sell_signals]))
    
    # Plot with ggplot2
    p <- ggplot(stock_df, aes(x = Date)) +
      geom_line(aes(y = Price), color = "blue", linewidth = 0.8) +
      geom_point(data = buy_df, aes(x = Date, y = Price), color = "green", size = 2, shape = 17) +
      geom_point(data = sell_df, aes(x = Date, y = Price), color = "red", size = 2, shape = 17) +
      ggtitle(paste(asset_name, "Closing Price with MACD Buy/Sell Signals")) +
      xlab("Date") + ylab("Price") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Convert ggplot to interactive plotly chart
    interactive_plot <- ggplotly(p)
    print(interactive_plot)
    
    # Plot MACD and Signal line
    p_macd <- ggplot(stock_df, aes(x = Date)) +
      geom_line(aes(y = MACD), color = "purple", linewidth = 0.8) +
      geom_line(aes(y = Signal), color = "orange", linewidth = 0.8) +
      ggtitle(paste(asset_name, "MACD and Signal Line")) +
      xlab("Date") + ylab("Value") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Convert ggplot to interactive plotly chart
    interactive_macd_plot <- ggplotly(p_macd)
    print(interactive_macd_plot)
    
  }, error = function(e) {
    cat("Error during plotting MACD and signals:", conditionMessage(e), "\n")
  })
  
  # Plot performance summary
  if (!is.null(best_strategy$returns)) {
    tryCatch({
      comparison_returns <- cbind(dailyReturn(stock_data), best_strategy$returns)
      colnames(comparison_returns) <- c("Buy and Hold", "MACD Strategy")
      charts.PerformanceSummary(
        comparison_returns, 
        main = paste("MACD Strategy vs. Buy and Hold on", asset_name),
        legend.loc = "topleft"
      )
    }, error = function(e) {
      cat("Error during plotting performance summary:", conditionMessage(e), "\n")
    })
  } else {
    cat("Warning: Best strategy returns are not available for plotting.\n")
  }
  
  # Modify macd$signal to represent buy (1), sell (-1), and hold (0)
  macd_signal <- best_strategy$signal
  
  # Logic to generate buy, sell, and hold signals
  # - If the signal was 0, we keep it at 0.
  # - If there is a positive signal, we generate a "buy" signal (1).
  # - If a position is closed (i.e., transitioning from hold to no position), generate a "sell" signal (-1).
  macd_modified_signal <- numeric(length(macd_signal))
  for (i in 2:length(macd_signal)) {
    if (macd_signal[i] == 1 && macd_signal[i - 1] == 0) {
      macd_modified_signal[i] <- 1  # Buy signal when transitioning from cash to holding
    } else if (macd_signal[i] == 0 && macd_signal[i - 1] == 1) {
      macd_modified_signal[i] <- -1  # Sell signal when transitioning from holding to cash
    } else {
      macd_modified_signal[i] <- 0  # Hold if no significant transition
    }
  }
  best_strategy$signal <- xts(macd_modified_signal, order.by = index(macd_signal))
  
  return(list(best_strategy=best_strategy,nFast=best_nFast,nSlow=best_nSlow,nSig=best_nSig))
}
