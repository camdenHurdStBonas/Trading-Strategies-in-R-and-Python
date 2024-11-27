mvcd_strategy <- function(asset_name, start_date, risk_free_rate, transaction_cost, nFast_values, nSlow_values, nSig_values) {
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
  
  # Function to evaluate MVCD with given parameters
  evaluate_mvcd <- function(nFast, nSlow, nSig) {
    # Calculate standard deviations for fast and slow periods
    fast_sd <- rollapply(stock_returns, width = nFast, FUN = sd, fill = NA, align = "right")
    slow_sd <- rollapply(stock_returns, width = nSlow, FUN = sd, fill = NA, align = "right")
    
    if (length(fast_sd) != length(slow_sd)) {
      return(list(returns = NA, SharpeRatio = NA, position = NA, signal = NA))
    }
    
    # Calculate the difference between fast and slow standard deviations
    mvcd_line <- fast_sd - slow_sd
    
    # Calculate the signal line (rolling average of the MVCD line)
    signal_line <- rollapply(mvcd_line, width = nSig, FUN = mean, fill = NA, align = "right")
    
    # Generate buy (1) and sell (-1) signals based on mvcd_line crossing signal_line
    signal <- rep(0, length(stock_data))
    
    for (i in 2:length(stock_data)) {
      if (!is.na(mvcd_line[i]) && !is.na(signal_line[i]) && !is.na(mvcd_line[i - 1]) && !is.na(signal_line[i - 1])) {
        if (mvcd_line[i - 1] < signal_line[i - 1] && mvcd_line[i] >= signal_line[i]) {
          signal[i] <- 1  # Buy signal
        } else if (mvcd_line[i - 1] > signal_line[i - 1] && mvcd_line[i] <= signal_line[i]) {
          signal[i] <- -1  # Sell signal
        }
      }
    }
    
    # Lag the signal by 1 day to reflect next-day execution
    signal <- Lag(signal, k = 1)
    signal[is.na(signal)] <- 0  # Replace NA values with 0 (no action)
    
    # Convert signal to time series
    signal_xts <- xts(signal, order.by = index(stock_data))
    
    # Track position (1 for holding the asset, 0 for holding cash)
    position <- numeric(length(signal))
    for (i in 2:length(signal)) {
      if (signal[i] == 1) {
        position[i] <- 1  # Buy
      } else if (signal[i] == -1) {
        position[i] <- 0  # Sell
      } else {
        position[i] <- position[i - 1]  # Hold the same position
      }
    }
    
    # Convert position to time series
    position_xts <- xts(position, order.by = index(stock_data))
    
    # Calculate strategy returns: account for transaction costs and risk-free rate
    strategy_returns <- numeric(length(position))
    for (i in 2:length(position)) {
      if (position[i] == 1 && position[i - 1] == 0) {
        strategy_returns[i] <- stock_returns[i] - transaction_cost
      } else if (position[i] == 0 && position[i - 1] == 1) {
        strategy_returns[i] <- -transaction_cost
      } else if (position[i] == 1) {
        strategy_returns[i] <- stock_returns[i]
      } else {
        strategy_returns[i] <- risk_free_rate / 365
      }
    }
    
    # Convert to time series format using xts
    strategy_returns_xts <- xts(strategy_returns, order.by = index(stock_data))
    
    # Calculate Sharpe Ratio
    excess_returns <- strategy_returns_xts - (risk_free_rate / 365)
    sharpe_ratio <- ifelse(any(!is.na(excess_returns)), 
                           mean(excess_returns, na.rm = TRUE) / sd(excess_returns, na.rm = TRUE) * sqrt(252), 
                           NA)
    
    return(list(returns = strategy_returns_xts, SharpeRatio = sharpe_ratio, position = position, signal = signal))
  }
  
  # Generate parameter combinations
  results <- expand.grid(Fast = nFast_values, Slow = nSlow_values, Signal = nSig_values)
  results$SharpeRatio <- NA
  strategy_data <- vector("list", length = nrow(results))
  
  # Optimize parameters
  for (i in 1:nrow(results)) {
    # Ensure nFast < nSlow for valid volatility MVCD settings
    if (results$Fast[i] < results$Slow[i]) {
      res <- evaluate_mvcd(results$Fast[i], results$Slow[i], results$Signal[i])
      strategy_data[[i]] <- res  # Save strategy data
      results$SharpeRatio[i] <- res$SharpeRatio
    }
  }
  
  # Ensure SharpeRatio is numeric
  results$SharpeRatio <- as.numeric(results$SharpeRatio)
  
  # Identify the best parameters
  best_index <- which.max(results$SharpeRatio)
  if (length(best_index) == 0 || is.na(best_index)) {
    stop("Error: No valid Sharpe Ratio found. Please check your data or parameter range.")
  }
  
  # Retrieve the best parameters
  best_result <- results[best_index, ]
  best_nFast <- best_result$Fast
  best_nSlow <- best_result$Slow
  best_nSig <- best_result$Signal
  
  # Display the best parameters
  cat("Best Fast SD Window:", best_nFast, "\n")
  cat("Best Slow SD Window:", best_nSlow, "\n")
  cat("Best Signal Line Window:", best_nSig, "\n")
  cat("Best Sharpe Ratio:", results$SharpeRatio[best_index], "\n")
  
  # Retrieve the best strategy data
  best_strategy <- strategy_data[[best_index]]
  best_strategy$signal <- xts(best_strategy$signal, order.by = index(stock_data))
  
  # Plot volatility mvcd, Price Data, Buy and Sell Signals using ggplot2 and plotly
  tryCatch({
    fast_sd <- rollapply(stock_returns, width = best_nFast, FUN = sd, fill = NA, align = "right")
    slow_sd <- rollapply(stock_returns, width = best_nSlow, FUN = sd, fill = NA, align = "right")
    mvcd_line <- fast_sd - slow_sd
    signal_line <- EMA(mvcd_line, n = best_nSig)
    
    buy_signals <- index(stock_data)[which(best_strategy$signal == 1)]
    sell_signals <- index(stock_data)[which(best_strategy$signal == -1)]
    
    # Prepare data for plotting
    stock_df <- data.frame(
      Date = index(stock_data),
      Price = as.numeric(stock_data),
      mvcdLine = as.numeric(mvcd_line),
      SignalLine = as.numeric(signal_line)
    )
    
    buy_df <- data.frame(Date = buy_signals, Price = as.numeric(stock_data[buy_signals]))
    sell_df <- data.frame(Date = sell_signals, Price = as.numeric(stock_data[sell_signals]))
    
    # Plot with ggplot2
    p <- ggplot(stock_df, aes(x = Date)) +
      geom_line(aes(y = Price), color = "blue", linewidth = 0.8) +
      geom_point(data = buy_df, aes(x = Date, y = Price), color = "green", size = 2, shape = 17) +
      geom_point(data = sell_df, aes(x = Date, y = Price), color = "red", size = 2, shape = 17) +
      ggtitle(paste(asset_name, "Closing Price with MVCD Buy/Sell Signals")) +
      xlab("Date") + ylab("Price") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Convert ggplot to interactive plotly chart
    interactive_plot <- ggplotly(p)
    print(interactive_plot)
    
    # Plot mvcd and Signal line
    p_mvcd <- ggplot(stock_df, aes(x = Date)) +
      geom_line(aes(y = mvcdLine), color = "purple", linewidth = 0.8) +
      geom_line(aes(y = SignalLine), color = "orange", linewidth = 0.8) +
      ggtitle(paste(asset_name, "MVCD Line and Signal Line")) +
      xlab("Date") + ylab("Value") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Convert ggplot to interactive plotly chart
    interactive_mvcd_plot <- ggplotly(p_mvcd)
    print(interactive_mvcd_plot)
    
  }, error = function(e) {
    cat("Error during plotting MVCD and signals:", conditionMessage(e), "\n")
  })
  
  # Plot performance summary
  if (!is.null(best_strategy$returns)) {
    tryCatch({
      comparison_returns <- cbind(dailyReturn(stock_data), best_strategy$returns)
      colnames(comparison_returns) <- c("Buy and Hold", "MVCD Strategy")
      charts.PerformanceSummary(
        comparison_returns, 
        main = paste("MVCD Strategy vs. Buy and Hold on", asset_name),
        legend.loc = "topleft"
      )
    }, error = function(e) {
      cat("Error during plotting performance summary:", conditionMessage(e), "\n")
    })
  } else {
    cat("Warning: Best strategy returns are not available for plotting.\n")
  }
  
  return(list(best_strategy=best_strategy,nFast=best_nFast,nSlow=best_nSlow,nSig=best_nSig))
}
