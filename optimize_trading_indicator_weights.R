print_serial_correlation <- function(stock_returns){
  # Calculate and print the serial correlation of price data and returns
  
  # Calculate lagged price data and returns
  lagged_returns <- stats::lag(stock_returns, k = 1)
  
  # Remove the NA values that result from lagging
  valid_returns <- na.omit(cbind(stock_returns, lagged_returns))
  
  # Calculate serial correlation for returns
  serial_corr_returns <- cor(valid_returns[, 1], valid_returns[, 2], use = "complete.obs")
  
  # Print serial correlation results
  cat("Serial Correlation of Returns:", serial_corr_returns, "\n")
}

get_optimal_weights <- function(aligned_strategy_returns){
  # Define the objective function for optimization
  objective_function <- function(weights) {
    # Normalize weights to sum to 1
    weights <- weights / sum(weights)
    
    # Combine strategy returns based on weights
    combined_returns <- rowSums(sweep(aligned_strategy_returns, 2, weights, FUN = "*"))
    
    # Calculate Sharpe Ratio
    excess_returns <- log(combined_returns+1) - (risk_free_rate / 365)
    sharpe_ratio <- (mean(excess_returns, na.rm = TRUE) - risk_free_rate/365) / sd(excess_returns, na.rm = TRUE) * sqrt(365)
    
    # Return negative Sharpe Ratio for minimization
    return(-sharpe_ratio)
  }
  
  # Number of strategies to optimize
  num_strategies <- ncol(aligned_strategy_returns)
  
  # Run Genetic Algorithm to optimize the weights
  ga_result <- ga(
    type = "real-valued", 
    fitness = function(w) -objective_function(w),  # GA maximizes fitness, so we minimize the objective
    lower = rep(0, num_strategies),  # Lower bounds for weights (no negative weights)
    upper = rep(1, num_strategies),  # Upper bounds for weights (between 0 and 1)
    popSize = 50,  # Population size
    maxiter = 100,  # Maximum iterations
    run = 50,  # Stop after 50 generations without improvement
    seed = 123,  # Seed for reproducibility
    monitor = NULL
  )
  
  # Extract the best weights
  optimal_weights <- ga_result@solution
  
  # Normalize to sum to 1
  optimal_weights <- optimal_weights / sum(optimal_weights)
  
  # Assign strategy names to weights for better readability
  colnames(optimal_weights) <- strategy_names
  
  cat("Optimal Weights:\n")
  print(optimal_weights)
  return(optimal_weights)
}

get_optimal_buy_sell_thershold <- function(optimal_weights,aligned_strategy_signals,aligned_stock_returns,buy_thresholds,sell_thresholds,transaction_cost,risk_free_rate){
  # Initialize variables to track the best results
  best_sharpe <- -Inf
  best_weighted_signal <- NULL
  best_position <- NULL
  best_strategy_returns <- NULL
  best_buy_threshold <- NULL
  best_sell_threshold <- NULL
  
  # Iterate over different threshold combinations
  for (buy_threshold in buy_thresholds) {
    for (sell_threshold in sell_thresholds) {
      # Generate combined strategy signals with optimal weights
      combined_signals <- rowSums(sweep(aligned_strategy_signals, 2, optimal_weights, FUN = "*"))
      combined_signals_xts <- xts(combined_signals, order.by = index(aligned_stock_returns))
      
      # Generate a final signal based on thresholds (-1 for sell, 1 for buy, 0 for hold)
      weighted_signal <- numeric(length(combined_signals))
      in_position <- FALSE
      for (i in 1:length(combined_signals)) {
        if (!in_position && combined_signals[i] > buy_threshold) {
          # Enter position (buy)
          weighted_signal[i] <- 1
          in_position <- TRUE
        } else if (in_position && combined_signals[i] < sell_threshold) {
          # Exit position (sell)
          weighted_signal[i] <- -1
          in_position <- FALSE
        } else {
          # Maintain current position
          weighted_signal[i] <- 0
        }
      }
      
      weighted_signal_xts <- xts(weighted_signal, order.by = index(aligned_stock_returns))
      
      # Track position based on weighted signals (1 for holding the asset, 0 for holding cash)
      position <- numeric(length(weighted_signal))
      for (i in 2:length(weighted_signal)) {
        if (weighted_signal[i] == 1) {
          position[i] <- 1  # Buy signal
        } else if (weighted_signal[i] == -1) {
          position[i] <- 0  # Sell signal
        } else {
          position[i] <- position[i - 1]  # Hold the previous position
        }
      }
      
      position_xts <- xts(position, order.by = index(aligned_stock_returns))
      
      # Calculate new strategy returns based on positions
      strategy_returns <- numeric(length(position))
      for (i in 2:length(position)) {
        if (position[i] == 1 && position[i - 1] == 0) {
          # Buy at today's close and hold until the next day's close (apply transaction cost)
          strategy_returns[i] <- aligned_stock_returns[i] - transaction_cost
        } else if (position[i] == 0 && position[i - 1] == 1) {
          # Sell at today's close (transaction cost applies)
          strategy_returns[i] <- -transaction_cost
        } else if (position[i] == 1) {
          # Hold the position from today's close to the next day's close
          strategy_returns[i] <- aligned_stock_returns[i]
        } else {
          # Hold cash at risk-free rate
          strategy_returns[i] <- risk_free_rate / 365
        }
      }
      
      strategy_returns_xts <- xts(strategy_returns, order.by = index(aligned_stock_returns))
      
      # Calculate Sharpe Ratio for the current thresholds
      excess_strategy_returns <- log(strategy_returns_xts+1) - (risk_free_rate / 365)
      sharpe_ratio <- (mean(excess_strategy_returns, na.rm = TRUE) - risk_free_rate/365) / sd(excess_strategy_returns, na.rm = TRUE) * sqrt(365)
      
      # Update the best results if the current Sharpe Ratio is better
      if (sharpe_ratio > best_sharpe) {
        best_sharpe <- sharpe_ratio
        best_weighted_signal <- weighted_signal_xts
        best_position <- position_xts
        best_strategy_returns <- strategy_returns_xts
        best_buy_threshold <- buy_threshold
        best_sell_threshold <- sell_threshold
      }
    }
  }
  
  cat("Best Sharpe Ratio:", best_sharpe, "\n")
  cat("Best Buy Threshold:", best_buy_threshold, "\n")
  cat("Best Sell Threshold:", best_sell_threshold, "\n")
  
  return(list(best_sharpe= best_sharpe, best_buy_threshold= best_buy_threshold,best_sell_threshold= best_sell_threshold, best_weighted_signal= best_weighted_signal, best_position= best_position, best_strategy_returns= best_strategy_returns))
}

get_optimal_sl_tp <- function(stop_loss_levels,take_profit_levels, stock_data, aligned_stock_returns,risk_free_rate,transaction_cost,best_weighted_signal){
  # Initialize variables to track the best results for stop loss and take profit
  best_stop_sharpe <- -Inf
  best_stop_loss <- NULL
  best_take_profit <- NULL
  best_stop_strategy_returns <- NULL
  best_position <- NULL
  
  # Iterate over different stop loss and take profit levels
  for (stop_loss in stop_loss_levels) {
    for (take_profit in take_profit_levels) {
      # Track position based on weighted signals and apply stop loss/take profit
      position <- numeric(length(best_weighted_signal))
      entry_price <- NA
      for (i in 2:length(best_weighted_signal)) {
        if (best_weighted_signal[i] == 1) {
          position[i] <- 1  # Buy signal
          entry_price <- as.numeric(stock_data[i])  # Record entry price
        } else if (best_weighted_signal[i] == -1) {
          position[i] <- 0  # Sell signal
          entry_price <- NA  # Reset entry price
        } else if (!is.na(entry_price) && !is.na(stock_data[i])) {
          # Check for stop loss or take profit
          if (position[i - 1] == 1 && (as.numeric(stock_data[i]) <= entry_price * (1 - stop_loss) || as.numeric(stock_data[i]) >= entry_price * (1 + take_profit))) {
            position[i] <- 0  # Exit position due to stop loss or take profit
            entry_price <- NA
          } else {
            position[i] <- position[i - 1]  # Hold the previous position
          }
        } else {
          position[i] <- position[i - 1]  # Hold the previous position
        }
      }
      
      position_xts <- xts(position, order.by = index(aligned_stock_returns))
      
      # Calculate new strategy returns based on positions with stop loss and take profit
      strategy_returns <- numeric(length(position))
      for (i in 2:length(position)) {
        if (position[i] == 1 && position[i - 1] == 0) {
          # Buy at today's close and hold until the next day's close (apply transaction cost)
          strategy_returns[i] <- aligned_stock_returns[i] - transaction_cost
        } else if (position[i] == 0 && position[i - 1] == 1) {
          # Sell at today's close (transaction cost applies)
          strategy_returns[i] <- -transaction_cost
        } else if (position[i] == 1) {
          # Hold the position from today's close to the next day's close
          strategy_returns[i] <- aligned_stock_returns[i]
        } else {
          # Hold cash at risk-free rate
          strategy_returns[i] <- risk_free_rate / 365
        }
      }
      
      strategy_returns_xts <- xts(strategy_returns, order.by = index(aligned_stock_returns))
      
      # Calculate Sharpe Ratio for the current stop loss and take profit levels
      excess_strategy_returns <- log(strategy_returns_xts+1) - (risk_free_rate / 365)
      sharpe_ratio <- (mean(excess_strategy_returns, na.rm = TRUE)-risk_free_rate/365) / sd(excess_strategy_returns, na.rm = TRUE) * sqrt(365)
      
      # Update the best results if the current Sharpe Ratio is better
      if (sharpe_ratio > best_stop_sharpe) {
        best_stop_sharpe <- sharpe_ratio
        best_stop_loss <- stop_loss
        best_take_profit <- take_profit
        best_stop_strategy_returns <- strategy_returns_xts
        best_position <- position_xts
      }
    }
  }
  
  cat("Best Sharpe Ratio with Stop Loss and Take Profit:", best_stop_sharpe, "\n")
  cat("Best Stop Loss Level:", best_stop_loss, "\n")
  cat("Best Take Profit Level:", best_take_profit, "\n")
  
  return(list(best_stop_sharpe=best_stop_sharpe, best_stop_loss=best_stop_loss, best_take_profit=best_take_profit, best_stop_strategy_returns=best_stop_strategy_returns,best_position= best_position))
}

plot_performance <- function(aligned_stock_returns, best_stop_strategy_returns){
  # Plot performance summary comparing Buy and Hold vs Optimized Strategy
  comparison_returns <- merge(aligned_stock_returns, best_stop_strategy_returns, all = FALSE)
  
  tryCatch({
    charts.PerformanceSummary(
      comparison_returns, 
      main = paste("Performance Summary: Optimized Strategy vs Buy and Hold for", asset_name),
      legend.loc = "topleft"
    )
  }, error = function(e) {
    cat("Error during performance summary plotting:", conditionMessage(e), "\n")
  })
}

plot_signal <- function(best_weighted_signal,aligned_stock_returns,stock_data,best_buy_threshold,best_sell_threshold) {
  # Plot combined signals for interactive visualization
  combined_signals_df <- data.frame(
    Date = index(best_weighted_signal),
    CombinedSignals = as.numeric(best_weighted_signal)
  )

  p_combined_signals <- ggplot(combined_signals_df, aes(x = Date, y = CombinedSignals)) +
    geom_line(color = "orange", linewidth = 0.8) +
    geom_hline(yintercept = as.numeric(best_buy_threshold), color = "green", linetype = "dashed", size = 0.6) +
    geom_hline(yintercept = as.numeric(best_sell_threshold), color = "red", linetype = "dashed", size = 0.6) +
    ggtitle("Combined Signals Over Time with Buy/Sell Thresholds") +
    xlab("Date") + ylab("Combined Signal Value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  interactive_combined_signals_plot <- ggplotly(p_combined_signals)
  print(interactive_combined_signals_plot)
}

plot_trades <- function(best_position,best_weighted_signal,aligned_stock_returns,stock_data){
  # Prepare data for plotting buy/sell signals
  buy_signals <- index(best_position)[which(diff(best_position) == 1)]  # Buy signals (position changes from 0 to 1)
  sell_signals <- index(best_position)[which(diff(best_position) == -1)] # Sell signals (position changes from 1 to 0)
  
  stock_df <- data.frame(
    Date = index(aligned_stock_returns),
    Price = as.numeric(stock_data[index(aligned_stock_returns)])
  )
  
  buy_df <- data.frame(Date = buy_signals, Price = as.numeric(stock_data[buy_signals]))
  sell_df <- data.frame(Date = sell_signals, Price = as.numeric(stock_data[sell_signals]))
  
  # Plot price data with buy/sell signals using ggplot2 and plotly
  p <- ggplot(stock_df, aes(x = Date)) +
    geom_line(aes(y = Price), color = "blue", linewidth = 0.8) +
    geom_point(data = buy_df, aes(x = Date, y = Price), color = "green", size = 2, shape = 17) +
    geom_point(data = sell_df, aes(x = Date, y = Price), color = "red", size = 2, shape = 17) +
    ggtitle(paste(asset_name,"Stock Price with Optimized Strategy Buy/Sell Signals")) +
    xlab("Date") + ylab("Price") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Convert ggplot to interactive plotly chart
  interactive_plot <- ggplotly(p)
  print(interactive_plot)
}

optimize_risk_percentage <- function(risk_percentages,best_position, stock_data, risk_free_rate, transaction_cost){
  # Initialize variables to track the best results
  best_sharpe <- -Inf
  best_risk_percentage <- NULL
  best_bankroll_history <- NULL
  
  # Loop over different risk percentages to determine which one yields the best Sharpe ratio
  for (risk_percentage in risk_percentages) {
    # Initialize bankroll and track bankroll history
    initial_bankroll <- 100  # Start with $100
    bankroll <- initial_bankroll
    bankroll_history <- numeric(length(best_position))
    bankroll_history[1] <- bankroll
    
    # Calculate the bankroll changes over time based on the fixed risk percentage
    for (i in 2:length(best_position)) {
      if (best_position[i] == 1 && best_position[i - 1] == 0) {
        # Buy: risk a certain percentage of bankroll
        trade_size <- bankroll * risk_percentage
        bankroll <- bankroll - trade_size  # Reduce bankroll by the risk amount
        # Store bankroll value after initiating position (asset value added later)
        bankroll_history[i] <- bankroll * (1 + risk_free_rate/365)
      } else if (best_position[i] == 0 && best_position[i - 1] == 1) {
        # Sell: exit position and add value of the position back to bankroll
        sell_value <- trade_size * (as.numeric(stock_data[i]) / as.numeric(stock_data[i - 1]))
        bankroll <- bankroll + sell_value  # Add proceeds from the sale
        bankroll_history[i] <- bankroll * (1+ risk_free_rate/365)
      } else {
        # Hold: Track current bankroll (including value of the position if in one)
        if (best_position[i] == 1) {
          # Track the value of the current position
          trade_size <- trade_size * (as.numeric(stock_data[i]) / as.numeric(stock_data[i - 1]))
          bankroll_history[i] <- bankroll * (1+ risk_free_rate/365)
        } else {
          # No active position, bankroll remains unchanged
          bankroll_history[i] <- bankroll * (1+ risk_free_rate/365)
        }
      }
    }
    
    bankroll_xts <- xts(bankroll_history, order.by = index(stock_data))
    
    # Calculate returns based on changes in bankroll
    strategy_returns <- diff(bankroll_xts) / stats::lag(bankroll_xts, k = 1)
    strategy_returns <- na.omit(strategy_returns)
    
    # Calculate Sharpe Ratio for the current risk percentage
    excess_strategy_returns <- log(strategy_returns+1) - (risk_free_rate / 365)
    sharpe_ratio <- (mean(excess_strategy_returns, na.rm = TRUE) - risk_free_rate/365) / sd(excess_strategy_returns, na.rm = TRUE) * sqrt(365)
    
    # Update the best results if the current Sharpe Ratio is better
    if (sharpe_ratio > best_sharpe) {
      best_sharpe <- sharpe_ratio
      best_risk_percentage <- risk_percentage
      best_bankroll_history <- bankroll_xts
    }
  }
  
  # Output the best results
  cat("Best Sharpe Ratio with Risk Percentage:", best_sharpe, "\n")
  cat("Best Risk Percentage per Trade:", best_risk_percentage * 100, "%\n")
  
  # Plot cumulative returns for the best strategy
  plot(index(best_bankroll_history), best_bankroll_history, type = "l",
       col = "blue", lwd = 2, main = "Cumulative Returns with Optimized Risk Percentage",
       xlab = "Date", ylab = "Bankroll ($)")
}

strategy_stats <- function(best_stop_strategy_returns, aligned_stock_returns,best_weighted_signal,best_buy_threshold, risk_free_rate, n_periods_per_year = 365) {
  library(PerformanceAnalytics)
  library(TTR)
  
  # Combine the strategy and buy-and-hold returns into a comparison object
  comparison_returns <- merge(aligned_stock_returns, best_stop_strategy_returns, all = FALSE)
  colnames(comparison_returns) <- c("Buy and Hold", "Optimized Strategy")
  
  # Calculate annualized returns
  annualized_returns <- table.AnnualizedReturns(comparison_returns, scale = n_periods_per_year)
  
  # Annualized standard deviation
  annualized_stddev <- apply(comparison_returns, 2, function(x) StdDev.annualized(x, scale = n_periods_per_year))
  
  # Calculate Sharpe Ratio
  excess_returns <- sweep(log(comparison_returns + 1), 2, risk_free_rate / n_periods_per_year, "-")
  sharpe_ratios <- apply(excess_returns, 2, function(x) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(n_periods_per_year))
  
  # Sortino Ratio
  sortino_ratios <- apply(comparison_returns, 2, function(x) SortinoRatio(x, MAR = risk_free_rate / n_periods_per_year) * sqrt(n_periods_per_year))
  
  # Expected Shortfall (ES)
  es_values <- apply(comparison_returns, 2, function(x) ES(x, p = 0.95, method = "historical") * sqrt(n_periods_per_year))
  
  # Modified Sharpe Ratio
  modified_sharpe <- c (
    "Buy and Hold" = (mean(excess_returns[,"Buy and Hold"], na.rm=TRUE) - (risk_free_rate/n_periods_per_year)) / abs(as.numeric(es_values["Buy and Hold"])) * sqrt(n_periods_per_year),
    "Optimized Strategy" = (mean(excess_returns[,"Optimized Strategy"], na.rm=TRUE) - (risk_free_rate/n_periods_per_year)) / abs(as.numeric(es_values["Optimized Strategy"])) * sqrt(n_periods_per_year)
  )
  
  # Cumulative returns
  cumulative_returns <- apply(comparison_returns, 2, Return.cumulative)
  
  # CAGR
  n_years <- nrow(comparison_returns) / n_periods_per_year
  cagr <- (cumulative_returns + 1)^(1 / n_years) - 1
  
  # Maximum Drawdown
  max_drawdowns <- apply(comparison_returns, 2, maxDrawdown)
  
  # Average Drawdown
  avg_drawdowns <- apply(comparison_returns, 2, function(x) {
    dd <- Drawdowns(x)
    abs(mean(dd[dd != 0], na.rm = TRUE))
  })
  
  # Downside deviation
  downside_deviation <- apply(comparison_returns, 2, function(x) DownsideDeviation(x, MAR = risk_free_rate / n_periods_per_year) * sqrt(n_periods_per_year))
  
  # Calmar Ratio
  calmar_ratios <- cumulative_returns / max_drawdowns
  
  # Omega Ratio
  omega_ratios <- apply(comparison_returns, 2, function(x) Omega(x, L = risk_free_rate / n_periods_per_year))
  
  # Win rates
  win_rates <- apply(comparison_returns, 2, function(x) mean(x > risk_free_rate/n_periods_per_year))
  
  # Win rates
  num_years <- apply(comparison_returns, 2, function(x) length(x)/n_periods_per_year)
  
  # Skewness and Kurtosis
  skewness <- apply(excess_returns, 2, skewness)
  kurtosis <- apply(excess_returns, 2, kurtosis)
  
  # Tail Ratio
  tail_ratios <- apply(excess_returns, 2, function(x) {
    quantile(x, probs = 0.95, na.rm = TRUE) / abs(quantile(x, probs = 0.05, na.rm = TRUE))
  })
  
  # Recovery Factor
  recovery_factors <- cumulative_returns / max_drawdowns
  
  # CAPM metrics (Beta and Alpha)
  beta_values <- c(
    "Buy and Hold" = 1,  # Beta of Buy and Hold relative to itself is 1
    "Optimized Strategy" = as.numeric(CAPM.beta(excess_returns[, "Optimized Strategy"], excess_returns[, "Buy and Hold"]))
  )
  
  alpha_values <- c(
    "Buy and Hold" = 0,  # Alpha of Buy and Hold relative to itself is 0
    "Optimized Strategy" = as.numeric(CAPM.alpha(excess_returns[, "Optimized Strategy"], excess_returns[, "Buy and Hold"]))
  )
  
  treynor_ratios <- c(
    "Buy and Hold" =  (mean(excess_returns[, "Buy and Hold"],na.rm=TRUE) - risk_free_rate/n_periods_per_year) * sqrt(n_periods_per_year),  # Alpha of Buy and Hold relative to itself is 0
    "Optimized Strategy" = (mean(excess_returns[, "Optimized Strategy"],na.rm=TRUE) - risk_free_rate/n_periods_per_year) / as.numeric(beta_values["Optimized Strategy"]) * sqrt(n_periods_per_year)
  )
  
  # Downside Frequency
  downside_freq_bh <- DownsideFrequency(comparison_returns[, "Buy and Hold"], MAR = risk_free_rate / n_periods_per_year)
  downside_freq_macd <- DownsideFrequency(comparison_returns[, "Optimized Strategy"], MAR = risk_free_rate / n_periods_per_year)
  
  # Additional Trading Metrics
  avg_days_per_trade <- nrow(comparison_returns) / sum(best_weighted_signal > best_buy_threshold)
  
  average_return_per_trade <- mean(best_stop_strategy_returns[best_stop_strategy_returns != risk_free_rate/n_periods_per_year], na.rm = TRUE)
  average_gain_per_winning_trade <- mean(best_stop_strategy_returns[best_stop_strategy_returns > risk_free_rate/n_periods_per_year], na.rm = TRUE)
  average_loss_per_loosing_trade <- abs(mean(best_stop_strategy_returns[best_stop_strategy_returns < risk_free_rate/n_periods_per_year], na.rm = TRUE))
  
  edge_ratio <- average_gain_per_winning_trade / average_loss_per_loosing_trade
  expected_value <- (win_rates["Optimized Strategy"] * average_gain_per_winning_trade) -
    ((1 - win_rates["Optimized Strategy"]) * average_loss_per_loosing_trade)
  
  # Profit Factor
  profit_factor <- (prod(best_stop_strategy_returns[best_stop_strategy_returns > 0] +1, na.rm = TRUE)-1) /
    abs(prod(best_stop_strategy_returns[best_stop_strategy_returns < 0] +1, na.rm = TRUE)-1)
  
  # Combine all metrics into a data frame
  performance_metrics <- data.frame(
    Metric = c(
      "Annualized Return",
      "Annualized StdDev",
      "Sharpe Ratio",
      "Sortino Ratio",
      "Expected Shortfall (ES)",
      "Modified Sharpe",
      "Cumulative Return",
      "CAGR",
      "Max Drawdown",
      "Average Drawdown",
      "Downside Deviation",
      "Calmar Ratio",
      "Omega Ratio",
      "Win Rate",
      "Skewness",
      "Kurtosis",
      "Tail Ratio",
      "Recovery Factor",
      "Beta",
      "Alpha",
      "Treynor Ratio",
      "Number of Years",
      "Avg Days per Trade",
      "Average Return per Trade",
      "Average Gain per Winning Trade",
      "Average Loss per Losing Trade",
      "Edge Ratio",
      "Expected Value",
      "Profit Factor"
    ),
    Buy_and_Hold = c(
      annualized_returns[1, "Buy and Hold"],
      annualized_stddev["Buy and Hold"],
      sharpe_ratios["Buy and Hold"],
      sortino_ratios["Buy and Hold"],
      es_values["Buy and Hold"],
      modified_sharpe["Buy and Hold"],
      cumulative_returns["Buy and Hold"],
      cagr["Buy and Hold"],
      max_drawdowns["Buy and Hold"],
      avg_drawdowns["Buy and Hold"],
      downside_deviation["Buy and Hold"],
      calmar_ratios["Buy and Hold"],
      omega_ratios["Buy and Hold"],
      win_rates["Buy and Hold"],
      skewness["Buy and Hold"],
      kurtosis["Buy and Hold"],
      tail_ratios["Buy and Hold"],
      recovery_factors["Buy and Hold"],
      beta_values["Buy and Hold"],
      alpha_values["Buy and Hold"],
      treynor_ratios["Buy and Hold"],
      num_years["Buy and Hold"],
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    Optimized_Strategy = c(
      annualized_returns[1, "Optimized Strategy"],
      annualized_stddev["Optimized Strategy"],
      sharpe_ratios["Optimized Strategy"],
      sortino_ratios["Optimized Strategy"],
      es_values["Optimized Strategy"],
      modified_sharpe["Optimized Strategy"],
      cumulative_returns["Optimized Strategy"],
      cagr["Optimized Strategy"],
      max_drawdowns["Optimized Strategy"],
      avg_drawdowns["Optimized Strategy"],
      downside_deviation["Optimized Strategy"],
      calmar_ratios["Optimized Strategy"],
      omega_ratios["Optimized Strategy"],
      win_rates["Optimized Strategy"],
      skewness["Optimized Strategy"],
      kurtosis["Optimized Strategy"],
      tail_ratios["Optimized Strategy"],
      recovery_factors["Optimized Strategy"],
      beta_values["Optimized Strategy"],
      alpha_values["Optimized Strategy"],
      treynor_ratios["Optimized Strategy"],
      num_years["Optimized Strategy"],
      avg_days_per_trade,
      average_return_per_trade,
      average_gain_per_winning_trade,
      average_loss_per_loosing_trade,
      edge_ratio,
      expected_value,
      profit_factor
    )
  )
  
  print(performance_metrics)
  
  return(performance_metrics)
}

optimize_weights <- function(asset_name, start_date, strategy_returns, strategy_signals, stock_returns, risk_free_rate,transaction_cost, strategy_names, buy_thresholds, sell_thresholds, stop_loss_levels, take_profit_levels, risk_percentages) {
  # Load necessary libraries
  library(quantmod)
  library(PerformanceAnalytics)
  library(GA)
  library(ggplot2)
  library(plotly)
  library(TTR)
  
  # Fetch asset data
  suppressWarnings(getSymbols(asset_name, from = start_date, to = Sys.Date(), src = "yahoo"))
  
  if (!exists(asset_name)) {
    stop("Error: Asset data could not be retrieved. Please check the asset name or internet connection.")
  }
  stock_data <- na.omit(Cl(get(asset_name)))
  stock_returns <- na.omit(dailyReturn(stock_data))
  
  print_serial_correlation(stock_returns)
  
  # Align all time series data by merging them
  combined_data <- merge(stock_returns, strategy_returns, strategy_signals, all = FALSE)
  aligned_stock_returns <- combined_data[, 1]
  aligned_strategy_returns <- combined_data[, 2:(1 + ncol(strategy_returns))]
  aligned_strategy_signals <- combined_data[, (2 + ncol(strategy_returns)):ncol(combined_data)]
  
  optimal_weights <- get_optimal_weights(aligned_strategy_returns)
  
  buy_sell_thershold <- get_optimal_buy_sell_thershold(optimal_weights,aligned_strategy_signals,aligned_stock_returns,buy_thresholds,sell_thresholds,transaction_cost,risk_free_rate)
  
  best_sharpe <- buy_sell_thershold$best_sharpe
  best_buy_threshold <- buy_sell_thershold$best_buy_threshold
  best_sell_threshold <- buy_sell_thershold$best_sell_threshold
  best_weighted_signal <- buy_sell_thershold$best_weighted_signal
  
  optimal_sl_tp <- get_optimal_sl_tp(stop_loss_levels,take_profit_levels, stock_data, aligned_stock_returns,risk_free_rate,transaction_cost,best_weighted_signal)
  
  best_stop_sharpe <- optimal_sl_tp$best_stop_sharpe
  best_stop_loss <- optimal_sl_tp$best_stop_loss
  best_take_profit <- optimal_sl_tp$best_take_profit
  best_stop_strategy_returns <- optimal_sl_tp$best_stop_strategy_returns
  best_position <-  optimal_sl_tp$best_position
  
  plot_performance(aligned_stock_returns, best_stop_strategy_returns)
  
  plot_signal(best_weighted_signal,aligned_stock_returns,stock_data,best_buy_threshold,best_sell_threshold)
  
  plot_trades(best_position,best_weighted_signal,aligned_stock_returns,stock_data)
  
  optimal_risk <- optimize_risk_percentage(risk_percentages,best_position, stock_data, risk_free_rate, transaction_cost)
  
  best_risk_percentage <- optimal_risk$best_risk_percentage
  
  strategy_stats <- strategy_stats(best_stop_strategy_returns,aligned_stock_returns,best_weighted_signal,best_buy_threshold,risk_free_rate)
  
  # Return the optimized weights, new strategy returns, and weighted signals
  return(list(OptimalWeights = optimal_weights, CombinedStrategyReturns = best_stop_strategy_returns, WeightedSignal = best_weighted_signal, BestStopLoss = best_stop_loss, BestTakeProfit = best_take_profit,BestRiskPercentage = best_risk_percentage, best_buy_threshold = best_buy_threshold, best_sell_threshold = best_sell_threshold,strategy_stats=strategy_stats))
}
