# Define the function
macd_strategy <- function(asset_name, start_date, risk_free_rate, transaction_cost, borrowing_cost, nFast_values, nSlow_values, nSig_values, short) {
  
  # Load necessary libraries
  library(quantmod)
  library(PerformanceAnalytics)
  library(TTR)
  library(ggplot2)
  library(parallel)
  library(plotly)
  library(zoo)
  library(reshape2)  # For melting the data frame
  
  # Validate asset_name
  if (!is.character(asset_name) || nchar(asset_name) == 0) {
    stop("Error: 'asset_name' must be a non-empty string.")
  }
  
  # Validate start_date
  start_date_converted <- tryCatch(as.Date(start_date), error = function(e) NA)
  
  if (is.na(start_date_converted)) {
    stop("Error: 'start_date' must be a valid Date object in 'YYYY-MM-DD' format.")
  }
  
  # Validate risk_free_rate
  if (!is.numeric(risk_free_rate) || length(risk_free_rate) != 1 || risk_free_rate < 0) {
    stop("Error: 'risk_free_rate' must be a non-negative numeric value.")
  }
  
  # Validate transaction_cost
  if (!is.numeric(transaction_cost) || length(transaction_cost) != 1 || transaction_cost < 0) {
    stop("Error: 'transaction_cost' must be a non-negative numeric value.")
  }
  
  # Validate borrowing_cost
  if (!is.numeric(borrowing_cost) || length(borrowing_cost) != 1 || borrowing_cost < 0) {
    stop("Error: 'borrowing_cost' must be a non-negative numeric value.")
  }
  
  # Validate nFast_values
  if (!is.numeric(nFast_values) || any(nFast_values <= 0) || length(nFast_values) == 0) {
    stop("Error: 'nFast_values' must be a non-empty numeric vector with positive values.")
  }
  
  # Validate nSlow_values
  if (!is.numeric(nSlow_values) || any(nSlow_values <= 0) || length(nSlow_values) == 0) {
    stop("Error: 'nSlow_values' must be a non-empty numeric vector with positive values.")
  }
  
  # Validate nSig_values
  if (!is.numeric(nSig_values) || any(nSig_values <= 0) || length(nSig_values) == 0) {
    stop("Error: 'nSig_values' must be a non-empty numeric vector with positive values.")
  }
  
  # Validate short
  if (!is.numeric(short) || length(short) != 1 || !short %in% c(-1, 0, 1)) {
    stop("Error: 'short' must be one of the following values: -1 (buy and short), 0 (only buy), or 1 (only short).")
  }
  
  # Create an environment to store local variables
  obj <- new.env()
  
  suppressWarnings({
    # Download stock data
    getSymbols(asset_name, from = start_date, to = Sys.Date(), src = "yahoo")
  })
  
  # Get closing prices using the asset name
  stock_data <- na.omit(Cl(get(asset_name)))  # Closing price of stock
  stock_returns <- dailyReturn(stock_data)
  
  n_periods_per_year <- 252
  
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
                               ifelse(signal == -1, -stock_returns - borrow_cost / n_periods_per_year, rf_rate / n_periods_per_year))
    strategy_returns <- strategy_returns - transaction_cost * trade_occurred
    
    # Calculate Sharpe Ratio for the strategy (subtract risk-free rate)
    excess_returns <- log(1 + strategy_returns) - (rf_rate / n_periods_per_year)
    sharpe_ratio <- SharpeRatio.annualized(excess_returns, scale = n_periods_per_year) - (rf_rate/StdDev.annualized(excess_returns,scale = n_periods_per_year))
    
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
                             ifelse(signal == -1, -stock_returns - borrowing_cost / n_periods_per_year, risk_free_rate / n_periods_per_year))
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
  
  # Initialize variables to track trades and their outcomes
  trade_returns <- numeric()  # To store the returns of each trade
  trade_direction <- numeric() # To track the direction of each trade (1 for long, -1 for short)
  cumulative_return <- 1  # Initialize to 1 for multiplicative returns
  
  # Loop through the strategy returns to capture trade outcomes
  for (i in 2:length(strategy_returns)) {
    if (trade_occurred[i]) {
      # If a trade occurred, store the previous trade outcome
      if (cumulative_return != 1) {  # Check if there was a previous trade
        trade_returns <- c(trade_returns, cumulative_return - 1)  # Capture gain/loss, subtracting 1 to get the actual return
        trade_direction <- c(trade_direction, signal[i-1])
      }
      
      # Reset cumulative return for the new trade
      cumulative_return <- 1 + as.numeric(strategy_returns[i])  # Start the new trade with its return
    } 
    else {
      # If no trade occurred, accumulate returns
      cumulative_return <- cumulative_return * (1 + as.numeric(strategy_returns[i]))
    }
  }
  
  # If a trade is still active at the end of the data, capture its outcome
  if (cumulative_return != 1) {  # Only capture if the trade was active
    trade_returns <- c(trade_returns, cumulative_return - 1)  # Capture the final trade outcome
    trade_direction <- c(trade_direction, signal[length(signal) - 1])
  }
  
  # Output basic performance metrics for both strategies
  annualized_returns <- table.AnnualizedReturns(comparison_returns, scale = n_periods_per_year)
  
  # Calculate the annualized standard deviation for both strategies
  stddev_bh <- StdDev.annualized(comparison_returns[, "Buy and Hold"], scale = n_periods_per_year)
  stddev_macd <- StdDev.annualized(comparison_returns[, "MACD Strategy"], scale = n_periods_per_year)
  
  # Subtract risk-free rate for Sharpe Ratio calculation
  excess_buy_hold <- log(1 + comparison_returns[, "Buy and Hold"]) - (risk_free_rate / n_periods_per_year)
  excess_macd <- log(1 + comparison_returns[, "MACD Strategy"]) - (risk_free_rate / n_periods_per_year)
  
  # Calculate Sharpe Ratios
  sharpe_bh <- SharpeRatio.annualized(excess_buy_hold, scale = n_periods_per_year) - (risk_free_rate/StdDev.annualized(excess_buy_hold, scale = n_periods_per_year))
  sharpe_macd <- SharpeRatio.annualized(excess_macd, scale = n_periods_per_year) - (risk_free_rate/StdDev.annualized(excess_macd, scale = n_periods_per_year))
  sharpe_edge <- sharpe_macd - sharpe_bh
  
  # Sortino Ratio 
  sortino_bh <- SortinoRatio(excess_buy_hold, MAR = risk_free_rate / n_periods_per_year) * sqrt(n_periods_per_year) - (risk_free_rate/StdDev.annualized(excess_buy_hold[excess_buy_hold < risk_free_rate / n_periods_per_year], scale = n_periods_per_year))
  sortino_macd <- SortinoRatio(excess_macd, MAR = risk_free_rate / n_periods_per_year) * sqrt(n_periods_per_year) - (risk_free_rate/StdDev.annualized(excess_macd[excess_macd < risk_free_rate / n_periods_per_year], scale = n_periods_per_year))
  sortino_edge <- sortino_macd - sortino_bh
  
  # Calculate Modified Sharpe Ratio using ES (Expected Shortfall)
  ES_bh <- ES(excess_buy_hold, p = 0.95, method = "historical") * sqrt(n_periods_per_year)
  ES_macd <- ES(excess_macd, p = 0.95, method = "historical") * sqrt(n_periods_per_year)
  
  modified_sharpe_bh <- (Return.annualized(excess_buy_hold, scale = n_periods_per_year) - risk_free_rate) / abs(ES_bh)
  modified_sharpe_macd <- (Return.annualized(excess_macd,  scale = n_periods_per_year) - risk_free_rate) / abs(ES_macd)
  modified_sharpe_edge <- modified_sharpe_macd - modified_sharpe_bh
  
  # Cumulative return
  cum_return_bh <- Return.cumulative(buy_hold_returns)
  cum_return_macd <- Return.cumulative(strategy_returns)
  cum_return_edge <- cum_return_macd - cum_return_bh
  
  # Calculate CAGR
  n_years <- nrow(buy_hold_returns) / n_periods_per_year  # Assuming daily data; adjust accordingly if not
  cagr_bh <- (cum_return_bh + 1)^(1/n_years) - 1
  cagr_macd <- (cum_return_macd + 1)^(1/n_years) - 1
  cagr_edge <- cagr_macd - cagr_bh
  
  # Calculate Max Drawdown for each strategy
  max_drawdown_bh <- maxDrawdown(comparison_returns[,"Buy and Hold"])
  max_drawdown_macd <- maxDrawdown(comparison_returns[,"MACD Strategy"])
  drawdown_edge <- max_drawdown_bh - max_drawdown_macd
  
  # Calculate Drawdowns and Average Drawdown for each strategy
  drawdowns_bh <- Drawdowns(comparison_returns[,"Buy and Hold"])
  drawdowns_macd <- Drawdowns(comparison_returns[,"MACD Strategy"])
  
  # Calculate Average Drawdown (mean of all drawdowns)
  avg_drawdown_bh <- abs(mean(drawdowns_bh[drawdowns_bh != 0], na.rm = TRUE))
  avg_drawdown_macd <- abs(mean(drawdowns_macd[drawdowns_macd != 0], na.rm = TRUE))
  avg_drawdown_edge <- avg_drawdown_bh - avg_drawdown_macd
  
  # Downside Deviation Edge
  downside_dev_bh <- DownsideDeviation(buy_hold_returns, MAR = risk_free_rate / n_periods_per_year) * sqrt(n_periods_per_year)
  downside_dev_macd <- DownsideDeviation(strategy_returns, MAR = risk_free_rate / n_periods_per_year) * sqrt(n_periods_per_year)
  downside_dev_edge <- downside_dev_bh - downside_dev_macd

  # Calculate Downside Frequency for each strategy
  # Assume the minimum acceptable return (MAR) is 0 (no loss is desired)
  downside_freq_bh <- DownsideFrequency(comparison_returns[,"Buy and Hold"], MAR = risk_free_rate / n_periods_per_year)
  downside_freq_macd <- DownsideFrequency(comparison_returns[,"MACD Strategy"], MAR = risk_free_rate / n_periods_per_year)
  downside_freq_edge <- downside_freq_bh - downside_freq_macd
  
  # Calmar Ratio Edge
  calmar_bh <- CalmarRatio(excess_buy_hold,scale = n_periods_per_year) - (risk_free_rate/maxDrawdown(excess_buy_hold))
  calmar_macd <- CalmarRatio(excess_macd, scale = n_periods_per_year) - (risk_free_rate/maxDrawdown(excess_macd))
  calmar_edge <- calmar_macd - calmar_bh
  
  # Win rate
  win_rate_bh <- length(buy_hold_returns[buy_hold_returns > 0]) / length(buy_hold_returns)
  win_rate_macd <- length(trade_returns[trade_returns > 0]) / length(trade_returns) # Calculate win rate
  win_rate_edge <- win_rate_macd - win_rate_bh
  
  # Calculate Skewness and Kurtosis
  skewness_bh <- skewness(excess_buy_hold)
  skewness_macd <- skewness(excess_macd)
  
  kurtosis_bh <- kurtosis(excess_buy_hold)
  kurtosis_macd <- kurtosis(excess_macd)
  
  # Calculate Tail Ratio manually
  tail_ratio_bh <- quantile(excess_buy_hold, probs = 0.95) / abs(quantile(excess_buy_hold, probs = 0.05))
  tail_ratio_macd <- quantile(excess_macd, probs = 0.95) / abs(quantile(excess_macd, probs = 0.05))
  tail_ratio_edge <- tail_ratio_macd - tail_ratio_bh
  
  # Calculate Recovery Factor
  recovery_factor_bh <- cum_return_bh / max_drawdown_bh
  recovery_factor_macd <- cum_return_macd / max_drawdown_macd
  recovery_factor_edge <- recovery_factor_macd - recovery_factor_bh
  
  # Calculate Omega Ratio
  omega_ratio_bh <- Omega(excess_buy_hold, L = risk_free_rate / 252)
  omega_ratio_macd <- Omega(excess_macd, L = risk_free_rate / 252)
  omega_edge <- omega_ratio_macd - omega_ratio_bh
  
  # Combine all metrics into a data frame
  performance_metrics <- data.frame(
    Metric = c("Annualized Return", "Annualized Standard Deviation", "Annualized Sharpe Ratio","Annualized Sortino Ratio","Estimated Shortfall","Modified Sharpe", "Cumulative Return","CAGR", "Max Drawdown", "Average Drawdown","Annualized Drawdown Deviation", "Drawdown Frequencey","Calmar Ratio","Recovery Factor", "Omega Ratio","Win Rate","Skewness","Kurtosis","Tail Ratio"),
    Buy_and_Hold = c(annualized_returns[1, "Buy and Hold"], stddev_bh, sharpe_bh, sortino_bh,ES_bh,modified_sharpe_bh, cum_return_bh,cagr_bh, max_drawdown_bh,avg_drawdown_bh, downside_dev_bh,downside_freq_bh, calmar_bh,recovery_factor_bh, omega_ratio_bh, win_rate_bh,skewness_bh,kurtosis_bh,tail_ratio_bh),
    MACD_Strategy = c(annualized_returns[1, "MACD Strategy"], stddev_macd, sharpe_macd, sortino_macd, ES_macd, modified_sharpe_macd, cum_return_macd,cagr_macd, max_drawdown_macd,avg_drawdown_macd, downside_dev_macd, downside_freq_macd,calmar_macd,recovery_factor_macd,omega_ratio_macd, win_rate_macd,skewness_macd,kurtosis_macd,tail_ratio_macd)
  )
  
  print(performance_metrics)
  
  
  # Calculate win rate, average gain per trade, average loss per trade
  average_return_per_trade <- mean(trade_returns)
  average_gain_per_winning_trade <- mean(trade_returns[trade_returns > 0]) # Average gain of winning trades
  average_loss_per_loosing_trade <- abs(mean(trade_returns[trade_returns < 0])) # Average loss of loosing trades
  
  # Edge calculation
  edge_ratio <- average_gain_per_winning_trade / average_loss_per_loosing_trade
  expected_value <- (win_rate_macd * average_gain_per_winning_trade) - ((1 - win_rate_macd) * average_loss_per_loosing_trade)
  
  # Profit Factor
  profit_factor <- ((1+ Return.cumulative(trade_returns[trade_returns > 0]))^(1/n_years) - 1) / abs((1+Return.cumulative(trade_returns[trade_returns < 0]))^(1/n_years) - 1)
  
  # information Ratio
  information_ratio <- InformationRatio(excess_macd,excess_buy_hold,scale = n_periods_per_year) * sqrt(n_periods_per_year)
  
  # CAPM
  beta_macd <- CAPM.beta(excess_macd,excess_buy_hold)
  
  alpha_macd <- CAPM.alpha(excess_macd,excess_buy_hold)
  
  timing_ratio <- TimingRatio(excess_macd,excess_buy_hold)
  
  treynor_ratio <- TreynorRatio(excess_macd,excess_buy_hold, scale = n_periods_per_year,modified = FALSE)
  
  modified_treynor_ratio <- TreynorRatio(excess_macd,excess_buy_hold, scale = n_periods_per_year,modified = TRUE)
  
  # Average Days per Trade
  avg_days_per_trade <- nrow(stock_data) / sum(trade_occurred)
  
  # Combine all metrics into a data frame
  edge_metrics <- data.frame(
    Metric = c("Sharpe Edge","Sortino Edge","Modified Sharpe Edge","Cumulative Return Edge","CAGR Edge","Max Drawdown Edge","Average Drawdown Edge", "Downside Deviation Edge","Downside Frequencey Edge", "Calmar Edge", "Win Rate Edge","Tail Ratio Edge", "Recovery Factor Edge","Omega Ratio Edge","Beta","Alpha","Treynor Ratio","Timing Ratio","Modified Treynor Ratio","Average Return Per Trade","Average Gain Per Winning Trade","Average Loss per Loosing Trade","Edge Ratio","Expected Value", "Annualized Profit Factor","Annualized Information Ratio", "Avg Days Per Trade","Number of Years"),
    MACD_Strategy = c(sharpe_edge, sortino_edge, modified_sharpe_edge, cum_return_edge,cagr_edge, drawdown_edge,avg_drawdown_edge, downside_dev_edge,downside_freq_edge, calmar_edge, win_rate_edge, tail_ratio_edge,recovery_factor_edge, omega_edge, beta_macd, alpha_macd,treynor_ratio, timing_ratio, modified_treynor_ratio, average_return_per_trade, average_gain_per_winning_trade, average_loss_per_loosing_trade, edge_ratio, expected_value, profit_factor,information_ratio, avg_days_per_trade,n_years)
  )
  
  print(edge_metrics)
  
  # Rolling Sharpe
  rolling_sharpe_bh <- rollapply(excess_buy_hold, width = n_periods_per_year, 
                                 FUN = function(x) ((mean(x) - (risk_free_rate/n_periods_per_year)) / sd(x))*sqrt(n_periods_per_year), 
                                 by.column = FALSE, align = "right")
  rolling_sharpe_macd <- rollapply(excess_macd, width = n_periods_per_year, 
                                   FUN = function(x) ((mean(x) - (risk_free_rate/n_periods_per_year)) / sd(x))*sqrt(n_periods_per_year), 
                                   by.column = FALSE, align = "right")
  
  # Create a data frame for ggplot
  rolling_sharpe_df <- data.frame(
    Time = index(rolling_sharpe_bh),  # Assuming rolling_sharpe_bh is a zoo object
    BuyHold = coredata(rolling_sharpe_bh),
    MACD = coredata(rolling_sharpe_macd)
  )
  
  # Melt the data frame for ggplot
  rolling_sharpe_melted <- melt(rolling_sharpe_df, id.vars = "Time", variable.name = "Strategy", value.name = "Sharpe")
  
  # Create the plot using ggplot2
  rolling_sharpe_plot <- ggplot(rolling_sharpe_melted, aes(x = Time, y = Sharpe, color = Strategy)) +
    geom_line() +
    labs(title = "Rolling Sharpe Ratios", x = "", y = "Annualized Sharpe Ratio") +
    scale_color_manual(values = c("BuyHold" = "blue", "MACD" = "red")) +
    theme_minimal()
  
  suppressWarnings({
  print(rolling_sharpe_plot)
  })
  
  # Return the environment and store key variables in the environment
  obj$best_nFast <- best_nFast
  obj$best_nSlow <- best_nSlow
  obj$best_nSig <- best_nSig
  obj$comparison_returns <- comparison_returns
  obj$performance_metrics <- performance_metrics
  obj$edge_metrics <- edge_metrics
  
  return(obj)
}

# Example of calling the function with parameters
# Replace with your desired inputs
# macd_strategy("AAPL", "2018-01-01", 0.03, 0.05, 0.005, seq(8,12,1), seq(20,30,1), seq(7,9,1), short = -1)
