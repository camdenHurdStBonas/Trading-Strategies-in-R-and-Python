# Trading Strategies in R and Python

This repository contains various algorithmic trading strategies implemented in R (and possibly in Python in the future). The first strategy provided here is the **MACD (Moving Average Convergence Divergence) Trading Strategy**. The strategy is designed to test different parameter combinations, incorporate transaction costs and risk-free rates, and compare the performance of a trading strategy to a buy-and-hold strategy.

## Description

This project focuses on implementing trading strategies with a focus on the MACD trading strategy. The current R function runs a MACD-based trading strategy on a selected asset and evaluates different parameter combinations for the fast, slow, and signal moving averages to identify the most optimal strategy based on the Sharpe Ratio. The strategy can account for different assumptions, including transaction costs, risk-free rates, and shorting options.

### Key Features:
- Fetches data for a given asset using `quantmod` directly from Yahoo Finance.
- Evaluates the best combination of MACD parameters (nFast, nSlow, nSig) using the Sharpe Ratio.
- Allows assumptions on risk-free rates and transaction costs.
- Supports long-only, short-only, or both buy and short trading.
- Provides 3D visualization of MACD parameter optimization.
- Compares the strategy's performance to a buy-and-hold strategy.
- Calculates performance metrics like Annualized Returns, Sharpe Ratio, and Max Drawdown.

## Prerequisites

To run the script, you will need the following R packages installed:
- `quantmod`
- `PerformanceAnalytics`
- `TTR`
- `ggplot2`
- `parallel`
- `plotly`

You can install these packages by running:
```r
install.packages(c("quantmod", "PerformanceAnalytics", "TTR", "ggplot2", "parallel", "plotly"))
