# Trading Strategies in R and Python
This repository contains algorithmic trading strategies developed in both R and Python. It currently includes a function to execute a Moving Average Convergence Divergence (MACD) trading strategy in R. The function allows for the testing of multiple parameter combinations (nFast, nSlow, nSig), includes transaction costs, and incorporates shorting options. The goal is to help traders evaluate the best trading strategies based on Sharpe Ratios and visualize performance relative to a buy-and-hold approach.

In the future, additional strategies will be added in R and Python to expand the scope of algorithmic trading.

## Features
MACD Trading Strategy: Automatically tests different combinations of MACD parameters and evaluates the performance of the strategy using the Sharpe Ratio.
- Transaction Costs: Incorporates transaction costs to reflect real-world trading conditions.
- Shorting Options: Allows users to choose whether the strategy includes shorting or is restricted to long positions.
- Performance Comparison: Compares MACD strategy to a buy-and-hold strategy using performance metrics like Sharpe Ratio, Max Drawdown, and Annualized Returns.
- 3D Plot Visualization: Displays the relationship between different MACD parameters and the Sharpe Ratio using an interactive 3D scatter plot.

## Instructions
To get started with the MACD strategy in R:

1. Clone the Repository:
Open your terminal or command prompt, and run the following:
```bash
git clone https://github.com/yourusername/Trading-Strategies-in-R-and-Python.git
```
2. Install Required Packages: Ensure that you have all the necessary packages installed by running:
```r
# install the packages (only needs to ran once per directory)
install.packages(c("quantmod", "PerformanceAnalytics", "TTR", "ggplot2", "parallel", "plotly"))
```
3. Load the Code: Source the MACD strategy function in your R session:
```r
# Source the MACD function
source("MACD.R")
```
4. Run the MACD Strategy: Customize the following example code to test a MACD strategy on your chosen asset:
```r
# Define the parameters
asset_name <- "AAPL"
start_date <- "2019-01-01"
risk_free_rate <- 0.03   # Annualized risk-free rate
transaction_cost <- 0.005 # Transaction cost per trade
nFast_values <- seq(1, 30, 1)
nSlow_values <- seq(1, 60, 1)
nSig_values <- seq(3, 9, 1)
short <- 0  # 0 = only buy, -1 = buy and short, 1 = only short

# Run the MACD strategy
strategy <- macd_strategy(asset_name = asset_name, start_date = start_date, risk_free_rate = risk_free_rate, transaction_cost = transaction_cost, nFast_values = nFast_values, nSlow_values = nSlow_values, nSig_values = nSig_values, short = short)
```
5. Analyze the Results: After running the strategy, the function will output the best MACD parameters and performance metrics like the Sharpe Ratio and Max Drawdown. Additionally, you'll get a visual comparison of the strategy versus a buy-and-hold approach.
6. Visualize Parameter Sensitivity: The function will generate an interactive 3D plot showing how different MACD parameters affect the strategy’s Sharpe Ratio.
7. Performance Summary: The function will also print a table summarizing the Annualized Returns, Sharpe Ratios, and Max Drawdowns for both the MACD strategy and the buy-and-hold approach.

## Full Example
```r
# install the packages (only needs to ran once per directory)
install.packages(c("quantmod", "PerformanceAnalytics", "TTR", "ggplot2", "parallel", "plotly"))

# Clear the workspace
rm(list = ls())

# Set working directory
path <- "set/path/here"
setwd(path)

# Source the MACD function
source("MACD.R")

# Define the parameters
asset_name <- "AAPL"
start_date <- "2019-01-01"
risk_free_rate <- 0.03   # Annualized risk-free rate
transaction_cost <- 0.005 # Transaction cost per trade
nFast_values <- seq(1, 30, 1)
nSlow_values <- seq(1, 60, 1)
nSig_values <- seq(3, 9, 1)
short <- 0  # 0 = only buy, -1 = buy and short, 1 = only short

# Run the MACD strategy
strategy <- macd_strategy(asset_name = asset_name, start_date = start_date, risk_free_rate = risk_free_rate, transaction_cost = transaction_cost, nFast_values = nFast_values, nSlow_values = nSlow_values, nSig_values = nSig_values, short = short)
```
   
## Contribution
This repository is open for collaboration, especially for SIMM and the students in the algorithmic trading sector. Feel free to contribute additional trading strategies or improve the existing code!
