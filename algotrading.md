
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {

current_price <- amd_df$close[i]

 if (previous_price == 0 || current_price < previous_price) {
 amd_df$trade_type[i] <- 'buy'
 amd_df$costs_proceeds[i] <- -current_price * share_size
 accumulated_shares <- accumulated_shares + share_size
 }

 if (i == nrow(amd_df)) {
 amd_df$trade_type[i] <- 'sell'
 amd_df$costs_proceeds[i] <- accumulated_shares * current_price
 }
 previous_price <- current_price
 amd_df$accumulated_shares[i] <- accumulated_shares

}

```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2024-05-17")
amd_df <- subset(amd_df, date >= start_date & date <= end_date)

```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Calculating profit/loss
total_pnl <- sum(amd_df$costs_proceeds, na.rm = TRUE)
# Calculating the total amount invested
total_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
# ROI
roi <- (total_pnl / total_invested) * 100
sprintf("Total Profit/Loss: $%.2f", total_pnl)
## [1] "Total Profit/Loss: $8164779.07"
sprintf("Total Capital Invested: $%.2f", total_invested)
## [1] "Total Capital Invested: $1999466.99"
sprintf("Return on Investments: %.2f%%", roi)
## [1] "Return on Investments: 408.35%"

```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Option 1:
profit_threshold <- 0.2
# Initialise columns for average purchase price and profit-taking
amd_df$avg_purchase_price <- NA
amd_df$action <- NA
avg_purchase_price <- 0
shares_held <- 0
for (i in 1:nrow(amd_df)) {
 if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == 'buy') {
 total_cost <- shares_held * avg_purchase_price + share_size * amd_df$close[i]
 shares_held <- shares_held + share_size
 avg_purchase_price <- total_cost / shares_held
 }

 if (shares_held > 0 && (amd_df$close[i] >= avg_purchase_price * (1 + profit_threshold))) {
 amd_df$action[i] <- 'sell half'
 amd_df$costs_proceeds[i] <- (shares_held / 2) * amd_df$close[i]
 shares_held <- shares_held / 2
 }

 amd_df$avg_purchase_price[i] <- avg_purchase_price
 amd_df$accumulated_shares[i] <- shares_held
}

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Fill your code here and Disucss
# Calculating profit/loss
total_pnl <- sum(amd_df$costs_proceeds, na.rm = TRUE)
# Calculating the total amount invested
total_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
# ROI
roi <- (total_pnl / total_invested) * 100
sprintf("Total Profit/Loss: $%.2f", total_pnl)
## [1] "Total Profit/Loss: $10092816.84"
sprintf("Total Capital Invested: $%.2f", total_invested)
## [1] "Total Capital Invested: $1862989.96"
sprintf("Return on Investments: %.2f%%", roi)
## [1] "Return on Investments: 541.75%"
```

Discussion: In the 2023-01-01 to 2024-05-17 period, the profit-taking strategy had managed to consistently outperform trading without a specific strategy (with a 542% ROI vs a 408% ROI). This success can be  attributed to the fact that we are able to sell shares when there is a price increase of 20%, allowing us to secure early profits and reducing our risk by minimising our exposure to potential downside. We can see this as AMD’s earnings declined due to weakness in the PC market, where sales dropped by nearly 15% during 2023. As AMD is a major supplier of PC components, this market event was reflected in the bearish price action during the second and third quarters of that year, dropping nearly 18% . Consequently, the profit-taking strategy allowed us to exit out of positions where we could see the potential decline in our shares, and instead secure the profits whilst also minimised our exposure to risk.




