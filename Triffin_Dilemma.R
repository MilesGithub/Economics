
# Parameters
n_periods <- 200               # Number of periods for the simulation
initial_currency_reserves <- 1000    # Initial reserves in the system (e.g., USD)
initial_trade_balance <- -100        # Initial trade deficit
trade_deficit_growth <- 10    # Growth in the trade deficit per period
interest_rate_sensitivity <- 0.05   # Sensitivity of interest rates to currency reserves
inflation_rate <- 0.02        # Base inflation rate
confidence_decay_rate <- 0.02   # Decay in confidence as reserves increase
reserve_currency_demand <- 500  # Demand for the currency by the rest of the world
confidence_threshold <- 0.2   # Minimum confidence level before crisis occurs
debt_growth_rate <- 0.05      # Rate at which debt grows to finance trade deficits
debt_to_gdp_ratio <- 0.3      # Debt to GDP ratio target

# External shock factors
global_demand_fluctuation <- 0.1   # Global demand fluctuation due to shocks

# Initialize vectors to store simulation data
currency_reserves <- rep(0, n_periods)
trade_balance <- rep(0, n_periods)
confidence <- rep(1, n_periods)
inflation <- rep(0, n_periods)
interest_rate <- rep(0, n_periods)
debt_level <- rep(0, n_periods)
global_demand <- rep(0, n_periods)
currency_supply <- rep(0, n_periods)

# Initial conditions
currency_reserves[1] <- initial_currency_reserves
trade_balance[1] <- initial_trade_balance
confidence[1] <- 1
inflation[1] <- inflation_rate
interest_rate[1] <- 0.05  # Assume initial interest rate at 5%
debt_level[1] <- 0
global_demand[1] <- reserve_currency_demand

# Simulation
for (t in 2:n_periods) {
  # Trade deficit grows over time
  trade_balance[t] <- trade_balance[t-1] + trade_deficit_growth
  
  # Currency reserves grow based on the trade deficit (representing the supply of currency to the world)
  currency_reserves[t] <- currency_reserves[t-1] + abs(trade_balance[t])
  
  # Global demand for currency fluctuates due to external factors
  global_demand[t] <- reserve_currency_demand * (1 + global_demand_fluctuation * sin(2 * pi * t / 50))
  
  # Inflation increases as more currency is issued and as trade deficits rise
  inflation[t] <- inflation[t-1] + inflation_rate * (currency_reserves[t] / 1000)
  
  # Interest rates respond to inflation and the growth of currency reserves
  interest_rate[t] <- max(0, 0.05 + interest_rate_sensitivity * inflation[t])
  
  # Debt level increases as the country finances its trade deficits
  debt_level[t] <- debt_level[t-1] + abs(trade_balance[t]) * debt_growth_rate
  
  # Currency supply to the world (linked to trade balance and global demand)
  currency_supply[t] <- min(currency_reserves[t], global_demand[t])
  
  # Confidence in the currency decreases as inflation rises and as more currency is issued
  confidence[t] <- max(0, confidence[t-1] - confidence_decay_rate * (currency_reserves[t] / 1000))
  
  # Check for a currency crisis (if confidence drops below a threshold)
  if (confidence[t] < confidence_threshold) {
    cat("Currency crisis occurred at period", t, "due to lack of confidence in the currency.\n")
    break
  }
}

# Plot results
par(mfrow=c(3,1))

# Currency Reserves Over Time
plot(1:n_periods, currency_reserves, type='l', col='blue', lwd=2, xlab='Period', ylab='Currency Reserves', 
     main='Currency Reserves Over Time')

# Trade Balance and Debt Over Time
plot(1:n_periods, trade_balance, type='l', col='red', lwd=2, xlab='Period', ylab='Trade Balance (Deficit)', 
     main='Trade Balance and Debt Over Time')
lines(1:n_periods, debt_level, col='green', lwd=2, lty=2)

# Confidence, Inflation, and Interest Rates Over Time
plot(1:n_periods, confidence, type='l', col='green', lwd=2, xlab='Period', ylab='Confidence Level', 
     main='Confidence in Currency, Inflation, and Interest Rates')
lines(1:n_periods, inflation, col='red', lwd=2, lty=2)
lines(1:n_periods, interest_rate, col='blue', lwd=2, lty=2)
