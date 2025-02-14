
# Parameters
n_agents <- 100          # Number of agents
n_rounds <- 100          # Number of rounds in simulation
good_money_value <- 2    # Value of good money in terms of "bad" money
bad_money_value <- 1     # Value of bad money in terms of itself
fiat_money_value <- 0.5  # Value of fiat money in terms of bad money
initial_good_money <- 1000  # Initial amount of good money each agent has
initial_bad_money <- 2000  # Initial amount of bad money each agent has
initial_fiat_money <- 500  # Initial amount of fiat money each agent has
transaction_cost_good <- 0.05  # 5% transaction cost for good money
transaction_cost_bad <- 0.02  # 2% transaction cost for bad money

# Economic shocks
inflation_shock <- 0.05  # Inflation rate for bad money
deflation_shock <- 0.02  # Deflation rate for good money

# Initialize agent data with variability
agents <- data.frame(
  id = 1:n_agents,
  good_money = rep(initial_good_money, n_agents) + rnorm(n_agents, 0, 200),  # Vary initial good money
  bad_money = rep(initial_bad_money, n_agents) + rnorm(n_agents, 0, 200),   # Vary initial bad money
  fiat_money = rep(initial_fiat_money, n_agents) + rnorm(n_agents, 0, 100),  # Vary initial fiat money
  wealth = rep(initial_good_money * good_money_value + initial_bad_money * bad_money_value + initial_fiat_money * fiat_money_value, n_agents), # Total wealth in terms of bad money
  spending_behavior = runif(n_agents, 0.5, 1),  # Randomize spending behavior (50% - 100% of wealth spent)
  hoarding_behavior = runif(n_agents, 0.1, 0.5),  # Randomize hoarding behavior (10% - 50% of wealth hoarded)
  transaction_history = I(rep(list(data.frame(time=integer(0), transaction_amount=double(0), money_type=character(0))), n_agents))
)

# Simulate economic shocks
apply_inflation_deflation <- function(agents, inflation_shock, deflation_shock) {
  # Inflation on bad money: money becomes less valuable
  agents$bad_money <- agents$bad_money * (1 - inflation_shock)
  
  # Deflation on good money: money becomes more valuable
  agents$good_money <- agents$good_money * (1 + deflation_shock)
  
  return(agents)
}

# Simulate transactions and hoarding behavior
simulate_round <- function(agents, good_money_value, bad_money_value, fiat_money_value, inflation_shock, deflation_shock, transaction_cost_good, transaction_cost_bad) {
  
  # Apply inflation and deflation shocks
  agents <- apply_inflation_deflation(agents, inflation_shock, deflation_shock)
  
  # Determine how much of each type of money is used for transactions
  for (i in 1:nrow(agents)) {
    # Each agent spends a fraction of their total wealth
    spending_amount <- agents$wealth[i] * agents$spending_behavior[i]
    
    # Initialize the money spent variables to avoid the "not found" error
    bad_money_spent <- 0
    good_money_spent <- 0
    fiat_money_spent <- 0
    
    # Agents have a higher tendency to spend bad money
    if (spending_amount > 0) {
      # Transaction costs are higher for good money
      if (spending_amount <= agents$bad_money[i]) {
        bad_money_spent <- spending_amount * (1 - transaction_cost_bad)
      } else if (spending_amount <= (agents$bad_money[i] + agents$fiat_money[i])) {
        bad_money_spent <- agents$bad_money[i]
        fiat_money_spent <- (spending_amount - agents$bad_money[i]) * (1 - transaction_cost_bad)
      } else {
        bad_money_spent <- agents$bad_money[i]
        fiat_money_spent <- agents$fiat_money[i]
        good_money_spent <- (spending_amount - (agents$bad_money[i] + agents$fiat_money[i])) * (1 - transaction_cost_good)
      }
    }
    
    # Update agents' balances
    agents$bad_money[i] <- agents$bad_money[i] - bad_money_spent
    agents$fiat_money[i] <- agents$fiat_money[i] - fiat_money_spent
    agents$good_money[i] <- agents$good_money[i] - good_money_spent
    
    # Update wealth after transaction
    agents$wealth[i] <- agents$bad_money[i] * bad_money_value + agents$good_money[i] * good_money_value + agents$fiat_money[i] * fiat_money_value
    
    # Record the transaction in history
    new_transaction <- data.frame(time = rep(n_rounds, length(c(bad_money_spent, fiat_money_spent, good_money_spent))), 
                                  transaction_amount = c(bad_money_spent, fiat_money_spent, good_money_spent),
                                  money_type = c("bad", "fiat", "good"))
    agents$transaction_history[[i]] <- rbind(agents$transaction_history[[i]], new_transaction)
  }
  
  # Increase hoarding behavior dynamically as good money becomes more valuable
  agents$hoarding_behavior <- agents$hoarding_behavior * (1 + deflation_shock)
  
  return(agents)
}

# Simulation
for (round in 1:n_rounds) {
  agents <- simulate_round(agents, good_money_value, bad_money_value, fiat_money_value, inflation_shock, deflation_shock, transaction_cost_good, transaction_cost_bad)
  
  # Display status at intervals
  if (round %% 10 == 0) {
    cat("Round", round, "completed.\n")
    # Print summary statistics
    cat("Total Good Money in Circulation:", sum(agents$good_money), "\n")
    cat("Total Bad Money in Circulation:", sum(agents$bad_money), "\n")
    cat("Total Fiat Money in Circulation:", sum(agents$fiat_money), "\n")
    cat("Average Wealth per Agent:", mean(agents$wealth), "\n\n")
  }
}

# Summarize results
agents$transaction_history <- NULL
summary(agents)
