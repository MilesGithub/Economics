
# Parameters
n_agents <- 100
n_rounds <- 100
initial_money <- 1000
new_money_injection <- 200  # Amount of new money injected into economy each round
initial_price <- 1  # Starting price for all sectors
sectors <- c("Financial", "Consumer Goods", "Technology", "Services", "Agriculture")

# Initialize agent data
agents <- data.frame(
  id = 1:n_agents,
  sector = sample(sectors, n_agents, replace = TRUE),  # Randomly assign sectors
  money = rep(initial_money, n_agents),
  price = rep(initial_price, n_agents),
  wealth = rep(initial_money, n_agents),  # Initial wealth based on initial money
  consumption = rep(0.5, n_agents),  # Fraction of wealth spent (consumption rate)
  savings = rep(0.5, n_agents)  # Fraction of wealth saved
)

# Simulate money flow and price adjustments
simulate_round <- function(agents, new_money_injection) {
  
  # Randomly inject money into one sector
  injected_sector <- sample(sectors, 1)
  message(paste("Injecting money into", injected_sector, "sector"))
  
  # Add money to the chosen sector agents
  affected_agents <- agents[agents$sector == injected_sector,]
  affected_agents$money <- affected_agents$money + new_money_injection
  
  # Money flows to other sectors based on wealth and consumption
  for (i in 1:nrow(affected_agents)) {
    # Flow money from financial agents to others based on wealth
    wealth_factor <- affected_agents$wealth[i] / sum(agents$wealth)
    sector_to_influence <- sample(sectors[sectors != injected_sector], 1)  # Flow money to a random other sector
    
    # Flow money from current agent to another sector
    amount_to_transfer <- wealth_factor * new_money_injection
    agents[agents$sector == sector_to_influence, "money"] <- 
      agents[agents$sector == sector_to_influence, "money"] + amount_to_transfer
    
    # Adjust the wealth of the agent in the current sector
    affected_agents$wealth[i] <- affected_agents$wealth[i] + amount_to_transfer
  }
  
  # Update prices based on the money flow and wealth consumption (price elasticity)
  for (sector in sectors) {
    sector_agents <- agents[agents$sector == sector,]
    
    # Average consumption in sector
    avg_consumption <- mean(sector_agents$consumption)
    
    # Update price based on increased demand from wealthier agents (higher wealth -> higher price)
    price_change <- 1 + (avg_consumption * 0.02)  # 2% price increase based on consumption demand
    agents[agents$sector == sector, "price"] <- 
      agents[agents$sector == sector, "price"] * price_change
  }
  
  # Update agent consumption and savings behavior based on wealth and prices
  for (i in 1:nrow(agents)) {
    # Higher wealth agents consume a larger fraction of their wealth
    agents$consumption[i] <- 0.5 + (agents$wealth[i] / sum(agents$wealth)) * 0.5
    agents$savings[i] <- 1 - agents$consumption[i]
    
    # Adjust wealth based on consumption and savings rate
    agents$wealth[i] <- agents$wealth[i] + (agents$money[i] * agents$savings[i])
  }
  
  return(agents)
}

# Simulation
for (round in 1:n_rounds) {
  agents <- simulate_round(agents, new_money_injection)
  if (round %% 10 == 0) {
    cat("Round", round, "completed.\n")
  }
}

# Summarize results
summary(agents)
