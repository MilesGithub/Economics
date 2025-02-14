
# Parameters
n_periods <- 100             # Number of periods for the simulation
initial_inventory <- 100     # Initial inventory at each level
order_up_to_level <- 120     # Target order-up-to level for each level of the supply chain
order_delay <- 2             # Number of periods of delay in orders
demand_variability <- 10     # Variability in demand (standard deviation)
replenishment_time <- 3      # Time taken for orders to be fulfilled (lead time)
cost_per_order <- 10         # Cost per order
holding_cost <- 1            # Holding cost per unit of inventory
backorder_cost <- 5          # Backorder cost per unit of unmet demand

# Forecasting and smoothing parameters
smoothing_factor <- 0.2      # Exponential smoothing factor for forecasting

# Variables initialization with length equal to n_periods
retailer_demand <- rep(0, n_periods)
retailer_inventory <- rep(0, n_periods)
wholesaler_inventory <- rep(0, n_periods)
manufacturer_inventory <- rep(0, n_periods)
wholesaler_orders <- rep(0, n_periods)
manufacturer_orders <- rep(0, n_periods)
retailer_forecast <- rep(0, n_periods)
wholesaler_forecast <- rep(0, n_periods)
manufacturer_forecast <- rep(0, n_periods)
retailer_order <- rep(0, n_periods)
wholesaler_order <- rep(0, n_periods)
manufacturer_order <- rep(0, n_periods)

# Initial inventories and forecasts
retailer_inventory[1] <- initial_inventory
wholesaler_inventory[1] <- initial_inventory
manufacturer_inventory[1] <- initial_inventory
retailer_forecast[1] <- 50
wholesaler_forecast[1] <- 50
manufacturer_forecast[1] <- 50

# Initialize orders for first period
wholesaler_orders[1] <- 0
manufacturer_orders[1] <- 0

# Retailer Demand Generation (random fluctuations with seasonal component)
for (t in 1:n_periods) {
  seasonal_demand_factor <- 1 + 0.1 * sin(2 * pi * t / 12)  # Add seasonal fluctuations
  retailer_demand[t] <- round(rnorm(1, mean=50, sd=demand_variability) * seasonal_demand_factor)
}

# Simulation
for (t in 2:n_periods) {
  
  # Retailer's behavior with order smoothing
  if (t > order_delay) {
    retailer_inventory[t] <- retailer_inventory[t-1] - retailer_demand[t] + wholesaler_orders[t - order_delay]
  } else {
    retailer_inventory[t] <- retailer_inventory[t-1] - retailer_demand[t]
  }
  
  # Retailer Forecasting using Exponential Smoothing
  retailer_forecast[t] <- smoothing_factor * retailer_demand[t-1] + (1 - smoothing_factor) * retailer_forecast[t-1]
  
  # Retailer Order Calculation (using forecast)
  retailer_order[t] <- max(0, order_up_to_level - retailer_inventory[t] - retailer_forecast[t])
  
  # Wholesaler's behavior: reacts to demand from retailer but with forecast and delay
  if (t > order_delay) {
    wholesaler_inventory[t] <- wholesaler_inventory[t-1] - wholesaler_orders[t] 
    # Ensure we do not reference an out-of-bounds index for manufacturer orders
    if (t - replenishment_time > 0) {
      wholesaler_inventory[t] <- wholesaler_inventory[t] + manufacturer_orders[t - replenishment_time]
    } else {
      wholesaler_inventory[t] <- wholesaler_inventory[t] # No orders yet from manufacturer
    }
  } else {
    wholesaler_inventory[t] <- wholesaler_inventory[t-1]
  }
  
  # Ensure wholesaler orders are initialized and properly updated
  wholesaler_orders[t] <- max(0, wholesaler_order[t])
  
  # Wholesaler Forecasting using Exponential Smoothing
  wholesaler_forecast[t] <- smoothing_factor * retailer_order[t-1] + (1 - smoothing_factor) * wholesaler_forecast[t-1]
  
  # Wholesaler Order Calculation (based on forecast)
  wholesaler_order[t] <- max(0, order_up_to_level - wholesaler_inventory[t] - wholesaler_forecast[t])
  
  # Manufacturer's behavior: reacts to orders from wholesaler but with forecast and delay
  if (t > replenishment_time) {
    manufacturer_inventory[t] <- manufacturer_inventory[t-1] - manufacturer_orders[t] 
  } else {
    manufacturer_inventory[t] <- manufacturer_inventory[t-1]
  }
  
  # Ensure manufacturer orders are initialized and properly updated
  manufacturer_orders[t] <- max(0, manufacturer_order[t])
  
  # Manufacturer Forecasting using Exponential Smoothing
  manufacturer_forecast[t] <- smoothing_factor * wholesaler_order[t-1] + (1 - smoothing_factor) * manufacturer_forecast[t-1]
  
  # Manufacturer Order Calculation (based on forecast)
  manufacturer_order[t] <- max(0, order_up_to_level - manufacturer_inventory[t] - manufacturer_forecast[t])
  
  # **Fix**: Safeguard when referencing out-of-bound indices
  # Check if t - replenishment_time is a valid index
  if (t - replenishment_time > 0) {
    wholesaler_inventory[t] <- wholesaler_inventory[t-1] - wholesaler_orders[t] + manufacturer_orders[t - replenishment_time]
  } else {
    wholesaler_inventory[t] <- wholesaler_inventory[t-1] - wholesaler_orders[t]  # No orders yet from manufacturer
  }
  
  # Store orders (ensure they are not empty)
  wholesaler_orders[t] <- wholesaler_order[t]
  manufacturer_orders[t] <- manufacturer_order[t]
}

# Plot results
par(mfrow=c(3,1))

# Retailer Inventory, Demand, and Forecast
plot(1:n_periods, retailer_inventory, type='l', col='blue', lwd=2, xlab='Period', ylab='Retailer Inventory', main='Retailer Inventory')
lines(1:n_periods, retailer_demand, col='red', lwd=2, lty=2)
lines(1:n_periods, retailer_forecast, col='green', lwd=2, lty=2)

# Wholesaler Inventory, Orders, and Forecast
plot(1:n_periods, wholesaler_inventory, type='l', col='blue', lwd=2, xlab='Period', ylab='Wholesaler Inventory', main='Wholesaler Inventory')
lines(1:n_periods, wholesaler_orders, col='green', lwd=2, lty=2)
lines(1:n_periods, wholesaler_forecast, col='orange', lwd=2, lty=2)

# Manufacturer Inventory, Orders, and Forecast
plot(1:n_periods, manufacturer_inventory, type='l', col='blue', lwd=2, xlab='Period', ylab='Manufacturer Inventory', main='Manufacturer Inventory')
lines(1:n_periods, manufacturer_orders, col='purple', lwd=2, lty=2)
lines(1:n_periods, manufacturer_forecast, col='pink', lwd=2, lty=2)
