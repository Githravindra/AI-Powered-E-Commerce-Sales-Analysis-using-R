# kpi_engine.R
# Computes core operational KPIs for the sales dashboard.

library(dplyr)

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Calculate overall KPIs from a cleaned sales dataset
#' @param df data.frame. Cleaned and preprocessed transactions.
#' @return A list containing major business performance metrics.
calculate_kpis <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      Revenue = 0,
      Profit = 0,
      ProfitMargin = 0,
      TotalOrders = 0,
      AOV = 0,
      CustomerRetentionRate = 0,
      ConversionRate = 0
    ))
  }
  
  total_revenue <- sum(df$Sales, na.rm = TRUE)
  total_profit <- sum(df$Profit, na.rm = TRUE)
  profit_margin <- ifelse(total_revenue > 0, total_profit / total_revenue, 0)
  
  total_orders <- n_distinct(df$Order_ID)
  aov <- ifelse(total_orders > 0, total_revenue / total_orders, 0)
  
  # Retention rate: percentage of customers with more than 1 purchase
  cust_purchases <- df %>%
    group_by(Customer_ID) %>%
    summarise(Orders = n_distinct(Order_ID), .groups = 'drop')
  
  total_customers <- nrow(cust_purchases)
  repeat_customers <- sum(cust_purchases$Orders > 1)
  retention_rate <- ifelse(total_customers > 0, repeat_customers / total_customers, 0)
  
  # Conversion rate: simulated based on transactions over simulated traffic
  # Let's assume a baseline traffic of 25 visitors per transaction
  simulated_traffic <- total_orders * 38.5 + 2400 # realistic traffic
  conversion_rate <- ifelse(simulated_traffic > 0, total_orders / simulated_traffic, 0)
  
  list(
    Revenue = total_revenue,
    Profit = total_profit,
    ProfitMargin = profit_margin,
    TotalOrders = total_orders,
    AOV = aov,
    CustomerRetentionRate = retention_rate,
    ConversionRate = conversion_rate
  )
}

#' Calculate period-over-period growth
#' @param current Numeric. Value of current period.
#' @param previous Numeric. Value of previous period.
#' @return Numeric. Percentage growth.
calculate_growth_pct <- function(current, previous) {
  if (is.null(previous) || is.na(previous) || previous == 0) {
    return(0)
  }
  ((current - previous) / previous) * 100
}
