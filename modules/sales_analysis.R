# sales_analysis.R
# Provides sales-focused business logic, trends, growth rates, and quarterly distributions.

library(dplyr)

#' Retrieve monthly sales trends
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame containing sales, profit, margins, and order counts per month.
get_monthly_trends <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    group_by(Year, Month, Month_Name) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      Quantity = sum(Quantity, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0),
      AOV = ifelse(Orders > 0, Sales / Orders, 0)
    ) %>%
    arrange(Year, Month)
}

#' Yearly sales growth analysis
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame showing Year, Sales, Profit, and YoY growth percentages.
get_yearly_growth <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  yearly <- df %>%
    group_by(Year) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      .groups = 'drop'
    ) %>%
    arrange(Year)
  
  # Calculate YoY growth
  yearly$Sales_Growth_Pct <- 0
  yearly$Profit_Growth_Pct <- 0
  
  if (nrow(yearly) > 1) {
    for (i in 2:nrow(yearly)) {
      yearly$Sales_Growth_Pct[i] <- ((yearly$Sales[i] - yearly$Sales[i - 1]) / yearly$Sales[i - 1]) * 100
      yearly$Profit_Growth_Pct[i] <- ((yearly$Profit[i] - yearly$Profit[i - 1]) / yearly$Profit[i - 1]) * 100
    }
  }
  
  yearly
}

#' Quarter-wise sales analysis
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame containing Quarter-wise summaries.
get_quarter_analysis <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    group_by(Quarter) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      Quantity = sum(Quantity, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0)
    ) %>%
    arrange(Quarter)
}
