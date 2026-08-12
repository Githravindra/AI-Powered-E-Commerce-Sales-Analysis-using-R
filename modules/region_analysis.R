# region_analysis.R
# Provides geographic insights across States, Cities, and Regions, and computes regional growth rates.

library(dplyr)

#' Retrieve regional level summaries
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame with Region-wise metrics.
get_region_metrics <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    group_by(Region) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      Quantity = sum(Quantity, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0),
      Share_Pct = (Sales / sum(Sales)) * 100
    ) %>%
    arrange(desc(Sales))
}

#' Retrieve state level sales
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame of states sorted by sales.
get_state_sales <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    group_by(State, Region) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0)
    ) %>%
    arrange(desc(Sales))
}

#' Retrieve city level revenue
#' @param df data.frame. Cleaned transaction dataset.
#' @param n Integer. Number of top cities.
#' @return A data.frame of top cities.
get_city_revenue <- function(df, n = 10) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  res <- df %>%
    group_by(City, State, Region) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0)
    ) %>%
    arrange(desc(Sales))
  
  head(res, n)
}

#' Calculate regional sales growth rate over years
#' @param df data.frame. Cleaned transaction dataset with Year derived.
#' @return A data.frame containing Region, Year, Sales, and YoY Growth Pct.
get_regional_growth <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  regional_yearly <- df %>%
    group_by(Region, Year) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(Region, Year)
  
  # Calculate growth rates within each region group
  regional_yearly <- regional_yearly %>%
    group_by(Region) %>%
    mutate(
      Prev_Sales = lag(Sales),
      Prev_Profit = lag(Profit),
      Sales_Growth_Pct = ifelse(!is.na(Prev_Sales) & Prev_Sales > 0, ((Sales - Prev_Sales) / Prev_Sales) * 100, 0),
      Profit_Growth_Pct = ifelse(!is.na(Prev_Profit) & Prev_Profit > 0, ((Profit - Prev_Profit) / Prev_Profit) * 100, 0)
    ) %>%
    ungroup() %>%
    select(Region, Year, Sales, Profit, Sales_Growth_Pct, Profit_Growth_Pct)
  
  regional_yearly
}
