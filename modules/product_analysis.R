# product_analysis.R
# Provides insights on product catalog performance, category contributions, and product scoring/rankings.

library(dplyr)

#' Retrieve product performance summaries
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame summarizing sales, profit, margins, and quantities per product.
get_product_metrics <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    group_by(Product, Category) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Quantity = sum(Quantity, na.rm = TRUE),
      Avg_Price = mean(Price, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0)
    )
}

#' Retrieve top selling products
#' @param df data.frame. Cleaned transaction dataset.
#' @param n Integer. Number of top products.
#' @param by Character. Metric to sort by ("sales", "quantity", or "profit").
#' @return A data.frame of top products.
get_top_products <- function(df, n = 10, by = "sales") {
  pm <- get_product_metrics(df)
  if (is.null(pm)) return(NULL)
  
  if (by == "quantity") {
    pm <- pm %>% arrange(desc(Quantity))
  } else if (by == "profit") {
    pm <- pm %>% arrange(desc(Profit))
  } else {
    pm <- pm %>% arrange(desc(Sales))
  }
  
  head(pm, n)
}

#' Retrieve category level summaries
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame with Category-wise metrics.
get_top_categories <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    group_by(Category) %>%
    summarise(
      Sales = sum(Sales, na.rm = TRUE),
      Profit = sum(Profit, na.rm = TRUE),
      Quantity = sum(Quantity, na.rm = TRUE),
      Orders = n_distinct(Order_ID),
      .groups = 'drop'
    ) %>%
    mutate(
      Profit_Margin = ifelse(Sales > 0, Profit / Sales, 0),
      Share_Pct = (Sales / sum(Sales)) * 100
    ) %>%
    arrange(desc(Sales))
}

#' Retrieve low performing products
#' @param df data.frame. Cleaned transaction dataset.
#' @param threshold_sales Numeric. Low performance sales threshold.
#' @return A data.frame of products with total sales below the threshold.
get_low_performing_products <- function(df, threshold_sales = 50000) {
  pm <- get_product_metrics(df)
  if (is.null(pm)) return(NULL)
  
  pm %>%
    filter(Sales < threshold_sales) %>%
    arrange(Sales)
}

#' Generate product ranking system
#' Uses a composite score of Sales (40%), Quantity (30%), and Profit Margin (30%)
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame with Product names, their composite score, and their rank.
get_product_ranking <- function(df) {
  pm <- get_product_metrics(df)
  if (is.null(pm) || nrow(pm) == 0) return(NULL)
  
  # Min-max helper
  min_max_scale <- function(x) {
    if (max(x) == min(x)) return(rep(0, length(x)))
    (x - min(x)) / (max(x) - min(x))
  }
  
  ranked <- pm %>%
    mutate(
      Sales_Norm = min_max_scale(Sales),
      Quantity_Norm = min_max_scale(Quantity),
      Margin_Norm = min_max_scale(Profit_Margin),
      Composite_Score = (Sales_Norm * 0.40) + (Quantity_Norm * 0.30) + (Margin_Norm * 0.30)
    ) %>%
    arrange(desc(Composite_Score)) %>%
    mutate(Rank = row_number()) %>%
    select(Rank, Product, Category, Sales, Profit, Quantity, Profit_Margin, Composite_Score)
  
  ranked
}
