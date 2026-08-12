# customer_analysis.R
# Provides customer-centric metrics, lifetime value, loyalty indicators, and RFM scoring.

library(dplyr)

#' Calculate Customer metrics (CLV, Frequency, etc.)
#' @param df data.frame. Cleaned transaction dataset.
#' @return A list with customer KPI summaries and a compiled customer data.frame.
get_customer_metrics <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Calculate aggregate customer indicators
  cust_summary <- df %>%
    group_by(Customer_ID, Customer_Name) %>%
    summarise(
      Total_Spent = sum(Sales, na.rm = TRUE),
      Total_Profit = sum(Profit, na.rm = TRUE),
      Total_Orders = n_distinct(Order_ID),
      Total_Units = sum(Quantity, na.rm = TRUE),
      Last_Purchase = max(Order_Date),
      .groups = 'drop'
    ) %>%
    mutate(
      AOV = ifelse(Total_Orders > 0, Total_Spent / Total_Orders, 0),
      Profit_Margin = ifelse(Total_Spent > 0, Total_Profit / Total_Spent, 0)
    )
  
  # Global statistics
  total_customers <- nrow(cust_summary)
  repeat_customers <- sum(cust_summary$Total_Orders > 1)
  repeat_rate <- ifelse(total_customers > 0, repeat_customers / total_customers, 0)
  avg_clv <- ifelse(total_customers > 0, mean(cust_summary$Total_Spent, na.rm = TRUE), 0)
  avg_freq <- ifelse(total_customers > 0, mean(cust_summary$Total_Orders, na.rm = TRUE), 0)
  
  list(
    CustomerTable = cust_summary,
    TotalCustomers = total_customers,
    RepeatCustomersCount = repeat_customers,
    RepeatRate = repeat_rate,
    AverageCLV = avg_clv,
    AverageFrequency = avg_freq
  )
}

#' Retrieve top customers
#' @param df data.frame. Cleaned transaction dataset.
#' @param n Integer. Number of top customers to return.
#' @param by Character. Metric to sort by ("sales" or "profit").
#' @return A data.frame of top n customers.
get_top_customers <- function(df, n = 10, by = "sales") {
  metrics <- get_customer_metrics(df)
  if (is.null(metrics)) return(NULL)
  
  tbl <- metrics$CustomerTable
  
  if (by == "profit") {
    tbl <- tbl %>% arrange(desc(Total_Profit))
  } else {
    tbl <- tbl %>% arrange(desc(Total_Spent))
  }
  
  head(tbl, n)
}

#' Compute RFM Metrics per customer
#' @param df data.frame. Cleaned transaction dataset.
#' @return A data.frame with Customer_ID, Customer_Name, Recency (days), Frequency (orders), Monetary (sales).
calculate_rfm_metrics <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  max_date <- max(df$Order_Date, na.rm = TRUE)
  
  df %>%
    group_by(Customer_ID, Customer_Name) %>%
    summarise(
      Recency = as.numeric(difftime(max_date, max(Order_Date), units = "days")),
      Frequency = n_distinct(Order_ID),
      Monetary = sum(Sales, na.rm = TRUE),
      .groups = 'drop'
    )
}
