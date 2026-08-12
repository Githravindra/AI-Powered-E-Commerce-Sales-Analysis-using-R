# data_preprocessing.R
# Cleans invalid records, detects outliers, creates derived metrics, and normalizes inputs for ML.

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Clean invalid sales records
#' Filters out transactions with invalid pricing or quantities
#' @param df data.frame to process.
#' @return A cleaned data.frame.
clean_invalid_records <- function(df) {
  if (is.null(df)) return(NULL)
  
  initial_rows <- nrow(df)
  # Price and Quantity must be positive
  df_clean <- df[df$Quantity > 0 & df$Price > 0 & df$Sales > 0, ]
  removed_count <- initial_rows - nrow(df_clean)
  
  if (removed_count > 0) {
    log_warn(sprintf("Cleaned %d invalid records (negative quantity, free items, or zero sales).", removed_count))
  } else {
    log_info("No invalid records detected.")
  }
  
  df_clean
}

#' Outlier detection using IQR method
#' Flags records with extreme Sales or Profit values
#' @param df data.frame to analyze.
#' @return A data.frame with an added logical column 'Is_Outlier'.
detect_outliers <- function(df) {
  if (is.null(df)) return(NULL)
  
  log_info("Detecting outliers using IQR method...")
  
  # Outliers in Sales
  sales_q25 <- quantile(df$Sales, 0.25, na.rm = TRUE)
  sales_q75 <- quantile(df$Sales, 0.75, na.rm = TRUE)
  sales_iqr <- sales_q75 - sales_q25
  sales_lower <- sales_q25 - 1.5 * sales_iqr
  sales_upper <- sales_q75 + 1.5 * sales_iqr
  
  # Outliers in Profit
  profit_q25 <- quantile(df$Profit, 0.25, na.rm = TRUE)
  profit_q75 <- quantile(df$Profit, 0.75, na.rm = TRUE)
  profit_iqr <- profit_q75 - profit_q25
  profit_lower <- profit_q25 - 1.5 * profit_iqr
  profit_upper <- profit_q75 + 1.5 * profit_iqr
  
  # Flag outliers
  df$Is_Outlier <- df$Sales < sales_lower | df$Sales > sales_upper | 
                    df$Profit < profit_lower | df$Profit > profit_upper
  
  outlier_count <- sum(df$Is_Outlier, na.rm = TRUE)
  log_info(sprintf("Outlier detection finished. Detected %d outlier records (%0.1f%% of dataset).", 
                   outlier_count, (outlier_count / nrow(df)) * 100))
  
  df
}

#' Normalize numeric columns using Min-Max scaling
#' @param df data.frame.
#' @param columns Character vector. Names of columns to normalize.
#' @return A data.frame with normalized columns (e.g. Sales_Norm, Profit_Norm).
normalize_data <- function(df, columns = c("Sales", "Profit", "Quantity")) {
  if (is.null(df)) return(NULL)
  
  for (col in columns) {
    if (col %in% colnames(df)) {
      min_val <- min(df[[col]], na.rm = TRUE)
      max_val <- max(df[[col]], na.rm = TRUE)
      
      norm_col_name <- paste0(col, "_Norm")
      if (max_val == min_val) {
        df[[norm_col_name]] <- 0
      } else {
        df[[norm_col_name]] <- (df[[col]] - min_val) / (max_val - min_val)
      }
    }
  }
  
  log_info(paste("Normalized columns:", paste(columns, collapse = ", ")))
  df
}

#' Add derived temporal and financial metrics to the dataset
#' Creates Month, Year, Quarter, Month_Name, and Profit_Margin columns
#' @param df data.frame. Must have Order_Date, Sales, and Profit columns.
#' @return A data.frame with derived features.
add_derived_metrics <- function(df) {
  if (is.null(df)) return(NULL)
  
  log_info("Generating derived metrics and date features...")
  
  # Extract temporal components using lubridate
  df$Year <- year(df$Order_Date)
  df$Month <- month(df$Order_Date)
  df$Quarter <- paste0("Q", quarter(df$Order_Date))
  df$Month_Name <- format(df$Order_Date, "%b")
  df$Month_Name <- factor(df$Month_Name, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  # Calculate profit margin (handle Sales = 0)
  df$Profit_Margin <- ifelse(df$Sales > 0, round(df$Profit / df$Sales, 4), 0)
  
  log_info("Derived metrics successfully appended.")
  df
}
