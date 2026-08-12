# data_ingestion.R
# Handles data ingestion, schema validation, and missing value imputation with audit logs.

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

# Require lubridate for date parsing
library(lubridate)

#' Read CSV sales data
#' @param file_path Character. Path to the CSV file.
#' @return A data.frame or NULL on error.
read_sales_csv <- function(file_path) {
  log_info(paste("Reading CSV file from:", file_path))
  tryCatch({
    df <- read.csv(file_path, stringsAsFactors = FALSE)
    log_info(paste("Successfully read CSV with", nrow(df), "rows and", ncol(df), "columns."))
    df
  }, error = function(e) {
    log_error(paste("Failed to read CSV. Error:", e$message))
    NULL
  })
}

#' Read Excel sales data
#' @param file_path Character. Path to the Excel file.
#' @return A data.frame or NULL on error.
read_sales_excel <- function(file_path) {
  log_info(paste("Reading Excel file from:", file_path))
  if (requireNamespace("readxl", quietly = TRUE)) {
    tryCatch({
      df <- as.data.frame(readxl::read_excel(file_path))
      log_info(paste("Successfully read Excel with", nrow(df), "rows."))
      df
    }, error = function(e) {
      log_error(paste("Failed to read Excel. Error:", e$message))
      NULL
    })
  } else {
    log_warn("readxl package not installed. Attempting to check if file is CSV instead.")
    # Fallback if the user named a CSV file as .xlsx or if we can read it as CSV
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
      return(read_sales_csv(file_path))
    }
    log_error("Excel files are unsupported without the readxl package.")
    NULL
  }
}

#' Validate column names against required schema
#' @param df data.frame to validate.
#' @return Logical. TRUE if valid, FALSE otherwise.
validate_columns <- function(df) {
  if (is.null(df)) return(FALSE)
  
  required_cols <- c("Order_ID", "Order_Date", "Customer_ID", "Customer_Name", 
                     "Product", "Category", "Quantity", "Price", "Sales", 
                     "Profit", "City", "State", "Region")
  
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    log_error(paste("Validation Failed. Missing columns:", paste(missing_cols, collapse = ", ")))
    FALSE
  } else {
    log_info("Column validation passed.")
    TRUE
  }
}

#' Handle missing values and compile an audit log
#' @param df data.frame to clean.
#' @return List containing cleaned data.frame and an audit_log data.frame.
handle_missing_values <- function(df) {
  if (is.null(df)) return(NULL)
  
  log_info("Handling missing values (NAs and blanks)...")
  audit_logs <- data.frame(
    Audit_ID = character(),
    Order_ID = character(),
    Issue = character(),
    Action = character(),
    Time = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
  
  log_idx <- 1
  
  # Helper to check NA or empty
  is.isna <- function(x) {
    is.na(x) | x == "" | x == "NA" | x == "null" | x == "NaN"
  }
  
  # Helper to add audit logs
  add_audit_log <- function(order_id, issue, action) {
    audit_id <- sprintf("AUD-%04d", 1000 + log_idx)
    log_idx <<- log_idx + 1
    new_log <- data.frame(
      Audit_ID = audit_id,
      Order_ID = as.character(order_id),
      Issue = issue,
      Action = action,
      Time = format(Sys.time(), "%H:%M:%S"),
      Status = "Resolved",
      stringsAsFactors = FALSE
    )
    audit_logs <<- rbind(audit_logs, new_log)
  }
  
  # Calculate average profit margins from clean records for imputation
  clean_records <- df[!is.isna(df$Profit) & !is.isna(df$Sales) & df$Sales > 0, ]
  avg_margins <- list(
    Electronics = 0.235,
    Furniture = 0.05,
    Clothing = 0.40,
    `Office Supplies` = 0.50
  )
  
  # Update margins based on actual clean records if we have enough data
  if (nrow(clean_records) > 0) {
    try({
      for (cat in names(avg_margins)) {
        cat_df <- clean_records[clean_records$Category == cat, ]
        if (nrow(cat_df) > 5) {
          avg_margins[[cat]] <- mean(cat_df$Profit / cat_df$Sales, na.rm = TRUE)
        }
      }
    }, silent = TRUE)
  }
  
  # Impute Profit
  na_profit_rows <- which(is.na(df$Profit))
  if (length(na_profit_rows) > 0) {
    for (i in na_profit_rows) {
      cat <- df$Category[i]
      sales <- df$Sales[i]
      margin <- ifelse(cat %in% names(avg_margins), avg_margins[[cat]], 0.25)
      imputed_profit <- round(sales * margin, 2)
      df$Profit[i] <- imputed_profit
      
      add_audit_log(
        df$Order_ID[i],
        "Missing Profit (NA value detected)",
        sprintf("Imputed profit (₹%s) using %d%% average margin for %s", 
                format(imputed_profit, big.mark=","), round(margin * 100), cat)
      )
    }
  }
  
  # Helper to check NA or empty
  is.isna <- function(x) {
    is.na(x) | x == "" | x == "NA" | x == "null" | x == "NaN"
  }
  
  # Impute Customer Name based on ID
  na_name_rows <- which(is.isna(df$Customer_Name))
  if (length(na_name_rows) > 0) {
    for (i in na_name_rows) {
      cust_id <- df$Customer_ID[i]
      # Look up customer name from other rows
      matching_names <- df$Customer_Name[!is.isna(df$Customer_Name) & df$Customer_ID == cust_id]
      if (length(matching_names) > 0) {
        resolved_name <- matching_names[1]
      } else {
        resolved_name <- "Valued Partner"
      }
      df$Customer_Name[i] <- resolved_name
      
      add_audit_log(
        df$Order_ID[i],
        "Missing Client Identity",
        sprintf("Mapped ID '%s' to database record '%s'", cust_id, resolved_name)
      )
    }
  }
  
  # Impute City based on State
  na_city_rows <- which(is.isna(df$City))
  if (length(na_city_rows) > 0) {
    for (i in na_city_rows) {
      state <- df$State[i]
      # Find most common city for this state
      matching_cities <- df$City[!is.isna(df$City) & df$State == state]
      if (length(matching_cities) > 0) {
        # get mode
        resolved_city <- names(sort(table(matching_cities), decreasing = TRUE))[1]
      } else {
        resolved_city <- "Corporate Headquarters"
      }
      df$City[i] <- resolved_city
      
      add_audit_log(
        df$Order_ID[i],
        "Missing City Location",
        sprintf("Resolved geographic scope to '%s' from State context '%s'", resolved_city, state)
      )
    }
  }
  
  log_info(paste("Data imputation complete.", nrow(audit_logs), "anomalies corrected."))
  
  list(data = df, audit_log = audit_logs)
}

#' Remove duplicate transactions
#' @param df data.frame to process.
#' @return A data.frame without duplicate transactions.
remove_duplicates <- function(df) {
  if (is.null(df)) return(NULL)
  
  initial_rows <- nrow(df)
  # Keep first occurrence of each Order_ID (assuming Order_ID is the primary key)
  df_clean <- df[!duplicated(df$Order_ID), ]
  removed_count <- initial_rows - nrow(df_clean)
  
  if (removed_count > 0) {
    log_info(sprintf("Removed %d duplicate records based on Order_ID.", removed_count))
  } else {
    log_info("No duplicate records found.")
  }
  
  df_clean
}

#' Convert date columns and set appropriate data classes
#' @param df data.frame to transform.
#' @return Transformed data.frame.
convert_data_types <- function(df) {
  if (is.null(df)) return(NULL)
  
  log_info("Converting data types...")
  
  # Date conversion with lubridate
  df$Order_Date <- ymd(df$Order_Date)
  
  # Numeric conversion
  df$Quantity <- as.integer(df$Quantity)
  df$Price <- as.numeric(df$Price)
  df$Sales <- as.numeric(df$Sales)
  df$Profit <- as.numeric(df$Profit)
  
  # Factor conversion
  df$Category <- as.factor(df$Category)
  df$Region <- as.factor(df$Region)
  
  log_info("Data type conversions successfully completed.")
  df
}
