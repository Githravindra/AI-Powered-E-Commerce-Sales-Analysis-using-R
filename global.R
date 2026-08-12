# global.R
# Main bootstrap file. Loads libraries, imports all modules, defines the API integration layer, and processes the default dataset.

# Ensure logging is sourced first
source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")

log_info("Initializing AI-Powered Sales Intelligence Platform...")

# --- Package Loader System ---
# Check and load packages with warnings for missing optional ones
required_packages <- c("dplyr", "tidyr", "lubridate", "ggplot2", "plotly", "cluster", "DT", "shiny")
optional_packages <- c("DBI", "RMySQL", "forecast", "caret", "randomForest")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_warn(sprintf("CRITICAL: Required package '%s' is not installed. Shiny dashboard may not run properly.", pkg))
  }
}

for (pkg in optional_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_warn(sprintf("OPTIONAL: Optional package '%s' is not installed. Using local fallbacks.", pkg))
  }
}

# --- Module Sourcing ---
source_module <- function(file_path) {
  if (file.exists(file_path)) {
    source(file_path)
    log_info(paste("Sourced module:", basename(file_path)))
  } else {
    log_error(paste("Module file not found:", file_path))
  }
}

source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/db_connection.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/queries.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/data_ingestion.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/data_preprocessing.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/kpi_engine.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/sales_analysis.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/customer_analysis.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/product_analysis.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/region_analysis.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/forecasting.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/clustering.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/recommendation_engine.R")
source_module("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/reports/report_generator.R")

# --- Dashboard Integration Layer (Reusable Wrappers) ---

#' Wrapper for Sales KPIs
getSalesMetrics <- function(df) {
  calculate_kpis(df)
}

#' Wrapper for Customer Lifetime Value & Segments
getCustomerMetrics <- function(df) {
  get_customer_metrics(df)
}

#' Wrapper for Product performance summaries
getProductMetrics <- function(df) {
  get_product_metrics(df)
}

#' Wrapper for Regional sales breakdowns
getRegionMetrics <- function(df) {
  get_region_metrics(df)
}

#' Wrapper for Sales Forecasting
forecastSales <- function(df, periods = 6) {
  forecast_sales(df, periods)
}

#' Wrapper for Customer RFM clustering
segmentCustomers <- function(df, centers = 3) {
  segment_customers(df, centers)
}

#' Wrapper for Business Intelligence insights
generateInsights <- function(df) {
  generate_insights(df)
}

# --- Base Data Bootstrapping ---
baseline_csv_path <- "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/data/sales_data.csv"

process_raw_data <- function(raw_df) {
  # 1. Validation
  if (!validate_columns(raw_df)) {
    log_error("Columns are invalid. Skipping further pipeline stages.")
    return(NULL)
  }
  
  # 2. Cleaning missing values (Generates clean dataset + audit logs)
  clean_list <- handle_missing_values(raw_df)
  df <- clean_list$data
  audit_trail <- clean_list$audit_log
  
  # 3. Deduplication
  df <- remove_duplicates(df)
  
  # 4. Remove invalid pricing/quantities
  df <- clean_invalid_records(df)
  
  # 5. Class type conversions
  df <- convert_data_types(df)
  
  # 6. derived columns (Dates & Margins)
  df <- add_derived_metrics(df)
  
  # 7. Outliers detection
  df <- detect_outliers(df)
  
  list(data = df, audit_log = audit_trail)
}

process_dataset <- function(file_path) {
  log_info(paste("Processing dataset:", file_path))
  raw_df <- read_sales_csv(file_path)
  if (is.null(raw_df)) {
    log_error("Could not load baseline dataset.")
    return(NULL)
  }
  process_raw_data(raw_df)
}

# Load baseline data on startup: Attempt MySQL first, fall back to CSV
log_info("Attempting to load baseline data from MySQL Database...")
db_loaded <- FALSE

tryCatch({
  # Check if connection can be established
  conn_test <- connect_db()
  if (!is.null(conn_test)) {
    close_db(conn_test)
    
    db_raw_data <- get_all_sales()
    if (!is.null(db_raw_data) && nrow(db_raw_data) > 0) {
      bootstrapped <- process_raw_data(db_raw_data)
      if (!is.null(bootstrapped)) {
        global_cleaned_data <- bootstrapped$data
        global_audit_trail <- bootstrapped$audit_log
        log_info("Baseline data loaded successfully from MySQL Database.")
        db_loaded <- TRUE
      }
    } else {
      log_warn("MySQL Database loaded, but vw_sales_summary is empty.")
    }
  }
}, error = function(e) {
  log_error(paste("MySQL startup load failed. Error:", e$message))
})

if (!db_loaded) {
  log_info("Falling back to local CSV baseline dataset...")
  if (file.exists(baseline_csv_path)) {
    bootstrapped <- process_dataset(baseline_csv_path)
    if (!is.null(bootstrapped)) {
      global_cleaned_data <- bootstrapped$data
      global_audit_trail <- bootstrapped$audit_log
      log_info("Baseline data loaded and cleaned from CSV. System initialized.")
    } else {
      log_warn("Failed to bootstrap baseline data from CSV. Dashboard running empty.")
      global_cleaned_data <- data.frame()
      global_audit_trail <- data.frame()
    }
  } else {
    log_warn(paste("Baseline file not found at", baseline_csv_path, ". Creating empty structures."))
    global_cleaned_data <- data.frame()
    global_audit_trail <- data.frame()
  }
}

# ==============================================================================
# Dynamic Dispatch Wrappers for Name Collision Resolution
# Resolves conflict between analytics module functions (data.frame-based)
# and database query layer functions (limit-based).
# ==============================================================================

# Wrapper for Top Products
if (exists("get_top_products", mode = "function")) {
  get_top_products_df <- get_top_products
  get_top_products <- function(x = 10, ...) {
    if (is.data.frame(x)) {
      get_top_products_df(x, ...)
    } else {
      get_top_products_db(x)
    }
  }
  log_info("Registered dispatch wrapper for get_top_products.")
}

# Wrapper for Top Customers
if (exists("get_top_customers", mode = "function")) {
  get_top_customers_df <- get_top_customers
  get_top_customers <- function(x = 10, ...) {
    if (is.data.frame(x)) {
      get_top_customers_df(x, ...)
    } else {
      get_top_customers_db(x)
    }
  }
  log_info("Registered dispatch wrapper for get_top_customers.")
}

