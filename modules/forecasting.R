# forecasting.R
# Implements Linear Regression and ARIMA modeling to forecast monthly sales.

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Forecast sales using Linear Regression and ARIMA
#' @param df data.frame. Preprocessed sales transactions.
#' @param periods Integer. Number of months to forecast.
#' @return A list containing forecast data.frame, model details, and comparison metrics.
forecast_sales <- function(df, periods = 6) {
  log_info(paste("Starting sales forecasting for horizon of", periods, "months..."))
  
  if (is.null(df) || nrow(df) == 0) {
    log_warn("Empty dataset provided for forecasting.")
    return(NULL)
  }
  
  # Source monthly trends helper if not available
  if (!exists("get_monthly_trends")) {
    source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/sales_analysis.R")
  }
  
  # Aggregate monthly sales
  monthly <- get_monthly_trends(df)
  
  if (nrow(monthly) < 6) {
    log_warn("Insufficient data points (< 6 months) to execute statistical forecasting. Returning flat forecast.")
    # Return flat forecast based on average sales
    avg_sales <- mean(monthly$Sales, na.rm = TRUE)
    future_dates <- seq(max(df$Order_Date) + 30, by = "month", length.out = periods)
    flat_forecast <- data.frame(
      Month_Index = seq(nrow(monthly) + 1, length.out = periods),
      Month_Name = format(future_dates, "%b"),
      Year = as.numeric(format(future_dates, "%Y")),
      Month = as.numeric(format(future_dates, "%m")),
      Sales_LR = avg_sales,
      Sales_ARIMA = avg_sales,
      LR_Lower80 = avg_sales * 0.8,
      LR_Upper80 = avg_sales * 1.2,
      ARIMA_Lower80 = avg_sales * 0.8,
      ARIMA_Upper80 = avg_sales * 1.2,
      stringsAsFactors = FALSE
    )
    return(list(forecast = flat_forecast, historical = monthly, method = "Flat Average Fallback"))
  }
  
  # Append a linear trend variable t
  monthly <- monthly %>%
    mutate(t = row_number())
  
  N <- nrow(monthly)
  
  # --- Model A: Linear Regression ---
  log_info("Fitting Linear Regression model...")
  fit_lr <- lm(Sales ~ t, data = monthly)
  
  # Setup forecast periods
  future_t <- seq(N + 1, N + periods)
  future_dates <- seq(max(df$Order_Date) + 30, by = "month", length.out = periods)
  future_years <- as.numeric(format(future_dates, "%Y"))
  future_months <- as.numeric(format(future_dates, "%m"))
  future_month_names <- format(future_dates, "%b")
  
  # Predict LR with intervals
  pred_lr <- predict(fit_lr, newdata = data.frame(t = future_t), interval = "prediction", level = 0.80)
  
  # --- Model B: ARIMA (with fallback) ---
  has_forecast_package <- requireNamespace("forecast", quietly = TRUE)
  
  arima_forecast <- rep(NA, periods)
  arima_lower80 <- rep(NA, periods)
  arima_upper80 <- rep(NA, periods)
  method_used <- "Linear Regression & Seasonal Dummy Regression"
  
  if (has_forecast_package) {
    log_info("Fitting ARIMA model using forecast package...")
    tryCatch({
      # Convert sales to TS object
      # Find starting year and month
      start_yr <- monthly$Year[1]
      start_mo <- monthly$Month[1]
      
      ts_sales <- ts(monthly$Sales, frequency = 12, start = c(start_yr, start_mo))
      
      # Fit Auto ARIMA
      fit_arima <- forecast::auto.arima(ts_sales)
      # Forecast
      fc_arima <- forecast::forecast(fit_arima, h = periods, level = 80)
      
      arima_forecast <- as.numeric(fc_arima$mean)
      arima_lower80 <- as.numeric(fc_arima$lower[, "80%"])
      arima_upper80 <- as.numeric(fc_arima$upper[, "80%"])
      
      method_used <- "Linear Regression & auto.arima"
      log_info("ARIMA forecasting successful.")
    }, error = function(e) {
      log_warn(paste("ARIMA failed. Error:", e$message, "- Falling back to Seasonal Dummy Regression."))
      has_forecast_package <<- FALSE
    })
  }
  
  if (!has_forecast_package) {
    log_info("Fitting Seasonal Dummy Regression model...")
    # Base-R Seasonal Dummy model: Sales ~ t + Month (dummy)
    fit_seasonal <- lm(Sales ~ t + factor(Month), data = monthly)
    
    # Predict with seasonal regression
    # Prepare future data frame
    future_df <- data.frame(
      t = future_t,
      Month = factor(future_months, levels = 1:12)
    )
    
    # Handle factor levels matching (if training set didn't have all months, fallback to linear model)
    pred_s <- tryCatch({
      predict(fit_seasonal, newdata = future_df, interval = "prediction", level = 0.80)
    }, error = function(e) {
      # Fallback to pure linear model if factor issues arise
      pred_lr
    })
    
    arima_forecast <- pred_s[, "fit"]
    arima_lower80 <- pred_s[, "lwr"]
    arima_upper80 <- pred_s[, "upr"]
  }
  
  # Ensure no negative sales in forecasts
  arima_forecast <- pmax(arima_forecast, 0)
  arima_lower80 <- pmax(arima_lower80, 0)
  arima_upper80 <- pmax(arima_upper80, 0)
  
  lr_forecast <- pmax(pred_lr[, "fit"], 0)
  lr_lower80 <- pmax(pred_lr[, "lwr"], 0)
  lr_upper80 <- pmax(pred_lr[, "upr"], 0)
  
  # Combine results
  forecast_df <- data.frame(
    Month_Index = future_t,
    Month_Name = future_month_names,
    Year = future_years,
    Month = future_months,
    Sales_LR = lr_forecast,
    Sales_ARIMA = arima_forecast,
    LR_Lower80 = lr_lower80,
    LR_Upper80 = lr_upper80,
    ARIMA_Lower80 = arima_lower80,
    ARIMA_Upper80 = arima_upper80,
    stringsAsFactors = FALSE
  )
  
  log_info("Forecasting pipeline completed successfully.")
  
  list(
    forecast = forecast_df,
    historical = monthly,
    method = method_used
  )
}
