# recommendation_engine.R
# Trains ML demand predictors and generates automated business intelligence insights.

library(dplyr)

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Train a Product Demand Prediction Model
#' Predicts transaction Quantity based on Category, Price, Region, and Month
#' @param df data.frame. Preprocessed transaction dataset.
#' @return A list containing the trained model object, variable importance, and model performance.
train_demand_predictor <- function(df) {
  log_info("Training Product Demand Prediction model...")
  
  if (is.null(df) || nrow(df) == 0) {
    log_warn("Empty dataset provided for demand training.")
    return(NULL)
  }
  
  # Ensure Month and factors exist
  model_df <- df %>%
    select(Quantity, Category, Price, Region, Month) %>%
    mutate(
      Category = as.factor(Category),
      Region = as.factor(Region),
      Month = as.factor(Month)
    )
  
  has_rf <- requireNamespace("randomForest", quietly = TRUE)
  model_object <- NULL
  model_type <- "Linear Regression Model (GLM Fallback)"
  importance_df <- data.frame(Variable = character(), Importance = numeric(), stringsAsFactors = FALSE)
  
  if (has_rf) {
    log_info("Fitting Random Forest Model using randomForest package...")
    model_object <- tryCatch({
      fit_rf <- randomForest::randomForest(Quantity ~ Category + Price + Region + Month, 
                                           data = model_df, ntree = 100)
      
      # Extract variable importance
      imp <- randomForest::importance(fit_rf)
      importance_df <- data.frame(
        Variable = rownames(imp),
        Importance = as.numeric(imp[, 1]),
        stringsAsFactors = FALSE
      ) %>% arrange(desc(Importance))
      
      model_type <- "Random Forest Regressor"
      log_info("Random Forest training successful.")
      fit_rf
    }, error = function(e) {
      log_warn(paste("Random Forest training failed. Error:", e$message, "- Falling back to GLM."))
      has_rf <<- FALSE
      NULL
    })
  }
  
  if (!has_rf) {
    log_info("Fitting GLM model for demand prediction...")
    model_object <- glm(Quantity ~ Category + Price + Region + Month, 
                        family = poisson(link = "log"), data = model_df)
    
    # Calculate pseudo-importance using anova coefficients
    coef_vals <- summary(model_object)$coefficients
    # Get significance scores (t-statistic / z-value absolute values)
    vars <- rownames(coef_vals)[-1] # Exclude intercept
    if (length(vars) > 0) {
      importance_df <- data.frame(
        Variable = vars,
        Importance = abs(coef_vals[-1, "z value"]),
        stringsAsFactors = FALSE
      ) %>% arrange(desc(Importance))
    }
  }
  
  # Calculate training performance metrics (R-squared / RMSE)
  predictions <- tryCatch({
    if (model_type == "Random Forest Regressor") {
      predict(model_object, newdata = model_df)
    } else {
      # GLM predicts log quantity, take exponent
      exp(predict(model_object, newdata = model_df))
    }
  }, error = function(e) {
    rep(mean(model_df$Quantity), nrow(model_df))
  })
  
  rmse <- sqrt(mean((model_df$Quantity - predictions)^2, na.rm = TRUE))
  mean_qty <- mean(model_df$Quantity, na.rm = TRUE)
  mae <- mean(abs(model_df$Quantity - predictions), na.rm = TRUE)
  
  log_info(sprintf("Demand model training finished. Model Type: %s, RMSE: %0.4f, MAE: %0.4f", 
                   model_type, rmse, mae))
  
  list(
    model = model_object,
    type = model_type,
    importance = importance_df,
    metrics = list(RMSE = rmse, MAE = mae, Mean_Quantity = mean_qty)
  )
}

#' Generate automated natural-language business insights
#' @param df data.frame. Preprocessed transaction dataset.
#' @return A list of structured text insights and strategic recommendations.
generate_insights <- function(df) {
  log_info("Generating automated business intelligence insights...")
  
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      insights = list("No data available to generate insights."),
      recommendations = list("Please ingest data first.")
    ))
  }
  
  insights <- list()
  recommendations <- list()
  
  # Source modules if missing
  if (!exists("get_top_categories")) {
    source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/product_analysis.R")
  }
  if (!exists("get_region_metrics")) {
    source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/region_analysis.R")
  }
  if (!exists("get_top_customers")) {
    source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/customer_analysis.R")
  }
  if (!exists("get_yearly_growth")) {
    source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/sales_analysis.R")
  }
  
  # 1. Best performing category
  cats <- get_top_categories(df)
  if (!is.null(cats) && nrow(cats) > 0) {
    best_cat <- cats$Category[1]
    best_cat_sales <- cats$Sales[1]
    best_cat_margin <- cats$Profit_Margin[1]
    
    insights$BestCategory <- sprintf(
      "**Best Performing Category**: **%s** is the primary driver of top-line revenue, generating **₹%s** with an operational profit margin of **%0.1f%%**.",
      best_cat, format(round(best_cat_sales), big.mark = ","), best_cat_margin * 100
    )
  }
  
  # 2. Worst performing region
  regions <- get_region_metrics(df)
  if (!is.null(regions) && nrow(regions) > 0) {
    worst_reg_row <- regions %>% arrange(Sales) %>% slice(1)
    worst_reg <- worst_reg_row$Region
    worst_reg_sales <- worst_reg_row$Sales
    worst_reg_margin <- worst_reg_row$Profit_Margin
    
    insights$WorstRegion <- sprintf(
      "**Weakest Region Performance**: **%s Region** registered the lowest sales volume of **₹%s**, operating at a profit margin of **%0.1f%%**.",
      worst_reg, format(round(worst_reg_sales), big.mark = ","), worst_reg_margin * 100
    )
    
    # Check if margin is negative or low for region recommendations
    if (worst_reg_margin < 0.05) {
      recommendations <- c(recommendations, sprintf(
        "Optimize regional pricing strategy or delivery costs in the **%s Region**, which suffers from extremely narrow profit margins (%0.1f%%).",
        worst_reg, worst_reg_margin * 100
      ))
    }
  }
  
  # 3. Top customer
  top_cust <- get_top_customers(df, n = 1)
  if (!is.null(top_cust) && nrow(top_cust) > 0) {
    insights$TopCustomer <- sprintf(
      "**High Value Corporate Partner**: **%s** (%s) is our highest spending customer, generating **₹%s** in revenue across **%d** orders.",
      top_cust$Customer_Name[1], top_cust$Customer_ID[1], 
      format(round(top_cust$Total_Spent[1]), big.mark = ","), top_cust$Total_Orders[1]
    )
  }
  
  # 4. Highest profit product
  top_prof_prod <- get_top_products(df, n = 1, by = "profit")
  if (!is.null(top_prof_prod) && nrow(top_prof_prod) > 0) {
    insights$HighestProfitProduct <- sprintf(
      "**Top Profit Generating Asset**: **%s** is our most profitable product, yielding a total profit of **₹%s** with an individual profit margin of **%0.1f%%**.",
      top_prof_prod$Product[1], format(round(top_prof_prod$Profit[1]), big.mark = ","), top_prof_prod$Profit_Margin[1] * 100
    )
  }
  
  # 5. Seasonal Trends
  # Aggregate by month name
  monthly_seasonality <- df %>%
    group_by(Month_Name) %>%
    summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(Sales))
  
  if (nrow(monthly_seasonality) > 0) {
    peak_month <- monthly_seasonality$Month_Name[1]
    insights$SeasonalTrend <- sprintf(
      "**Seasonal Peak Period**: Transaction logs indicate **%s** represents the highest-velocity sales month, showing elevated demand peaks.",
      peak_month
    )
  }
  
  # 6. Revenue growth percentage
  growth <- get_yearly_growth(df)
  if (!is.null(growth) && nrow(growth) > 1) {
    latest_growth <- growth$Sales_Growth_Pct[nrow(growth)]
    latest_year <- growth$Year[nrow(growth)]
    insights$GrowthTrend <- sprintf(
      "**Yearly Growth Momentum**: E-Commerce sales grew by **%0.1f%%** YoY in **%d**, reflecting robust demand expansions.",
      latest_growth, latest_year
    )
  } else {
    insights$GrowthTrend <- "**Growth Momentum**: Baseline year established. Awaiting historical data points to compile YoY trend lines."
  }
  
  # Strategic Recommendations: Heuristics based on metrics
  # Add default recommendations if empty
  if (length(recommendations) == 0) {
    recommendations <- c(
      recommendations,
      "Bundle low-margin Furniture products with high-velocity Accessories to improve aggregate cart margins.",
      "Launch a re-engagement loyalty campaign targeted at the 'At Risk' K-Means customer cluster to reverse churn trend."
    )
  }
  
  # Add high-value customer recommendation
  if (!is.null(top_cust) && nrow(top_cust) > 0) {
    recommendations <- c(recommendations, sprintf(
      "Provide premium account benefits and volume discounts to **%s** and other top-tier accounts to secure long-term Customer Lifetime Value.",
      top_cust$Customer_Name[1]
    ))
  }
  
  # Add product focus recommendation
  if (!is.null(cats) && nrow(cats) > 0) {
    recommendations <- c(recommendations, sprintf(
      "Reallocate digital advertising budgets towards the **%s** category to capitalize on its high conversion rate and profit volumes.",
      cats$Category[1]
    ))
  }
  
  log_info("Business intelligence generation completed.")
  
  list(
    insights = insights,
    recommendations = recommendations
  )
}
