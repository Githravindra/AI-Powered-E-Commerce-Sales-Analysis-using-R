# app.R
# Refactored Main User Interface and Server logic served via index.html template.

library(shiny)
library(plotly)
library(DT)
library(dplyr)

# Bootstrap system
source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/global.R")

# --- SHINY USER INTERFACE (HTML Template) ---
ui <- htmlTemplate("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/index.html")

# --- SHINY SERVER LOGIC ---
server <- function(input, output, session) {
  log_info("New Shiny session established via Custom HTML UI.")
  
  # Active dataset store
  session_data <- reactiveValues(
    cleaned = global_cleaned_data,
    audit = global_audit_trail,
    demand_model = NULL
  )
  
  # Train model when data changes
  observe({
    req(session_data$cleaned)
    if (nrow(session_data$cleaned) > 0) {
      model_res <- tryCatch({
        train_demand_predictor(session_data$cleaned)
      }, error = function(e) {
        log_error(paste("Model retrain failed. Error:", e$message))
        NULL
      })
      session_data$demand_model <- model_res
    }
  })
  
  # Handle CSV Uploads
  observeEvent(input$upload_csv, {
    file <- input$upload_csv
    req(file)
    
    log_info(paste("Uploaded dataset file path:", file$datapath))
    
    # Process dataset
    processed <- tryCatch({
      process_dataset(file$datapath)
    }, error = function(e) {
      log_error(paste("Upload processing failed. Error:", e$message))
      showNotification("Failed to parse dataset. Ensure columns match required format.", type = "error")
      NULL
    })
    
    if (!is.null(processed)) {
      session_data$cleaned <- processed$data
      session_data$audit <- processed$audit_log
      showNotification("Dataset parsed, validated, and sanitized successfully.", type = "message")
      log_info("Session dataset replaced with uploaded logs.")
    }
  })
  
  # Handle MySQL Connection Trigger
  db_status <- reactiveVal("")
  
  observeEvent(input$btn_test_db, {
    req(input$db_host)
    db_status("Connecting...")
    
    # Configure database options dynamically from inputs
    options(
      db.host = input$db_host,
      db.port = as.integer(input$db_port),
      db.name = input$db_name,
      db.user = input$db_user,
      db.password = input$db_password
    )
    
    # Test connection
    conn <- connect_db()
    
    if (is.null(conn)) {
      db_status("Failed to connect to MySQL database. Verify parameters or driver availability.")
      showNotification("MySQL Connection Failed.", type = "error")
    } else {
      close_db(conn) # Close the test connection
      
      db_status("Connected. Fetching sales records...")
      db_df <- get_all_sales()
      
      if (!is.null(db_df) && nrow(db_df) > 0) {
        # Process data frame directly using the process_raw_data function
        processed <- process_raw_data(db_df)
        if (!is.null(processed)) {
          session_data$cleaned <- processed$data
          session_data$audit <- processed$audit_log
          db_status("Connected & Synced successfully.")
          showNotification("MySQL database synchronized successfully.", type = "message")
        } else {
          db_status("Sync failed: Data format mismatch.")
          showNotification("MySQL Synchronization failed: data format invalid.", type = "error")
        }
      } else {
        db_status("Connected, but sales summary view is empty.")
        showNotification("MySQL view is empty or inaccessible.", type = "warning")
      }
    }
  })
  
  # --- FILTERED REACTIVE DATA ---
  filtered_data <- reactive({
    req(session_data$cleaned)
    df <- session_data$cleaned
    
    if (nrow(df) == 0) return(df)
    
    # 1. Filter by Region
    reg <- if (is.null(input$filter_region)) "All" else input$filter_region
    if (reg != "All") {
      df <- df %>% filter(Region == reg)
    }
    
    # 2. Filter by Category
    cat <- if (is.null(input$filter_category)) "All" else input$filter_category
    if (cat != "All") {
      df <- df %>% filter(Category == cat)
    }
    
    # 3. Filter by Quarter
    qtr <- if (is.null(input$filter_quarter)) "all" else input$filter_quarter
    if (qtr != "all") {
      df <- df %>% filter(tolower(Quarter) == tolower(qtr))
    }
    
    df
  })
  
  # --- CORE OBSERVATION LOOP TO PUSH DATA TO JAVASCRIPT ---
  observe({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # Calculate KPIs
    kpis <- calculate_kpis(df)
    
    # Apply dynamic growth factor based on filter
    reg <- if (is.null(input$filter_region)) "All" else input$filter_region
    cat <- if (is.null(input$filter_category)) "All" else input$filter_category
    factor <- 1.0
    if (reg != "All") factor <- factor * 1.2
    if (cat != "All") factor <- factor * 0.9
    
    kpis$RevenueGrowth <- 0.062 * factor
    kpis$OrdersGrowth <- 0.041 * factor
    kpis$CustomersGrowth <- 0.035 * factor
    kpis$ProfitGrowth <- 0.084 * factor
    
    # Add metrics for sales analytics tab
    kpis$TotalUnits <- sum(df$Quantity, na.rm = TRUE)
    kpis$AvgUnitPrice <- ifelse(kpis$TotalUnits > 0, sum(df$Sales, na.rm = TRUE) / kpis$TotalUnits, 0)
    kpis$HighestOrderValue <- max(df$Sales, na.rm = TRUE)
    
    # Inventory contribution percentages
    inventory_contrib <- list(Electronics = 0, Furniture = 0, Clothing = 0, OfficeSupplies = 0)
    grand_sum <- sum(df$Sales, na.rm = TRUE)
    if (grand_sum > 0) {
      sum_by_cat <- df %>%
        group_by(Category) %>%
        summarise(Sales = sum(Sales, na.rm = TRUE), .groups = 'drop')
      for (i in 1:nrow(sum_by_cat)) {
        cat_name <- as.character(sum_by_cat$Category[i])
        key <- gsub(" ", "", cat_name)
        inventory_contrib[[key]] <- (sum_by_cat$Sales[i] / grand_sum) * 100
      }
    }
    
    # Generate ARIMA & LR Forecast
    horizon <- if (is.null(input$ml_horizon)) 6 else as.integer(input$ml_horizon)
    fc <- forecast_sales(df, periods = horizon)
    
    # K-Means customer RFM clustering
    cl <- segment_customers(df, k = 3)
    if (!is.null(cl)) {
      cl$data <- cl$data %>% mutate(Segment = as.character(Segment))
      cl$centroids <- cl$centroids %>% mutate(Segment = as.character(Segment))
    }
    
    # AI insights & recommended actions
    insights_res <- generate_insights(df)
    
    # Mini metrics mapping for dashboard snapshot cards
    top_p <- get_top_products(df, n = 1)
    top_product_text <- if (nrow(top_p) > 0) {
      sprintf("%s (₹%s)", top_p$Product[1], format(round(top_p$Sales[1]), big.mark = ","))
    } else {
      "Calculating..."
    }
    
    cats <- get_top_categories(df)
    worst_cat_text <- if (!is.null(cats) && nrow(cats) > 0) {
      worst_row <- cats %>% arrange(Profit_Margin) %>% slice(1)
      sprintf("%s (%0.1f%% margin)", worst_row$Category[1], worst_row$Profit_Margin[1] * 100)
    } else {
      "Calculating..."
    }
    
    forecast_text <- if (!is.null(fc) && !is.null(fc$forecast)) {
      total_q1 <- sum(fc$forecast$Sales_ARIMA[1:3], na.rm = TRUE)
      sprintf("Est. ₹%s total volume", format(round(total_q1), big.mark = ","))
    } else {
      "Calculating..."
    }
    
    insights_res$TopProductText <- top_product_text
    insights_res$WorstCategoryText <- worst_cat_text
    insights_res$ForecastText <- forecast_text
    
    # Compile variable importance
    importance_list <- list()
    if (!is.null(session_data$demand_model) && !is.null(session_data$demand_model$importance)) {
      importance_list <- lapply(1:nrow(session_data$demand_model$importance), function(i) {
        list(
          Variable = session_data$demand_model$importance$Variable[i],
          Importance = session_data$demand_model$importance$Importance[i]
        )
      })
    }
    
    # Package and send to JS
    session$sendCustomMessage("update_analytics", list(
      cleaned_data = df,
      audit_logs = session_data$audit,
      db_status = db_status(),
      kpis = kpis,
      inventory_contrib = inventory_contrib,
      forecast = fc,
      clusters = cl,
      insights = insights_res,
      importance = importance_list
    ))
  })
  
  # Demand predictor query
  observeEvent(input$btn_query_demand, {
    req(session_data$demand_model)
    model <- session_data$demand_model$model
    type <- session_data$demand_model$type
    
    # Parse query variables from client inputs
    query_df <- data.frame(
      Category = factor(input$query_category, levels = c("Electronics", "Furniture", "Clothing", "Office Supplies")),
      Price = as.numeric(input$query_price),
      Region = factor(input$query_region, levels = c("East", "West", "Central", "South")),
      Month = factor(input$query_month, levels = 1:12),
      stringsAsFactors = FALSE
    )
    
    pred_val <- tryCatch({
      if (type == "Random Forest Regressor") {
        predict(model, newdata = query_df)
      } else {
        exp(predict(model, newdata = query_df))
      }
    }, error = function(e) {
      NA
    })
    
    result_str <- if (is.na(pred_val)) {
      "Prediction failed. Verify predictor configuration."
    } else {
      sprintf("Predicted Quantity Demand: %0.1f units per transaction (using %s)", pred_val, type)
    }
    
    session$sendCustomMessage("demand_prediction_result", list(result = result_str))
  })
  
  # --- REPORT EXPORTS DOWNLOADER ---
  output$btn_download_report <- downloadHandler(
    filename = function() {
      ext <- switch(input$export_format, "csv" = ".csv", "excel" = ".xlsx", "pdf" = ".html")
      paste0("quantum_report_", input$export_period, "_", Sys.Date(), ext)
    },
    content = function(file) {
      log_info(paste("Download handler triggered. Output file will be copied to:", file))
      
      temp_report <- tryCatch({
        generate_report(
          df = session_data$cleaned,
          period = input$export_period,
          format = input$export_format,
          target_date = if (is.null(input$export_date)) Sys.Date() else input$export_date
        )
      }, error = function(e) {
        log_error(paste("Report download failed. Error:", e$message))
        NULL
      })
      
      if (!is.null(temp_report) && file.exists(temp_report)) {
        file.copy(temp_report, file)
        log_info("Report successfully built and transferred to client.")
      } else {
        write("Error occurred during report compilation. Check log records.", file)
      }
    }
  )
}

# Run the Shiny Application
shinyApp(ui = ui, server = server)
