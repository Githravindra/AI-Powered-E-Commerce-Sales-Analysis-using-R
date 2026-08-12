# report_generator.R
# Exposes routines to generate business reports and export in CSV, Excel, or PDF/HTML formats.

library(dplyr)

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Generate and export performance report
#' @param df data.frame. Cleaned transaction dataset.
#' @param period Character. Report frequency ("daily", "monthly", "quarterly", "yearly").
#' @param format Character. File format ("csv", "excel", "pdf").
#' @param target_date Date or Character. Specific date anchor for the report. If NULL, uses the max date in dataset.
#' @return Character path of the generated export file.
generate_report <- function(df, period = "monthly", format = "csv", target_date = NULL) {
  log_info(sprintf("Generating %s report in %s format...", period, format))
  
  if (is.null(df) || nrow(df) == 0) {
    log_error("Cannot generate report from an empty dataset.")
    return(NULL)
  }
  
  # Set target date anchor
  if (is.null(target_date)) {
    anchor_date <- max(df$Order_Date, na.rm = TRUE)
  } else {
    anchor_date <- as.Date(target_date)
  }
  
  anchor_year <- as.numeric(format(anchor_date, "%Y"))
  anchor_month <- as.numeric(format(anchor_date, "%m"))
  
  # Filter data depending on report period
  report_df <- df
  period_label <- ""
  
  if (period == "daily") {
    report_df <- df %>% filter(Order_Date == anchor_date)
    period_label <- sprintf("Daily_%s", as.character(anchor_date))
  } else if (period == "monthly") {
    report_df <- df %>% filter(Year == anchor_year & Month == anchor_month)
    period_label <- sprintf("Monthly_%04d_%02d", anchor_year, anchor_month)
  } else if (period == "quarterly") {
    # Find quarter of target date
    q_val <- paste0("Q", quarter(anchor_date))
    report_df <- df %>% filter(Year == anchor_year & Quarter == q_val)
    period_label <- sprintf("Quarterly_%04d_%s", anchor_year, q_val)
  } else if (period == "yearly") {
    report_df <- df %>% filter(Year == anchor_year)
    period_label <- sprintf("Yearly_%04d", anchor_year)
  }
  
  if (nrow(report_df) == 0) {
    log_warn("No transaction records found for the specified period filters. Using full dataset as a fallback.")
    report_df <- df
    period_label <- "Full_Dataset"
  }
  
  # Source analytics modules if missing
  if (!exists("calculate_kpis")) source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/kpi_engine.R")
  if (!exists("get_top_products")) source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/product_analysis.R")
  if (!exists("get_region_metrics")) source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/region_analysis.R")
  
  # Calculate summaries
  kpis <- calculate_kpis(report_df)
  top_prods <- get_top_products(report_df, n = 5)
  top_regions <- get_region_metrics(report_df)
  
  # Define export folder
  export_dir <- "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/exports"
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  
  filename <- sprintf("%s/sales_report_%s", export_dir, period_label)
  
  # Export based on format
  if (format == "csv") {
    file_path <- paste0(filename, ".csv")
    write.csv(report_df, file_path, row.names = FALSE)
    log_info(paste("Report exported as CSV to:", file_path))
    return(file_path)
    
  } else if (format == "excel") {
    file_path <- paste0(filename, ".xlsx")
    
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      wb <- openxlsx::createWorkbook()
      
      # Sheet 1: Summary Dashboard KPIs
      openxlsx::addWorksheet(wb, "Summary KPIs")
      summary_kpis <- data.frame(
        KPI = c("Total Revenue", "Total Profit", "Gross Profit Margin", "Total Orders", "Average Order Value (AOV)", "Retention Rate"),
        Value = c(
          kpis$Revenue, kpis$Profit, sprintf("%0.2f%%", kpis$ProfitMargin * 100),
          kpis$TotalOrders, kpis$AOV, sprintf("%0.2f%%", kpis$CustomerRetentionRate * 100)
        )
      )
      openxlsx::writeData(wb, "Summary KPIs", summary_kpis)
      
      # Sheet 2: Top Products
      openxlsx::addWorksheet(wb, "Top Products")
      openxlsx::writeData(wb, "Top Products", top_prods)
      
      # Sheet 3: Regional Analytics
      openxlsx::addWorksheet(wb, "Regional Analytics")
      openxlsx::writeData(wb, "Regional Analytics", top_regions)
      
      # Sheet 4: Transactions
      openxlsx::addWorksheet(wb, "Detailed Transactions")
      openxlsx::writeData(wb, "Detailed Transactions", report_df)
      
      openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
      log_info(paste("Report exported as multi-sheet Excel to:", file_path))
      return(file_path)
    } else {
      # Fallback to writing standard CSV
      log_warn("openxlsx package is not available. Exporting detailed transaction list as CSV instead.")
      file_path <- paste0(filename, "_fallback.csv")
      write.csv(report_df, file_path, row.names = FALSE)
      return(file_path)
    }
    
  } else if (format == "pdf") {
    # Generate a gorgeous HTML page which can be saved/printed as PDF
    file_path <- paste0(filename, ".html")
    
    # Render direct HTML content
    html_content <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Sales Performance Report - %s</title>
  <style>
    body { font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; color: #333; line-height: 1.5; padding: 30px; background-color: #f9f9f9; }
    .report-container { max-width: 800px; margin: 0 auto; background: white; padding: 40px; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.05); }
    h1 { color: #1e3a8a; border-bottom: 2px solid #3b82f6; padding-bottom: 10px; font-size: 28px; }
    h2 { color: #2563eb; margin-top: 30px; font-size: 20px; border-bottom: 1px solid #e5e7eb; padding-bottom: 6px; }
    .meta-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 30px; font-size: 14px; color: #666; }
    .kpi-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 15px; margin: 20px 0; }
    .kpi-card { background: #f0fdf4; border: 1px solid #bbf7d0; padding: 15px; border-radius: 8px; text-align: center; }
    .kpi-card.blue { background: #eff6ff; border: 1px solid #bfdbfe; }
    .kpi-card.yellow { background: #fef3c7; border: 1px solid #fde68a; }
    .kpi-title { font-size: 11px; text-transform: uppercase; color: #555; letter-spacing: 0.05em; margin-bottom: 5px; }
    .kpi-value { font-size: 20px; font-weight: bold; color: #111; }
    table { width: 100%%; border-collapse: collapse; margin-top: 15px; font-size: 13px; }
    th { background-color: #f3f4f6; text-align: left; padding: 10px; font-weight: 600; border-bottom: 2px solid #e5e7eb; }
    td { padding: 10px; border-bottom: 1px solid #f3f4f6; }
    tr:hover { background-color: #fafafa; }
    .footer { margin-top: 50px; text-align: center; font-size: 11px; color: #999; border-top: 1px solid #e5e7eb; padding-top: 15px; }
  </style>
</head>
<body>
  <div class="report-container">
    <h1>Sales Performance Report</h1>
    <div class="meta-grid">
      <div><strong>Report Scope:</strong> %s Performance</div>
      <div><strong>Generated Time:</strong> %s</div>
      <div><strong>Record Volume:</strong> %d Ingested Transactions</div>
      <div><strong>Reporting Currency:</strong> INR (₹)</div>
    </div>
    
    <h2>Operational KPI Dashboard</h2>
    <div class="kpi-grid">
      <div class="kpi-card blue">
        <div class="kpi-title">Gross Sales Revenue</div>
        <div class="kpi-value">₹%s</div>
      </div>
      <div class="kpi-card">
        <div class="kpi-title">Gross Operating Profit</div>
        <div class="kpi-value">₹%s</div>
      </div>
      <div class="kpi-card yellow">
        <div class="kpi-title">Average Profit Margin</div>
        <div class="kpi-value">%0.2f%%</div>
      </div>
      <div class="kpi-card blue">
        <div class="kpi-title">Total Processed Orders</div>
        <div class="kpi-value">%d</div>
      </div>
      <div class="kpi-card">
        <div class="kpi-title">Average Order Value (AOV)</div>
        <div class="kpi-value">₹%s</div>
      </div>
      <div class="kpi-card yellow">
        <div class="kpi-title">Customer Retention Rate</div>
        <div class="kpi-value">%0.2f%%</div>
      </div>
    </div>
    
    <h2>Top Performing Categories & Regions</h2>
    <table>
      <thead>
        <tr>
          <th>Region</th>
          <th>Sales Volume</th>
          <th>Profit Amount</th>
          <th>Profit Margin</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>
    
    <h2>Top 5 High-Value Products</h2>
    <table>
      <thead>
        <tr>
          <th>Product Name</th>
          <th>Category</th>
          <th>Quantity Sold</th>
          <th>Revenue Generated</th>
          <th>Total Profit</th>
        </tr>
      </thead>
      <tbody>
        %s
      </tbody>
    </table>
    
    <div class="footer">
      QuantumSales Enterprise Analytics Platform &copy; 2026. Confidential Business Document.
    </div>
  </div>
</body>
</html>
',
    period_label,
    toupper(period),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    nrow(report_df),
    format(round(kpis$Revenue), big.mark = ","),
    format(round(kpis$Profit), big.mark = ","),
    kpis$ProfitMargin * 100,
    kpis$TotalOrders,
    format(round(kpis$AOV), big.mark = ","),
    kpis$CustomerRetentionRate * 100,
    # Inject Region Rows
    paste(sapply(1:nrow(top_regions), function(r) {
      sprintf("<tr><td>%s</td><td>₹%s</td><td>₹%s</td><td>%0.1f%%</td></tr>", 
              top_regions$Region[r], 
              format(round(top_regions$Sales[r]), big.mark=","), 
              format(round(top_regions$Profit[r]), big.mark=","),
              top_regions$Profit_Margin[r] * 100)
    }), collapse="\n"),
    # Inject Product Rows
    paste(sapply(1:nrow(top_prods), function(p) {
      sprintf("<tr><td>%s</td><td>%s</td><td>%d units</td><td>₹%s</td><td>₹%s</td></tr>", 
              top_prods$Product[p], top_prods$Category[p], top_prods$Quantity[p],
              format(round(top_prods$Sales[p]), big.mark=","), 
              format(round(top_prods$Profit[p]), big.mark=","))
    }), collapse="\n")
    )
    
    write(html_content, file = file_path)
    log_info(paste("Report exported as print-ready HTML to:", file_path))
    
    # We return the HTML path. In a server environment, the user can download this file, and the browser displays a beautiful print dashboard.
    return(file_path)
  }
  
  NULL
}
