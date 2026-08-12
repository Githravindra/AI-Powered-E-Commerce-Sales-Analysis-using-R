# queries.R
# Reusable Database Query and CRUD functions using prepared statements and transaction control.
# Includes error handling with logging and automatic connection pooling.

# Ensure db_connection.R and logging.R are sourced
if (!exists("connect_db")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/db_connection.R")
}

# ==============================================================================
# READ QUERIES (Data Fetching APIs)
# ==============================================================================

#' Get All Sales Data
#' Reads all records from the vw_sales_summary SQL View
#' @return data.frame containing the flat transaction records.
get_all_sales <- function() {
  log_info("Query: get_all_sales() triggered.")
  result <- with_db_connection({
    tryCatch({
      df <- DBI::dbGetQuery(conn, "SELECT * FROM vw_sales_summary;")
      log_info(sprintf("Successfully fetched %d sales transactions.", nrow(df)))
      df
    }, error = function(e) {
      log_error(paste("get_all_sales failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Sales By Date Range
#' @param start_date Character or Date. Start date in YYYY-MM-DD format.
#' @param end_date Character or Date. End date in YYYY-MM-DD format.
#' @return data.frame containing transactions matching the range.
get_sales_by_date <- function(start_date, end_date) {
  log_info(sprintf("Query: get_sales_by_date(start_date='%s', end_date='%s') triggered.", start_date, end_date))
  result <- with_db_connection({
    tryCatch({
      sd <- as.character(start_date)
      ed <- as.character(end_date)
      res <- DBI::dbSendQuery(conn, "SELECT * FROM vw_sales_summary WHERE Order_Date BETWEEN ? AND ?;")
      DBI::dbBind(res, list(sd, ed))
      df <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      log_info(sprintf("Successfully fetched %d sales records for date range.", nrow(df)))
      df
    }, error = function(e) {
      log_error(paste("get_sales_by_date failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Sales By Product Category
#' @param category Character. Category name filter.
#' @return data.frame containing transactions matching the category.
get_sales_by_category <- function(category) {
  log_info(sprintf("Query: get_sales_by_category(category='%s') triggered.", category))
  result <- with_db_connection({
    tryCatch({
      res <- DBI::dbSendQuery(conn, "SELECT * FROM vw_sales_summary WHERE Category = ?;")
      DBI::dbBind(res, list(as.character(category)))
      df <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      log_info(sprintf("Successfully fetched %d records for category.", nrow(df)))
      df
    }, error = function(e) {
      log_error(paste("get_sales_by_category failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Sales By Region
#' @param region Character. Region name filter.
#' @return data.frame containing transactions matching the region.
get_sales_by_region <- function(region) {
  log_info(sprintf("Query: get_sales_by_region(region='%s') triggered.", region))
  result <- with_db_connection({
    tryCatch({
      res <- DBI::dbSendQuery(conn, "SELECT * FROM vw_sales_summary WHERE Region = ?;")
      DBI::dbBind(res, list(as.character(region)))
      df <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      log_info(sprintf("Successfully fetched %d records for region.", nrow(df)))
      df
    }, error = function(e) {
      log_error(paste("get_sales_by_region failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Top Products By Sales Volume (Database version)
#' Calls the TopProducts stored procedure
#' @param limit Integer. Number of records to return.
#' @return data.frame containing product sales summary.
get_top_products_db <- function(limit = 10) {
  log_info(sprintf("Query: get_top_products_db(limit=%d) triggered.", limit))
  result <- with_db_connection({
    tryCatch({
      res <- DBI::dbSendQuery(conn, "CALL TopProducts(?);")
      DBI::dbBind(res, list(as.integer(limit)))
      df <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      df
    }, error = function(e) {
      log_error(paste("get_top_products_db failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Top Customers By Sales Volume (Database version)
#' Calls the TopCustomers stored procedure
#' @param limit Integer. Number of records to return.
#' @return data.frame containing customer purchasing summaries.
get_top_customers_db <- function(limit = 10) {
  log_info(sprintf("Query: get_top_customers_db(limit=%d) triggered.", limit))
  result <- with_db_connection({
    tryCatch({
      res <- DBI::dbSendQuery(conn, "CALL TopCustomers(?);")
      DBI::dbBind(res, list(as.integer(limit)))
      df <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      df
    }, error = function(e) {
      log_error(paste("get_top_customers_db failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Monthly Sales Summaries
#' Calls the MonthlySales stored procedure
#' @return data.frame containing sales aggregations grouped by calendar month.
get_monthly_sales <- function() {
  log_info("Query: get_monthly_sales() triggered.")
  result <- with_db_connection({
    tryCatch({
      res <- DBI::dbSendQuery(conn, "CALL MonthlySales();")
      df <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      df
    }, error = function(e) {
      log_error(paste("get_monthly_sales failed:", e$message))
      data.frame()
    })
  })
  return(result)
}

#' Get Yearly Sales Summaries
#' Groups the SQL view data by year
#' @return data.frame with Year, Yearly_Sales, and Yearly_Profit.
get_yearly_sales <- function() {
  log_info("Query: get_yearly_sales() triggered.")
  result <- with_db_connection({
    tryCatch({
      df <- DBI::dbGetQuery(conn, "SELECT YEAR(Order_Date) AS Year, SUM(Sales) AS Yearly_Sales, SUM(Profit) AS Yearly_Profit FROM vw_sales_summary GROUP BY YEAR(Order_Date) ORDER BY Year DESC;")
      df
    }, error = function(e) {
      log_error(paste("get_yearly_sales failed:", e$message))
      data.frame()
    })
  })
  return(result)
}


# ==============================================================================
# WRITE OPERATIONS (CRUD APIs with Transaction and SQL Injection Protection)
# ==============================================================================

# --- CUSTOMER CRUD ---

#' Insert a New Customer
#' @param customer_code Character. Code identifying the client (e.g. CUST-123).
#' @param customer_name Character. Name of the customer.
#' @param city Character. City.
#' @param state Character. State.
#' @return Logical. TRUE on success, FALSE otherwise.
insert_customer <- function(customer_code, customer_name, city, state) {
  log_info(sprintf("CRUD: insert_customer(code='%s', name='%s') triggered.", customer_code, customer_name))
  result <- with_db_connection({
    DBI::dbBegin(conn)
    success <- tryCatch({
      res <- DBI::dbSendStatement(conn, "INSERT INTO customers (customer_code, customer_name, city, state) VALUES (?, ?, ?, ?);")
      DBI::dbBind(res, list(as.character(customer_code), as.character(customer_name), as.character(city), as.character(state)))
      DBI::dbClearResult(res)
      DBI::dbCommit(conn)
      log_info("Customer inserted successfully.")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      log_error(paste("insert_customer failed:", e$message))
      FALSE
    })
    success
  })
  return(result)
}

# --- CATEGORY CRUD ---

#' Insert a New Product Category
#' @param category_name Character. Name of the category.
#' @return Logical. TRUE on success, FALSE otherwise.
insert_category <- function(category_name) {
  log_info(sprintf("CRUD: insert_category(name='%s') triggered.", category_name))
  result <- with_db_connection({
    DBI::dbBegin(conn)
    success <- tryCatch({
      res <- DBI::dbSendStatement(conn, "INSERT INTO categories (category_name) VALUES (?);")
      DBI::dbBind(res, list(as.character(category_name)))
      DBI::dbClearResult(res)
      DBI::dbCommit(conn)
      log_info("Category inserted successfully.")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      log_error(paste("insert_category failed:", e$message))
      FALSE
    })
    success
  })
  return(result)
}

# --- PRODUCT CRUD ---

#' Insert a New Product
#' @param product_name Character. Product designation.
#' @param category_name Character. Category.
#' @param price Numeric. Selling price.
#' @param stock Integer. Initial inventory levels.
#' @return Logical. TRUE on success, FALSE otherwise.
insert_product <- function(product_name, category_name, price, stock = 100) {
  log_info(sprintf("CRUD: insert_product(name='%s', price=%f) triggered.", product_name, price))
  result <- with_db_connection({
    DBI::dbBegin(conn)
    success <- tryCatch({
      # Resolve category_id or insert new category if missing
      cat_res <- DBI::dbGetQuery(conn, "SELECT category_id FROM categories WHERE category_name = ?;", params = list(as.character(category_name)))
      if (nrow(cat_res) == 0) {
        res_cat <- DBI::dbSendStatement(conn, "INSERT INTO categories (category_name) VALUES (?);")
        DBI::dbBind(res_cat, list(as.character(category_name)))
        DBI::dbClearResult(res_cat)
        cat_res <- DBI::dbGetQuery(conn, "SELECT category_id FROM categories WHERE category_name = ?;", params = list(as.character(category_name)))
      }
      cat_id <- cat_res$category_id[1]
      
      res_prod <- DBI::dbSendStatement(conn, "INSERT INTO products (product_name, category_id, price, stock) VALUES (?, ?, ?, ?);")
      DBI::dbBind(res_prod, list(as.character(product_name), as.integer(cat_id), as.numeric(price), as.integer(stock)))
      DBI::dbClearResult(res_prod)
      
      DBI::dbCommit(conn)
      log_info("Product inserted successfully.")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      log_error(paste("insert_product failed:", e$message))
      FALSE
    })
    success
  })
  return(result)
}

# --- ORDER CRUD ---

#' Insert a Complete Order (Transactional, utilizes stored procedure)
#' @param order_code Character. Order code (e.g. EC-1001).
#' @param order_date Character or Date. Order placement date.
#' @param customer_code Character. Customer unique code.
#' @param customer_name Character. Customer display name.
#' @param city Character. Location city.
#' @param state Character. Location state.
#' @param product_name Character. Item ordered.
#' @param category_name Character. Product category.
#' @param quantity Integer. Quantity purchased.
#' @param price Numeric. Unit price.
#' @param sales Numeric. Total sales amount.
#' @param profit Numeric. Profit generated.
#' @param region_name Character. Region of order.
#' @return Logical. TRUE on success, FALSE otherwise.
insert_order <- function(order_code, order_date, customer_code, customer_name, city, state, 
                         product_name, category_name, quantity, price, sales, profit, region_name) {
  log_info(sprintf("CRUD: insert_order(code='%s', customer='%s', product='%s') triggered.", order_code, customer_code, product_name))
  result <- with_db_connection({
    DBI::dbBegin(conn)
    success <- tryCatch({
      res <- DBI::dbSendStatement(conn, "CALL InsertOrder(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      DBI::dbBind(res, list(
        as.character(order_code),
        as.character(order_date),
        as.character(customer_code),
        as.character(customer_name),
        as.character(city),
        as.character(state),
        as.character(product_name),
        as.character(category_name),
        as.integer(quantity),
        as.numeric(price),
        as.numeric(sales),
        as.numeric(profit),
        as.character(region_name)
      ))
      DBI::dbClearResult(res)
      DBI::dbCommit(conn)
      log_info("Order details inserted successfully (Trigger auto-updated product stock).")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      log_error(paste("insert_order failed:", e$message))
      FALSE
    })
    success
  })
  return(result)
}

#' Update Order Line Item Details
#' @param order_code Character. Target order code.
#' @param product_name Character. Target product.
#' @param quantity Integer. Quantity.
#' @param price Numeric. Price per unit.
#' @param sales Numeric. Sales total.
#' @param profit Numeric. Net profit.
#' @return Logical. TRUE on success, FALSE otherwise.
update_order <- function(order_code, product_name, quantity, price, sales, profit) {
  log_info(sprintf("CRUD: update_order(code='%s', product='%s') triggered.", order_code, product_name))
  result <- with_db_connection({
    DBI::dbBegin(conn)
    success <- tryCatch({
      # 1. Resolve order_id and product_id references
      order_res <- DBI::dbGetQuery(conn, "SELECT order_id FROM orders WHERE order_code = ?;", params = list(as.character(order_code)))
      prod_res <- DBI::dbGetQuery(conn, "SELECT product_id FROM products WHERE product_name = ?;", params = list(as.character(product_name)))
      
      if (nrow(order_res) == 0) {
        stop(sprintf("Target Order code '%s' does not exist in orders database.", order_code))
      }
      if (nrow(prod_res) == 0) {
        stop(sprintf("Target Product name '%s' does not exist in products catalog.", product_name))
      }
      
      o_id <- order_res$order_id[1]
      p_id <- prod_res$product_id[1]
      
      # 2. Update line item details
      res <- DBI::dbSendStatement(conn, "UPDATE order_items SET quantity = ?, price_per_unit = ?, sales = ?, profit = ? WHERE order_id = ? AND product_id = ?;")
      DBI::dbBind(res, list(as.integer(quantity), as.numeric(price), as.numeric(sales), as.numeric(profit), as.integer(o_id), as.integer(p_id)))
      DBI::dbClearResult(res)
      
      DBI::dbCommit(conn)
      log_info("Order item updated successfully.")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      log_error(paste("update_order failed:", e$message))
      FALSE
    })
    success
  })
  return(result)
}

#' Delete an Order
#' Deletes the parent order record (triggers child cascading deletion and deleted audit logging)
#' @param order_code Character. Order code to delete.
#' @return Logical. TRUE on success, FALSE otherwise.
delete_order <- function(order_code) {
  log_info(sprintf("CRUD: delete_order(code='%s') triggered.", order_code))
  result <- with_db_connection({
    DBI::dbBegin(conn)
    success <- tryCatch({
      res <- DBI::dbSendStatement(conn, "DELETE FROM orders WHERE order_code = ?;")
      DBI::dbBind(res, list(as.character(order_code)))
      DBI::dbClearResult(res)
      DBI::dbCommit(conn)
      log_info("Order deleted successfully (Trigger logged deletion event to deleted_orders_log).")
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      log_error(paste("delete_order failed:", e$message))
      FALSE
    })
    success
  })
  return(result)
}
