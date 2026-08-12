# verify_db.R
# Diagnostic script to verify MySQL connection, views, stored procedures,
# triggers, and R CRUD query operations in the developer workspace.

# Source database modules
source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/db_connection.R")
source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/queries.R")

cat("=========================================================\n")
cat("      MYSQL DATABASE INTEGRATION VERIFICATION TEST       \n")
cat("=========================================================\n\n")

# Verify connection parameters
cat("Connecting to Database...\n")
conn <- connect_db()
if (is.null(conn)) {
  cat("\n[-] FAILED: Could not connect to MySQL server.\n")
  cat("TROUBLESHOOTING:\n")
  cat(" 1. Verify that XAMPP is running and the MySQL module is started.\n")
  cat(" 2. Verify that you have built the database using the SQL scripts:\n")
  cat("    - schema.sql\n")
  cat("    - sample_data.sql\n")
  cat("    - views.sql\n")
  cat("    - procedures.sql\n")
  cat("    - triggers.sql\n")
  cat(" 3. Verify connection configuration options inside project/database/db_connection.R.\n\n")
  q(status = 1)
}

# Close connection since queries manage their own session context
close_db(conn)
cat("[+] PASSED: Connected to MySQL database successfully!\n\n")

cat("--- Running Data Queries ---\n")

# 1. Test get_all_sales
df_sales <- get_all_sales()
cat(sprintf(" - get_all_sales(): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_sales), "SUCCESS", "FAILED"), nrow(df_sales)))

# 2. Test get_sales_by_date
df_date <- get_sales_by_date("2024-01-01", "2024-12-31")
cat(sprintf(" - get_sales_by_date(): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_date), "SUCCESS", "FAILED"), nrow(df_date)))

# 3. Test get_sales_by_category
df_cat <- get_sales_by_category("Electronics")
cat(sprintf(" - get_sales_by_category('Electronics'): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_cat), "SUCCESS", "FAILED"), nrow(df_cat)))

# 4. Test get_sales_by_region
df_reg <- get_sales_by_region("East")
cat(sprintf(" - get_sales_by_region('East'): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_reg), "SUCCESS", "FAILED"), nrow(df_reg)))

# 5. Test top products stored procedure
df_top_prod <- get_top_products_db(5)
cat(sprintf(" - get_top_products_db(5): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_top_prod), "SUCCESS", "FAILED"), nrow(df_top_prod)))

# 6. Test top customers stored procedure
df_top_cust <- get_top_customers_db(5)
cat(sprintf(" - get_top_customers_db(5): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_top_cust), "SUCCESS", "FAILED"), nrow(df_top_cust)))

# 7. Test monthly sales stored procedure
df_monthly <- get_monthly_sales()
cat(sprintf(" - get_monthly_sales(): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_monthly), "SUCCESS", "FAILED"), nrow(df_monthly)))

# 8. Test yearly sales view query
df_yearly <- get_yearly_sales()
cat(sprintf(" - get_yearly_sales(): %s (%d rows fetched)\n", 
            ifelse(is.data.frame(df_yearly), "SUCCESS", "FAILED"), nrow(df_yearly)))


cat("\n--- Running CRUD Writes & Triggers ---\n")

# 9. Insert Customer
c_success <- insert_customer("TEST-CUST", "Test Integration User", "Boston", "Massachusetts")
cat(sprintf(" - insert_customer(): %s\n", ifelse(c_success, "SUCCESS", "FAILED")))

# 10. Insert Category
cat_success <- insert_category("Custom Category")
cat(sprintf(" - insert_category(): %s\n", ifelse(cat_success, "SUCCESS", "FAILED")))

# 11. Insert Product
p_success <- insert_product("Custom Test Product", "Custom Category", 450.00, 250)
cat(sprintf(" - insert_product(): %s\n", ifelse(p_success, "SUCCESS", "FAILED")))

# 12. Insert Order (calls InsertOrder Stored Procedure & triggers stock deduction)
o_success <- insert_order(
  order_code = "TEST-ORDER",
  order_date = "2026-06-26",
  customer_code = "TEST-CUST",
  customer_name = "Test Integration User",
  city = "Boston",
  state = "Massachusetts",
  product_name = "Custom Test Product",
  category_name = "Custom Category",
  quantity = 10,
  price = 450.00,
  sales = 4500.00,
  profit = 1350.00,
  region_name = "East"
)
cat(sprintf(" - insert_order() [Stored Proc]: %s\n", ifelse(o_success, "SUCCESS", "FAILED")))

# Verify stock decrement trigger worked
if (o_success) {
  prod_check <- get_all_sales()
  # Verify stock of our test product is updated
  with_db_connection({
    res <- DBI::dbGetQuery(conn, "SELECT stock FROM products WHERE product_name = 'Custom Test Product';")
    if (nrow(res) > 0) {
      cat(sprintf("   * Stock decrement trigger check: Stock is now %d (Expected: 240, started at 250)\n", res$stock[1]))
    }
  })
}

# 13. Update Order item
u_success <- update_order(
  order_code = "TEST-ORDER",
  product_name = "Custom Test Product",
  quantity = 15,
  price = 450.00,
  sales = 6750.00,
  profit = 2025.00
)
cat(sprintf(" - update_order(): %s\n", ifelse(u_success, "SUCCESS", "FAILED")))

# 14. Delete Order (triggers audit logger deleted_orders_log)
d_success <- delete_order("TEST-ORDER")
cat(sprintf(" - delete_order() [Cascading & Audit Log]: %s\n", ifelse(d_success, "SUCCESS", "FAILED")))

# Verify deletion audit log trigger worked
if (d_success) {
  with_db_connection({
    res <- DBI::dbGetQuery(conn, "SELECT * FROM deleted_orders_log WHERE order_code = 'TEST-ORDER';")
    if (nrow(res) > 0) {
      cat(sprintf("   * Audit trigger log check: Row successfully added in deleted_orders_log (deleted at %s)\n", res$deleted_at[1]))
    }
  })
}

# Clean up helper products / customers
with_db_connection({
  DBI::dbExecute(conn, "DELETE FROM customers WHERE customer_code = 'TEST-CUST';")
  DBI::dbExecute(conn, "DELETE FROM products WHERE product_name = 'Custom Test Product';")
  DBI::dbExecute(conn, "DELETE FROM categories WHERE category_name = 'Custom Category';")
})

cat("\n=========================================================\n")
cat("            VERIFICATION COMPLETE                        \n")
cat("=========================================================\n")
