# generate_sample_sql.R
# Preprocesses the raw sales_data.csv and generates the SQL inserts with mapped IDs to bypass trigger subquery restrictions.

source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/global.R")

library(dplyr)
library(lubridate)

# Helper function to escape SQL string values
escape_str <- function(val) {
  if (is.na(val) || val == "NA" || val == "null" || val == "") {
    return("NULL")
  }
  escaped <- gsub("'", "''", as.character(val))
  paste0("'", escaped, "'")
}

# Base clean data frame
clean_df <- global_cleaned_data

if (nrow(clean_df) == 0) {
  stop("Cleaned data frame is empty. Please verify that data/sales_data.csv is present and valid.")
}

sql_lines <- c(
  "-- sample_data.sql",
  "-- Auto-generated realistic sample data based on preprocessed sales_data.csv",
  "USE ecommerce_sales;",
  "SET FOREIGN_KEY_CHECKS = 0;",
  "TRUNCATE TABLE order_items;",
  "TRUNCATE TABLE orders;",
  "TRUNCATE TABLE products;",
  "TRUNCATE TABLE customers;",
  "TRUNCATE TABLE regions;",
  "TRUNCATE TABLE categories;",
  "SET FOREIGN_KEY_CHECKS = 1;",
  ""
)

# 1. Categories
unique_categories <- unique(as.character(clean_df$Category))
unique_categories <- unique_categories[unique_categories != ""]
categories_df <- data.frame(
  category_id = 1:length(unique_categories),
  category_name = unique_categories,
  stringsAsFactors = FALSE
)
cat_inserts <- sprintf("INSERT INTO categories (category_id, category_name) VALUES (%d, %s);", 
                       categories_df$category_id, sapply(categories_df$category_name, escape_str))
sql_lines <- c(sql_lines, "-- Inserting Categories", cat_inserts, "")

# 2. Regions
unique_regions <- unique(as.character(clean_df$Region))
unique_regions <- unique_regions[unique_regions != ""]
regions_df <- data.frame(
  region_id = 1:length(unique_regions),
  region_name = unique_regions,
  stringsAsFactors = FALSE
)
reg_inserts <- sprintf("INSERT INTO regions (region_id, region_name) VALUES (%d, %s);", 
                       regions_df$region_id, sapply(regions_df$region_name, escape_str))
sql_lines <- c(sql_lines, "-- Inserting Regions", reg_inserts, "")

# 3. Customers
customers_df <- clean_df %>%
  group_by(Customer_ID) %>%
  summarise(
    Customer_Name = first(Customer_Name),
    City = first(City),
    State = first(State),
    .groups = "drop"
  ) %>%
  mutate(customer_id = 1:n())

cust_inserts <- sprintf(
  "INSERT INTO customers (customer_id, customer_code, customer_name, city, state) VALUES (%d, %s, %s, %s, %s);",
  customers_df$customer_id,
  sapply(customers_df$Customer_ID, escape_str),
  sapply(customers_df$Customer_Name, escape_str),
  sapply(customers_df$City, escape_str),
  sapply(customers_df$State, escape_str)
)
sql_lines <- c(sql_lines, "-- Inserting Customers", cust_inserts, "")

# 4. Products
products_df <- clean_df %>%
  group_by(Product) %>%
  summarise(
    Category = first(Category),
    Price = first(Price),
    .groups = "drop"
  ) %>%
  left_join(categories_df, by = c("Category" = "category_name")) %>%
  mutate(product_id = 1:n())

prod_inserts <- sprintf(
  "INSERT INTO products (product_id, product_name, category_id, price, stock) VALUES (%d, %s, %d, %f, 100);",
  products_df$product_id,
  sapply(products_df$Product, escape_str),
  products_df$category_id,
  products_df$Price
)
sql_lines <- c(sql_lines, "-- Inserting Products", prod_inserts, "")

# 5. Orders
orders_df <- clean_df %>%
  group_by(Order_ID) %>%
  summarise(
    Customer_ID = first(Customer_ID),
    Order_Date = first(Order_Date),
    Region = first(Region),
    .groups = "drop"
  ) %>%
  left_join(customers_df, by = c("Customer_ID" = "Customer_ID")) %>%
  left_join(regions_df, by = c("Region" = "region_name")) %>%
  mutate(order_id = 1:n())

order_inserts <- sprintf(
  "INSERT INTO orders (order_id, order_code, customer_id, order_date, region_id) VALUES (%d, %s, %d, %s, %d);",
  orders_df$order_id,
  sapply(orders_df$Order_ID, escape_str),
  orders_df$customer_id,
  sapply(as.character(orders_df$Order_Date), escape_str),
  orders_df$region_id
)
sql_lines <- c(sql_lines, "-- Inserting Orders", order_inserts, "")

# 6. Order Items
# Match keys for order items
order_items_df <- clean_df %>%
  left_join(orders_df, by = "Order_ID") %>%
  left_join(products_df, by = "Product")

item_inserts <- sprintf(
  "INSERT INTO order_items (order_id, product_id, quantity, price_per_unit, sales, profit) VALUES (%d, %d, %d, %f, %f, %f);",
  order_items_df$order_id,
  order_items_df$product_id,
  order_items_df$Quantity,
  order_items_df$Price.y, # Price from product dimension
  order_items_df$Sales,
  order_items_df$Profit
)
sql_lines <- c(sql_lines, "-- Inserting Order Items", item_inserts, "")

# Write to file
writeLines(sql_lines, "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/sample_data.sql")
cat("Successfully generated project/database/sample_data.sql with mapped literal IDs.\n")
