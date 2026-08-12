# generate_data.R
# Generate simulated E-commerce transactions with deliberate anomalies for data cleansing demonstration

set.seed(42)

# Locations pool
locations <- list(
  list(city = "New York", state = "New York", region = "East"),
  list(city = "Boston", state = "Massachusetts", region = "East"),
  list(city = "Philadelphia", state = "Pennsylvania", region = "East"),
  list(city = "Los Angeles", state = "California", region = "West"),
  list(city = "San Francisco", state = "California", region = "West"),
  list(city = "Seattle", state = "Washington", region = "West"),
  list(city = "Chicago", state = "Illinois", region = "Central"),
  list(city = "Houston", state = "Texas", region = "Central"),
  list(city = "Dallas", state = "Texas", region = "Central"),
  list(city = "Atlanta", state = "Georgia", region = "South"),
  list(city = "Miami", state = "Florida", region = "South"),
  list(city = "Nashville", state = "Tennessee", region = "South")
)

# Products pool
products <- list(
  list(product = "iPhone 15 Pro", category = "Electronics", price = 82900),
  list(product = "Dell XPS 15", category = "Electronics", price = 124000),
  list(product = "Sony WH-1000XM5", category = "Electronics", price = 33000),
  list(product = "iPad Air", category = "Electronics", price = 49000),
  list(product = "Ergonomic Office Chair", category = "Furniture", price = 20500),
  list(product = "Solid Oak Dining Table", category = "Furniture", price = 66000),
  list(product = "Modern Sofa", category = "Furniture", price = 74500),
  list(product = "LED Desk Lamp", category = "Furniture", price = 4000),
  list(product = "Cotton Slim Fit T-Shirt", category = "Clothing", price = 2000),
  list(product = "Waterproof Leather Jacket", category = "Clothing", price = 14900),
  list(product = "Running Shoes", category = "Clothing", price = 9900),
  list(product = "Denim Jeans", category = "Clothing", price = 4900),
  list(product = "Premium Notebook Pack", category = "Office Supplies", price = 1200),
  list(product = "Gel Pen Set (12-pack)", category = "Office Supplies", price = 900),
  list(product = "Heavy Duty Stapler", category = "Office Supplies", price = 1800),
  list(product = "Dry Erase Whiteboard", category = "Office Supplies", price = 3700)
)

# Customers pool
customers <- data.frame(
  id = paste0("CUST-", 101:150),
  name = c(
    "John Doe", "Jane Smith", "Michael Johnson", "Emily Davis", "William Brown",
    "Olivia Jones", "James Garcia", "Sophia Martinez", "Benjamin Miller", "Isabella Wilson",
    "Lucas Anderson", "Mia Thomas", "Alexander Taylor", "Charlotte Moore", "Ethan Jackson",
    "Amelia Martin", "Daniel Lee", "Harper Thompson", "Matthew White", "Evelyn Harris",
    "Logan Martin", "Abigail Clark", "David Rodriguez", "Elizabeth Lewis", "Joseph Lee",
    "Avery Walker", "Jackson Hall", "Sofia Allen", "Sebastian Young", "Ella Hernandez",
    "Jack King", "Scarlett Wright", "Luke Lopez", "Grace Hill", "Owen Scott",
    "Victoria Green", "Gabriel Adams", "Chloe Baker", "Carter Gonzalez", "Camila Nelson",
    "Henry Carter", "Penelope Mitchell", "Wyatt Perez", "Layla Roberts", "Matthew Turner",
    "Lillian Phillips", "Daniel Campbell", "Nora Parker", "Julian Evans", "Zoe Edwards"
  ),
  stringsAsFactors = FALSE
)

# Number of transactions to generate
n_records <- 1000

# Generating baseline dataset
order_ids <- paste0("EC-", 1000 + 1:n_records)

# Dates spanning Jan 1, 2024 to June 15, 2026
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2026-06-15")
dates <- seq(start_date, end_date, by="day")
order_dates <- sample(dates, n_records, replace = TRUE)

# Select random customers, products, and locations
cust_indices <- sample(1:nrow(customers), n_records, replace = TRUE)
prod_indices <- sample(1:length(products), n_records, replace = TRUE)
loc_indices <- sample(1:length(locations), n_records, replace = TRUE)

quantities <- sample(1:5, n_records, replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.1, 0.05))

df <- data.frame(
  Order_ID = order_ids,
  Order_Date = as.character(order_dates),
  Customer_ID = customers$id[cust_indices],
  Customer_Name = customers$name[cust_indices],
  Product = sapply(prod_indices, function(i) products[[i]]$product),
  Category = sapply(prod_indices, function(i) products[[i]]$category),
  Quantity = quantities,
  Price = sapply(prod_indices, function(i) products[[i]]$price),
  stringsAsFactors = FALSE
)

df$Sales <- df$Quantity * df$Price

# Compute profit margins based on category
margins <- sapply(df$Category, function(cat) {
  if (cat == "Electronics") {
    runif(1, 0.12, 0.35)
  } else if (cat == "Furniture") {
    runif(1, -0.15, 0.25) # Furniture can have losses
  } else if (cat == "Clothing") {
    runif(1, 0.25, 0.55)
  } else { # Office Supplies
    runif(1, 0.35, 0.65)
  }
})
df$Profit <- round(df$Sales * margins, 2)

df$City <- sapply(loc_indices, function(i) locations[[i]]$city)
df$State <- sapply(loc_indices, function(i) locations[[i]]$state)
df$Region <- sapply(loc_indices, function(i) locations[[i]]$region)

# Seed intentional anomalies for preprocessing test
# 1. Missing Profit (15 records)
missing_profit_idx <- sample(1:n_records, 15)
df$Profit[missing_profit_idx] <- NA

# 2. Missing Customer Name (8 records)
missing_name_idx <- sample(setdiff(1:n_records, missing_profit_idx), 8)
df$Customer_Name[missing_name_idx] <- NA

# 3. Missing City (5 records)
missing_city_idx <- sample(setdiff(1:n_records, c(missing_profit_idx, missing_name_idx)), 5)
df$City[missing_city_idx] <- ""

# 4. Invalid price or quantity (5 records)
invalid_idx <- sample(setdiff(1:n_records, c(missing_profit_idx, missing_name_idx, missing_city_idx)), 5)
df$Quantity[invalid_idx[1:2]] <- -1
df$Price[invalid_idx[3:5]] <- 0
df$Sales[invalid_idx[3:5]] <- 0
df$Profit[invalid_idx[3:5]] <- 0

# 5. Duplicates (3 duplicate rows)
dup_idx <- sample(1:n_records, 3)
duplicates <- df[dup_idx, ]
df <- rbind(df, duplicates)

# Write to project/data/sales_data.csv
dir.create("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/data", recursive = TRUE, showWarnings = FALSE)
write.csv(df, "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/data/sales_data.csv", row.names = FALSE)
cat("Baseline dataset successfully generated with deliberate anomalies at project/data/sales_data.csv\n")
