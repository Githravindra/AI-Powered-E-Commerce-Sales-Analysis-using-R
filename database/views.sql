-- views.sql
-- Define SQL Views for reporting and application loading

USE ecommerce_sales;

-- -----------------------------------------------------
-- View: vw_sales_summary
-- Reconstructs the original flat dataset schema by joining normalized tables
-- -----------------------------------------------------
CREATE OR REPLACE VIEW vw_sales_summary AS
SELECT 
    o.order_code AS Order_ID,
    o.order_date AS Order_Date,
    cust.customer_code AS Customer_ID,
    cust.customer_name AS Customer_Name,
    p.product_name AS Product,
    cat.category_name AS Category,
    oi.quantity AS Quantity,
    oi.price_per_unit AS Price,
    oi.sales AS Sales,
    oi.profit AS Profit,
    cust.city AS City,
    cust.state AS State,
    reg.region_name AS Region
FROM order_items oi
JOIN orders o ON oi.order_id = o.order_id
JOIN customers cust ON o.customer_id = cust.customer_id
JOIN products p ON oi.product_id = p.product_id
JOIN categories cat ON p.category_id = cat.category_id
JOIN regions reg ON o.region_id = reg.region_id;

-- -----------------------------------------------------
-- View: vw_customer_summary
-- Aggregates purchase history and value statistics by customer
-- -----------------------------------------------------
CREATE OR REPLACE VIEW vw_customer_summary AS
SELECT 
    cust.customer_id AS id,
    cust.customer_code AS Customer_ID,
    cust.customer_name AS Customer_Name,
    cust.city AS City,
    cust.state AS State,
    COUNT(DISTINCT o.order_id) AS Total_Orders,
    IFNULL(SUM(oi.sales), 0.00) AS Total_Sales,
    IFNULL(SUM(oi.profit), 0.00) AS Total_Profit,
    IFNULL(SUM(oi.sales) / COUNT(DISTINCT o.order_id), 0.00) AS Avg_Order_Value
FROM customers cust
LEFT JOIN orders o ON cust.customer_id = o.customer_id
LEFT JOIN order_items oi ON o.order_id = oi.order_id
GROUP BY cust.customer_id, cust.customer_code, cust.customer_name, cust.city, cust.state;

-- -----------------------------------------------------
-- View: vw_product_summary
-- Summarizes sales, profit margin, units sold, and current stock for each product
-- -----------------------------------------------------
CREATE OR REPLACE VIEW vw_product_summary AS
SELECT 
    p.product_id,
    p.product_name AS Product,
    cat.category_name AS Category,
    p.price AS List_Price,
    p.stock AS Current_Stock,
    IFNULL(SUM(oi.quantity), 0) AS Total_Units_Sold,
    IFNULL(SUM(oi.sales), 0.00) AS Total_Sales,
    IFNULL(SUM(oi.profit), 0.00) AS Total_Profit
FROM products p
JOIN categories cat ON p.category_id = cat.category_id
LEFT JOIN order_items oi ON p.product_id = oi.product_id
GROUP BY p.product_id, p.product_name, cat.category_name, p.price, p.stock;

-- -----------------------------------------------------
-- View: vw_monthly_sales
-- Aggregates transactional performance metrics by calendar month
-- -----------------------------------------------------
CREATE OR REPLACE VIEW vw_monthly_sales AS
SELECT 
    YEAR(o.order_date) AS Year,
    MONTH(o.order_date) AS Month,
    DATE_FORMAT(o.order_date, '%b') AS Month_Name,
    SUM(oi.sales) AS Monthly_Sales,
    SUM(oi.profit) AS Monthly_Profit,
    COUNT(DISTINCT o.order_id) AS Total_Orders
FROM orders o
JOIN order_items oi ON o.order_id = oi.order_id
GROUP BY YEAR(o.order_date), MONTH(o.order_date), DATE_FORMAT(o.order_date, '%b')
ORDER BY Year DESC, Month DESC;
