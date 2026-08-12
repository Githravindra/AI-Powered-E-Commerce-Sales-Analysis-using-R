-- procedures.sql
-- Define Stored Procedures for operations and analytics

USE ecommerce_sales;

-- Drop procedures if they already exist
DROP PROCEDURE IF EXISTS InsertOrder;
DROP PROCEDURE IF EXISTS MonthlySales;
DROP PROCEDURE IF EXISTS TopProducts;
DROP PROCEDURE IF EXISTS TopCustomers;

DELIMITER //

-- -----------------------------------------------------
-- Procedure: InsertOrder
-- Inserts order header and item info within a transactional context,
-- resolving or creating entity records dynamically.
-- -----------------------------------------------------
CREATE PROCEDURE InsertOrder(
    IN p_order_code VARCHAR(50),
    IN p_order_date DATE,
    IN p_customer_code VARCHAR(50),
    IN p_customer_name VARCHAR(255),
    IN p_city VARCHAR(100),
    IN p_state VARCHAR(100),
    IN p_product_name VARCHAR(255),
    IN p_category_name VARCHAR(100),
    IN p_quantity INT,
    IN p_price_per_unit DECIMAL(12, 2),
    IN p_sales DECIMAL(12, 2),
    IN p_profit DECIMAL(12, 2),
    IN p_region_name VARCHAR(100)
)
BEGIN
    DECLARE v_customer_id INT;
    DECLARE v_category_id INT;
    DECLARE v_product_id INT;
    DECLARE v_region_id INT;
    DECLARE v_order_id INT;
    
    -- Error rollback handler
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
        ROLLBACK;
        RESIGNAL;
    END;

    START TRANSACTION;
    
    -- 1. Get or Create Region reference
    SELECT region_id INTO v_region_id FROM regions WHERE region_name = p_region_name LIMIT 1;
    IF v_region_id IS NULL THEN
        INSERT INTO regions (region_name) VALUES (p_region_name);
        SET v_region_id = LAST_INSERT_ID();
    END IF;
    
    -- 2. Get or Create Customer profile
    SELECT customer_id INTO v_customer_id FROM customers WHERE customer_code = p_customer_code LIMIT 1;
    IF v_customer_id IS NULL THEN
        INSERT INTO customers (customer_code, customer_name, city, state) 
        VALUES (p_customer_code, p_customer_name, p_city, p_state);
        SET v_customer_id = LAST_INSERT_ID();
    ELSE
        -- Sync customer data if it exists
        UPDATE customers 
        SET customer_name = p_customer_name, city = p_city, state = p_state
        WHERE customer_id = v_customer_id;
    END IF;
    
    -- 3. Get or Create Category
    SELECT category_id INTO v_category_id FROM categories WHERE category_name = p_category_name LIMIT 1;
    IF v_category_id IS NULL THEN
        INSERT INTO categories (category_name) VALUES (p_category_name);
        SET v_category_id = LAST_INSERT_ID();
    END IF;
    
    -- 4. Get or Create Product
    SELECT product_id INTO v_product_id FROM products WHERE product_name = p_product_name LIMIT 1;
    IF v_product_id IS NULL THEN
        INSERT INTO products (product_name, category_id, price, stock) 
        VALUES (p_product_name, v_category_id, p_price_per_unit, 100);
        SET v_product_id = LAST_INSERT_ID();
    ELSE
        -- Update product price to current
        UPDATE products 
        SET price = p_price_per_unit 
        WHERE product_id = v_product_id;
    END IF;
    
    -- 5. Get or Create Order header
    SELECT order_id INTO v_order_id FROM orders WHERE order_code = p_order_code LIMIT 1;
    IF v_order_id IS NULL THEN
        INSERT INTO orders (order_code, customer_id, order_date, region_id) 
        VALUES (p_order_code, v_customer_id, p_order_date, v_region_id);
        SET v_order_id = LAST_INSERT_ID();
    END IF;
    
    -- 6. Insert Order line item detail
    INSERT INTO order_items (order_id, product_id, quantity, price_per_unit, sales, profit) 
    VALUES (v_order_id, v_product_id, p_quantity, p_price_per_unit, p_sales, p_profit);
    
    COMMIT;
END //

-- -----------------------------------------------------
-- Procedure: MonthlySales
-- Extracts monthly performance data
-- -----------------------------------------------------
CREATE PROCEDURE MonthlySales()
BEGIN
    SELECT Year, Month, Month_Name, Monthly_Sales, Monthly_Profit, Total_Orders
    FROM vw_monthly_sales;
END //

-- -----------------------------------------------------
-- Procedure: TopProducts
-- Extracts top selling products
-- -----------------------------------------------------
CREATE PROCEDURE TopProducts(IN p_limit INT)
BEGIN
    SELECT Product, Category, List_Price, Current_Stock, Total_Units_Sold, Total_Sales, Total_Profit
    FROM vw_product_summary
    ORDER BY Total_Sales DESC
    LIMIT p_limit;
END //

-- -----------------------------------------------------
-- Procedure: TopCustomers
-- Extracts top customers by sales volume
-- -----------------------------------------------------
CREATE PROCEDURE TopCustomers(IN p_limit INT)
BEGIN
    SELECT Customer_ID AS customer_code, Customer_Name, City, State, Total_Orders, Total_Sales, Total_Profit, Avg_Order_Value
    FROM vw_customer_summary
    ORDER BY Total_Sales DESC
    LIMIT p_limit;
END //

DELIMITER ;
