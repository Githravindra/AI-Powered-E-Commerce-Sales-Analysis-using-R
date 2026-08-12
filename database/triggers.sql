-- triggers.sql
-- Define Triggers for automated processes and data audit logs

USE ecommerce_sales;

-- Drop triggers if they already exist
DROP TRIGGER IF EXISTS trg_update_stock_after_order;
DROP TRIGGER IF EXISTS trg_log_deleted_orders;
DROP TRIGGER IF EXISTS trg_customers_timestamp;
DROP TRIGGER IF EXISTS trg_products_timestamp;
DROP TRIGGER IF EXISTS trg_orders_timestamp;

DELIMITER //

-- -----------------------------------------------------
-- Trigger: trg_update_stock_after_order
-- Automatically updates stock levels in products when order items are placed
-- -----------------------------------------------------
CREATE TRIGGER trg_update_stock_after_order
AFTER INSERT ON order_items
FOR EACH ROW
BEGIN
    UPDATE products
    SET stock = stock - NEW.quantity
    WHERE product_id = NEW.product_id;
END //

-- -----------------------------------------------------
-- Trigger: trg_log_deleted_orders
-- Logs order details into audit logs prior to database deletion
-- -----------------------------------------------------
CREATE TRIGGER trg_log_deleted_orders
BEFORE DELETE ON orders
FOR EACH ROW
BEGIN
    INSERT INTO deleted_orders_log (order_id, order_code, customer_id, order_date)
    VALUES (OLD.order_id, OLD.order_code, OLD.customer_id, OLD.order_date);
END //

-- -----------------------------------------------------
-- Trigger: trg_customers_timestamp
-- Sets updated_at timestamp when customer records change
-- -----------------------------------------------------
CREATE TRIGGER trg_customers_timestamp
BEFORE UPDATE ON customers
FOR EACH ROW
BEGIN
    SET NEW.updated_at = CURRENT_TIMESTAMP;
END //

-- -----------------------------------------------------
-- Trigger: trg_products_timestamp
-- Sets updated_at timestamp when product records change
-- -----------------------------------------------------
CREATE TRIGGER trg_products_timestamp
BEFORE UPDATE ON products
FOR EACH ROW
BEGIN
    SET NEW.updated_at = CURRENT_TIMESTAMP;
END //

-- -----------------------------------------------------
-- Trigger: trg_orders_timestamp
-- Sets updated_at timestamp when order records change
-- -----------------------------------------------------
CREATE TRIGGER trg_orders_timestamp
BEFORE UPDATE ON orders
FOR EACH ROW
BEGIN
    SET NEW.updated_at = CURRENT_TIMESTAMP;
END //

DELIMITER ;
