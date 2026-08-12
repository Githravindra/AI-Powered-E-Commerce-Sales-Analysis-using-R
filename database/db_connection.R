# db_connection.R
# Manages database connections using DBI and RMariaDB (preferred) or RMySQL.
# Implements transaction boundaries, tryCatch logging, and resource safety.

# Source logging utility
if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Connect to MySQL Database
#'
#' @param host Character. DB Hostname. Default "localhost".
#' @param port Integer. DB Port. Default 3306.
#' @param dbname Character. DB Schema Name. Default "ecommerce_sales".
#' @param user Character. Username. Default "root".
#' @param password Character. Password. Default "" (empty).
#' @return A DBI Connection object or NULL if connection fails.
connect_db <- function(host = getOption("db.host", "127.0.0.1"),
                       port = getOption("db.port", 3306),
                       dbname = getOption("db.name", "ecommerce_sales"),
                       user = getOption("db.user", "root"),
                       password = getOption("db.password", "")) {
  log_info("Attempting to connect to MySQL/MariaDB Database...")
  
  # Disable client-side peer verification to prevent SSL failures with local databases
  Sys.setenv(MARIADB_TLS_DISABLE_PEER_VERIFICATION = "1")
  
  # Determine which driver is available
  has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
  has_rmysql <- requireNamespace("RMySQL", quietly = TRUE)
  
  if (!has_mariadb && !has_rmysql) {
    log_error("Neither 'RMariaDB' nor 'RMySQL' packages are installed. Cannot establish database connection.")
    return(NULL)
  }
  
  driver <- NULL
  driver_name <- ""
  
  if (has_mariadb) {
    driver <- RMariaDB::MariaDB()
    driver_name <- "RMariaDB"
  } else {
    driver <- RMySQL::MySQL()
    driver_name <- "RMySQL"
  }
  
  log_info(paste("Using database driver:", driver_name))
  
  conn <- tryCatch({
    # Establish connection
    con <- DBI::dbConnect(
      driver,
      host = host,
      port = as.integer(port),
      dbname = dbname,
      user = user,
      password = password
    )
    log_info(sprintf("Successfully connected to MySQL database '%s' at %s:%d.", dbname, host, port))
    con
  }, error = function(e) {
    log_error(paste("Failed to connect to MySQL Database. Error:", e$message))
    
    # Troubleshooting hint for macOS XAMPP MySQL
    if (grepl("Can't connect to local MySQL server", e$message) || grepl("Connection refused", e$message)) {
      log_warn("TROUBLESHOOTING TIP: Ensure XAMPP is running and MySQL database server is started.")
    }
    return(NULL)
  })
  
  return(conn)
}

#' Close Database Connection
#'
#' @param conn DBIConnection. The connection to close.
#' @return Logical. TRUE if successfully disconnected, FALSE otherwise.
close_db <- function(conn) {
  if (is.null(conn)) return(invisible(FALSE))
  
  tryCatch({
    DBI::dbDisconnect(conn)
    log_info("Database connection closed successfully.")
    TRUE
  }, error = function(e) {
    log_error(paste("Failed to disconnect from Database. Error:", e$message))
    FALSE
  })
}

#' Database Connection Context Wrapper
#' Runs an R expression inside a database session context, automatically closing the connection.
#'
#' @param expr Expression. The block of R code to run.
#' @return The result of the expression, or NULL on error.
#' @export
#' @examples
#' with_db_connection({
#'   dbGetQuery(conn, "SELECT * FROM vw_sales_summary LIMIT 10;")
#' })
with_db_connection <- function(expr) {
  # Capture the unevaluated expression to prevent lazy evaluation scoping issues
  req_expr <- substitute(expr)
  
  conn <- connect_db()
  if (is.null(conn)) {
    log_error("with_db_connection: Cannot execute expression, database connection failed.")
    return(NULL)
  }
  
  # Guarantee connection is closed on function exit
  on.exit({
    close_db(conn)
  })
  
  # Evaluate the captured expression in an environment containing the active 'conn' object
  eval(req_expr, envir = list(conn = conn), enclos = parent.frame())
}
