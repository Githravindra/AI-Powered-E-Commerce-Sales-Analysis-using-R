# build_db.R
# Automates the construction of the database schema, views, stored procedures,
# triggers, and loads sample data directly from the R console.

source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database/db_connection.R")

cat("=========================================================\n")
cat("          AUTOMATED DATABASE BUILD UTILITY               \n")
cat("=========================================================\n\n")

execute_sql_file <- function(conn, file_path) {
  cat("Executing SQL script:", basename(file_path), "... ")
  
  if (!file.exists(file_path)) {
    cat("FAILED - File not found.\n")
    stop(paste("File missing:", file_path))
  }
  
  lines <- readLines(file_path, warn = FALSE)
  queries <- list()
  current_query <- ""
  custom_delimiter <- ";"
  
  for (line in lines) {
    trimmed <- trimws(line)
    # Skip empty lines or SQL comments
    if (trimmed == "" || startsWith(trimmed, "--")) {
      next
    }
    
    # Check for DELIMITER statement change
    if (startsWith(toupper(trimmed), "DELIMITER")) {
      parts <- strsplit(trimmed, "\\s+")[[1]]
      custom_delimiter <- parts[2]
      next
    }
    
    current_query <- paste(current_query, line, sep = "\n")
    
    # Check if the query ends with the active delimiter
    if (endsWith(trimmed, custom_delimiter)) {
      # Strip custom delimiter from the query
      query_clean <- sub(paste0(gsub("([./\\*\\+\\?\\(\\)\\[\\]\\{\\}\\^\\$\\|])", "\\\\\\1", custom_delimiter), "$"), "", trimws(current_query))
      queries <- c(queries, query_clean)
      current_query <- ""
    }
  }
  
  # Execute parsed statements
  success_count <- 0
  for (q in queries) {
    q_trimmed <- trimws(q)
    if (q_trimmed == "") next
    tryCatch({
      DBI::dbExecute(conn, q_trimmed)
      success_count <- success_count + 1
    }, error = function(e) {
      cat("\n[-] Error executing statement:\n", q_trimmed, "\nError message:", e$message, "\n")
      stop("Database build aborted due to execution error.")
    })
  }
  cat("PASSED (executed", success_count, "statements)\n")
}

# Run the database construction sequence
tryCatch({
  # Connect without database first (since schema.sql creates the db)
  cat("Connecting to MySQL server (without DB scope)...\n")
  conn <- connect_db(dbname = "")
  if (is.null(conn)) {
    stop("Could not establish connection to MySQL server. Verify server status.")
  }
  on.exit(close_db(conn))
  
  db_dir <- "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/database"
  
  execute_sql_file(conn, file.path(db_dir, "schema.sql"))
  execute_sql_file(conn, file.path(db_dir, "views.sql"))
  execute_sql_file(conn, file.path(db_dir, "procedures.sql"))
  execute_sql_file(conn, file.path(db_dir, "triggers.sql"))
  execute_sql_file(conn, file.path(db_dir, "sample_data.sql"))
  
  cat("\n[+] SUCCESS: E-Commerce database build complete with schemas, views, stored procedures, triggers, and realistic sample data!\n")
}, error = function(e) {
  cat("\n[-] FAILED: Database build process aborted:\n", e$message, "\n")
})
