# logging.R
# Custom logging library supporting INFO, WARN, and ERROR levels with timestamp format.

log_file_path <- "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/logs/app.log"

#' Helper function to perform raw log printing and file writing
#' @param level Character indicating log level (INFO, WARN, ERROR)
#' @param msg Character containing message to log
write_log <- function(level, msg) {
  # Create logs directory if missing
  dir.create(dirname(log_file_path), recursive = TRUE, showWarnings = FALSE)
  
  # Format timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_log <- sprintf("[%s] [%s] %s", timestamp, level, msg)
  
  # Print to console
  cat(formatted_log, "\n")
  
  # Append to file
  tryCatch({
    write(formatted_log, file = log_file_path, append = TRUE)
  }, error = function(e) {
    # Fail silently to avoid breaking execution if writing to file fails
  })
}

#' Log informational messages
#' @param message Character. The message to log.
log_info <- function(message) {
  write_log("INFO", message)
}

#' Log warning messages
#' @param message Character. The message to log.
log_warn <- function(message) {
  write_log("WARN", message)
}

#' Log error messages
#' @param message Character. The message to log.
log_error <- function(message) {
  write_log("ERROR", message)
}
