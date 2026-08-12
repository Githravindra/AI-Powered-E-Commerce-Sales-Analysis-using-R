# fix_paths.R
# Replaces old path references with the new .CNG active workspace path.

old_path <- "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/"
new_path <- "/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/"

# Recursively find R and HTML files
files <- list.files("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG", recursive = TRUE, full.names = TRUE)
target_files <- files[grepl("\\.(R|html)$", files, ignore.case = TRUE)]

cat("Found", length(target_files), "files to inspect.\n")

replaced_count <- 0
for (f in target_files) {
  content <- readLines(f, warn = FALSE)
  if (any(grepl(old_path, content, fixed = TRUE))) {
    new_content <- gsub(old_path, new_path, content, fixed = TRUE)
    writeLines(new_content, f)
    cat("[+] Updated paths in:", basename(f), "\n")
    replaced_count <- replaced_count + 1
  }
}

cat("Replacement complete. Updated", replaced_count, "files.\n")
