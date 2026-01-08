# .Rprofile for RStudies applications
# Ensures proper configuration and paths are set when running RStudies apps

# Load configuration from environment variables
config_file <- Sys.getenv("R_CONFIG_FILE")
db_path <- Sys.getenv("R_DB_PATH")
log_dir <- Sys.getenv("R_LOG_DIR")

# Verify environment variables are set
if (config_file == "" || db_path == "" || log_dir == "") {
  warning("Environment variables not set. Please check Renviron.site configuration.")
  cat("Expected environment variables:\n")
  cat("R_CONFIG_FILE:", Sys.getenv("R_CONFIG_FILE"), "\n")
  cat("R_DB_PATH:", Sys.getenv("R_DB_PATH"), "\n")
  cat("R_LOG_DIR:", Sys.getenv("R_LOG_DIR"), "\n")
  cat("R_BOX_PATH:", Sys.getenv("R_BOX_PATH"), "\n")
}

# Initialize with proper working directory context
if (interactive()) {
  cat("🔬 RStudies Research Applications\n")
  cat("✅ Configuration loaded from:", config_file, "\n")
  cat("✅ Database path:", db_path, "\n")
  cat("✅ Box modules path:", Sys.getenv("R_BOX_PATH"), "\n")
}