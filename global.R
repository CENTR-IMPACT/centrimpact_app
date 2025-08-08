# Configure logger
if (!requireNamespace("logger", quietly = TRUE)) {
  install.packages("logger")
}
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
if (!requireNamespace("shinyvalidate", quietly = TRUE)) {
  install.packages("shinyvalidate")
}
library(logger)
library(phosphoricons)
library(DT)
library(htmltools)
library(shinyvalidate)

# Set up logger to write to console with timestamps and log level
log_threshold(INFO)
log_layout(layout_glue_colors)
log_info("Logger initialized")

# load required shiny framework packages
library(shinymgr)

# Define the app path
shinyMgrPath <- getwd()

# load required module packages (parse headers)
app_mods <- list.files(
  path = paste0(shinyMgrPath, "/modules"),
  full.names = TRUE
)

# source in all manager (framework) modules
mgr_mods <- list.files(
  path = paste0(shinyMgrPath, "/modules_mgr"),
  full.names = TRUE
)

sapply(mgr_mods, FUN = source)

# source in all user modules

sapply(app_mods, FUN = source)

# source in all manager (framework) modules
app_mods <- list.files(
  path = paste0(shinyMgrPath, "/modules_app"),
  full.names = TRUE
)

sapply(app_mods, FUN = source)

# Source utility functions
source(file.path(shinyMgrPath, "utils", "utils_validate_project_data.R"))

# source in all utils
utils_mods <- list.files(
  path = paste0(shinyMgrPath, "/utils"),
  full.names = TRUE
)

sapply(utils_mods, FUN = source)
