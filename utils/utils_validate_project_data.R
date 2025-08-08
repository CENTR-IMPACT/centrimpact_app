#' Check project_data structure (non-reactive)
#' 
#' Lightweight check for project_data structure. This function is completely
#' non-reactive and only checks the structure when explicitly called from within
#' a reactive context.
#' 
#' @param project_data The project_data object to validate
#' @param required_sections Character vector of top-level sections to check for
#' @param module_name Name of the module performing the validation (for error messages)
#' 
#' @return Returns TRUE if validation passes, FALSE otherwise
#' @export
validate_project_data <- function(project_data, 
                                required_sections = c("project_info", "cleaned_data", "analysis", "visualization", "status"),
                                module_name = "") {
  # Skip validation in non-interactive mode
  if (!interactive()) {
    return(TRUE)
  }
  
  # Skip validation if project_data is NULL
  if (is.null(project_data)) {
    return(TRUE)
  }
  
  # Skip validation if we can't safely check the structure
  if (shiny::is.reactive(project_data) || shiny::is.reactivevalues(project_data)) {
    return(TRUE)
  }
  
  # For non-reactive objects, check the structure
  if (is.list(project_data)) {
    names_list <- names(project_data)
    missing_sections <- setdiff(required_sections, names_list)
    
    if (length(missing_sections) > 0) {
      warning("Missing sections in project_data in ", module_name, ": ", 
             paste(missing_sections, collapse = ", "))
      return(FALSE)
    }
    return(TRUE)
  }
  
  warning("project_data is not a list in ", module_name)
  return(FALSE)
}

#' Safely get a value from project_data with a default
#' 
#' @param project_data The project_data reactiveValues object
#' @param path Character vector specifying the path to the value
#' @param default The default value to return if the path doesn't exist
#' @return The value at the specified path or the default value
#' @export
get_project_data <- function(project_data, path, default = NULL) {
  tryCatch({
    value <- project_data
    for (p in path) {
      if (is.reactivevalues(value)) {
        value <- value[[p]]
      } else if (is.list(value)) {
        value <- value[[p]]
      } else {
        return(default)
      }
      if (is.null(value)) return(default)
    }
    value
  }, error = function(e) {
    warning("Error accessing project_data$", paste(path, collapse = "$"), ": ", e$message)
    default
  })
}

#' Safely set a value in project_data
#' 
#' @param project_data The project_data reactiveValues object
#' @param path Character vector specifying the path to set
#' @param value The value to set
#' @export
set_project_data <- function(project_data, path, value) {
  tryCatch({
    if (length(path) == 1) {
      project_data[[path]] <- value
    } else {
      current <- project_data
      for (i in seq_along(path[-length(path)])) {
        p <- path[i]
        if (is.null(current[[p]])) {
          current[[p]] <- if (i == length(path) - 1) reactiveValues() else list()
        }
        current <- current[[p]]
      }
      current[[path[length(path)]]] <- value
    }
    invisible(TRUE)
  }, error = function(e) {
    warning("Error setting project_data$", paste(path, collapse = "$"), ": ", e$message)
    FALSE
  })
}
