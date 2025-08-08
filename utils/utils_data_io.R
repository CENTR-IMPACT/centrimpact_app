#' clean_data_helpers 
#' Load and Process Survey Data
#'
#' @description 
#' A utility function to load survey data from either Qualtrics export or CSV format.
#' It first attempts to read the file as a Qualtrics export, and if that fails,
#' falls back to reading as a standard CSV file.
#'
#' @param file_path Character. Path to the survey data file (either Qualtrics export or CSV).
#' 
#' @return A data frame containing the survey data.
#' 
#' @importFrom qualtRics read_survey
#' @importFrom readr read_csv
#' @importFrom logger log_info log_warn
#' 
#' @examples
#' \dontrun{
#' # Load a Qualtrics survey file
#' survey_data <- load_survey_data("path/to/qualtrics_export.csv")
#' 
#' # Load a standard CSV file
#' csv_data <- load_survey_data("path/to/survey_data.csv")
#' }
#' 
#' @export

load_survey_data <- function(file_path) {
  # First attempt: Try to read as Qualtrics export
  tryCatch({
    logger::log_info("Attempting to read as Qualtrics survey...")
    suppressMessages({
      df <- qualtRics::read_survey(
        file_path,
        strip_html = TRUE,
        add_var_labels = FALSE
      )
    })
    logger::log_info("Successfully loaded Qualtrics survey data")
    return(df)
  }, error = function(e) {
    logger::log_warn("Not a Qualtrics file, trying as standard CSV")
    
    # Second attempt: Try to read as standard CSV
    tryCatch({
      df <- readr::read_csv(file_path, show_col_types = FALSE)
      
      # Validate required columns for standard CSV format
      expected_cols <- c("qualtrics_id", "rank")
      if (!all(expected_cols %in% names(df))) {
        stop("Invalid CEnTR*IMPACT data file: missing required columns")
      }
      
      logger::log_info("Successfully loaded standard CSV survey data")
      return(df)
    }, error = function(e) {
      logger::log_error("Failed to load survey data: ", conditionMessage(e))
      stop("Failed to load survey data: ", conditionMessage(e), call. = FALSE)
    })
  })
}

#' Convert YAML List to Data Frame
#' 
#' @description
#' Converts a nested YAML list structure into a tidy data frame format.
#' The function expects a specific nested structure where domains contain dimensions,
#' which in turn contain survey items with their properties.
#'
#' @param yaml_list A nested list structure, typically from reading a YAML file.
#' 
#' @return A data frame with columns: domain, dimension, qualtrics_id, descriptor,
#' salience, rank, and weight.
#' 
#' @importFrom purrr imap_dfr
#' @importFrom tibble tibble
#' 
#' @export
yaml_to_dataframe_purrr <- function(yaml_list) {
  yaml_list |>
    purrr::imap_dfr(~ {
      domain_name <- .y
      .x |>
        purrr::imap_dfr(~ {
          dimension_name <- .y
          .x |>
            purrr::imap_dfr(~ {
              tibble::tibble(
                domain = domain_name,
                dimension = dimension_name,
                qualtrics_id = .y,
                descriptor = .x$descriptor,
                salience = .x$salience,
                rank = .x$rank,
                weight = .x$weight
              )
            })
        })
    })
}

create_hierarchy <- function(df) {
  result <- list()
  
  for(i in 1:nrow(df)) {
    domain <- df$domain[i]
    dimension <- df$dimension[i]
    qualtrics_id <- df$qualtrics_id[i]  # Use qualtrics_id as the key
    
    # Initialize domain if it doesn't exist
    if(is.null(result[[domain]])) {
      result[[domain]] <- list()
    }
    
    # Initialize dimension if it doesn't exist
    if(is.null(result[[domain]][[dimension]])) {
      result[[domain]][[dimension]] <- list()
    }
    
    # Add qualtrics_id with all its properties
    result[[domain]][[dimension]][[qualtrics_id]] <- list(
      descriptor = df$descriptor[i],  # Include the full descriptor text
      salience = df$salience[i],
      rank = df$rank[i],
      weight = df$weight[i]
    )
  }
  
  return(result)
}
