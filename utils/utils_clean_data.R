#' clean_data
#'
#' @description A fct function to clean and process survey data
#'
#' @return A list containing processed data frames for indicators and dynamics
#'
#' @importFrom dplyr slice select filter left_join mutate rename case_when everything
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_starts
#' @importFrom yaml write_yaml read_yaml
#' @importFrom purrr imap_dfr
#' @importFrom here here
#'
#' @param dirty_data The input data frame to clean

clean_data <- function(dirty_data) {
  # Load dynamics_map from the www directory
  dynamics_map <- readr::read_csv("www/dynamics_map.csv")

  # Initialize result list
  result <- list()
  qualtrics_metadata_removed <- FALSE

  # Input validation
  if (missing(dirty_data) || is.null(dirty_data)) {
    logger::log_error("dirty_data parameter is required and cannot be NULL")
    stop("dirty_data parameter is required and cannot be NULL")
  }
  # Clean and structure the input data
  if (!("qualtrics_id" %in% names(dirty_data))) {
    logger::log_info("Cleaning data...")

    # Handle Qualtrics data format
    if ("StartDate" %in% names(dirty_data)) {
      logger::log_info("Processing Qualtrics data format")
      clean_data <- dirty_data[-c(2, 3), -c(1:18)]
      logger::log_info("Qualtrics data cleaned - removed metadata rows and columns")
      qualtrics_metadata_removed <- TRUE
    } else {
      clean_data <- dirty_data
    }

    # Transform to the expected structure
    clean_data <- clean_data |>
      dplyr::slice(dplyr::n()) |>
      dplyr::select(
        starts_with("indicators_"),
        starts_with("dynamics_"),
        starts_with("cascade_")
      ) |>
      dplyr::select(!ends_with("_GROUP")) |>
      dplyr::select(!ends_with("_selection")) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "qualtrics_id",
        values_to = "rank"
      ) |>
      dplyr::mutate(qualtrics_id = stringr::str_remove(qualtrics_id, "_RANK$"))
    logger::log_info("Data cleaned and structured successfully")
  } else {
    logger::log_info("Using provided clean data (qualtrics_id already present)")
    clean_data <- dirty_data
  }
  logger::log_info("Data structured successfully.")

  # Process indicators data
  result <- process_indicators(clean_data)

  # Process dynamics data using the loaded mapping
  logger::log_info("Processing dynamics data using mapping from www/dynamics_map.csv ...")
  dynamics_result <- process_dynamics(clean_data, dynamics_map)
  logger::log_info("Dynamics data processing completed")

  # Process cascade data
  logger::log_info("Starting to process cascade data...")
  cascade_result <- process_cascade(clean_data)
  logger::log_info("Cascade data processing completed")

  # Combine all results
  result$dynamics <- dynamics_result$dynamics
  result$cascade <- cascade_result$cascade
  result$qualtrics_metadata_removed <- qualtrics_metadata_removed

  # Return the complete result
  return(result)
}

# Process indicators data
process_indicators <- function(clean_data) {
  result <- list()

  indicators_list <- data.frame(
    indicator = c(
      "Partnerships",
      "Engagement Hours",
      "Individuals Served",
      "Infrastructure Tools",
      "Students Involved",
      "Output Products",
      "Successful Outcomes"
    ),
    qualtrics_id = c(
      "indicators_partners",
      "indicators_hours",
      "indicators_served",
      "indicators_tools",
      "indicators_students",
      "indicators_outputs",
      "indicators_outcomes"
    )
  )

  indicators_data <- data.frame(indicator = character(), value = numeric())

  logger::log_info("Starting to process indicators data...")
  tryCatch(
    {
      filtered_indicators <- clean_data |>
        dplyr::filter(
          stringr::str_starts(qualtrics_id, "indicators_")
        )
      logger::log_info("Filtered indicators data. Rows: {nrow(filtered_indicators)}")

      if (nrow(filtered_indicators) > 0) {
        indicators_data <- filtered_indicators |>
          dplyr::left_join(
            indicators_list,
            by = "qualtrics_id"
          ) |>
          dplyr::select(-qualtrics_id) |>
          dplyr::mutate(
            value = as.numeric(rank),  # Ensure value is numeric
            value = ifelse(is.na(value), 0, value)  # Replace NA with 0 if needed
          ) |>
          dplyr::select(-rank)  # Remove the original rank column

        logger::log_info("Processed indicators data successfully")
      } else {
        logger::log_info("No indicators data found after filtering")
        indicators_data <- data.frame(indicator = character(), value = numeric())
      }
    },
    error = function(e) {
      logger::log_error("Error processing indicators data: {conditionMessage(e)}")
      indicators_data <- data.frame(indicator = character(), value = numeric())
    }
  )

  if (nrow(indicators_data) > 0) {
    result$indicators <- indicators_data
  } else {
    result$indicators <- data.frame(indicator = character(), value = numeric())
  }

  return(result)
}

# Process dynamics data - Modified to accept dynamics_map
process_dynamics <- function(clean_data, dynamics_map) {
  logger::log_info("Inside process_dynamics function")
  result <- list()

  dynamics_data <- clean_data |>
    dplyr::filter(
      stringr::str_starts(qualtrics_id, "dynamics_")
    )
  logger::log_debug("Found {nrow(dynamics_data)} dynamics records")

  if (nrow(dynamics_data) > 0) {
    # Debug: Print structure of dynamics_map and dynamics_data
    logger::log_info("Structure of dynamics_map columns: {paste(names(dynamics_map), collapse = ', ')}")
    logger::log_info("Structure of dynamics_data columns: {paste(names(dynamics_data), collapse = ', ')}")

    dynamics_data <- dynamics_data |>
      dplyr::left_join(
        dynamics_map,
        by = c("qualtrics_id" = "qualtrics_id"),
        suffix = c("", "_map")
      )

    # Debug: Print structure after join
    logger::log_info("Structure after join: {paste(names(dynamics_data), collapse = ', ')}")

    dynamics_data <- dynamics_data |>
      stats::na.omit() |>
      dplyr::mutate(
        rank = as.numeric(rank),  # Ensure rank is numeric for calculations
        weight = dplyr::case_when(
          rank == 1 ~ 1,
          rank == 2 ~ 0.95,
          rank == 3 ~ 0.9,
          rank == 4 ~ 0.84,
          rank == 5 ~ 0.78,
          TRUE ~ 0
        ),
        # Calculate dimension_value if it doesn't exist
        dimension_value = if ("dimension_value" %in% names(dynamics_data)) {
          dimension_value
        } else {
          # Use the same weights as the weight column
          dplyr::case_when(
            rank == 1 ~ 1.0,
            rank == 2 ~ 0.95,
            rank == 3 ~ 0.9,
            rank == 4 ~ 0.84,
            rank == 5 ~ 0.78,
            TRUE ~ 0
          )
        }
      )

    # Select only the columns we need and that exist in the data
    selected_cols <- c("domain", "dimension", "descriptor", "rank", "weight")

    # Add salience if it exists
    if ("salience" %in% names(dynamics_data)) {
      selected_cols <- c(selected_cols, "salience")
    } else {
      logger::log_warn("Column 'salience' not found in dynamics_data. Available columns: {paste(names(dynamics_data), collapse = ', ')}")
    }
    
    # Add dimension_value if it exists (required by centrimpact::analyze_dynamics)
    if ("dimension_value" %in% names(dynamics_data)) {
      selected_cols <- c(selected_cols, "dimension_value")
    } else {
      logger::log_warn("Column 'dimension_value' not found in dynamics_data. This column is required for analysis. Available columns: {paste(names(dynamics_data), collapse = ', ')}")
    }

    # Only keep columns that exist in the data
    existing_cols <- intersect(selected_cols, names(dynamics_data))

    # Log which columns will be kept
    logger::log_info("Selecting columns: {paste(existing_cols, collapse = ', ')}")

    # Select only the existing columns
    dynamics_data <- dynamics_data |>
      dplyr::select(dplyr::all_of(existing_cols))

    logger::log_info("Processing {nrow(dynamics_data)} dynamics records")
    result$dynamics <- dynamics_data
  } else {
    result$dynamics <- data.frame()
  }
  return(result)
}

# Process cascade data
process_cascade <- function(clean_data) {
  logger::log_info("Starting cascade data processing")
  result <- list()
  cascade_data <- data.frame()
  cascade_yaml <- NULL

  tryCatch(
    {
      cascade_data <- clean_data |>
        dplyr::filter(
          stringr::str_starts(qualtrics_id, "cascade_")
        )
      logger::log_debug("Found {nrow(cascade_data)} cascade records")

      if (nrow(cascade_data) > 0) {
        cascade_data <- cascade_data |>
          dplyr::rename(value = rank) |>
          dplyr::mutate(
            measure = dplyr::case_when(
              qualtrics_id == "cascade_d1_people_1_1" ~ "degree_1_researchers_count",
              qualtrics_id == "cascade_d1_people_2_1" ~ "degree_1_community_partners_count",
              qualtrics_id == "cascade_d2_people_1_1" ~ "degree_1_researchers_influence",
              qualtrics_id == "cascade_d2_people_2_1" ~ "degree_2_community_partners_influence",
              qualtrics_id == "cascade_d3_people" ~ "degree_2_agents_influence",
              qualtrics_id == "cascade_d2_stats_2" ~ "degree_1_cross_connection_probability",
              qualtrics_id == "cascade_d2_stats_1" ~ "degree_2_connection_probability",
              qualtrics_id == "cascade_d3_stats_1" ~ "degree_3_connection_probability",
              qualtrics_id == "cascade_d3_stats_2" ~ "degree_3_cross_conection_probability",
              TRUE ~ NA_character_
            ),
            degree = dplyr::case_when(
              stringr::str_starts(qualtrics_id, "cascade_d1_") ~ 1,
              stringr::str_starts(qualtrics_id, "cascade_d2_") ~ 2,
              stringr::str_starts(qualtrics_id, "cascade_d3_") ~ 3,
              TRUE ~ NA_integer_
            )
          ) |>
          dplyr::select(measure, value, degree) |>
          tidyr::drop_na(measure)

        logger::log_info("Processing {nrow(cascade_data)} cascade records")

        # These functions would need to be defined or sourced as well
        # For now, we'll assume they exist.
        cascade_yaml <- structure_network_parameters(data = cascade_data)
        # Extract and convert values to numeric
        get_numeric_value <- function(measure_name) {
          as.numeric(cascade_data$value[cascade_data$measure == measure_name])
        }

        # Convert probabilities to percentages (0-100)
        cascade_edgelist <- create_edgelist(
          degree_1_researchers_count = get_numeric_value("degree_1_researchers_count"),
          degree_1_community_partners_count = get_numeric_value("degree_1_community_partners_count"),
          degree_1_researchers_influence = get_numeric_value("degree_1_researchers_influence"),
          degree_2_community_partners_influence = get_numeric_value("degree_2_community_partners_influence"),
          degree_2_agents_influence = get_numeric_value("degree_2_agents_influence"),
          degree_1_cross_connection_pct = get_numeric_value("degree_1_cross_connection_probability") * 100, # Convert to percentage
          degree_2_connection_pct = get_numeric_value("degree_2_connection_probability") * 100, # Convert to percentage
          degree_3_connection_pct = get_numeric_value("degree_3_connection_probability") * 100, # Convert to percentage
          degree_3_cross_connection_pct = get_numeric_value("degree_3_cross_conection_probability") * 100 # Convert to percentage
        )

        # Debug: Print structure of cascade_edgelist
        logger::log_info("Structure of cascade_edgelist:")
        # Use cat to avoid glue parsing issues with str() output
        cat("\n=== DEBUG: cascade_edgelist structure ===\n")
        str(cascade_edgelist)
        cat("\n")
        logger::log_info("cascade_edgelist has {length(cascade_edgelist)} components: {paste(names(cascade_edgelist), collapse = ', ')}")

        # Check if cascade_edgelist is a list with the expected structure
        if (!is.list(cascade_edgelist)) {
          msg <- sprintf("cascade_edgelist is not a list, it's a %s", class(cascade_edgelist)[1])
          logger::log_error(msg)
          stop(msg)
        }

        if (is.null(cascade_edgelist$edgelist)) {
          msg <- "cascade_edgelist$edgelist is NULL"
          logger::log_error(msg)
          stop(msg)
        }

        if (is.null(cascade_edgelist$nodes)) {
          msg <- "cascade_edgelist$nodes is NULL"
          logger::log_error(msg)
          stop(msg)
        }

        # Log the structure of the components
        logger::log_info("cascade_edgelist$edgelist is a {class(cascade_edgelist$edgelist)[1]} with {nrow(cascade_edgelist$edgelist)} rows")
        logger::log_info("cascade_edgelist$nodes is a {class(cascade_edgelist$nodes)[1]} with {nrow(cascade_edgelist$nodes)} rows")

        # Store both edges and nodes from the cascade_edgelist result
        result$cascade <- list(
          edges = cascade_edgelist$edgelist,
          nodes = cascade_edgelist$nodes,
          config = cascade_data,
          model = cascade_yaml
        )

        logger::log_info("Cascade data structured with {nrow(cascade_edgelist$edgelist)} edges and {if(!is.null(cascade_edgelist$nodes)) nrow(cascade_edgelist$nodes) else 0} nodes")
      } else {
        logger::log_info("No cascade data found after filtering")
        result$cascade <- list(
          edges = data.frame(),
          nodes = data.frame(),
          config = data.frame(),
          model = NULL
        )
      }
    },
    error = function(e) {
      logger::log_error("Error processing cascade data: {conditionMessage(e)}")
      result$cascade <- list(
        edges = data.frame(),
        nodes = data.frame(),
        config = data.frame(),
        model = NULL
      )
    }
  )

  return(result)
}

# Function to convert CSV to YAML structure using tidyverse
structure_network_parameters <- function(data) {
  # Parse the measure column and create a structured dataframe
  parsed_data <- data |>
    # Extract degree number
    dplyr::mutate(degree_num = stringr::str_extract(measure, "(?<=degree_)\\d+")) |>
    # Remove the degree prefix
    dplyr::mutate(clean_measure = stringr::str_remove(measure, "^degree_\\d+_")) |>
    # Identify if it's a nested measure (researchers/community_partners)
    dplyr::mutate(
      role = dplyr::case_when(
        stringr::str_starts(clean_measure, "researchers_") ~ "researchers",
        stringr::str_starts(clean_measure, "community_partners_") ~ "community_members",
        stringr::str_starts(clean_measure, "agents_") ~ "agents",
        TRUE ~ NA_character_
      ),
      final_measure = dplyr::case_when(
        !is.na(role) ~ stringr::str_remove(clean_measure, "^(researchers_|community_partners_|agents_)"),
        TRUE ~ clean_measure
      )
    ) |>
    dplyr::select(degree_num, role, final_measure, value)

  # Create the nested list structure
  yaml_structure <- list()

  # Process each degree
  degree_names <- c("1" = "first_degree", "2" = "second_degree", "3" = "third_degree")

  for (deg in names(degree_names)) {
    degree_name <- degree_names[deg]
    degree_data <- parsed_data |> dplyr::filter(degree_num == deg)

    yaml_structure[[degree_name]] <- list()

    # Handle nested categories using map
    nested_data <- degree_data |> dplyr::filter(!is.na(role))
    if (nrow(nested_data) > 0) {
      nested_list <- nested_data |>
        split(nested_data$role) |>
        purrr::map(~ setNames(.x$value, .x$final_measure) |> as.list())

      yaml_structure[[degree_name]] <- c(yaml_structure[[degree_name]], nested_list)
    }

    # Handle direct measures
    direct_data <- degree_data |> dplyr::filter(is.na(role))
    if (nrow(direct_data) > 0) {
      direct_list <- setNames(direct_data$value, direct_data$final_measure) |> as.list()
      yaml_structure[[degree_name]] <- c(yaml_structure[[degree_name]], direct_list)
    }
  }

  return(yaml_structure)
}
