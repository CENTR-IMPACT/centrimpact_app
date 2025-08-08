source("global.R")

# Define null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

server <- function(input, output, session) {
  # Initialize startup check reactive value
  startup_check <- reactiveVal(TRUE)

  # Run startup checks and initializations
  observe({
    req(startup_check())

    # This runs once at startup
    logger::log_info("STARTUP CHECK")

    # Hide the Enter Data tab by default
    nav_hide(id = "main_navbar", target = "Enter Data")

    # Prevent this from running again
    startup_check(FALSE)
  })

  # call database module
  output$my_db_output <- renderUI({
    my_db_output()
  })

  # Initialize database connection - check if it exists first
  tryCatch(
    {
      con <- pool::dbPool(
        drv = RPostgreSQL::PostgreSQL(),
        host = config::get("postgresql")$host,
        dbname = config::get("postgresql")$dbname,
        user = config::get("postgresql")$user,
        password = config::get("postgresql")$password,
        port = config::get("postgresql")$port
      )

      # Register the pool for automatic disconnection when the session ends
      onStop(function() {
        pool::poolClose(con)
      })
    },
    error = function(e) {
      logger::log_error("Database connection failed: ", e$message)
      con <- NULL
    }
  )

  # Initialize a reactive value for temporary storage
  temp_storage <- reactiveValues()

  # Call the home module
  mod_home_server("home_1")

  # Function to insert authors into the database
  insert_author <- function(author_name, author_email, author_institution, author_role, author_orcid) {
    # Insert logic here
  }

  # Direct module calls without wrappers
  setup_result <- mod_setup_server(
    id = "setup_1", # Must match the ID used in ui.R
    project_data = project_data
  )

  # also disconnect if session stops
  onStop(function() {
    if (exists("con") && !is.null(con)) {
      pool::poolClose(con)
    }
  })

  # Render icons with conditional coloring (now using project_data$analyzed_data) with tryCatch
  output$alignment_icon <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$alignment_score)) {
          has_score <- FALSE
          score_value <- "-.--"
        } else {
          has_score <- !is.null(pd$analyzed_data$alignment_score)
          score_value <- if (has_score) format(round(as.numeric(pd$analyzed_data$alignment_score), 2), nsmall = 2) else "-.--"
        }
        icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D366;"
        ph("target", weight = "regular", style = icon_style)
      },
      error = function(e) {
        print(paste("ERROR[alignment_icon]:", e))
        icon_style <- "font-size: 1em; color: #D3D3D3;"
        ph("target", weight = "regular", style = icon_style)
      }
    )
  })

  # Render dynamics icon with conditional color (now using project_data$analyzed_data) with tryCatch
  output$dynamics_icon <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$dynamics_score)) {
          has_score <- FALSE
          score_value <- "-.--"
        } else {
          has_score <- !is.null(pd$analyzed_data$dynamics_score)
          score_value <- if (has_score) format(round(as.numeric(pd$analyzed_data$dynamics_score), 2), nsmall = 2) else "-.--"
        }
        icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D366;"
        ph("chart-line", weight = "regular", style = icon_style)
      },
      error = function(e) {
        print(paste("ERROR[dynamics_icon]:", e))
        icon_style <- "font-size: 1em; color: #D3D3D3;"
        ph("chart-line", weight = "regular", style = icon_style)
      }
    )
  })

  # Render cascade icon with conditional color (now using project_data$analyzed_data) with tryCatch and debug
  output$cascade_icon <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        print(paste("DEBUG[cascade_icon]: typeof(pd) =", typeof(pd)))
        print(paste("DEBUG[cascade_icon]: class(pd) =", paste(class(pd), collapse = ", ")))
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$cascade_score)) {
          has_score <- FALSE
        } else {
          has_score <- !is.null(pd$analyzed_data$cascade_score)
        }
        icon_style <- if (has_score) "font-size: 1em;" else "font-size: 1em; color: #D3D3D366;"
        ph("waveform", weight = "regular", style = icon_style)
      },
      error = function(e) {
        print(paste("ERROR[cascade_icon]:", e))
        icon_style <- "font-size: 1em; color: #D3D3D3;"
        ph("waveform", weight = "regular", style = icon_style)
      }
    )
  })

  # Initialize reactive values for app state
  data_entry_mode <- reactiveVal(FALSE) # FALSE = manual entry, TRUE = upload
  snapshot_mode <- reactiveVal(FALSE) # Add this if not already present

  # Track tabset changes for change detection
  tab_visits <- reactiveValues()
  tab_changes <- reactiveValues()

  # Log all reactive values for debugging
  observe({
    logger::log_debug("Current data_entry_mode:", data_entry_mode())
  })

  # Initialize data mode selector state
  observe({
    # Send initial mode to client
    initial_mode <- if (data_entry_mode()) "enter" else "upload"
    session$sendCustomMessage("setDataModeSelector", list(mode = initial_mode))
  })

  # Project data and author information
  ns_project <- reactiveValues(
    project_title = NULL,
    project_description = NULL,
    project_report_date = NULL,
    project_report_outputs = list(
      pdf = NULL,
      html = NULL,
      word = NULL
    ),
    # Add variables from mod_load_clean.R
    main_data_raw = NULL,
    main_cleaned = NULL,
    alignment_data_raw = NULL,
    alignment_data = NULL,
    dynamics_data = NULL,
    cascade_data = NULL,
    params = NULL,
    edges = NULL,
    nodes = NULL,
    yaml_content = NULL,
    file_name = NULL,
    rv_status = NULL
  )

  ns_authors <- reactiveValues(
    author_list = data.frame(
      author_name = character(),
      author_email = character(),
      author_orcid = character(),
      author_institution = character(),
      author_role = character(),
      stringsAsFactors = FALSE
    )
  )

  # Create a reactive expression for the project title and date
  project_header <- reactive({
    # Get the namespace for the setup module
    setup_ns <- NS("setup")

    # Get the current input values with proper reactivity
    title <- input[[paste0("setup-project_title")]]
    report_date <- input[[paste0("setup-report_date")]]

    # Debug logging
    logger::log_info("Project header update - Title:", title, " Date:", report_date)

    # If title is empty, use default
    if (is.null(title) || title == "") {
      title <- "New Project"
    }

    # Format the date if available
    date_text <- NULL
    if (!is.null(report_date) && report_date != "" && !all(is.na(report_date))) {
      date_text <- tryCatch(
        {
          formatted_date <- format(as.Date(report_date), "%Y-%m-%d")
          div(
            style = "font-family: var(--bs-font-monospace); font-size: 0.7em; color: #D3D3D3; margin-top: 2px; display: block; text-transform: uppercase;",
            phosphoricons::ph("calendar", weight = "regular"),
            " ",
            formatted_date
          )
        },
        error = function(e) {
          logger::log_warn("Error formatting date:", e$message)
          NULL
        }
      )
    }

    # Return title and date
    tagList(
      div(
        style = "text-transform: uppercase; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100%;",
        phosphoricons::ph("tree-structure", weight = "light"),
        " ",
        toupper(title)
      ),
      date_text
    )
  })

  # Create a reactive expression for project header that depends on project_data
  project_header_react <- reactive({
    # Force reactivity to project_data changes
    pd <- reactiveValuesToList(project_data)

    # Get the current title or use a default
    title <- if (!is.null(project_data$project_info$title) &&
      nzchar(project_data$project_info$title)) {
      project_data$project_info$title
    } else {
      "Untitled Project"
    }

    # Format the date
    date_text <- NULL
    if (!is.null(project_data$project_info$report_date) &&
      nzchar(project_data$project_info$report_date)) {
      tryCatch(
        {
          formatted_date <- format(as.Date(project_data$project_info$report_date), "%Y-%m-%d")
          date_text <- div(
            style = "font-family: var(--bs-font-monospace); font-size: 0.7em; color: #D3D3D3; margin-top: 2px; display: block; text-transform: uppercase;",
            phosphoricons::ph("calendar", weight = "regular"),
            " ",
            formatted_date
          )
        },
        error = function(e) {
          logger::log_warn("Error formatting date:", e$message)
          NULL
        }
      )
    }

    # Return the header content
    tagList(
      div(
        style = "font-weight: bold; text-transform: uppercase; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100%;",
        phosphoricons::ph("tree-structure", weight = "light"),
        " ",
        toupper(title)
      ),
      date_text
    )
  })

  # Render the project title and date in the sidebar
  output$project_title_text <- renderUI({
    div(
      style = "font-family: var(--bs-font-sans-serif); margin: 0 auto; width: 100%; font-size: 12px;",
      project_header_react()
    )
  })

  # Handle custom message to update the sidebar
  observeEvent(session$input$update_sidebar,
    {
      logger::log_info("Received request to update sidebar")
      # Force a reactive update of the project header
      project_data$last_updated <- Sys.time()
    },
    ignoreNULL = FALSE
  )

  # Remove the automatic refresh of the sidebar to prevent flickering
  # The sidebar will now only update when explicitly triggered by the update_sidebar message

  # Render alignment score icon based on score range
  output$alignment_score_icon <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$alignment_score)) {
          return(NULL)
        }

        score <- as.numeric(pd$analyzed_data$alignment_score)
        icon_name <- if (score < 0.40) {
          "caret-circle-double-down"
        } else if (score >= 0.40 && score <= 0.59) {
          "caret-circle-up-down"
        } else if (score >= 0.60 && score <= 0.74) {
          "caret-circle-up"
        } else {
          "caret-circle-double-up"
        }

        # Add margin to the right of the icon
        div(
          style = "margin-right: 5px;",
          ph(icon_name, weight = "bold")
        )
      },
      error = function(e) {
        print(paste("ERROR[alignment_score_icon]:", e))
        NULL
      }
    )
  })

  # Render alignment score display with default value and dimmed style logic
  output$alignment_score_display <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$alignment_score)) {
          has_score <- FALSE
          score_value <- "-.--"
        } else {
          has_score <- !is.null(pd$analyzed_data$alignment_score)
          score_value <- if (has_score) format(round(as.numeric(pd$analyzed_data$alignment_score), 2), nsmall = 2) else "-.--"
        }
        div(
          class = if (has_score) "score-value has-score" else "score-value",
          style = "font-family: var(--bs-font-monospace); font-weight: 700;",
          score_value
        )
      },
      error = function(e) {
        print(paste("ERROR[alignment_score_display]:", e))
        div(
          class = "no-score",
          style = "font-family: var(--bs-font-monospace); color: #D3D3D366;",
          div(
            class = "score-value",
            style = "font-family: var(--bs-font-monospace); font-weight: 700;",
            "-.--"
          )
        )
      }
    )
  })

  # Initialize project data structure
  project_data <- reactiveValues(
    # Project Information
    project_info = list(
      title = "New Project", # character(1)
      description = "", # character(1)
      authors = list(), # list of author information
      report_formats = character(0), # character vector of selected formats
      report_date = format(Sys.Date(), "%Y-%m-%d"), # Default to today's date
      report_keywords = NULL # character vector
    ),

    # Cleaned Data
    cleaned_data = list(
      indicators = NULL,
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL
    ),

    # Analysis Results
    analysis = list(
      alignment = NULL,
      dynamics = NULL,
      cascade = NULL
    ),

    # Visualization Objects (including ggplot2 objects)
    visualization = list(
      indicators = NULL, # ggplot2 object
      alignment = NULL, # ggplot2 object
      dynamics = NULL, # ggplot2 object
      cascade = NULL # ggplot2 object
    ),

    # Status Tracking
    status = list(
      setup_complete = FALSE,
      data_loaded = FALSE,
      analysis_complete = FALSE,
      visualization_complete = FALSE
    )
  )
  print(paste("DEBUG: class of project_data is", paste(class(project_data), collapse = ", ")))

  # Store module instances
  modules <- reactiveValues(
    load_clean = NULL,
    analyze = NULL,
    visualize = NULL,
    generate = NULL
  )

  # --- Project Setup Module: Call unconditionally at top-level ---
  setup_module <- setup_result

  # Initialize other modules after setup module to avoid dependency issues
  observe({
    req(setup_module)
    if (is.null(modules$load_clean)) {
      logger::log_info("Initializing load_clean module...")
      modules$load_clean <- mod_load_clean_server(
        id = "load_clean_1", # Match the ID used in UI
        project_data = project_data
      )
      logger::log_info("load_clean module initialized successfully")
    }
  })

  observe({
    req(setup_module)
    if (is.null(modules$analyze)) {
      logger::log_info("Initializing analyze module...")
      modules$analyze <- mod_analyze_server(
        id = "analyze_data_1", # Match the ID used in UI
        project_data = project_data
      )
      logger::log_info("analyze module initialized successfully")
    }
  })

  observe({
    req(setup_module)
    if (is.null(modules$visualize)) {
      logger::log_info("Initializing visualize module...")
      modules$visualize <- mod_visualize_server(
        id = "visualize_1", # Match the ID used in UI
        project_data = project_data
      )
      logger::log_info("visualize module initialized successfully")
    }
  })

  # Initialize generate module
  observe({
    req(setup_module)
    if (is.null(modules$generate)) {
      logger::log_info("Initializing generate module...")
      modules$generate <- mod_generate_server(
        id = "generate_1", # Match the ID used in UI
        project_data = project_data
      )
      logger::log_info("generate module initialized successfully")
    }
  })

  # Track previous tab for change detection
  previous_tab <- reactiveVal(NULL)

  # Simplified observer for main_navbar changes
  observeEvent(input$main_navbar,
    {
      req(input$main_navbar)
      current_tab <- tolower(input$main_navbar)
      logger::log_info("Main tab changed to:", current_tab)

      # Update the current tab
      previous_tab(current_tab)

      # Reset change tracking for the new tab
      tab_changes[[current_tab]] <- FALSE
    },
    ignoreInit = TRUE
  )

  # Observer for skip_overview toggle to switch tabs
  # Track the overview mode state
  overview_mode <- reactiveVal(FALSE)
  theme_mode <- reactiveVal("light")

  # Toggle dark mode when the theme button is clicked
  observeEvent(input$toggle_theme, {
    if (theme_mode() == "light") {
      theme_mode("dark")
    } else {
      theme_mode("light")
    }
    bslib::toggle_dark_mode(mode = theme_mode(), session = session)
  })

  # Render the theme indicator icon
  output$theme_indicator <- renderUI({
    if (theme_mode() == "dark") {
      ph("moon-stars", weight = "bold")
    } else {
      ph("sun", weight = "bold")
    }
  })

  output$save_snapshot <- downloadHandler(
    filename = function() {
      # Try to get project title and report date for a more descriptive filename
      project_title <- NULL
      report_date <- NULL

      # Safely extract project info
      tryCatch(
        {
          if (!is.null(project_data$project_info)) {
            project_title <- project_data$project_info$title
            report_date <- project_data$project_info$report_date
          }
        },
        error = function(e) {
          logger::log_warn("Error accessing project info for filename: {e$message}")
        }
      )

      # Create filename components
      filename_parts <- c()
      has_project_title <- FALSE

      # Add project title if available (clean it for filename use)
      if (!is.null(project_title) && is.character(project_title) && length(project_title) > 0 && nzchar(trimws(project_title))) {
        clean_title <- trimws(project_title)
        # Remove or replace problematic characters for filenames
        clean_title <- gsub("[<>:\"/\\|?*]", "_", clean_title) # Windows problematic chars
        clean_title <- gsub("[^A-Za-z0-9_\\-\\s]", "_", clean_title) # Keep only safe chars
        clean_title <- gsub("\\s+", "_", clean_title) # Replace spaces with underscores
        clean_title <- gsub("_{2,}", "_", clean_title) # Replace multiple underscores
        clean_title <- gsub("^_+|_+$", "", clean_title) # Remove leading/trailing underscores
        clean_title <- substr(clean_title, 1, 40) # Limit length for reasonable filename

        if (nzchar(clean_title)) {
          filename_parts <- c(filename_parts, clean_title)
          has_project_title <- TRUE
        }
      }

      # Add report date if available
      if (!is.null(report_date) && is.character(report_date) && length(report_date) > 0 && nzchar(trimws(report_date))) {
        # Try to parse and format the date
        tryCatch(
          {
            # Handle various date formats
            date_str <- trimws(report_date)
            parsed_date <- as.Date(date_str)

            if (!is.na(parsed_date)) {
              formatted_date <- format(parsed_date, "%Y%m%d")
              filename_parts <- c(filename_parts, formatted_date)
            } else {
              # Date parsing failed, use current date
              filename_parts <- c(filename_parts, format(Sys.time(), "%Y%m%d"))
            }
          },
          error = function(e) {
            # If date parsing fails, use current date
            filename_parts <- c(filename_parts, format(Sys.time(), "%Y%m%d"))
          }
        )
      } else {
        # No report date available, use current date
        filename_parts <- c(filename_parts, format(Sys.time(), "%Y%m%d"))
      }

      # Add timestamp for uniqueness
      filename_parts <- c(filename_parts, format(Sys.time(), "%H%M%S"))

      # Combine parts, fallback to generic name if no valid project title
      if (has_project_title && length(filename_parts) > 0) {
        filename <- paste(filename_parts, collapse = "_")
        # Final cleanup
        filename <- gsub("_{2,}", "_", filename)
        filename <- gsub("^_+|_+$", "", filename)
      } else {
        filename <- paste0("project_snapshot_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }

      # Ensure we have a valid filename
      if (nzchar(filename)) {
        final_filename <- paste0(filename, ".rds")
      } else {
        # Ultimate fallback
        final_filename <- paste0("snapshot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      }

      logger::log_info("Generated snapshot filename: {final_filename}")
      final_filename
    },
    content = function(file) {
      tryCatch(
        {
          # Debug project_data state before saving
          logger::log_info("=== PROJECT DATA STATE CHECK ===")
          logger::log_info("Project data reactive values structure:")
          logger::log_info("  - project_info exists: {!is.null(project_data$project_info)}")
          logger::log_info("  - data exists: {!is.null(project_data$data)}")
          logger::log_info("  - cleaned_data exists: {!is.null(project_data$cleaned_data)}")
          if (!is.null(project_data$cleaned_data)) {
            logger::log_info("    - indicators: {!is.null(project_data$cleaned_data$indicators)} ({if(!is.null(project_data$cleaned_data$indicators)) nrow(project_data$cleaned_data$indicators) else 0} rows)")
            logger::log_info("    - alignment: {!is.null(project_data$cleaned_data$alignment)} ({if(!is.null(project_data$cleaned_data$alignment)) nrow(project_data$cleaned_data$alignment) else 0} rows)")
            logger::log_info("    - dynamics: {!is.null(project_data$cleaned_data$dynamics)} ({if(!is.null(project_data$cleaned_data$dynamics)) nrow(project_data$cleaned_data$dynamics) else 0} rows)")
            logger::log_info("    - cascade: {!is.null(project_data$cleaned_data$cascade)}")
          }
          logger::log_info("  - raw_main_data: {!is.null(project_data$raw_main_data)} ({if(!is.null(project_data$raw_main_data)) nrow(project_data$raw_main_data) else 0} rows)")
          logger::log_info("  - raw_alignment_data: {!is.null(project_data$raw_alignment_data)} ({if(!is.null(project_data$raw_alignment_data)) nrow(project_data$raw_alignment_data) else 0} rows)")
          logger::log_info("  - status: {!is.null(project_data$status)}")
          if (!is.null(project_data$status)) {
            logger::log_info("    - data_loaded: {project_data$status$data_loaded}")
            logger::log_info("    - setup_complete: {project_data$status$setup_complete}")
          }
          logger::log_info("=== END PROJECT DATA STATE CHECK ===")

          # Determine the last active step based on completion status
          last_step <- "setup"
          if (isolate(project_data$status$visualization_complete)) {
            last_step <- "visualize"
          } else if (isolate(project_data$status$analysis_complete)) {
            last_step <- "analysis"
          } else if (isolate(project_data$status$data_loaded)) {
            last_step <- "load_clean"
          }

          # Function to extract only visualization completion status (no ggplot objects)
          extract_viz_status <- function(viz_data) {
            if (is.null(viz_data)) {
              return(NULL)
            }

            # Only save completion flags, not actual plot objects
            if (is.list(viz_data)) {
              viz_status <- list()
              for (viz_type in names(viz_data)) {
                if (!is.null(viz_data[[viz_type]])) {
                  viz_status[[viz_type]] <- list(
                    completed = TRUE,
                    timestamp = Sys.time()
                  )
                }
              }
              return(viz_status)
            }
            return(NULL)
          }

          # Create snapshot data with all relevant fields - use tryCatch for each isolate
          snapshot_data <- list(
            project_info = tryCatch(isolate(project_data$project_info), error = function(e) NULL),
            data = tryCatch(isolate(project_data$data), error = function(e) NULL),
            cleaned_data = tryCatch(isolate(project_data$cleaned_data), error = function(e) NULL),
            analysis = tryCatch(isolate(project_data$analysis), error = function(e) NULL),
            analyzed_data = tryCatch(isolate(project_data$analyzed_data), error = function(e) NULL),
            # Include visualization completion status only (recreate plots from analyzed_data)
            visualization_status = extract_viz_status(tryCatch(isolate(project_data$visualizations), error = function(e) NULL)),
            # Include brand configuration
            brand_config = tryCatch(isolate(project_data$brand_config), error = function(e) NULL),
            brand_yaml = tryCatch(isolate(project_data$brand_yaml), error = function(e) NULL),
            # Include metadata fields
            qualtrics_metadata_removed = tryCatch(isolate(project_data$qualtrics_metadata_removed), error = function(e) NULL),
            last_updated = tryCatch(isolate(project_data$last_updated), error = function(e) NULL),
            # Include raw uploaded data for restoration
            raw_data = list(
              main_data = tryCatch(isolate(project_data$raw_main_data), error = function(e) NULL),
              alignment_data = tryCatch(isolate(project_data$raw_alignment_data), error = function(e) NULL)
            ),
            status = tryCatch(isolate(project_data$status), error = function(e) list()),
            last_step = last_step,
            app_state = list(
              data_entry_mode = tryCatch(isolate(data_entry_mode()), error = function(e) FALSE),
              snapshot_mode = tryCatch(isolate(snapshot_mode()), error = function(e) FALSE),
              overview_mode = tryCatch(isolate(overview_mode()), error = function(e) FALSE),
              theme_mode = tryCatch(isolate(theme_mode()), error = function(e) "light")
            )
          )

          # Debug what's being saved
          logger::log_info("=== SNAPSHOT SAVE DEBUG ===")
          logger::log_info("Project info: {!is.null(snapshot_data$project_info)}")
          logger::log_info("Data: {!is.null(snapshot_data$data)}")
          logger::log_info("Cleaned data present: {!is.null(snapshot_data$cleaned_data)}")
          if (!is.null(snapshot_data$cleaned_data)) {
            logger::log_info("  - indicators: {!is.null(snapshot_data$cleaned_data$indicators)}")
            logger::log_info("  - alignment: {!is.null(snapshot_data$cleaned_data$alignment)}")
            logger::log_info("  - dynamics: {!is.null(snapshot_data$cleaned_data$dynamics)}")
            logger::log_info("  - cascade: {!is.null(snapshot_data$cleaned_data$cascade)}")
          }
          logger::log_info("Analysis data: {!is.null(snapshot_data$analyzed_data)}")
          logger::log_info("Visualization status: {!is.null(snapshot_data$visualization_status)}")
          if (!is.null(snapshot_data$visualization_status)) {
            logger::log_info("  - completed visualizations: {length(snapshot_data$visualization_status)}")
          }
          logger::log_info("Raw data: {!is.null(snapshot_data$raw_data)}")
          if (!is.null(snapshot_data$raw_data)) {
            logger::log_info("  - main_data: {!is.null(snapshot_data$raw_data$main_data)}")
            logger::log_info("  - alignment_data: {!is.null(snapshot_data$raw_data$alignment_data)}")
          }
          logger::log_info("Status: {!is.null(snapshot_data$status)}")
          if (!is.null(snapshot_data$status)) {
            logger::log_info("  - data_loaded: {snapshot_data$status$data_loaded}")
          }
          logger::log_info("Snapshot size: {object.size(snapshot_data)} bytes (no ggplot objects)")
          logger::log_info("=== END SNAPSHOT SAVE DEBUG ===")

          # Save to file
          saveRDS(snapshot_data, file)

          # Log completion but don't show banner yet (will be shown via JavaScript)
          logger::log_info("Snapshot file prepared for download")
        },
        error = function(e) {
          logger::log_error("Error saving snapshot: {e$message}")
          # Create a minimal snapshot without problematic data
          minimal_snapshot <- list(
            project_info = tryCatch(isolate(project_data$project_info), error = function(e) NULL),
            data = tryCatch(isolate(project_data$data), error = function(e) NULL),
            status = tryCatch(isolate(project_data$status), error = function(e) list()),
            last_step = last_step,
            error = paste("Snapshot saved with limited data due to serialization error:", e$message)
          )
          saveRDS(minimal_snapshot, file)
        }
      )
    },
    contentType = "application/octet-stream"
  )

  # Create a success banner UI function for save
  create_snapshot_save_success_banner <- function() {
    div(
      id = "snapshot_save_success_banner",
      style = "background-color: #5D7359; border-left: 4px solid #476240; padding: 15px;
             border-radius: 4px; display: flex; justify-content: space-between;
             align-items: center; width: calc(100% - 280px); opacity: 0;
             position: fixed; top: 72px; left: 250px; right: 20px; z-index: 9999;
             box-shadow: 0 4px 10px rgba(0,0,0,0.2);
             transition: opacity 0.3s ease;",
      div(
        style = "display: flex; align-items: center;",
        phosphoricons::ph_i("check-circle", size = "lg", fill = "#F5F1E8", class = "me-3"),
        div(
          h4("Snapshot Saved Successfully!", style = "margin: 0 0 5px 0; color: #F5F1E8; font-size: 16px;"),
          p("Your project data has been saved to file.", style = "margin: 0; color: #F5F1E8; font-size: 14px;")
        )
      ),
      actionButton(
        inputId = "dismiss_save_snapshot_banner",
        label = phosphoricons::ph_i("x", size = "lg"),
        style = "background: none; border: none; color: #F5F1E8; cursor: pointer; padding: 5px;"
      )
    )
  }

  # Helper function to show and auto-dismiss the save banner
  show_snapshot_save_success_banner <- function() {
    # Remove any existing banner first
    shinyjs::runjs("if(document.getElementById('snapshot_save_success_banner')) { document.getElementById('snapshot_save_success_banner').remove(); }")

    # Insert the banner at the top of the body
    insertUI(
      selector = "body",
      where = "afterBegin",
      ui = create_snapshot_save_success_banner(),
      immediate = TRUE
    )

    # Animate the banner into view
    shinyjs::runjs("
      setTimeout(function() {
        var banner = document.getElementById('snapshot_save_success_banner');
        if (banner) {
          banner.style.opacity = '1';
          // Adjust position based on sidebar state if needed
          if (document.querySelector('.sidebar-collapsed')) {
            banner.style.left = '80px';
            banner.style.width = 'calc(100% - 110px)';
          }
        }
      }, 100);

      // Auto-hide after 4 seconds
      setTimeout(function() {
        var banner = document.getElementById('snapshot_save_success_banner');
        if (banner) {
          banner.style.opacity = '0';
          setTimeout(function() {
            if (document.getElementById('snapshot_save_success_banner')) {
              document.getElementById('snapshot_save_success_banner').remove();
            }
          }, 300);
        }
      }, 4000);
    ")
  }

  # Helper function to hide the save success banner with animation
  hide_snapshot_save_success_banner <- function() {
    shinyjs::runjs("
      var banner = document.getElementById('snapshot_save_success_banner');
      if (banner) {
        banner.style.opacity = '0';
        setTimeout(function() {
          if (document.getElementById('snapshot_save_success_banner')) {
            document.getElementById('snapshot_save_success_banner').remove();
          }
        }, 300);
      }
    ")
  }

  # Create a success banner UI function
  create_snapshot_success_banner <- function() {
    div(
      id = "snapshot_success_banner",
      style = "background-color: #5D7359; border-left: 4px solid #476240; padding: 15px;
             border-radius: 4px; display: flex; justify-content: space-between;
             align-items: center; width: calc(100% - 280px); opacity: 0;
             position: fixed; top: 72px; left: 250px; right: 20px; z-index: 9999;
             box-shadow: 0 4px 10px rgba(0,0,0,0.2);
             transition: opacity 0.3s ease;",
      div(
        style = "display: flex; align-items: center;",
        phosphoricons::ph_i("check-circle", size = "lg", fill = "#F5F1E8", class = "me-3"),
        div(
          h4("Snapshot Loaded Successfully!", style = "margin: 0 0 5px 0; color: #F5F1E8; font-size: 16px;"),
          p("Your project data has been restored.", style = "margin: 0; color: #F5F1E8; font-size: 14px;")
        )
      ),
      div(
        shiny::actionButton(
          "dismiss_snapshot_banner",
          "Dismiss",
          icon = phosphoricons::ph_i("x"),
          class = "btn-sm btn-outline-light"
        )
      )
    )
  }

  # Helper function to show and auto-dismiss the banner
  show_snapshot_success_banner <- function() {
    # Remove any existing banner first
    shinyjs::runjs("if(document.getElementById('snapshot_success_banner')) { document.getElementById('snapshot_success_banner').remove(); }")

    # Insert the banner at the top of the body
    insertUI(
      selector = "body",
      where = "afterBegin",
      ui = create_snapshot_success_banner(),
      immediate = TRUE
    )

    # Make banner visible with animation and setup auto-dismiss after 2 seconds
    shinyjs::runjs("
      setTimeout(function() {
        var banner = document.getElementById('snapshot_success_banner');
        if (banner) {
          banner.style.opacity = '1';
          // Adjust position based on sidebar state if needed
          if (document.querySelector('.sidebar-collapsed')) {
            banner.style.left = '80px';
            banner.style.width = 'calc(100% - 110px)';
          }
        }
      }, 100);

      // Auto-hide after 4 seconds
      setTimeout(function() {
        var banner = document.getElementById('snapshot_success_banner');
        if (banner) {
          banner.style.opacity = '0';
          setTimeout(function() {
            if (document.getElementById('snapshot_success_banner')) {
              document.getElementById('snapshot_success_banner').remove();
            }
          }, 300);
        }
      }, 4000);
    ")
  }

  # Helper function to hide the success banner with animation
  hide_snapshot_success_banner <- function() {
    shinyjs::runjs("
      var banner = document.getElementById('snapshot_success_banner');
      if (banner) {
        banner.style.opacity = '0';
        setTimeout(function() {
          if (document.getElementById('snapshot_success_banner')) {
            document.getElementById('snapshot_success_banner').remove();
          }
        }, 300);
      }
    ")
  }

  # Load uploaded RDS file into project_data
  observeEvent(input$load_snapshot, {
    req(input$load_snapshot)

    # Process snapshot file
    snapshot_path <- input$load_snapshot$datapath
    snapshot_data <- readRDS(snapshot_path)

    # Log the loaded snapshot contents
    logger::log_info("Snapshot loaded successfully")

    # Update project data with snapshot
    project_data$project_info <- snapshot_data$project_info
    project_data$data <- snapshot_data$data

    # Handle backwards compatibility for cleaned_data
    if (!is.null(snapshot_data$cleaned_data)) {
      project_data$cleaned_data <- snapshot_data$cleaned_data
    }

    project_data$analysis <- snapshot_data$analysis

    # Handle backwards compatibility for analyzed_data
    if (!is.null(snapshot_data$analyzed_data)) {
      project_data$analyzed_data <- snapshot_data$analyzed_data
    }

    # Restore visualization completion status and trigger recreation if needed
    if (!is.null(snapshot_data$visualization_status)) {
      # Set visualization completion flag
      project_data$status$visualization_complete <- length(snapshot_data$visualization_status) > 0

      # Store the completion status for potential recreation
      project_data$visualization_status <- snapshot_data$visualization_status

      # Note: Actual plots will be recreated when the visualization module is accessed
      logger::log_info("Visualization completion status restored - plots will be recreated when needed")
    } else {
      # Backward compatibility: check for old visualization fields
      has_old_viz <- !is.null(snapshot_data$visualization) || !is.null(snapshot_data$visualizations)
      if (has_old_viz) {
        project_data$status$visualization_complete <- TRUE
        logger::log_info("Old visualization format detected - setting completion flag")
      }
    }

    # Restore brand configuration
    if (!is.null(snapshot_data$brand_config)) {
      project_data$brand_config <- snapshot_data$brand_config
    }
    if (!is.null(snapshot_data$brand_yaml)) {
      project_data$brand_yaml <- snapshot_data$brand_yaml
    }

    # Restore metadata fields
    if (!is.null(snapshot_data$qualtrics_metadata_removed)) {
      project_data$qualtrics_metadata_removed <- snapshot_data$qualtrics_metadata_removed
    }
    if (!is.null(snapshot_data$last_updated)) {
      project_data$last_updated <- snapshot_data$last_updated
    }

    # Restore raw uploaded data
    if (!is.null(snapshot_data$raw_data)) {
      if (!is.null(snapshot_data$raw_data$main_data)) {
        project_data$raw_main_data <- snapshot_data$raw_data$main_data
      }
      if (!is.null(snapshot_data$raw_data$alignment_data)) {
        project_data$raw_alignment_data <- snapshot_data$raw_data$alignment_data
      }
    }

    # Restore status information if available
    if (!is.null(snapshot_data$status)) {
      project_data$status$setup_complete <- snapshot_data$status$setup_complete %||% FALSE
      project_data$status$data_loaded <- snapshot_data$status$data_loaded %||% FALSE
      project_data$status$analysis_complete <- snapshot_data$status$analysis_complete %||% FALSE
      project_data$status$visualization_complete <- snapshot_data$status$visualization_complete %||% FALSE
    }

    # Fallback: If we have cleaned data but data_loaded is FALSE, set it to TRUE
    has_cleaned_data <- !is.null(project_data$cleaned_data$indicators) ||
      !is.null(project_data$cleaned_data$alignment) ||
      !is.null(project_data$cleaned_data$dynamics) ||
      !is.null(project_data$cleaned_data$cascade)

    if (has_cleaned_data && !project_data$status$data_loaded) {
      project_data$status$data_loaded <- TRUE
      logger::log_info("Set data_loaded = TRUE based on presence of cleaned data")
    }

    # Restore app state if available
    if (!is.null(snapshot_data$app_state)) {
      if (!is.null(snapshot_data$app_state$data_entry_mode)) {
        data_entry_mode(snapshot_data$app_state$data_entry_mode)
      }
      if (!is.null(snapshot_data$app_state$snapshot_mode)) {
        snapshot_mode(snapshot_data$app_state$snapshot_mode)
      }
      if (!is.null(snapshot_data$app_state$overview_mode)) {
        overview_mode(snapshot_data$app_state$overview_mode)
      }
      if (!is.null(snapshot_data$app_state$theme_mode)) {
        theme_mode(snapshot_data$app_state$theme_mode)
        bslib::toggle_dark_mode(mode = theme_mode(), session = session)
      }
    }

    # Navigate to the last active step if available
    if (!is.null(snapshot_data$last_step)) {
      last_step <- snapshot_data$last_step
      if (last_step == "load_clean") {
        bslib::accordion_panel_set(
          id = "load_clean_1-load_clean_accordion",
          values = "results",
          session = session
        )
      } else if (last_step == "analysis") {
        bslib::accordion_panel_set(
          id = "analyze_data_1-analyze_accordion",
          values = "results",
          session = session
        )
      } else if (last_step == "visualize") {
        bslib::accordion_panel_set(
          id = "visualize_1-visualize_accordion",
          values = "results",
          session = session
        )
      } else if (last_step == "generate") {
        bslib::accordion_panel_set(
          id = "generate_1-generate_accordion",
          values = "generate_report",
          session = session
        )
      }
    }
    # Send message to UI to update button text
    session$sendCustomMessage("snapshotLoaded", NULL)

    # Force reactive invalidation to trigger UI updates
    isolate({
      # Touch the project_data to force reactive updates
      project_data$last_updated <- Sys.time()
    })

    # Small delay to ensure all reactive updates complete before showing banner
    shiny::invalidateLater(100, session)

    # Show success banner
    show_snapshot_success_banner()
  })

  # Handle snapshot download completion from JavaScript
  observeEvent(input$snapshot_download_completed, {
    show_snapshot_save_success_banner()
  })

  # Handle dismiss button for snapshot success banner
  observeEvent(input$dismiss_snapshot_banner, {
    hide_snapshot_success_banner()
  })

  # Handle dismiss button for snapshot save success banner
  observeEvent(input$dismiss_save_snapshot_banner, {
    hide_snapshot_save_success_banner()
  })

  # Render the overview indicator icon
  output$overview_indicator <- renderUI({
    if (isTRUE(overview_mode())) {
      ph("rocket-launch", weight = "light", class = "indicator-icon")
    } else {
      ph("lighthouse", weight = "light", class = "indicator-icon")
    }
  })



  # Render the snapshot indicator icon
  output$snapshot_indicator <- renderUI({
    if (isTRUE(snapshot_mode())) {
      bsicons::bs_icon("bookmark-check", class = "indicator-icon")
    } else {
      ph("camera", weight = "light", class = "indicator-icon")
    }
  })

  output$snapshot_load_indicator <- renderUI({
    ph("camera-plus", weight = "light", class = "indicator-icon")
  })

  observeEvent(input$data_mode_selector, {
    # Handle data mode selector input
    if (input$data_mode_selector == "enter") {
      data_entry_mode(TRUE)
      entry_mode <- "manual"
      nav_show(id = "main_navbar", target = "Enter Data")
      nav_hide(id = "main_navbar", target = "Upload Data")
      # Update visual selector state
      session$sendCustomMessage("setDataModeSelector", list(mode = "enter"))
    } else if (input$data_mode_selector == "upload") {
      data_entry_mode(FALSE)
      entry_mode <- "upload"
      nav_hide(id = "main_navbar", target = "Enter Data")
      nav_show(id = "main_navbar", target = "Upload Data")
      # Update visual selector state
      session$sendCustomMessage("setDataModeSelector", list(mode = "upload"))
    }
  })

  # Render dynamics score icon based on score range
  output$dynamics_score_icon <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$dynamics_score)) {
          return(NULL)
        }

        score <- as.numeric(pd$analyzed_data$dynamics_score)
        icon_name <- if (score < 0.50) {
          "caret-circle-double-down"
        } else if (score >= 0.50 && score <= 0.59) {
          "caret-circle-down"
        } else if (score >= 0.60 && score <= 0.69) {
          "caret-circle-up-down"
        } else if (score >= 0.70 && score <= 0.79) {
          "caret-circle-up"
        } else {
          "caret-circle-double-up"
        }

        # Add margin to the right of the icon
        div(
          style = "margin-right: 5px;",
          ph(icon_name, weight = "bold")
        )
      },
      error = function(e) {
        print(paste("ERROR[dynamics_score_icon]:", e))
        NULL
      }
    )
  })

  # Render dynamics score display (now using project_data$analyzed_data) with tryCatch and debug
  output$dynamics_score_display <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        print(paste("DEBUG[dynamics_score_display]: typeof(pd) =", typeof(pd)))
        print(paste("DEBUG[dynamics_score_display]: class(pd) =", paste(class(pd), collapse = ", ")))
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$dynamics_score)) {
          has_score <- FALSE
          score_value <- "-.--"
        } else {
          has_score <- !is.null(pd$analyzed_data$dynamics_score)
          score_value <- if (has_score) format(round(as.numeric(pd$analyzed_data$dynamics_score), 2), nsmall = 2) else "-.--"
        }

        div(
          class = if (has_score) "score-value has-score" else "score-value",
          style = "font-family: var(--bs-font-monospace); font-weight: 700;",
          score_value
        )
      },
      error = function(e) {
        print(paste("ERROR[dynamics_score_display]:", e))
        div(
          class = "no-score",
          style = "font-family: var(--bs-font-monospace); color: #D3D3D366;",
          div(
            class = "score-value",
            style = "font-family: var(--bs-font-monospace);",
            "-.--"
          )
        )
      }
    )
  })

  # Render cascade score icon based on score range
  output$cascade_score_icon <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$cascade_score)) {
          return(NULL)
        }

        score <- as.numeric(pd$analyzed_data$cascade_score)
        icon_name <- if (score < 0.50) {
          "caret-circle-double-down"
        } else if (score >= 0.50 && score <= 0.59) {
          "caret-circle-down"
        } else if (score >= 0.60 && score <= 0.69) {
          "caret-circle-up-down"
        } else if (score >= 0.70 && score <= 0.79) {
          "caret-circle-up"
        } else {
          "caret-circle-double-up"
        }

        # Add margin to the right of the icon
        div(
          style = "margin-right: 5px;",
          ph(icon_name, weight = "bold")
        )
      },
      error = function(e) {
        print(paste("ERROR[cascade_score_icon]:", e))
        NULL
      }
    )
  })

  # Render cascade score display (now using project_data$analyzed_data) with tryCatch and debug
  output$cascade_score_display <- renderUI({
    tryCatch(
      {
        pd <- get0("project_data", ifnotfound = NULL)
        print(paste("DEBUG[cascade_score_display]: typeof(pd) =", typeof(pd)))
        print(paste("DEBUG[cascade_score_display]: class(pd) =", paste(class(pd), collapse = ", ")))
        if (is.null(pd) || !("reactivevalues" %in% class(pd)) ||
          is.null(pd$analyzed_data) || is.null(pd$analyzed_data$cascade_score)) {
          has_score <- FALSE
          score_value <- "-.--"
        } else {
          has_score <- !is.null(pd$analyzed_data$cascade_score)
          score_value <- if (has_score) format(round(as.numeric(pd$analyzed_data$cascade_score), 2), nsmall = 2) else "-.--"
        }

        div(
          class = if (has_score) "score-value has-score" else "score-value",
          style = "font-family: var(--bs-font-monospace); font-weight: 700;",
          score_value
        )
      },
      error = function(e) {
        print(paste("ERROR[cascade_score_display]:", e))
        div(
          class = "no-score",
          style = "font-family: var(--bs-font-monospace); color: #D3D3D366;",
          div(
            class = "score-value",
            style = "font-family: var(--bs-font-monospace); font-weight: 700;",
            "-.--"
          )
        )
      }
    )
  })
}
