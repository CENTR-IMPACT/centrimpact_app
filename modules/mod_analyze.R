#!! ModName = mod_analyze
# !! ModDisplayName = Analyze Data
# !! ModDescription = Analyze project data including alignment, dynamics, and cascade metrics
# !! ModCitation = Price, Jeremy F. (2025). mod_analyze. [Source code].
# !! ModNotes = This module provides functionality to analyze project data across multiple dimensions.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for analysis !! reactive

# Load required libraries and utilities (assumed to be in global.R)
#' @importFrom phosphoricons ph ph_i
#' @importFrom shiny NS tagList observe observeEvent reactive reactiveVal reactiveValues req debounce updateTextInput updateDateInput updateTextAreaInput renderUI showNotification renderTable uiOutput
#' @importFrom shinyjs useShinyjs delay html runjs
#' @importFrom bslib navset_card_tab nav_panel card card_body accordion accordion_panel value_box value_box_theme tooltip layout_columns
#' @importFrom DT renderDataTable datatable dataTableOutput
#' @importFrom logger log_info log_warn log_error log_trace
#' @importFrom shinyWidgets radioGroupButtons progressBar updateProgressBar
#' @importFrom centrimpact analyze_alignment analyze_dynamics analyze_cascade
#' @importFrom utils capture.output str head write.csv

# Minimal logger:: logging for module load
logger::log_info("[@mod_analyze.R] Analyze module loaded")

# Define null-coalescing operator for cleaner code
`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============================================================================
#
# UI FUNCTION
#
# ==============================================================================

mod_analyze_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    # Add CSS for modal styling
    tags$style("
      /* Make modal backdrop more transparent */
      .modal-backdrop {
        background-color: rgba(0, 0, 0, 0.3) !important;
      }

      /* Ensure modal stays within viewport */
      .modal-dialog {
        max-height: 90vh;
        margin: 2rem auto;
      }

      /* Style data tables in modals */
      .modal .dataTable {
        width: 100% !important;
        margin-top: 1rem !important;
      }

      /* Improve modal footer spacing */
      .modal-footer {
        padding: 0.5rem 1rem;
        justify-content: center;
      }
    "),
    bslib::accordion(
      id = ns("analyze_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "analyze_data",
        title = tagList(
          ph("calculator"),
          HTML("&nbsp;"),
          "Analyze Data"
        ),
        fluidRow(
          column(
            width = 11,
            p("This module allows you to analyze your project data. Select the type of analysis you want to perform and run the analysis."),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend("Select", class = "custom-legend"),
              shinyWidgets::radioGroupButtons(
                inputId = ns("analysis_type"),
                label = "ANALYSIS TYPE",
                choices = list(
                  "All Data" = "full",
                  "Alignment Data" = "alignment",
                  "Dynamics Data" = "dynamics",
                  "Cascade Data" = "cascade"
                ),
                selected = "full",
                direction = "horizontal",
                justified = TRUE
              )
            ),
            tags$fieldset(
              class = "custom-fieldset",
              tags$legend("Analyze", class = "custom-legend"),
              fluidRow(
                class = "d-flex justify-content-center align-items-center",
                actionButton(
                  inputId = ns("run_analysis"),
                  label = tagList("Analyze Data ", ph("calculator", weight = "bold")),
                  width = "50%",
                  class = "btn btn-primary btn-lg"
                )
              ),
              fluidRow(
                class = "d-flex justify-content-center",
                div(
                  id = ns("progress_container"),
                  style = "width: 50%; visibility: hidden; margin-top: 1em;",
                  shinyWidgets::progressBar(
                    id = ns("progress_bar"),
                    value = 0,
                    total = 100,
                    status = "success",
                    title = "Ready to Analyze"
                  )
                )
              )
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        # Refactored: Use separate UI outputs for each result type.
        # This prevents re-rendering everything when only one result changes.
        uiOutput(ns("results_ui"))
      )
    )
  )
}


# ==============================================================================
#
# SERVER FUNCTION
#
# ==============================================================================

mod_analyze_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    logger::log_info("[@mod_analyze.R] Module server initialized for id: {id}")

    # Initialize analysis reactiveValues if they don't exist
    if (is.null(project_data$analysis)) {
      project_data$analysis <- reactiveValues(
        alignment_analyzed = FALSE,
        dynamics_analyzed = FALSE,
        cascade_analyzed = FALSE,
        last_analysis_type = NULL,
        last_updated = NULL,
        alignment_score = NULL,
        alignment_table = NULL,
        alignment_icc = NULL,
        dynamics_score = NULL,
        dynamics_domains = NULL,
        dynamics_dynamics = NULL,
        cascade_score = NULL,
        cascade_data = NULL
      )
    }

    # =========================================================================
    # >> HELPER: ANALYSIS FUNCTIONS
    #
    # These functions are now defined once in the server scope, not inside
    # an observer. They contain the core logic for running each analysis type
    # and return a standardized list structure.
    # =========================================================================

    #' @title Run Alignment Analysis
    #' @description Wraps the centrimpact::analyze_alignment call with validation.
    #' @param data A data.frame with alignment data.
    #' @return A standardized list: list(score, table, icc).
    run_alignment_analysis <- function(data) {
      req(data)
      logger::log_info("Running alignment analysis...")
      result <- centrimpact::analyze_alignment(alignment_df = data)
      logger::log_info("Alignment analysis complete.")
      return(list(
        score = result$alignment_score %||% NA_real_,
        table = result$alignment_medians %||% data.frame(),
        icc = result$icc_score %||% NA_real_
      ))
    }

    #' @title Run Dynamics Analysis
    #' @description Wraps the centrimpact::analyze_dynamics call with validation.
    #' @param data A data.frame with dynamics data.
    #' @return A standardized list: list(score, domains, dynamics).
    run_dynamics_analysis <- function(data) {
      req(data)
      logger::log_info("Running dynamics analysis...")

      # Debug input data
      cat("DEBUG: Input data for dynamics analysis:\n")
      cat("  Class:", class(data), "\n")
      cat("  Dimensions:", dim(data), "\n")
      cat("  Column names:", paste(names(data), collapse = ", "), "\n")
      if (nrow(data) > 0) {
        cat("  First few rows:\n")
        print(head(data))
      }

      result <- centrimpact::analyze_dynamics(dynamics_df = data)

      # Debug output result
      cat("DEBUG: Result from centrimpact::analyze_dynamics:\n")
      cat("  Result class:", class(result), "\n")
      cat("  Result names:", paste(names(result), collapse = ", "), "\n")

      if (!is.null(result$dynamics_score)) {
        cat("  dynamics_score:", result$dynamics_score, "\n")
      } else {
        cat("  dynamics_score: NULL\n")
      }

      if (!is.null(result$domain_df)) {
        cat("  domain_df dimensions:", dim(result$domain_df), "\n")
        cat("  domain_df names:", paste(names(result$domain_df), collapse = ", "), "\n")
        if (nrow(result$domain_df) > 0) {
          cat("  domain_df first few rows:\n")
          print(head(result$domain_df))
        }
      } else {
        cat("  domain_df: NULL\n")
      }

      if (!is.null(result$dynamics_df)) {
        cat("  dynamics_df dimensions:", dim(result$dynamics_df), "\n")
        cat("  dynamics_df names:", paste(names(result$dynamics_df), collapse = ", "), "\n")
        if (nrow(result$dynamics_df) > 0) {
          cat("  dynamics_df first few rows:\n")
          print(head(result$dynamics_df))
        }
      } else {
        cat("  dynamics_df: NULL\n")
      }

      logger::log_info("Dynamics analysis complete.")

      # Debug the final return structure
      final_result <- list(
        score = 1 - (result$dynamics_score %||% 0), # Invert Gini to represent balance
        domains = result$domain_df %||% data.frame(),
        dimensions = result$dynamics_df %||% data.frame()
      )

      cat("DEBUG: Final return structure from run_dynamics_analysis:\n")
      cat("  score:", final_result$score, "\n")
      cat("  domains dimensions:", dim(final_result$domains), "\n")
      cat("  dimensions dimensions:", dim(final_result$dimensions), "\n")
      if (!is.null(final_result$dimensions) && nrow(final_result$dimensions) > 0) {
        cat("  dimensions column names:", paste(names(final_result$dimensions), collapse = ", "), "\n")
      }

      return(final_result)
    }

    #' @title Run Cascade Analysis
    #' @description Wraps the centrimpact::analyze_cascade call with validation.
    #' @param data A data.frame or list with cascade data.
    #' @return A standardized list: list(score, data).
    run_cascade_analysis <- function(data) {
      req(data)
      # The analysis function might expect a specific structure (e.g., an edgelist)
      # This helper can prepare the data if needed.
      cascade_input <- if (is.list(data) && !is.null(data$edges)) data$edges else data

      logger::log_info("Running cascade analysis...")
      result <- centrimpact::analyze_cascade(network_df = cascade_input)
      logger::log_info("Cascade analysis complete.")

      # Log the result structure before returning
      logger::log_info("Cascade analysis result structure:")
      logger::log_info("  Result class: {class(result)}")
      if (is.list(result)) {
        logger::log_info("  Result contains: {paste(names(result), collapse=', ')}")

        if (!is.null(result$cascade_score)) {
          logger::log_info("  Cascade score: {result$cascade_score}")
        }

        if (!is.null(result$cascade_df)) {
          logger::log_info("  Cascade df class: {class(result$cascade_df)}")
          if (is.data.frame(result$cascade_df)) {
            logger::log_info("  Cascade df dimensions: {paste(dim(result$cascade_df), collapse='<U+00D7>')}")
            logger::log_info("  Cascade df columns: {paste(names(result$cascade_df), collapse=', ')}")
          }
        }
      }

      # Create return object
      ret_val <- list(
        score = 1 - (result$cascade_score %||% 0), # Invert Gini to represent balance
        data = result$cascade_df %||% data.frame()
      )

      # Log what we're returning
      logger::log_info("Returning cascade analysis:")
      logger::log_info("  Score: {ret_val$score}")
      logger::log_info("  Data dimensions: {paste(dim(ret_val$data), collapse='<U+00D7>')}")
      if (is.data.frame(ret_val$data) && ncol(ret_val$data) > 0) {
        logger::log_info("  Data columns: {paste(names(ret_val$data), collapse=', ')}")
      }

      return(ret_val)
    }

    # =========================================================================
    # >> REACTIVE DATA ACCESSORS
    # =========================================================================

    alignment_data <- reactive({
      project_data$cleaned_data$alignment %||% NULL
    })
    dynamics_data <- reactive({
      project_data$cleaned_data$dynamics %||% NULL
    })
    cascade_data <- reactive({
      project_data$cleaned_data$cascade %||% NULL
    })


    # =========================================================================
    # >> OBSERVER: RUN ANALYSIS
    #
    # This is the primary observer for the "Analyze Data" button. It has been
    # refactored to be a generic runner that calls the specific helper
    # functions. It handles progress updates and notifications.
    # =========================================================================

    observeEvent(input$run_analysis, {
      req(input$analysis_type)
      analysis_type <- input$analysis_type
      log_info("=== RUN ANALYSIS: {analysis_type} ===")

      # --- Progress Bar Setup ---
      shinyjs::runjs(sprintf('var elem = document.getElementById("%s"); if(elem) elem.style.visibility = "visible";', ns("progress_container")))
      update_progress <- function(value, text) {
        shinyWidgets::updateProgressBar(session, ns("progress_bar"), value = value, title = text)
      }
      update_progress(0, "Starting Analysis...")

      # --- Generic Analysis Runner ---
      run_single_analysis <- function(type, data_func, analysis_func) {
        data <- data_func()

        # FIX: More robust data validation to prevent errors with lists vs data.frames
        is_data_missing <- if (type %in% c("alignment", "dynamics")) {
          !is.data.frame(data) || nrow(data) == 0
        } else {
          is.null(data)
        }

        if (is_data_missing) {
          log_warn("No valid data available for '{type}' analysis. Skipping.")
          return(NULL)
        }

        tryCatch(
          {
            result <- analysis_func(data)
            # Store results in the main reactiveValues object
            isolate({
              # Set flags in project_data$analysis for visualization module
              project_data$analysis[[paste0(type, "_analyzed")]] <- TRUE
              project_data$analysis[[paste0(type, "_score")]] <- result$score

              # Also store in analyzed_data for backward compatibility
              project_data$analyzed_data[[paste0(type, "_score")]] <- result$score
              project_data$analyzed_data[[paste0(type, "_analyzed")]] <- TRUE

              # Store other parts of the result based on type
              if (type == "alignment") {
                project_data$analysis$alignment_table <- result$table
                project_data$analysis$alignment_icc <- result$icc
                project_data$analyzed_data$alignment_table <- result$table
                project_data$analyzed_data$alignment_score <- result$score
              } else if (type == "dynamics") {
                project_data$analysis$dynamics_domains <- result$domains
                project_data$analysis$dynamics_dimensions_table <- result$dimensions
                project_data$analysis$dynamics_score <- result$score
                project_data$analyzed_data$dynamics_domains_table <- result$domains
                project_data$analyzed_data$dynamics_dimensions_table <- result$dimensions
                project_data$analyzed_data$dynamics_score <- result$score

                # Debug what gets stored
                cat("DEBUG: Storing dynamics analysis results:\n")
                cat(
                  "  project_data$analysis$dynamics_dimensions_table dimensions:",
                  dim(project_data$analysis$dynamics_dimensions_table), "\n"
                )
                if (!is.null(project_data$analysis$dynamics_dimensions_table) &&
                  nrow(project_data$analysis$dynamics_dimensions_table) > 0) {
                  cat(
                    "  project_data$analysis$dynamics_dimensions_table column names:",
                    paste(names(project_data$analysis$dynamics_dimensions_table), collapse = ", "), "\n"
                  )
                }
              } else if (type == "cascade") {
                project_data$analysis$cascade_data <- result$data
                project_data$analysis$cascade_score <- result$score
                project_data$analyzed_data$cascade_table <- result$data
                project_data$analyzed_data$cascade_score <- result$score
              }
            })
            return(TRUE)
          },
          error = function(e) {
            log_error("Analysis failed for type '{type}': {e$message}")
            return(FALSE)
          }
        )
      }

      # --- Execute Analysis ---
      if (analysis_type == "full") {
        logger::log_info("Running full analysis with all data types")

        # Check data availability before each analysis
        update_progress(10, "Analyzing Alignment...")
        alignment_ok <- run_single_analysis("alignment", alignment_data, run_alignment_analysis)
        if (!alignment_ok) logger::log_warn("Alignment analysis failed or had no data")

        update_progress(40, "Analyzing Dynamics...")
        dynamics_ok <- run_single_analysis("dynamics", dynamics_data, run_dynamics_analysis)
        if (!dynamics_ok) logger::log_warn("Dynamics analysis failed or had no data")

        update_progress(70, "Analyzing Cascade...")
        cascade_ok <- run_single_analysis("cascade", cascade_data, run_cascade_analysis)
        if (!cascade_ok) logger::log_warn("Cascade analysis failed or had no data")

        update_progress(100, "All Analyses Finished")

        # Log available data after analysis
        logger::log_info("Analysis complete. Available data in project_data$analyzed_data: {paste(names(project_data$analyzed_data), collapse=', ')}")

        # Track last step and open the Results accordion panel
        isolate({
          project_data$status$last_step <- "results"
        })
        bslib::accordion_panel_set(
          id = "analyze_accordion",
          values = "results"
        )
      } else {
        logger::log_info("Running single analysis type: {analysis_type}")

        # Run the selected analysis
        analysis_ok <- run_single_analysis(analysis_type, get(paste0(analysis_type, "_data")), get(paste0("run_", analysis_type, "_analysis")))
        if (!analysis_ok) logger::log_warn("{analysis_type} analysis failed or had no data")

        update_progress(100, "Analysis Complete")

        # Log available data after analysis
        logger::log_info("Analysis complete. Available data in project_data$analyzed_data: {paste(names(project_data$analyzed_data), collapse=', ')}")

        # Track last step and open the Results accordion panel
        isolate({
          project_data$status$last_step <- "results"
        })
        bslib::accordion_panel_set(
          id = "analyze_accordion",
          values = "results"
        )
      }

      # --- Finalize ---
      isolate({
        project_data$analyzed_data$last_analysis_type <- analysis_type
        project_data$analyzed_data$last_updated <- Sys.time()
      })
      delay(2000, shinyjs::runjs(sprintf('var elem = document.getElementById("%s"); if(elem) elem.style.visibility = "hidden";', ns("progress_container"))))
    })

    output$results_ui <- renderUI({
      ns <- session$ns

      # Debug: Log basic structure information
      logger::log_info("analyzed_data names: {paste(names(project_data$analyzed_data), collapse = ", ")}")

      # Check if alignment data exists and log its structure
      if (!is.null(project_data$analyzed_data$alignment)) {
        logger::log_info("Alignment data exists. Names: {paste(names(project_data$analyzed_data$alignment), collapse = ", ")}")

        # Check if alignment_table exists and log its structure
        if (!is.null(project_data$analyzed_data$alignment$alignment_table)) {
          logger::log_info("alignment_table class: {class(project_data$analyzed_data$alignment$alignment_table)}")
          logger::log_info("alignment_table dimensions: {nrow(project_data$analyzed_data$alignment$alignment_table)} rows, {ncol(project_data$analyzed_data$alignment$alignment_table)} columns")
          logger::log_info("alignment_table column names: {paste(names(project_data$analyzed_data$alignment$alignment_table), collapse = ", ")}")
        } else {
          logger::log_info("alignment_table is NULL")
        }

        # Log alignment score
        if (!is.null(project_data$analyzed_data$alignment$alignment_score)) {
          logger::log_info("alignment_score: {project_data$analyzed_data$alignment$alignment_score}")
        } else {
          logger::log_info("alignment_score is NULL")
        }
      } else {
        logger::log_info("No alignment data found in project_data$analyzed_data")
      }

      tagList(
        # -- ALIGNMENT SECTION --
        metric_section_ui(
          data = project_data$analyzed_data$alignment_table,

          # Value box parameters
          title = "Alignment Score",
          value = project_data$analyzed_data$alignment_score,
          show_metrics = TRUE,

          # Card parameters
          card_header_text = "READY FOR VISUALIZATION",
          card_body = create_generate_list_taglist(c(
            "Partner Medians Calculated",
            "Researcher Medians Calculated",
            "Alignment Score Calculated"
          )),
          card_footer_button_text = "VIEW Data Table",
          card_footer_button_id = "view_alignment_overlay",

          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Project Alignment Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "lg"),

          # Required namespace function
          ns = ns,

          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Project Alignment"
        ),
        # -- DYNAMICS SECTION --
        metric_section_ui(
          data = project_data$analyzed_data$dynamics_domains_table,

          # Value box parameters
          title = "Project Dynamics Score",
          value = project_data$analyzed_data$dynamics_score,
          show_metrics = TRUE,

          # Card parameters
          card_header_text = "Ready for Visualization",
          card_body = create_generate_list_taglist(c(
            "Domain Scores Calculated",
            "Project Dynamics Analyzed",
            "Dynamics Score Calculated"
          )),
          card_footer_button_text = "VIEW Data Table",
          card_footer_button_id = "view_dynamics_overlay",

          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Project Dynamics Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "lg"),

          # Required namespace function
          ns = ns,

          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Project Dynamics"
        ),
        # -- CASCADE SECTION --
        metric_section_ui(
          data = project_data$analyzed_data$cascade_table,

          # Value box parameters
          title = "Cascade Effects Score",
          value = project_data$analyzed_data$cascade_score,
          show_metrics = TRUE,

          # Card parameters
          card_header_text = "Ready for Visualization",
          card_body = create_generate_list_taglist(c(
            "Cascade Effects Analyzed",
            "Impact Relationships Mapped",
            "Cascade Score Calculated"
          )),
          card_footer_button_text = "VIEW Data Table",
          card_footer_button_id = "view_cascade_overlay",

          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Cascade Effects Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "lg"),

          # Required namespace function
          ns = ns,

          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Cascade Effects"
        )
      )
    })


    # =========================================================================
    # >> DATA TABLE RENDERING
    # =========================================================================

    output$alignment_table <- DT::renderDataTable({
      req(project_data$analyzed_data$alignment_table)
      DT::datatable(
        project_data$analyzed_data$alignment_table,
        options = list(pageLength = 5, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "cell-border stripe hover compact"
      )
    })

    output$dynamics_table <- DT::renderDataTable({
      # The dynamics analysis returns multiple tables, choose the primary one
      req(project_data$analyzed_data$dynamics_domains)
      DT::datatable(
        project_data$analyzed_data$dynamics_domains,
        options = list(pageLength = 5, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "cell-border stripe hover compact"
      )
    })

    output$cascade_table <- DT::renderDataTable({
      # Debug information
      logger::log_info("Rendering cascade table")
      logger::log_info("project_data$analyzed_data exists: {!is.null(project_data$analyzed_data)}")

      if (!is.null(project_data$analyzed_data)) {
        # Log available keys
        logger::log_info("Keys in analyzed_data: {paste(names(project_data$analyzed_data), collapse=', ')}")

        # Look specifically for cascade-related keys
        cascade_keys <- grep("cascade", names(project_data$analyzed_data), value = TRUE, ignore.case = TRUE)
        logger::log_info("Cascade-related keys: {paste(cascade_keys, collapse=', ')}")

        # Check data$cascade if it exists
        if ("cascade" %in% names(project_data$analyzed_data)) {
          cascade_obj <- project_data$analyzed_data$cascade
          logger::log_info("cascade object class: {class(cascade_obj)}")

          if (is.list(cascade_obj)) {
            logger::log_info("cascade is a list with elements: {paste(names(cascade_obj), collapse=', ')}")

            if ("data" %in% names(cascade_obj)) {
              logger::log_info("cascade$data class: {class(cascade_obj$data)}")
              if (is.data.frame(cascade_obj$data)) {
                logger::log_info("cascade$data dimensions: {paste(dim(cascade_obj$data), collapse='<U+00D7>')}")
              }
            }

            if ("score" %in% names(cascade_obj)) {
              logger::log_info("cascade score: {cascade_obj$score}")
            }
          }
        }

        # Check cascade_data if it exists
        if ("cascade_data" %in% names(project_data$analyzed_data)) {
          cascade_data_obj <- project_data$analyzed_data$cascade_data
          logger::log_info("cascade_data class: {class(cascade_data_obj)}")

          if (is.data.frame(cascade_data_obj)) {
            logger::log_info("cascade_data dimensions: {paste(dim(cascade_data_obj), collapse='<U+00D7>')}")
            logger::log_info("cascade_data columns: {paste(names(cascade_data_obj), collapse=', ')}")
          }
        }
      }

      # Use a safer approach to get cascade data
      cascade_data_to_show <- if (!is.null(project_data$analyzed_data$cascade_data) &&
        is.data.frame(project_data$analyzed_data$cascade_data) &&
        nrow(project_data$analyzed_data$cascade_data) > 0) {
        logger::log_info("Using cascade_data directly")
        project_data$analyzed_data$cascade_data
      } else if (!is.null(project_data$analyzed_data$cascade)) {
        logger::log_info("Using cascade object")
        # Try alternative structure - just get the first data.frame in the cascade list
        cascade_obj <- project_data$analyzed_data$cascade
        if (is.list(cascade_obj) && !is.data.frame(cascade_obj)) {
          # Check if there's a data element directly
          if (!is.null(cascade_obj$data) && is.data.frame(cascade_obj$data)) {
            logger::log_info("Using cascade$data")
            cascade_obj$data
          } else {
            # Find the first data.frame in the list
            logger::log_info("Looking for first data.frame in cascade list")
            df_elements <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, cascade_obj)
            if (length(df_elements) > 0) {
              logger::log_info("Found data.frame in cascade list: {names(df_elements)[1]}")
              df_elements[[1]]
            } else {
              logger::log_info("No data.frames found in cascade list")
              data.frame(message = "No cascade data tables found")
            }
          }
        } else if (is.data.frame(cascade_obj)) {
          logger::log_info("cascade object is a data.frame")
          cascade_obj
        } else {
          logger::log_info("cascade object is not in expected format: {class(cascade_obj)}")
          data.frame(message = "Cascade data is not in expected format")
        }
      } else {
        logger::log_info("No cascade data found")
        data.frame(message = "No cascade data available")
      }

      DT::datatable(
        cascade_data_to_show,
        options = list(pageLength = 5, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "cell-border stripe hover compact"
      )
    })

    # =========================================================================
    # >> DATA TABLE MODAL DIALOGS
    # =========================================================================

    # Alignment data modal
    observeEvent(input$view_alignment_overlay, {
      logger::log_info("Alignment view button clicked")

      # Check for data availability before showing modal
      has_alignment_data <- !is.null(project_data$analyzed_data$alignment_table) &&
        is.data.frame(project_data$analyzed_data$alignment_table) &&
        nrow(project_data$analyzed_data$alignment_table) > 0

      logger::log_info("Alignment data available: {has_alignment_data}")

      showModal(modalDialog(
        title = "Project Alignment Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto;", # Constrain height
        if (has_alignment_data) {
          DT::dataTableOutput(ns("alignment_modal_table"))
        } else {
          div(
            style = "text-align: center; padding: 20px;",
            tags$i(class = "fa fa-table", style = "font-size: 48px; color: #d4d4d4; margin-bottom: 15px;"),
            tags$h4("No Alignment Data Available"),
            tags$p("Run the analysis first to generate alignment data.")
          )
        },
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Only attempt to render the table if data exists
      if (has_alignment_data) {
        output$alignment_modal_table <- DT::renderDataTable({
          DT::datatable(
            project_data$analyzed_data$alignment_table,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              dom = "ftip",
              scrollY = "50vh", # Constrain table height
              autoWidth = TRUE
            ),
            rownames = FALSE,
            class = "cell-border stripe hover compact"
          )
        })
      }
    })

    # Dynamics data modal
    observeEvent(input$view_dynamics_overlay, {
      logger::log_info("Dynamics view button clicked")

      # Check for data availability before showing modal
      has_dynamics_data <- !is.null(project_data$analyzed_data$dynamics_domains) &&
        is.data.frame(project_data$analyzed_data$dynamics_domains) &&
        nrow(project_data$analyzed_data$dynamics_domains) > 0

      logger::log_info("Dynamics data available: {has_dynamics_data}")

      showModal(modalDialog(
        title = "Project Dynamics Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto;", # Constrain height
        if (has_dynamics_data) {
          DT::dataTableOutput(ns("dynamics_modal_table"))
        } else {
          div(
            style = "text-align: center; padding: 20px;",
            tags$i(class = "fa fa-chart-line", style = "font-size: 48px; color: #d4d4d4; margin-bottom: 15px;"),
            tags$h4("No Dynamics Data Available"),
            tags$p("Run the analysis first to generate dynamics data.")
          )
        },
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Only attempt to render the table if data exists
      if (has_dynamics_data) {
        output$dynamics_modal_table <- DT::renderDataTable({
          DT::datatable(
            project_data$analyzed_data$dynamics_domains,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              dom = "ftip",
              scrollY = "50vh", # Constrain table height
              autoWidth = TRUE
            ),
            rownames = FALSE,
            class = "cell-border stripe hover compact"
          )
        })
      }
    })

    # Cascade data modal
    observeEvent(input$view_cascade_overlay, {
      logger::log_info("Cascade view button clicked")

      # Determine if we have cascade data
      has_cascade_data <- FALSE
      cascade_data_to_show <- NULL

      # Check different locations for cascade data
      if (!is.null(project_data$analyzed_data$cascade_table) &&
        is.data.frame(project_data$analyzed_data$cascade_table) &&
        nrow(project_data$analyzed_data$cascade_table) > 0) {
        logger::log_info("Found cascade_table data")
        has_cascade_data <- TRUE
        cascade_data_to_show <- project_data$analyzed_data$cascade_table
      } else if (!is.null(project_data$analyzed_data$cascade_data) &&
        is.data.frame(project_data$analyzed_data$cascade_data) &&
        nrow(project_data$analyzed_data$cascade_data) > 0) {
        logger::log_info("Found cascade_data directly")
        has_cascade_data <- TRUE
        cascade_data_to_show <- project_data$analyzed_data$cascade_data
      } else if (!is.null(project_data$analyzed_data$cascade)) {
        logger::log_info("Checking cascade object")
        cascade_obj <- project_data$analyzed_data$cascade
        if (is.data.frame(cascade_obj) && nrow(cascade_obj) > 0) {
          logger::log_info("cascade object is a data.frame")
          has_cascade_data <- TRUE
          cascade_data_to_show <- cascade_obj
        } else if (is.list(cascade_obj)) {
          # Check if there's a data element directly
          if (!is.null(cascade_obj$data) && is.data.frame(cascade_obj$data) && nrow(cascade_obj$data) > 0) {
            logger::log_info("Using cascade$data")
            has_cascade_data <- TRUE
            cascade_data_to_show <- cascade_obj$data
          } else {
            # Find the first data.frame in the list
            logger::log_info("Looking for first data.frame in cascade list")
            df_elements <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, cascade_obj)
            if (length(df_elements) > 0) {
              logger::log_info("Found data.frame in cascade list")
              has_cascade_data <- TRUE
              cascade_data_to_show <- df_elements[[1]]
            }
          }
        }
      }

      logger::log_info("Cascade data available: {has_cascade_data}")

      # Show modal with appropriate content
      showModal(modalDialog(
        title = "Cascade Effects Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto;", # Constrain height
        if (has_cascade_data) {
          DT::dataTableOutput(ns("cascade_modal_table"))
        } else {
          div(
            style = "text-align: center; padding: 20px;",
            tags$i(class = "fa fa-project-diagram", style = "font-size: 48px; color: #d4d4d4; margin-bottom: 15px;"),
            tags$h4("No Cascade Effects Data Available"),
            tags$p("Run the analysis first to generate cascade effects data.")
          )
        },
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Only attempt to render the table if data exists
      if (has_cascade_data && !is.null(cascade_data_to_show)) {
        output$cascade_modal_table <- DT::renderDataTable({
          DT::datatable(
            cascade_data_to_show,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              dom = "ftip",
              scrollY = "50vh", # Constrain table height
              autoWidth = TRUE
            ),
            rownames = FALSE,
            class = "cell-border stripe hover compact"
          )
        })
      }
    })
  }) # End moduleServer
} # End mod_analyze_server
