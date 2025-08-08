#!! ModName = mod_generate
# !! ModDisplayName = Generate Report
# !! ModDescription = Generate reports and verify project readiness.
# !! ModCitation = Price, Jeremy F. (2025). mod_generate. [Source code].
# !! ModNotes = This module provides functionality to verify project readiness and generate reports.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for report generation !! reactive
# !! FunctionReturn = report_data !! Generated report data !! reactive
# !! FunctionReturn = status !! Report generation status !! reactive

# Utilities are loaded in global.R



# UI function with accordion panels
# Helper function to generate section UI with lightbulb icons
generate_section_ui <- function(generate_title, generate_list, card_header_text = "PROCESSING", ns = identity) {
  # Create list items with lightbulb icons
  list_items <- lapply(seq_along(generate_list), function(i) {
    item_id <- paste0(gsub("[^a-zA-Z0-9]", "", tolower(generate_list[i])), "_", i)

    tags$div(
      class = "section-row d-flex align-items-center py-2 border-bottom",
      phosphoricons::ph_i("arrow-fat-lines-right", class = "text-info me-2"),
      tags$span(generate_list[i], class = "ms-2")
    )
  })

  # Create the card UI
  tags$div(
    class = "card mb-3 shadow-sm",
    tags$div(
      class = "card-header d-flex justify-content-between align-items-center py-2",
      tags$span(generate_title, class = "fw-medium"),
      tags$span(card_header_text, class = "badge bg-secondary text-light fw-medium")
    ),
    tags$div(
      class = "card-body p-3",
      tags$div(class = "list-group list-group-flush", list_items)
    )
  )
}

mod_generate_ui <- function(id) {
  ns <- shiny::NS(id)



  shiny::tagList(
    # Add shinyjs for UI interactions
    shinyjs::useShinyjs(),

    # Add JavaScript for force UI updates
    tags$script(HTML("
      Shiny.addCustomMessageHandler('force-update', function(message) {
        console.log('Force update triggered for:', message.id);
        var element = document.getElementById(message.id);
        if (element) {
          // Force a redraw by temporarily changing display
          var originalDisplay = element.style.display;
          element.style.display = 'none';
          element.offsetHeight; // Trigger reflow
          element.style.display = originalDisplay;
          console.log('Force update completed for:', message.id);
        } else {
          console.warn('Element not found for force update:', message.id);
        }
      });
    ")),

    # Add CSS for progress container visibility
    tags$style(HTML("
      .progress-container {
        width: 100% !important;
        margin: 15px 0 !important;
        visibility: visible !important;
        display: block !important;
      }
      .progress-container.hidden {
        display: none !important;
      }
      /* Ensure progress bar is visible */
      .progress {
        height: 20px !important;
        background-color: #f8f9fa !important;
        border-radius: 0.5rem !important;
        overflow: hidden !important;
      }
      .progress-bar {
        transition: width 0.3s ease !important;
      }
    ")),

    # Add custom JavaScript for progress handling
    shiny::tags$script(HTML(
      "// Debug function to check button functionality
      $(document).ready(function() {
        console.log('Generate module JavaScript loaded');
        setTimeout(function() {
          var checkBtn = document.getElementById('generate_1-check_readiness');
          console.log('Check Readiness button exists:', !!checkBtn);
          if (checkBtn) {
            console.log('Button element:', checkBtn);
            console.log('Button classes:', checkBtn.className);
            console.log('Button disabled:', checkBtn.disabled);
          } else {
            console.log('Button not found with ID: generate_1-check_readiness');
            console.log('All buttons on page:', document.querySelectorAll('button[id*=\"check_readiness\"]'));
          }

          // Also check for progress container
          var progressContainer = document.getElementById('generate_1-progress_container');
          console.log('Progress container exists:', !!progressContainer);
          if (progressContainer) {
            console.log('Progress container style:', progressContainer.style.cssText);
            console.log('Progress container display:', window.getComputedStyle(progressContainer).display);
          }
        }, 1000);
      });

      // Function to update section status text
      function updateSectionStatus(sectionId, status) {
        // Find the status element in the first column
        var statusEl = document.getElementById('status_' + sectionId);
        if (!statusEl) {
          // Find the status container in the first column
          var container = document.getElementById('status_container');
          if (!container) return;

          // Create new status element
          statusEl = document.createElement('div');
          statusEl.id = 'status_' + sectionId;
          statusEl.className = 'status-item';
          statusEl.style.textAlign = 'right';
          statusEl.style.padding = '10px 15px 0 0';
          statusEl.style.fontSize = '12px';
          statusEl.style.color = '#666';
          statusEl.style.minHeight = '40px';
          container.appendChild(statusEl);
        }

        // Update status text
        statusEl.textContent = status;
      }

      $(document).on('shiny:value', function(event) {
        if (event.target.id && event.target.id.endsWith('verification_results')) {
          // Scroll to results when they update
          var $target = $('#' + event.target.id);
          if ($target.length) {
            $('html, body').animate({
              scrollTop: $target.offset().top - 20
            }, 500);
          }
        }
      });

      // Custom handler for progress updates
      Shiny.addCustomMessageHandler('updateProgressBar', function(message) {
        try {
          if (!message || !message.id) return;

          // Only target the progress bar, not other elements
          var $progressBar = $('#' + message.id);
          if (!$progressBar.length) return;

          // Store the current value for cleanup
          var currentValue = message.value || 0;

          // Update progress bar
          $progressBar
            .css('width', currentValue + '%')
            .attr('aria-valuenow', currentValue);

          // Only target the progress bar's inner element
          var $progressBarInner = $progressBar.find('> .progress > .progress-bar');
          if ($progressBarInner.length) {
            $progressBarInner
              .css('width', currentValue + '%')
              .text(message.title || '')
              .removeClass('bg-info bg-success bg-danger bg-warning')
              .addClass('bg-' + (message.status || 'info'));
          }
        } catch(e) {
          console.error('Error updating progress bar:', e);
        }
      });
    "
    )),
    tags$style(HTML("
      .progress-bar-custom {
        width: 100%; height: 12px; background: #f0f0f0;
        border-radius: 6px; margin: 20px 0; overflow: hidden;
        border: 1px solid #ddd;
      }
      .progress-fill-custom {
        height: 100%; background: linear-gradient(90deg, #007bff, #0056b3);
        width: 0%; transition: width 0.8s ease-in-out; border-radius: 6px;
      }
      .status-text-custom {
        text-align: center; color: #666; margin: 15px 0; font-weight: 500;
        font-size: 16px; padding: 10px; background: #f8f9fa; border-radius: 6px;
      }
      .generate-btn {
        background: #007bff; color: white; border: none; padding: 12px 24px;
        border-radius: 6px; font-size: 16px; cursor: pointer;
        display: block; margin: 20px auto;
      }
      .generate-btn:hover { background: #0056b3; }
      .generate-btn:disabled { background: #6c757d; cursor: not-allowed; }
      .bslib-value-box .value-box-showcase {
      display: flex !important;
      align-items: center !important;  /* vertical centering */
      justify-content: center !important;  /* horizontal centering */
    }



      /* Section container styles - the key styling for sequential sections */
      .section-container {
        position: absolute;
        width: 100%;
        opacity: 0;
        visibility: hidden;
        transition: opacity 0.4s ease, visibility 0.4s ease;
        z-index: 1;
        top: 0;
        left: 0;
      }

      .section-container.active {
        opacity: 1;
        visibility: visible;
        z-index: 10;
      }



      /* Status text styles */
      .section-status {
        transition: all 0.3s ease;
        min-height: 40px;
        display: flex;
        align-items: center;
        justify-content: flex-end;
        padding-right: 10px;
        font-size: 12px;
      }

      .status-pending {
        color: #666;
      }

      .status-complete {
        color: #28a745;
        font-weight: 500;
      }

      /* Ensure status container stays aligned with sections */
      #status_container {
        position: relative;
      }

      /* Match the spacing of sections */
      .section-status {
        margin-bottom: 20px;
      }




    ")),

    # Main accordion
    bslib::accordion(
      id = ns("generate_accordion"),
      multiple = FALSE,
      open = "verify_readiness",

      # Panel 1: Verify Readiness
      bslib::accordion_panel(
        value = "verify_readiness",
        title = tagList(
          phosphoricons::ph("check-circle"),
          HTML("&nbsp;Verify Readiness")
        ),
        fluidRow(
          column(
            width = 5,
            shiny::actionButton(
              ns("check_readiness"),
              "Check Project Readiness",
              icon = phosphoricons::ph_i("list-checks"),
              class = "btn-primary btn-lg",
              onclick = "console.log('Button clicked directly!');"
            )
          ),
          column(
            width = 7,
            shiny::uiOutput(ns("verification_status"))
          )
        ),
        div(
          id = ns("progress_container"),
          class = "progress-container hidden",
          style = "width: 100%; margin: 15px 0;",
          tags$div(id = ns("progress_stage_message"), style = "text-align: left; font-family: var(--bs-code-font); text-transform: uppercase; margin-bottom: 5px; font-weight: 500; min-height: 20px;"),
          shinyWidgets::progressBar(
            id = ns("progress_bar"),
            value = 0, total = 100, status = "info",
            title = "", striped = TRUE, display_pct = FALSE
          )
        ),
        shiny::uiOutput(ns("verification_results"))
      ),

      # Panel 2: Generate Report
      bslib::accordion_panel(
        value = "generate_report",
        title = tagList(
          phosphoricons::ph_i("file-pdf"),
          HTML("&nbsp;Generate Report")
        ),
        # Action buttons at the top, outside the two-column layout
        div(
          style = "display: flex; align-items: center; justify-content: space-evenly; margin-bottom: 1.5em;",
          shinyjs::disabled(
            shiny::actionButton(
              ns("generate_report"),
              "Generate Report",
              icon = phosphoricons::ph_i("file-pdf"),
              class = "btn-primary"
            )
          ),
          shinyjs::disabled(
            shiny::downloadButton(
              ns("download_report"),
              "Download Report",
              icon = phosphoricons::ph_i("download"),
              class = "btn-primary"
            )
          )
        ),
        div(
          style = "width: 80%; margin: 0 auto;",
          # Progress section
          div(
            id = ns("progress_section"),
            class = "shinyjs-hide",
            shinyWidgets::progressBar(
              id = ns("progress_bar_generate"),
              value = 0,
              total = 100,
              status = "info",
              title = "Generating report...",
              striped = FALSE,
              display_pct = FALSE
            )
          ),

          # Container for all sections (they'll occupy the same space)
          div(
            id = ns("sections_wrapper"),
            style = "position: relative; min-height: 300px;",

            # Section 1: Collecting Plots and Tables
            div(
              id = ns("section1_container"),
              class = "section-container",
              generate_section_ui(
                generate_title = "Collect Plots and Tables",
                generate_list = c("Create Project Directory", "Setup Data Folders", "Prepare Plot Storage", "Initialize Temp Workspace"),
                card_header_text = "PROCESSING",
                ns = ns
              )
            ),

            # Section 2: Compile Metadata
            div(
              id = ns("section2_container"),
              class = "section-container",
              generate_section_ui(
                generate_title = "Compile Metadata",
                generate_list = c("Extract Project Information", "Process Report Settings", "Collect Analysis Results", "Prepare Data Exports"),
                card_header_text = "PROCESSING",
                ns = ns
              )
            ),

            # Section 3: Stitching Sections
            div(
              id = ns("section3_container"),
              class = "section-container",
              generate_section_ui(
                generate_title = "Stitch Sections Together",
                generate_list = c("Save Visualization Plots", "Export Data Tables", "Create Quarto Document", "Copy Template Assets"),
                card_header_text = "PROCESSING",
                ns = ns
              )
            ),

            # Section 4: Packaging Report
            div(
              id = ns("section4_container"),
              class = "section-container",
              generate_section_ui(
                generate_title = "Package Report",
                generate_list = c("Verify File Structure", "Bundle All Components", "Create ZIP Archive", "Prepare Download"),
                card_header_text = "PROCESSING",
                ns = ns
              )
            ),

            # Completion message banner (initially hidden)
            div(
              id = ns("completion_banner"),
              class = "section-container",
              style = "background-color: #5D7359; border-left: 4px solid #476240; padding: 20px; border-radius: 4px; min-height: 200px; display: flex; align-items: center;",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                div(
                  style = "display: flex; align-items: center;",
                  phosphoricons::ph_i("check-circle", size = "lg", fill = "#F5F1E8", class = "me-3"),
                  div(
                    h4("Report Generated Successfully!", style = "margin: 0 0 5px 0; color: #F5F1E8;"),
                    p("Your report is ready to download.", style = "margin: 0; color: #F5F1E8;")
                  )
                ),
                div(
                  # Only show the download button here, not in the UI
                  shiny::actionButton(
                    ns("dismiss_banner"),
                    "Dismiss",
                    icon = phosphoricons::ph_i("x"),
                    class = "btn-sm btn-outline-light"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server function with verification logic
mod_generate_server <- function(id, project_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    rv <- shiny::reactiveValues(
      verification_results = NULL,
      verification_status = NULL,
      all_checks_passed = FALSE,
      generation_status = "pending",
      last_step = NULL,
      generation_progress = 0,
      generation_message = "Ready to generate report...",
      show_partial_results = FALSE,
      partial_verification_results = NULL,
      current_stage_name = NULL,
      section_names = NULL,
      stage_display_names = list(),
      verification_stages = NULL,
      current_stage_index = 0,
      ui_initialized = FALSE
    )

    # Enable/disable download button based on data availability
    observe({
      shinyjs::toggleState("download_project_data",
        condition = !is.null(project_data) &&
          length(reactiveValuesToList(project_data)) > 0
      )
    })

    # Create a reactive value to trigger verification
    verification_trigger <- reactiveVal(NULL)

    # Initialize UI state - ensure verification results are hidden on startup
    observe({
      if (!rv$ui_initialized) {
        shinyjs::hide("progress_container")
        shinyjs::html("progress_stage_message", "")
        cat("DEBUG: Module initialized - verification status:", rv$verification_status, "\n")
        rv$ui_initialized <- TRUE
      }
    })

    # Helper function to save plots safely with error handling
    save_plot_safely <- function(plot_obj, plot_path, plot_name = "plot") {
      if (is.null(plot_obj)) {
        cat("WARNING: No plot object provided for:", plot_name, "\n")
        return(NULL)
      }

      tryCatch(
        {
          # Ensure directory exists
          plot_dir <- dirname(plot_path)
          if (!dir.exists(plot_dir)) {
            dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
            if (!dir.exists(plot_dir)) {
              stop("Failed to create directory: ", plot_dir)
            }
          }

          # Check if plot_obj is a valid ggplot object
          if (!inherits(plot_obj, "ggplot")) {
            cat("WARNING: Object is not a ggplot for:", plot_name, "\n")
            cat("Object class:", class(plot_obj), "\n")
            return(NULL)
          }

          # Extract plot metadata for dimensions and DPI
          plot_width <- attr(plot_obj, "width") %||% 12
          plot_height <- attr(plot_obj, "height") %||% 9
          plot_dpi <- attr(plot_obj, "dpi") %||% 300

          cat("Saving", plot_name, "with dimensions:", plot_width, "x", plot_height, "inches at", plot_dpi, "DPI\n")

          # Save the plot
          ggplot2::ggsave(
            filename = plot_path,
            plot = plot_obj,
            width = plot_width,
            height = plot_height,
            units = "in",
            dpi = plot_dpi,
            device = "png"
          )

          # Verify file was created and has reasonable size
          if (file.exists(plot_path)) {
            file_size <- file.size(plot_path)
            if (file_size > 1000) { # At least 1KB
              cat(
                "<U+2713> Successfully saved", plot_name, ":", basename(plot_path),
                "(", format(file_size, big.mark = ","), "bytes )\n"
              )
              return(plot_path)
            } else {
              cat(
                "ERROR: Plot file too small:", basename(plot_path),
                "(", file_size, "bytes )\n"
              )
              return(NULL)
            }
          } else {
            cat("ERROR: Plot file was not created:", plot_path, "\n")
            return(NULL)
          }
        },
        error = function(e) {
          cat("ERROR saving", plot_name, ":", conditionMessage(e), "\n")
          return(NULL)
        }
      )
    }

    # Observer for check_readiness button with debugging
    shiny::observeEvent(input$check_readiness, {
      logger::log_info("Check readiness button clicked - starting verification")
      cat("DEBUG: check_readiness button was clicked\n")

      # Show progress container IMMEDIATELY
      shinyjs::show(ns("progress_container"))
      shinyjs::runjs(sprintf("
        var container = document.getElementById('%s');
        if (container) {
          container.classList.remove('hidden');
          container.style.display = 'block';
        }
      ", ns("progress_container")))

      # Initialize progress bar immediately
      shinyWidgets::updateProgressBar(
        session = session,
        id = ns("progress_bar"),
        value = 0,
        title = "Starting verification...",
        status = "info"
      )

      # Set initial message immediately
      shinyjs::html("progress_stage_message", "<span style='color: #5A6C75; font-size: 1em;'>Preparing to verify project...</span>")

      # Reset verification status AFTER UI is shown
      rv$verification_status <- "checking"
      rv$verification_results <- NULL
      rv$all_checks_passed <- FALSE
      rv$show_partial_results <- FALSE

      # Show loading state
      shinyjs::disable("check_readiness")
      on.exit(shinyjs::enable("check_readiness"))

      # Set up the stages based on section_order
      rv$verification_stages <- c("Project Info", "Load/Entry", "Analysis", "Visualization")
      rv$current_stage_index <- 0
      rv$stage_results <- list()

      # Run verification immediately in the same context
      logger::log_info("Starting verification process immediately")

      # Execute verification synchronously
      tryCatch(
        {
          # Get project data
          proj_data <- tryCatch(
            {
              if (is.reactive(project_data)) {
                reactiveValuesToList(project_data)
              } else {
                project_data
              }
            },
            error = function(e) {
              logger::log_error("Error accessing project_data: ", e$message)
              list()
            }
          )

          # Helper function to safely access nested list elements
          safe_get <- function(x, path, default = NULL) {
            tryCatch(
              {
                # Handle environments and convert to list if needed
                if (is.environment(x)) {
                  x <- as.list(x)
                }

                result <- Reduce(function(l, k) {
                  # Return default for NULL or non-list/non-environment objects
                  if (is.null(l)) {
                    return(default)
                  }

                  # Convert environment to list if encountered
                  if (is.environment(l)) {
                    l <- as.list(l)
                  }

                  # Only proceed if l is a list or has named elements
                  if (!is.list(l) && !is.environment(l) && !is.data.frame(l)) {
                    return(default)
                  }

                  # Try to access the element
                  if (k %in% names(l) || (is.numeric(k) && k <= length(l))) {
                    return(l[[k]])
                  } else {
                    return(default)
                  }
                }, path, init = x)

                # If result is empty environment or list, check if that's valid
                if ((is.environment(result) && length(result) == 0) ||
                  (is.list(result) && length(result) == 0)) {
                  # Empty is still a valid result for our purposes
                  return(result)
                }

                return(result)
              },
              error = function(e) {
                # Log the error for debugging
                message("Error in safe_get: ", e$message)
                return(default)
              }
            )
          }

          # Helper function to debug cascade data presence and structure
          debug_cascade_data <- function(proj_data) {
            cat("\n===== CASCADE DATA DEBUG =====\n")

            # Check cleaned_data
            cat("\nChecking cleaned_data$cascade: ")
            cascade_clean <- safe_get(proj_data, c("cleaned_data", "cascade"))
            cat(ifelse(is.null(cascade_clean), "NULL", class(cascade_clean)))
            if (!is.null(cascade_clean) && is.data.frame(cascade_clean)) {
              cat(" (rows: ", nrow(cascade_clean), ", cols: ", ncol(cascade_clean), ")", sep = "")
            }

            # Check raw_data
            cat("\nChecking raw_data$cascade: ")
            cascade_raw <- safe_get(proj_data, c("raw_data", "cascade"))
            cat(ifelse(is.null(cascade_raw), "NULL", class(cascade_raw)))
            if (!is.null(cascade_raw) && is.data.frame(cascade_raw)) {
              cat(" (rows: ", nrow(cascade_raw), ", cols: ", ncol(cascade_raw), ")", sep = "")
            }

            # Check data
            cat("\nChecking data$cascade: ")
            cascade_data <- safe_get(proj_data, c("data", "cascade"))
            cat(ifelse(is.null(cascade_data), "NULL", class(cascade_data)))
            if (!is.null(cascade_data) && is.data.frame(cascade_data)) {
              cat(" (rows: ", nrow(cascade_data), ", cols: ", ncol(cascade_data), ")", sep = "")
            }

            # Check analysis
            cat("\nChecking analysis$cascade_analyzed: ")
            cascade_analyzed <- safe_get(proj_data, c("analysis", "cascade_analyzed"))
            cat(ifelse(is.null(cascade_analyzed), "NULL", class(cascade_analyzed)))

            cat("\nChecking analysis$cascade_data: ")
            cascade_anal_data <- safe_get(proj_data, c("analysis", "cascade_data"))
            cat(ifelse(is.null(cascade_anal_data), "NULL", class(cascade_anal_data)))
            if (!is.null(cascade_anal_data) && is.data.frame(cascade_anal_data)) {
              cat(" (rows: ", nrow(cascade_anal_data), ", cols: ", ncol(cascade_anal_data), ")", sep = "")
            }

            # Check visualizations
            cat("\nChecking visualizations$cascade: ")
            cascade_viz <- safe_get(proj_data, c("visualizations", "cascade"))
            cat(ifelse(is.null(cascade_viz), "NULL", class(cascade_viz)))
            if (is.list(cascade_viz)) {
              cat(" (keys: ", paste(names(cascade_viz), collapse = ", "), ")", sep = "")
            }

            # Test is_missing on each path
            cat("\n\nTesting is_missing function:")
            cat("\n  is_missing(cleaned_data$cascade): ", is_missing(cascade_clean))
            cat("\n  is_missing(raw_data$cascade): ", is_missing(cascade_raw))
            cat("\n  is_missing(data$cascade): ", is_missing(cascade_data))
            cat("\n  is_missing(analysis$cascade_analyzed): ", is_missing(cascade_analyzed))
            cat("\n  is_missing(analysis$cascade_data): ", is_missing(cascade_anal_data))
            cat("\n  is_missing(visualizations$cascade): ", is_missing(cascade_viz))

            # Test with OR operator
            data_test <- cascade_clean %||% cascade_raw %||% cascade_data
            analysis_test <- cascade_analyzed %||% cascade_anal_data

            cat("\n\nTesting OR combinations:")
            cat("\n  is_missing(cleaned || raw || data): ", is_missing(data_test))
            cat("\n  is_missing(analyzed || anal_data): ", is_missing(analysis_test))

            cat("\n===== END CASCADE DEBUG =====\n\n")
          }

          # Helper function to check if a value is missing
          is_missing <- function(val) {
            # Handle NULL or NA values first
            if (is.null(val) || (is.atomic(val) && all(is.na(val)))) {
              return(TRUE)
            }

            # For data frames, consider present if exists (even if empty)
            if (is.data.frame(val)) {
              return(FALSE)
            }

            # For lists, check more thoroughly
            if (is.list(val)) {
              # Empty list is considered missing
              if (length(val) == 0) {
                return(TRUE)
              }

              # Special case for authors
              if ("name" %in% names(val)) {
                # Single author with name field
                return(is.null(val$name) || is.na(val$name) || trimws(val$name) == "")
              }

              # Check if list has any non-NULL elements
              has_content <- FALSE

              # Recursively check list elements
              for (item_name in names(val)) {
                item <- val[[item_name]]
                # If any item is not missing, the whole list is not missing
                if (!is_missing(item)) {
                  has_content <- TRUE
                  break
                }
              }

              # If list has no names, check elements directly
              if (length(names(val)) == 0 && length(val) > 0) {
                for (i in seq_along(val)) {
                  if (!is_missing(val[[i]])) {
                    has_content <- TRUE
                    break
                  }
                }
              }

              return(!has_content)
            }

            # For character, present if not all NA or empty
            if (is.character(val)) {
              return(length(val) == 0 || all(is.na(val)) || all(!nzchar(trimws(val))))
            }

            # For logical, present if TRUE
            if (is.logical(val)) {
              return(!isTRUE(val))
            }

            # Any other type with a value is considered present
            FALSE
          }

          # Define checks with enhanced data structure handling
          check_categories <- list(
            project_info = list(
              list(label = "Project Title", required = TRUE, stage = "Project Info", value = safe_get(proj_data, c("project_info", "title"))),
              list(label = "Report Date", required = TRUE, stage = "Project Info", value = safe_get(proj_data, c("project_info", "report_date"))),
              list(label = "Project Description", required = FALSE, stage = "Project Info", value = safe_get(proj_data, c("project_info", "description"))),
              list(label = "Report Keywords", required = FALSE, stage = "Project Info", value = safe_get(proj_data, c("project_info", "report_keywords"))),
              list(label = "Report Formats", required = TRUE, stage = "Project Info", value = safe_get(proj_data, c("project_info", "report_formats"))),
              list(label = "Authors", required = TRUE, stage = "Project Info", value = safe_get(proj_data, c("project_info", "authors")))
            ),
            data_checks = list(
              list(label = "Alignment Data", required = TRUE, stage = "Load/Entry", value = safe_get(proj_data, c("cleaned_data", "alignment")) %||% safe_get(proj_data, c("raw_data", "alignment")) %||% safe_get(proj_data, c("data", "alignment"))),
              list(label = "Dynamics Data", required = TRUE, stage = "Load/Entry", value = safe_get(proj_data, c("cleaned_data", "dynamics")) %||% safe_get(proj_data, c("raw_data", "dynamics")) %||% safe_get(proj_data, c("data", "dynamics"))),
              list(label = "Cascade Data", required = TRUE, stage = "Load/Entry", value = safe_get(proj_data, c("cleaned_data", "cascade")) %||% safe_get(proj_data, c("raw_data", "cascade")) %||% safe_get(proj_data, c("data", "cascade")) %||% safe_get(proj_data, c("analysis", "cascade_data"))),
              list(label = "Indicators Data", required = TRUE, stage = "Load/Entry", value = safe_get(proj_data, c("cleaned_data", "indicators")) %||% safe_get(proj_data, c("raw_data", "indicators")) %||% safe_get(proj_data, c("data", "indicators")))
            ),
            analysis_checks = list(
              list(label = "Alignment Analysis", required = TRUE, stage = "Analysis", value = safe_get(proj_data, c("analysis", "alignment_analyzed")) %||% safe_get(proj_data, c("analysis", "alignment_table"))),
              list(label = "Dynamics Analysis", required = TRUE, stage = "Analysis", value = safe_get(proj_data, c("analysis", "dynamics_analyzed")) %||% safe_get(proj_data, c("analysis", "dynamics_domains"))),
              list(label = "Cascade Analysis", required = TRUE, stage = "Analysis", value = safe_get(proj_data, c("analysis", "cascade_analyzed")) %||% safe_get(proj_data, c("analysis", "cascade_data")) %||% safe_get(proj_data, c("visualizations", "cascade")))
            ),
            viz_checks = list(
              list(label = "Alignment Plot", required = TRUE, stage = "Visualization", value = safe_get(proj_data, c("visualizations", "alignment"))),
              list(label = "Dynamics Plot", required = TRUE, stage = "Visualization", value = safe_get(proj_data, c("visualizations", "dynamics"))),
              list(label = "Cascade Plot", required = TRUE, stage = "Visualization", value = safe_get(proj_data, c("visualizations", "cascade"))),
              list(label = "Indicators Plot", required = TRUE, stage = "Visualization", value = safe_get(proj_data, c("visualizations", "indicators"))),
              list(label = "Dashboard Plot", required = TRUE, stage = "Visualization", value = safe_get(proj_data, c("visualizations", "dashboard")))
            )
          )

          # Initialize results
          results <- list()
          total_checks <- sum(lengths(check_categories))
          completed_checks <- 0

          # Log the current structure of proj_data for debugging
          cat("\nDEBUG: Checking proj_data structure for verification\n")
          if (!is.null(proj_data)) {
            cat("Top-level keys:", paste(names(proj_data), collapse = ", "), "\n")

            # Check cascade data specifically
            if ("analysis" %in% names(proj_data)) {
              cat("Analysis keys:", paste(names(proj_data$analysis), collapse = ", "), "\n")
              if ("cascade_data" %in% names(proj_data$analysis)) {
                cat("cascade_data exists in analysis\n")
              }
            }

            if ("visualizations" %in% names(proj_data)) {
              cat("Visualizations keys:", paste(names(proj_data$visualizations), collapse = ", "), "\n")
              if ("cascade" %in% names(proj_data$visualizations)) {
                cat("cascade exists in visualizations\n")
              }
            }
          } else {
            cat("proj_data is NULL\n")
          }

          # Debug the cascade data structure
          debug_cascade_data(proj_data)

          # Process each stage
          for (stage_name in rv$verification_stages) {
            rv$current_stage_index <- match(stage_name, rv$verification_stages)
            progress_value <- (rv$current_stage_index - 1) * 25

            # Update progress bar for this stage
            shinyWidgets::updateProgressBar(
              session = session,
              id = ns("progress_bar"),
              value = progress_value,
              title = "",
              status = "info"
            )

            # Update stage message
            stage_display_name <- switch(stage_name,
              "Project Info" = "Project Information",
              "Load/Entry" = "Data Loading",
              "Analysis" = "Analysis Results",
              "Visualization" = "Visualizations",
              stage_name
            )

            shinyjs::html(
              "progress_stage_message",
              paste0("<span style='color: #5A6C75; font-size: 1em;'>Checking ", stage_display_name, "...</span>")
            )

            # Get stage categories
            stage_categories <- switch(stage_name,
              "Project Info" = list(project = check_categories$project_info),
              "Load/Entry" = list(data = check_categories$data_checks),
              "Analysis" = list(analysis = check_categories$analysis_checks),
              "Visualization" = list(viz = check_categories$viz_checks),
              list()
            )

            # Process stage checks
            stage_results <- list()
            for (cat_name in names(stage_categories)) {
              cat_checks <- stage_categories[[cat_name]]
              if (length(cat_checks) == 0) next

              for (item in cat_checks) {
                item$stage <- stage_name

                # Ensure required field is always logical
                if (is.null(item$required) || length(item$required) == 0 || any(is.na(item$required))) {
                  item$required <- TRUE
                } else {
                  item$required <- as.logical(item$required)[1]
                  if (is.na(item$required)) item$required <- TRUE
                }

                missing_status <- is_missing(item$value)
                # Debug log for cascade items
                if (grepl("Cascade", item$label)) {
                  cat("\nDEBUG: Item '", item$label, "' has value of class: ",
                    ifelse(is.null(item$value), "NULL", class(item$value)[1]),
                    ", is_missing: ", missing_status, "\n",
                    sep = ""
                  )
                }

                item$status <- if (missing_status) {
                  if (item$required) "missing_required" else "missing_optional"
                } else {
                  "present"
                }

                results <- c(results, list(item))
                stage_results <- c(stage_results, list(item))
                completed_checks <- completed_checks + 1
              }
            }

            # Small delay for visual feedback
            Sys.sleep(0.3)

            # Mark stage as complete
            progress_value <- rv$current_stage_index * 25
            shinyWidgets::updateProgressBar(
              session = session,
              id = ns("progress_bar"),
              value = progress_value,
              title = "",
              status = if (progress_value >= 100) "success" else "primary"
            )

            # Update completion message
            req_missing <- sum(vapply(stage_results, function(x) {
              safe_required <- isTRUE(x$required)
              safe_status <- if (is.null(x$status) || is.na(x$status)) "missing_required" else x$status
              safe_required && safe_status != "present"
            }, logical(1)))

            section_display <- toupper(stage_display_name)
            stage_status_message <- if (req_missing == 0) {
              paste0(section_display, " COMPLETE")
            } else {
              paste0(section_display, " INCOMPLETE")
            }
            stage_status_color <- if (req_missing == 0) "#5D7359" else "#8F524E"

            shinyjs::html(
              "progress_stage_message",
              paste0("<span style='color: ", stage_status_color, "; font-size: 1em;'><strong>", stage_status_message, "</strong></span>")
            )

            Sys.sleep(0.3)
          }

          # Final validation
          shinyWidgets::updateProgressBar(
            session = session,
            id = ns("progress_bar"),
            value = 80,
            title = ""
          )
          shinyjs::html(
            "progress_stage_message",
            "<span style='color: #5A6C75; font-size: 1em;'><strong>Finalizing verification...</strong></span>"
          )

          Sys.sleep(0.5)

          # Check final results
          all_statuses <- sapply(results, `[[`, "status")
          rv$all_checks_passed <- !("missing_required" %in% all_statuses)

          # Final message
          final_message <- if (isTRUE(rv$all_checks_passed)) {
            "Verification complete - All checks passed!"
          } else {
            "Verification complete - Issues found"
          }

          # Final update
          shinyWidgets::updateProgressBar(
            session = session,
            id = ns("progress_bar"),
            value = 100,
            title = "",
            status = if (isTRUE(rv$all_checks_passed)) "success" else "danger"
          )

          shinyjs::html(
            "progress_stage_message",
            paste0(
              "<span style='",
              ifelse(isTRUE(rv$all_checks_passed), "color: #5D7359;", "color: #8F524E;"),
              " font-size: 1.1em;'><strong>", final_message, "</strong></span>"
            )
          )

          # Set final results
          rv$verification_results <- list(
            results = results,
            stage_warnings = list()
          )
          rv$verification_status <- "complete"

          # Hide progress bar after delay
          shinyjs::delay(1000, function() {
            shinyjs::runjs(sprintf("
            var container = document.getElementById('%s');
            if (container) {
              container.classList.add('hidden');
            }
          ", ns("progress_container")))
          })
        },
        error = function(e) {
          logger::log_error("Verification error: ", e$message)

          shinyWidgets::updateProgressBar(
            session = session,
            id = ns("progress_bar"),
            value = 100,
            title = "",
            status = "danger"
          )

          error_msg <- paste("Error during verification:", e$message)
          shinyjs::html(
            "progress_stage_message",
            paste0("<span style='color: #8F524E; font-size: 1.1em;'><strong>", error_msg, "</strong></span>")
          )

          rv$verification_results <- list(
            results = list(),
            stage_warnings = list(),
            error_message = error_msg
          )
          rv$verification_status <- "complete"
          rv$all_checks_passed <- FALSE

          shinyjs::delay(1000, function() {
            shinyjs::runjs(sprintf("
            var container = document.getElementById('%s');
            if (container) {
              container.classList.add('hidden');
            }
          ", ns("progress_container")))
          })
        }
      )
    })

    # Watch for changes in visualizations and re-trigger verification if they appear later
    observe({
      if (!is.null(project_data$visualizations) && length(project_data$visualizations) > 0) {
        viz_count <- length(project_data$visualizations)
        logger::log_info("Detected {viz_count} visualizations available")
      }
    })



    # Helper function to update section status text
    update_section_status <- function(section_id, status_text, status_type = "info") {
      status_id <- paste0(section_id, "_status")
      status_class <- switch(status_type,
        "success" = "status-complete",
        "danger" = "status-danger",
        "warning" = "status-warning",
        "status-pending"
      )
      color <- switch(status_type,
        "success" = "#28a745",
        "danger" = "#dc3545",
        "warning" = "#C5A259",
        "#666"
      )
      font_weight <- if (status_type == "success") "500" else "normal"

      shinyjs::runjs(sprintf("
        try {
          var statusEl = document.getElementById('%s');
          if (statusEl) {
            statusEl.textContent = '%s';
            statusEl.className = 'section-status %s';
            statusEl.style.color = '%s';
            statusEl.style.fontWeight = '%s';
          }
        } catch(e) {
          console.error('Error updating section status:', e);
        }
      ", ns(status_id), status_text, status_class, color, font_weight))
    }

    # Helper function to activate a section
    activate_section <- function(section_id, section_name = NULL) {
      if (is.null(section_name)) {
        section_name <- gsub("^section", "", section_id)
        section_name <- gsub("([a-z])([A-Z])", "\\1 \\2", section_name) # Add space before capital letters
      }

      # Get container ID
      container_id <- paste0(section_id, "_container")

      # Update status text
      update_section_status(section_id, paste0("Compiling ", section_name, "..."))

      # First, ensure the section is visible and ready for updates
      section_js <- paste0("
        console.log('Activating section ", section_id, "');
        var section = document.getElementById('", ns(container_id), "');
        if (section) {
          // Make sure section is visible
          section.style.display = 'block';
          section.style.opacity = '1';
          console.log('Section is now visible');

          // Reset all icons in section-row elements to arrow-fat-lines-right
          var rows = section.querySelectorAll('.section-row');
          console.log('Found ' + rows.length + ' section-rows to reset in section');
          for (var i = 0; i < rows.length; i++) {
            var icon = rows[i].querySelector('i');
            if (icon) {
              icon.className = 'ph ph-arrow-fat-lines-right text-info';
            }
          }
        } else {
          console.error('Could not find section container');
        }
      ")
      shinyjs::runjs(section_js)

      # Then set list items to processing state with R function
      shinyjs::delay(50, {
        for (i in 0:3) {
          shinyjs::delay(i * 20, {
            update_list_item_status(section_id, i, "processing")
          })
        }
      })

      # JS to remove active class from all sections and add it to the current one
      js_code <- sprintf("
        (function() {
          try {
            // Remove active class from all section containers
            document.querySelectorAll('.section-container').forEach(function(el) {
              el.classList.remove('active');
            });

            // Add active class to this section container
            const container = document.getElementById('%s');
            if (container) {
              container.classList.add('active');
            }

            // Find the section by ID
            const section = document.getElementById('%s');
            if (!section) {
              console.warn('Section not found:', '%s');
              return;
            }


          } catch (e) {
            console.error('Error in activate_section:', e);
          }
        })();
      ", ns(container_id), ns(section_id), section_id)
      shinyjs::runjs(js_code)
    }

    # Helper function to update individual list item status
    update_list_item_status <- function(section_id, item_index, new_status) {
      cat("Updating list item status:", section_id, "index:", item_index, "to:", new_status, "\n")

      # Super simple approach - add an ID to every icon in the HTML template
      # Each icon has an id like "section1_icon_0", "section1_icon_1", etc.
      icon_id <- paste0(section_id, "_icon_", item_index)

      # Use arrow-fat-lines-right icon for all states - no changes needed
      icon_class <- "ph ph-arrow-fat-lines-right text-info"

      # We're not changing icons anymore, just log for tracking
      cat("No icon update needed - using static lightbulb icons\n")
    }

    # Helper function to complete a section and update list items
    complete_section <- function(section_id, title_text) {
      # Update status text to show completion first
      update_section_status(section_id, paste0(title_text, " <U+2713>"), TRUE)

      # Update card header to show completion - this is our main focus
      shinyjs::runjs(sprintf("
        try {
          var section = document.getElementById('%s_container');
          if (section) {
            var header = section.querySelector('.card-header .fw-medium');
            if (header) {
              header.innerHTML = header.innerHTML.replace('PROCESSING', 'COMPLETE');
              header.style.color = '#5D7359'; // Set to success green color
              header.style.fontWeight = 'bold';
            }
          }
        } catch(e) {
          console.error('Error updating card header:', e);
        }
      ", ns(section_id)))

      # We don't need to change icons anymore, just log the completion
      shinyjs::runjs(sprintf("console.log('Completed section %s');", section_id))

      # Keep section active for minimal visual feedback
      shinyjs::delay(100, function() {
        cat("Section", section_id, "completed and displayed for extended time\n")
      })
    }

    # Helper to update progress with optional stage message
    update_verification_progress <- function(value, message = NULL, stage_message = NULL, status = "info") {
      # Only update if the session is still valid
      if (!is.null(session) && !session$isClosed()) {
        tryCatch(
          {
            # Update progress bar
            # Update progress bar with value and status
            shinyWidgets::updateProgressBar(
              session = session,
              id = ns("progress_bar"),
              value = value,
              title = "",
              status = status
            )

            # Update stage message if provided
            if (!is.null(stage_message)) {
              # Determine color based on status
              color <- switch(status,
                "success" = "#5D7359",
                "danger" = "#8F524E",
                "warning" = "#C5A259",
                "#5A6C75" # Default to info color
              )

              styled_message <- paste0("<span style='color: ", color, "; font-size: 1em;'>", stage_message, "</span>")
              tryCatch(
                {
                  shinyjs::html("progress_stage_message", styled_message)
                  # Force UI update
                  session$sendCustomMessage("force-update", list(id = ns("progress_stage_message")))
                },
                error = function(e) {
                  cat("Error updating verification progress message:", e$message, "\n")
                }
              )
            }

            # Hide progress bar after completion if at 100%
            if (value >= 100) {
              shinyjs::delay(800, function() {
                shinyjs::runjs(sprintf("
                  var container = document.getElementById('%s');
                  if (container) {
                    container.classList.add('hidden');
                  }
                ", ns("progress_container")))
              })
            }
          },
          error = function(e) {
            warning("Error updating progress bar: ", e$message)
          }
        )
      }
    }

    # Helper to update progress for a specific verification stage
    update_stage_progress <- function(stage_index, stage_name, section_results = NULL, progress_within_stage = 100, status = "info") {
      if (stage_index < 1 || stage_index > 4) {
        return()
      }

      # Calculate overall progress (20% per stage)
      base_progress <- (stage_index - 1) * 20
      stage_progress <- base_progress + (progress_within_stage / 100 * 20)
      stage_progress <- min(stage_progress, stage_index * 20) # Cap at current stage's max

      # Create a display name map
      stage_display_names <- c(
        "Project Info" = "Project Information",
        "Load/Entry" = "Data Loading",
        "Analysis" = "Analysis Results",
        "Visualization" = "Visualizations"
      )

      # Get display name
      stage_display_name <- stage_display_names[[stage_name]] %||% stage_name

      # Determine stage_status_message and color
      if (!is.null(section_results)) {
        req_missing <- sum(vapply(section_results, function(x) x$required && x$status != "present", logical(1)))
        opt_missing <- sum(vapply(section_results, function(x) !x$required && x$status != "present", logical(1)))
        section_display <- toupper(stage_display_name)
        if (req_missing == 0 && opt_missing == 0) {
          stage_status_message <- paste(section_display, "VERIFIED")
          stage_status_type <- "success"
        } else if (req_missing > 0) {
          stage_status_message <- paste(section_display, "MISSING REQUIRED ITEMS")
          stage_status_type <- "danger"
        } else {
          stage_status_message <- paste(section_display, "MISSING OPTIONAL ITEMS")
          stage_status_type <- "warning"
        }
      } else {
        stage_status_message <- paste0("Checking ", stage_display_name, "...")
        stage_status_type <- "info"
      }

      # Update progress
      update_verification_progress(
        value = stage_progress,
        message = paste0("Stage ", stage_index, "/4: ", stage_name),
        stage_message = stage_status_message,
        status = stage_status_type
      )
    }

    # Removed complex timed progress implementation

    # NOTE: To use the new update_stage_progress, pass section_results for the current section during verification.

    # Null coalescing operator helper
    `%||%` <- function(x, y) {
      if (is.null(x)) y else x
    }

    # Helper to update progress text (for generation)
    update_generation_progress <- function(value, text, status = NULL) {
      if (is.null(status)) {
        status <- if (value < 100) "info" else "success"
      }

      # Update the progress bar
      shinyWidgets::updateProgressBar(
        session = session,
        id = ns("progress_bar_generate"),
        value = value,
        title = text,
        status = status
      )
    }

    construct_template <- function() {
      # Construct the report template using the collected data
      filepath <- file.path("www", "report_template.txt")
      report_template <- readr::read_file(filepath)

      # Safely extract and process keywords
      keywords_val <- ""
      tryCatch(
        {
          kw <- project_data$project_info$report_keywords
          if (!is.null(kw) && length(kw) > 0 && !all(is.na(kw))) {
            kw_clean <- trimws(as.character(kw)[1])
            if (nchar(kw_clean) > 0 && kw_clean != "NA") {
              keywords_val <- paste0("keywords: ", kw_clean)
            }
          }
        },
        error = function(e) {
          keywords_val <<- ""
        }
      )

      # Safely extract and process report formats
      formats_val <- ""
      tryCatch(
        {
          fmt <- project_data$project_info$report_formats
          if (!is.null(fmt) && length(fmt) > 0 && !all(is.na(fmt))) {
            # Filter out NA values
            fmt_clean <- fmt[!is.na(fmt) & fmt != "NA"]
            if (length(fmt_clean) > 0) {
              formats <- tolower(as.character(fmt_clean))
              # Map Word to docx for Quarto compatibility
              formats <- gsub("^word$", "docx", formats)
              if (length(formats) == 1) {
                formats_val <- paste0("format: ", formats)
              } else {
                formats_val <- paste0("format:\n  ", paste(paste0(formats, ": default"), collapse = "\n  "))
              }
            }
          }
        },
        error = function(e) {
          formats_val <<- ""
        }
      )

      template_content <- list(
        project_title = project_data$project_info$title,
        project_description = project_data$project_info$description,
        project_keywords = keywords_val,
        report_date = project_data$project_info$report_date,
        report_types = formats_val,
        alignment_score = project_data$analyzed_data$alignment_score %||% 0,
        dynamics_score = project_data$analyzed_data$dynamics_score %||% 0,
        cascade_score = project_data$analyzed_data$cascade_score %||% 0
      )

      # Replace placeholders with actual data
      report_compiled <<- whisker::whisker.render(report_template, template_content)

      # Create filename with pattern: {project_title}-{report_date}.qmd
      # Fallback to centr_impact-{today's date}.qmd if project_info is missing
      if (!is.null(project_data$project_info$title) && !is.null(project_data$project_info$report_date)) {
        filename <- sprintf(
          "%s-%s.qmd",
          gsub("[^[:alnum:]]", "-", project_data$project_info$title), # Replace special chars with hyphens
          gsub("-", "", as.character(project_data$project_info$report_date)) # Remove hyphens from date
        )
      } else {
        filename <- sprintf("centr_impact-%s.qmd", format(Sys.Date(), "%Y%m%d"))
      }

      # Get the project directory from reactive values
      project_dir <- rv$temp_dir
      if (is.null(project_dir) || !dir.exists(project_dir)) {
        stop("Project directory is not properly initialized")
      }

      # Write the file to the project directory (root of the zip)
      output_file <- file.path(project_dir, filename)
      readr::write_file(report_compiled, output_file)

      # Also write to www for potential direct download
      www_file <- file.path("www", filename)
      readr::write_file(report_compiled, www_file)

      # Store the output file path in reactive values for download handler
      rv$report_file <- output_file
      rv$qmd_filename <- filename # Store the filename for reference
    }

    # Define the sequence of steps for report generation
    generate_steps <- list(
      # Step 1: Show UI and start first section
      list(delay = 0, action = function() {
        cat("Step 1: Showing UI elements and creating temp directories\n")
        shinyjs::show(ns("progress_section"))
        shinyjs::show(ns("progress_container"))
        # Hide all sections initially
        shinyjs::runjs("try { document.querySelectorAll('.section-container').forEach(el => el.classList.remove('active')); } catch(e) { console.log('JS error:', e); }")

        # Initialize progress bar
        update_generation_progress(10, "Initializing report generation...")

        # Create base temp directory
        base_temp_dir <- tempdir()

        # Get project title and report date, with defaults
        # Get the current project data
        proj_data <- if (is.reactive(project_data)) {
          reactiveValuesToList(project_data)
        } else {
          project_data
        }

        project_title <- if (!is.null(proj_data$project_info$title) &&
          nzchar(trimws(proj_data$project_info$title))) {
          # Replace spaces and special characters with underscores
          gsub("[^a-zA-Z0-9-]", "_", trimws(proj_data$project_info$title))
        } else {
          "centr_impact"
        }

        report_date <- if (!is.null(proj_data$report_info$date) &&
          nzchar(trimws(proj_data$report_info$date))) {
          format(as.Date(proj_data$report_info$date), "%Y-%m-%d")
        } else {
          format(Sys.Date(), "%Y-%m-%d")
        }

        project_dir_name <- if (nzchar(trimws(project_title))) {
          clean_title <- gsub("[^a-zA-Z0-9-]", "_", trimws(project_title))
          clean_title <- gsub("_+", "_", clean_title) # Replace multiple underscores with single
          clean_title <- gsub("^_|_$", "", clean_title) # Remove leading/trailing underscores
          paste0(clean_title, "_report-", gsub("-", "", report_date)) # Remove dashes from date for filename
        } else {
          paste0("centrimpact_report-", gsub("-", "", report_date))
        }

        project_dir <- file.path(base_temp_dir, project_dir_name)
        data_dir <- file.path(project_dir, "data")
        plots_dir <- file.path(project_dir, "plots")

        # Create directories with error handling
        tryCatch(
          {
            if (!dir.exists(project_dir)) {
              dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
              if (!dir.exists(project_dir)) stop("Failed to create project directory")
            }

            if (!dir.exists(data_dir)) {
              dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
              if (!dir.exists(data_dir)) stop("Failed to create data directory")
            }

            if (!dir.exists(plots_dir)) {
              dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
              if (!dir.exists(plots_dir)) stop("Failed to create plots directory")
            }
          },
          error = function(e) {
            stop("Error creating directories: ", conditionMessage(e))
          }
        )

        cat("Project directory created at:", project_dir, "\n")
        cat("Data directory:", data_dir, "\n")
        cat("Plots directory:", plots_dir, "\n")

        # Store the paths in reactive values for later use
        rv$temp_dir <- project_dir
        rv$data_dir <- data_dir
        rv$plots_dir <- plots_dir

        filepath <- file.path("www", "report_template.txt")
        report_template <- readr::read_file(filepath)
        rv$is_generating <- TRUE
      }),
      list(delay = 500, action = function() {
        cat("Step 2: Creating directories and preparing for plot saving\n")
        update_generation_progress(20, "Preparing directories...")
        activate_section("section1", "Plots and Tables")

        # Update individual items as they complete with progressive delays
        shinyjs::delay(50, update_list_item_status("section1", 0, "complete")) # Create Project Directory

        # Just ensure directories exist - don't save plots here
        plots_dir <- rv$plots_dir
        data_dir <- rv$data_dir

        if (!dir.exists(plots_dir)) {
          dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
        }
        if (!dir.exists(data_dir)) {
          dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
        }

        shinyjs::delay(100, update_list_item_status("section1", 1, "complete")) # Setup Data Folders
        shinyjs::delay(150, update_list_item_status("section1", 2, "complete")) # Prepare Plot Storage

        cat("Directories prepared:\n")
        cat("  - Plots:", plots_dir, "(exists:", dir.exists(plots_dir), ")\n")
        cat("  - Data:", data_dir, "(exists:", dir.exists(data_dir), ")\n")

        shinyjs::delay(300, update_list_item_status("section1", 3, "complete")) # Initialize Temp Workspace
      }),
      list(delay = 400, action = function() {
        cat("Step 3: Completing section 1\n")
        update_generation_progress(30, "Plots and Tables configured")
        complete_section("section1", "Plots and Tables")
      }),
      list(delay = 400, action = function() {
        cat("Step 2: Activating section 2\n")
        update_generation_progress(40, "Compiling metadata...")
        activate_section("section2", "Metadata")

        # Simulate metadata compilation steps with JavaScript delays
        shinyjs::delay(100, update_list_item_status("section2", 0, "complete")) # Extract Project Information
        shinyjs::delay(150, update_list_item_status("section2", 1, "complete")) # Process Report Settings
        shinyjs::delay(200, update_list_item_status("section2", 2, "complete")) # Collect Analysis Results
        shinyjs::delay(300, update_list_item_status("section2", 3, "complete")) # Prepare Data Exports
      }),
      list(delay = 400, action = function() {
        cat("Step 3: Completing section 2\n")
        update_generation_progress(50, "Metadata compiled")
        complete_section("section2", "Metadata")
      }),
      list(delay = 400, action = function() {
        cat("Step 6: Activating section 3\n")
        update_generation_progress(60, "Stitching Sections...")
        activate_section("section3", "Stitching Sections")

        # Simulate stitching steps with progressive delays
        shinyjs::delay(75, update_list_item_status("section3", 0, "complete")) # Save Visualization Plots
        shinyjs::delay(150, update_list_item_status("section3", 1, "complete")) # Export Data Tables
        shinyjs::delay(225, update_list_item_status("section3", 2, "complete")) # Create Quarto Document
        shinyjs::delay(300, update_list_item_status("section3", 3, "complete")) # Copy Template Assets
      }),
      list(delay = 500, action = function() {
        cat("Step 7: Completing section 3\n")
        update_generation_progress(75, "Sections stitched")
        complete_section("section3", "Stitching Sections")

        # Debug: List all files in the project directory
        project_dir <- rv$temp_dir
        cat("\n=== Directory Contents Before Zipping ===\n")
        cat("Project directory:", project_dir, "\n")

        # Verify the project directory exists and has the expected structure
        if (dir.exists(project_dir)) {
          # List all files and directories
          all_files <- list.files(project_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
          all_dirs <- list.dirs(project_dir, recursive = FALSE, full.names = FALSE)

          cat("\nDirectory structure:")
          print(all_dirs)

          if (length(all_files) > 0) {
            # Filter out .DS_Store and other hidden files
            visible_files <- grep("^\\.", basename(all_files), invert = TRUE, value = TRUE)
            cat("\nFound", length(visible_files), "files in project directory:\n")
            print(head(visible_files, 20))
            if (length(visible_files) > 20) cat("... and", length(visible_files) - 20, "more files\n")

            # Check for required directories
            required_dirs <- c("data", "plots")
            missing_dirs <- setdiff(required_dirs, all_dirs)
            if (length(missing_dirs) > 0) {
              warning("Missing required directories: ", paste(missing_dirs, collapse = ", "))
            }
          } else {
            cat("No files found in project directory!\n")
          }
        } else {
          stop("Project directory does not exist: ", project_dir)
        }
        cat("==================================\n\n")
      }),
      list(delay = 400, action = function() {
        cat("Step 14: Saving all plots and creating zip file\n")
        update_generation_progress(85, "Saving plots and packaging report...")
        activate_section("section4", "Package Report")

        # Update packaging items progressively
        shinyjs::delay(50, update_list_item_status("section4", 0, "complete")) # Verify File Structure

        # Get project data
        proj_data <- tryCatch(
          {
            if (is.reactive(project_data)) {
              reactiveValuesToList(project_data)
            } else {
              project_data
            }
          },
          error = function(e) {
            cat("Error getting project data:", conditionMessage(e), "\n")
            return(list())
          }
        )

        # ADD THIS DEBUG CODE to Step 14 right after getting proj_data
        # This will help us understand the actual structure of your project data

        cat("\n=== DEBUGGING VISUALIZATION DATA STRUCTURE ===\n")

        # 1. Check if proj_data exists and what it contains
        if (is.null(proj_data)) {
          cat("ERROR: proj_data is NULL\n")
        } else {
          cat("proj_data exists. Top-level keys:\n")
          print(names(proj_data))
        }

        # 2. Check visualizations structure
        if (!is.null(proj_data$visualizations)) {
          cat("\nproj_data$visualizations exists. Keys:\n")
          print(names(proj_data$visualizations))

          # Check each visualization type
          for (viz_type in c("indicators", "alignment", "dynamics", "cascade", "dashboard")) {
            cat("\n--- Checking", viz_type, "---\n")
            viz_obj <- proj_data$visualizations[[viz_type]]

            if (is.null(viz_obj)) {
              cat("  ", viz_type, "is NULL\n")
            } else {
              cat("  ", viz_type, "exists. Type:", class(viz_obj), "\n")
              cat("  ", viz_type, "keys:", paste(names(viz_obj), collapse = ", "), "\n")

              # Check for plot in different possible locations
              possible_paths <- list(
                main = viz_obj$main,
                plot = viz_obj$plot,
                plots = viz_obj$plots,
                direct = viz_obj # Sometimes the plot is the object itself
              )

              for (path_name in names(possible_paths)) {
                path_obj <- possible_paths[[path_name]]
                if (!is.null(path_obj)) {
                  cat("    Found plot at $", path_name, " - Class:", class(path_obj), "\n")
                  if (inherits(path_obj, "ggplot")) {
                    cat("    <U+2713> Valid ggplot object found!\n")
                  }
                }
              }
            }
          }
        } else {
          cat("\nproj_data$visualizations is NULL\n")

          # Check alternative locations
          alternatives <- c("visualization", "viz", "plots", "charts")
          for (alt in alternatives) {
            if (!is.null(proj_data[[alt]])) {
              cat("Found visualizations at proj_data$", alt, "\n")
              print(names(proj_data[[alt]]))
            }
          }
        }

        # 3. If using reactive project_data, try different access methods
        cat("\n=== TRYING ALTERNATIVE DATA ACCESS METHODS ===\n")

        # Method 1: Direct reactive access
        if (is.reactive(project_data)) {
          cat("project_data is reactive, trying direct access...\n")
          tryCatch(
            {
              direct_viz <- project_data$visualizations
              if (!is.null(direct_viz)) {
                cat("<U+2713> Found visualizations via direct reactive access\n")
                cat("Keys:", paste(names(direct_viz), collapse = ", "), "\n")

                # Try to use this data instead
                proj_data$visualizations <- direct_viz
              }
            },
            error = function(e) {
              cat("Error with direct reactive access:", conditionMessage(e), "\n")
            }
          )
        }

        # Method 2: Check session userData (sometimes Shiny modules store data here)
        if (!is.null(session$userData)) {
          cat("Checking session$userData for visualization data...\n")
          user_data_keys <- names(session$userData)
          cat("session$userData keys:", paste(user_data_keys, collapse = ", "), "\n")

          # Look for visualization-related keys
          viz_keys <- grep("viz|visual|plot|chart", user_data_keys, ignore.case = TRUE, value = TRUE)
          if (length(viz_keys) > 0) {
            cat("Found potential visualization keys:", paste(viz_keys, collapse = ", "), "\n")
          }
        }

        cat("=== END DEBUG ===\n\n")

        # Get directories first
        project_dir <- rv$temp_dir
        plots_dir <- rv$plots_dir
        data_dir <- rv$data_dir

        # Verify directories exist
        if (!dir.exists(project_dir)) stop("Project directory missing:", project_dir)
        if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

        cat("Using directories:\n")
        cat("- Project:", project_dir, "\n")
        cat("- Plots:", plots_dir, "\n")
        cat("- Data:", data_dir, "\n")

        # IMPROVED PLOT SAVING with multiple path attempts
        saved_plots <- list()

        # Define multiple possible paths for each plot type prioritizing report plots
        plot_search_paths <- list(
          indicators = list(
            if (!is.null(proj_data$visualizations$indicators$plots)) proj_data$visualizations$indicators$plots$report,
            if (!is.null(proj_data$visualizations$indicators$plots)) proj_data$visualizations$indicators$plots$main,
            proj_data$visualizations$indicators$main,
            proj_data$visualizations$indicators$plot,
            proj_data$visualizations$indicators$plots,
            proj_data$visualizations$indicators
          ) %>% purrr::compact(),
          alignment = list(
            if (!is.null(proj_data$visualizations$alignment$plots)) proj_data$visualizations$alignment$plots$report,
            if (!is.null(proj_data$visualizations$alignment$plots)) proj_data$visualizations$alignment$plots$main,
            proj_data$visualizations$alignment$main,
            proj_data$visualizations$alignment$plot,
            proj_data$visualizations$alignment$plots,
            proj_data$visualizations$alignment
          ) %>% purrr::compact(),
          dynamics = list(
            if (!is.null(proj_data$visualizations$dynamics$plots)) proj_data$visualizations$dynamics$plots$report,
            if (!is.null(proj_data$visualizations$dynamics$plots)) proj_data$visualizations$dynamics$plots$main,
            proj_data$visualizations$dynamics$main,
            proj_data$visualizations$dynamics$plot,
            proj_data$visualizations$dynamics$plots,
            proj_data$visualizations$dynamics
          ) %>% purrr::compact(),
          cascade = list(
            if (!is.null(proj_data$visualizations$cascade$plots)) proj_data$visualizations$cascade$plots$report,
            if (!is.null(proj_data$visualizations$cascade$plots)) proj_data$visualizations$cascade$plots$main,
            proj_data$visualizations$cascade$main,
            proj_data$visualizations$cascade$plot,
            proj_data$visualizations$cascade$plots,
            proj_data$visualizations$cascade
          ) %>% purrr::compact(),
          dashboard = list(
            if (!is.null(proj_data$visualizations$dashboard$plots)) proj_data$visualizations$dashboard$plots$report,
            if (!is.null(proj_data$visualizations$dashboard$plots)) proj_data$visualizations$dashboard$plots$main,
            proj_data$visualizations$dashboard$main,
            proj_data$visualizations$dashboard$plot,
            proj_data$visualizations$dashboard$plots,
            proj_data$visualizations$dashboard
          ) %>% purrr::compact()
        )

        # Save each plot trying multiple paths
        for (plot_name in names(plot_search_paths)) {
          cat("Searching for", plot_name, "plot...\n")

          plot_obj <- NULL
          found_path <- NULL

          # Try each possible path
          for (i in seq_along(plot_search_paths[[plot_name]])) {
            candidate <- plot_search_paths[[plot_name]][[i]]

            if (!is.null(candidate) && inherits(candidate, "ggplot")) {
              plot_obj <- candidate
              found_path <- paste0("Path ", i)
              cat("  <U+2713> Found valid ggplot at", found_path, "\n")
              break
            }
          }

          if (!is.null(plot_obj)) {
            plot_path <- file.path(plots_dir, paste0(plot_name, "_plot.png"))
            saved_path <- save_plot_safely(plot_obj, plot_path, plot_name)
            if (!is.null(saved_path)) {
              saved_plots[[plot_name]] <- saved_path
              cat("  <U+2713> Successfully saved", plot_name, "plot\n")
            }
          } else {
            cat("  <U+2717> No valid ggplot found for", plot_name, "\n")
          }
        }

        cat("Final result: Saved", length(saved_plots), "out of", length(plot_search_paths), "plots\n")

        # Continue with the rest of your zip creation logic...

        # Get directories
        project_dir <- rv$temp_dir
        plots_dir <- rv$plots_dir
        data_dir <- rv$data_dir

        # Verify directories exist
        if (!dir.exists(project_dir)) stop("Project directory missing:", project_dir)
        if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

        cat("Saving plots to:", plots_dir, "\n")

        # Save all plots with better error handling
        saved_plots <- list()
        plot_configs <- list(
          indicators = list(
            plot = proj_data$visualizations$indicators$plots$report %||% proj_data$visualizations$indicators$plots$main %||% proj_data$visualizations$indicators$main,
            filename = "indicators_plot.png"
          ),
          alignment = list(
            plot = proj_data$visualizations$alignment$plots$report %||% proj_data$visualizations$alignment$plots$main %||% proj_data$visualizations$alignment$main,
            filename = "alignment_plot.png"
          ),
          dynamics = list(
            plot = proj_data$visualizations$dynamics$plots$report %||% proj_data$visualizations$dynamics$plots$main %||% proj_data$visualizations$dynamics$main,
            filename = "dynamics_plot.png"
          ),
          cascade = list(
            plot = proj_data$visualizations$cascade$plots$report %||% proj_data$visualizations$cascade$plots$main %||% proj_data$visualizations$cascade$main,
            filename = "cascade_plot.png"
          ),
          dashboard = list(
            plot = proj_data$visualizations$dashboard$plots$report %||% proj_data$visualizations$dashboard$plots$main %||% proj_data$visualizations$dashboard$main,
            filename = "dashboard_plot.png"
          )
        )

        # Save each plot
        for (plot_name in names(plot_configs)) {
          config <- plot_configs[[plot_name]]
          if (!is.null(config$plot)) {
            plot_path <- file.path(plots_dir, config$filename)
            saved_path <- save_plot_safely(config$plot, plot_path, plot_name)
            if (!is.null(saved_path)) {
              saved_plots[[plot_name]] <- saved_path
            }
          } else {
            cat("WARNING: No plot found for", plot_name, "\n")
          }
        }

        cat("Successfully saved", length(saved_plots), "out of", length(plot_configs), "plots\n")

        # Save data files
        data_configs <- list(
          indicators = list(
            data = proj_data$cleaned_data$indicators,
            filename = "indicators_data.csv"
          ),
          alignment = list(
            data = proj_data$analysis$alignment_table,
            filename = "alignment_data.csv"
          ),
          dynamics = list(
            data = proj_data$analysis$dynamics_domains,
            filename = "dynamics_data.csv"
          ),
          cascade = list(
            data = proj_data$analysis$cascade_data,
            filename = "cascade_data.csv"
          )
        )

        saved_data <- list()
        for (data_name in names(data_configs)) {
          config <- data_configs[[data_name]]
          if (!is.null(config$data)) {
            data_path <- file.path(data_dir, config$filename)
            tryCatch(
              {
                readr::write_csv(config$data, data_path)
                if (file.exists(data_path)) {
                  saved_data[[data_name]] <- data_path
                  cat("<U+2713> Saved", data_name, "data:", config$filename, "\n")
                }
              },
              error = function(e) {
                cat("ERROR saving", data_name, "data:", conditionMessage(e), "\n")
              }
            )
          }
        }

        # Create the Quarto document
        construct_template()

        # Copy essential template assets to project directory
        template_assets <- list(
          "_brand.yml" = file.path("www", "_brand.yml"),
          "custom.css" = file.path("www", "custom.css"),
          "centr-logo.png" = file.path("www", "centr-logo.png"),
          "centr_logo.png" = file.path("www", "centr_logo.png")
        )

        cat("Copying template assets...\n")
        for (asset_name in names(template_assets)) {
          source_path <- template_assets[[asset_name]]
          if (file.exists(source_path)) {
            dest_path <- file.path(project_dir, asset_name)
            tryCatch(
              {
                file.copy(source_path, dest_path, overwrite = TRUE)
                if (file.exists(dest_path)) {
                  cat("  <U+2713> Copied", asset_name, "\n")
                } else {
                  cat("  WARNING: Failed to copy", asset_name, "\n")
                }
              },
              error = function(e) {
                cat("  ERROR copying", asset_name, ":", conditionMessage(e), "\n")
              }
            )
          } else {
            cat("  WARNING: Template asset not found:", source_path, "\n")
          }
        }

        # Handle the specific logo referenced in template (centr-impact-logo.png)
        # Create fallback if it doesn't exist
        impact_logo_path <- file.path(project_dir, "centr-impact-logo.png")
        if (!file.exists(impact_logo_path)) {
          # Try to use one of the existing logo files as fallback
          fallback_logos <- c(
            file.path("www", "centr-logo.png"),
            file.path("www", "centr_logo.png"),
            file.path("www", "cafe_logo.png")
          )

          for (fallback in fallback_logos) {
            if (file.exists(fallback)) {
              tryCatch(
                {
                  file.copy(fallback, impact_logo_path, overwrite = TRUE)
                  if (file.exists(impact_logo_path)) {
                    cat("  <U+2713> Created centr-impact-logo.png fallback from", basename(fallback), "\n")
                    break
                  }
                },
                error = function(e) {
                  cat("  WARNING: Failed to create logo fallback:", conditionMessage(e), "\n")
                }
              )
            }
          }
        }

        # Create zip file with better file collection
        tryCatch(
          {
            # Get ALL files in the project directory
            all_files <- list.files(project_dir,
              recursive = TRUE, full.names = FALSE,
              all.files = FALSE, no.. = TRUE
            )

            # Filter out system files
            files_to_zip <- all_files[!grepl("^\\.|__MACOSX|\\.DS_Store", all_files)]

            cat("Files to include in zip:\n")
            print(data.frame(
              File = files_to_zip,
              Type = ifelse(grepl("\\.png$", files_to_zip), "Plot",
                ifelse(grepl("\\.csv$", files_to_zip), "Data",
                  ifelse(grepl("\\.qmd$", files_to_zip), "Report", "Other")
                )
              ),
              stringsAsFactors = FALSE
            ))

            if (length(files_to_zip) == 0) {
              stop("No files found to zip")
            }

            # Verify all files exist
            full_paths <- file.path(project_dir, files_to_zip)
            existing_files <- files_to_zip[file.exists(full_paths)]

            if (length(existing_files) < length(files_to_zip)) {
              missing <- setdiff(files_to_zip, existing_files)
              cat("WARNING: Missing files will be skipped:\n")
              print(missing)
            }

            # Create zip file
            timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
            project_name <- basename(project_dir)
            zip_filename <- paste0(project_name, "-", timestamp, ".zip")

            # Debug: Check what's actually in the directories
            cat("\n=== DIRECTORY CONTENTS DEBUG ===\n")
            cat("Project directory:", project_dir, "\n")
            cat("Contents:\n")
            print(list.files(project_dir, recursive = TRUE, full.names = TRUE))

            cat("\nPlots directory:", plots_dir, "\n")
            if (dir.exists(plots_dir)) {
              plot_files <- list.files(plots_dir, full.names = TRUE)
              cat("Plot files found:", length(plot_files), "\n")
              for (pf in plot_files) {
                cat("  ", basename(pf), "- Size:", file.size(pf), "bytes\n")
              }
            } else {
              cat("Plots directory does not exist!\n")
            }

            zip_dir <- file.path(tempdir(), session$token, "zip")
            dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
            zip_file <- file.path(zip_dir, zip_filename)

            cat("Creating zip file:", zip_file, "\n")
            cat("Including", length(existing_files), "files\n")

            # Create the zip
            zip_result <- zip::zip(
              zipfile = zip_file,
              files = existing_files,
              root = project_dir,
              mode = "mirror"
            )

            # Verify zip creation
            if (file.exists(zip_file) && file.size(zip_file) > 1000) {
              rv$zip_file <- zip_file

              # List zip contents for verification
              zip_contents <- utils::unzip(zip_file, list = TRUE)
              png_files <- grep("\\.png$", zip_contents$Name, value = TRUE)

              cat("<U+2713> ZIP FILE CREATED SUCCESSFULLY\n")
              cat("  File:", zip_file, "\n")
              cat("  Size:", format(file.size(zip_file), big.mark = ","), "bytes\n")
              cat("  Total files:", nrow(zip_contents), "\n")
              cat("  PNG files:", length(png_files), "\n")
              if (length(png_files) > 0) {
                cat("  PNG files included:\n")
                for (png in png_files) cat("    -", png, "\n")
              }
            } else {
              stop("Failed to create valid zip file")
            }

            # Complete remaining packaging steps with delays
            shinyjs::delay(75, update_list_item_status("section4", 1, "complete")) # Bundle All Components
            shinyjs::delay(150, update_list_item_status("section4", 2, "complete")) # Create ZIP Archive
            shinyjs::delay(225, update_list_item_status("section4", 3, "complete")) # Prepare Download
          },
          error = function(e) {
            error_msg <- paste("Error creating zip:", conditionMessage(e))
            cat(error_msg, "\n")
            showNotification(error_msg, type = "error")
            rv$zip_file <- NULL
          }
        )
      }),
      list(delay = 500, action = function() {
        cat("Step 15: Report generation complete\n")

        # Update all section headers to "COMPLETE" and ensure all icons are lightbulbs
        shinyjs::runjs("
          for (var s = 1; s <= 4; s++) {
            var sectionId = 'section' + s;
            var section = document.getElementById(sectionId + '_container');
            if (section) {
              var header = section.querySelector('.card-header .fw-medium');
              if (header) {
                header.innerHTML = header.innerHTML.replace('PROCESSING', 'COMPLETE');
                header.style.color = '#5D7359';
                header.style.fontWeight = 'bold';
              }
            }
          }
        ")

        # Also ensure all icons are arrow-fat-lines-right
        shinyjs::runjs("
          // Set only section-row icons to arrow-fat-lines-right
          var sectionRows = document.querySelectorAll('.section-row');
          for (var i = 0; i < sectionRows.length; i++) {
            var icon = sectionRows[i].querySelector('i');
            if (icon) {
              icon.className = 'ph ph-arrow-fat-lines-right text-info';
            }
          }
        ")

        # Also use R functions for each section as backup
        for (section_id in paste0("section", 1:4)) {
          complete_section(section_id, paste0("Section ", substr(section_id, 8, 8)))
        }

        # Complete all sections one by one with a slight delay
        for (i in 1:4) {
          local({
            local_i <- i
            section_id <- paste0("section", local_i)
            section_name <- c("Plots and Tables", "Metadata", "Stitching Sections", "Package Report")[local_i]
            shinyjs::delay(local_i * 75, {
              complete_section(section_id, section_name)
            })
          })
        }

        # Then update progress bar to show completion
        shinyjs::delay(300, {
          update_generation_progress(100, "Report ready for download")
        })

        # Report is now complete
        logger::log_info("Report generation complete")

        # Remove active class from all sections and activate completion banner
        shinyjs::runjs(paste0("
          try {
          // Remove active class from all section containers
          document.querySelectorAll('.section-container').forEach(function(el) {
            el.classList.remove('active');
          });

          // Make completion banner active
          document.getElementById('", ns("completion_banner"), "').classList.add('active');
          } catch(e) { console.log('JS error:', e); }
        "))

        shinyjs::enable(ns("generate_report"))
        shinyjs::enable(ns("download_report"))
        rv$is_generating <- FALSE

        # Hide progress bar with minimal delay
        shinyjs::delay(300, function() {
          shinyjs::runjs(sprintf("
            var container = document.getElementById('%s');
            if (container) {
              container.classList.add('hidden');
            }
          ", ns("progress_container")))
        })
      })
    )

    # Recursive runner function
    run_generate_steps_delay <- function(steps, i = 1) {
      if (i > length(steps)) {
        return()
      }
      # Run the first step immediately to reduce initial delay
      if (i == 1) {
        steps[[i]]$action()
        if (i + 1 <= length(steps)) {
          shinyjs::delay(steps[[i + 1]]$delay, {
            run_generate_steps_delay(steps, i + 1)
          })
        }
      } else {
        shinyjs::delay(steps[[i]]$delay, {
          steps[[i]]$action()
          if (i + 1 <= length(steps)) {
            run_generate_steps_delay(steps, i + 1)
          }
        })
      }
    }

    observeEvent(input$generate_report,
      {
        cat("Generate report button clicked\n")
        shinyjs::disable(ns("generate_report"))

        # Show progress bars immediately when button is clicked
        shinyjs::show(ns("progress_section"))
        shinyjs::show(ns("progress_container"))
        update_generation_progress(0, "Starting report generation...")

        run_generate_steps_delay(generate_steps)
      },
      ignoreInit = TRUE
    )

    # Download handler for the report
    output$download_report <- downloadHandler(
      filename = function() {
        if (is.null(rv$zip_file)) {
          timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
          filename <- paste0("centrimpact-report-", timestamp, ".zip")
        } else {
          filename <- basename(rv$zip_file)
        }
        cat("Downloading file:", filename, "\n")
        return(filename)
      },
      content = function(file) {
        req(rv$zip_file)
        cat("Copying file from", rv$zip_file, "to", file, "\n")

        if (!file.exists(rv$zip_file)) {
          stop("Report file is not available for download")
        }

        # Show a notification that the download is starting
        showNotification("Preparing download...", type = "message")

        # Copy the file to the download location
        file.copy(rv$zip_file, file)
        cat("File copied successfully\n")
      },
      contentType = "application/zip"
    )

    # Helper to update last_step in project_data$status
    update_last_step <- function() {
      if (!is.null(project_data$status)) {
        project_data$status$last_step <- "generate"
      } else {
        logger::log_warn("project_data$status is NULL, cannot update last_step")
      }
    }

    # Verification status UI
    output$verification_status <- shiny::renderUI({
      if (is.null(rv$verification_status)) {
        return(tags$div(class = "alert alert-info", "Click 'Check Project Readiness' to verify your project."))
      }
      if (!is.null(rv$verification_results$error_message)) {
        return(tags$div(class = "alert alert-danger", phosphoricons::ph_i("warning-octagon"), rv$verification_results$error_message))
      }
      if (rv$verification_status == "checking") {
        return(NULL) # Return NULL to completely hide
      }
      if (rv$verification_status == "complete") {
        if (rv$all_checks_passed) {
          return(tags$div(class = "alert alert-success", phosphoricons::ph("check-circle"), " All required components are present. You can now generate your report."))
        } else {
          return(tags$div(class = "alert alert-warning", phosphoricons::ph_i("warning"), " Some required components are missing. Please check the verification results below."))
        }
      }
    })

    # Verification results UI
    output$verification_results <- shiny::renderUI({
      # Debug output
      cat("\n--- DEBUG: verification_results renderUI ---\n")
      cat("Verification status:", rv$verification_status, "\n")
      cat("Show partial results:", rv$show_partial_results, "\n")
      cat("Verification results exists:", !is.null(rv$verification_results), "\n")

      # Don't show anything if verification hasn't started or is still in progress
      if (is.null(rv$verification_status) || rv$verification_status == "checking") {
        cat("Hiding verification results - status is NULL or checking\n")
        return(NULL) # Return NULL to completely hide
      }

      # Show partial results during verification
      if (isTRUE(rv$show_partial_results) && !is.null(rv$partial_verification_results)) {
        partial_results <- rv$partial_verification_results
        stage_name <- rv$current_stage_name

        # Use section names from rv if available, otherwise fall back to hardcoded values
        local_section_names <- rv$section_names %||% c(
          "Project Info" = "Project Information",
          "Load/Entry" = "Data Loading",
          "Analysis" = "Analysis Results",
          "Visualization" = "Visualizations"
        )

        # Ensure we have results for this stage
        stage_data <- Filter(function(x) x$stage == stage_name, partial_results$results)

        if (length(stage_data) > 0) {
          # Get display name for this stage
          stage_display_name <- rv$stage_display_names[[stage_name]] %||% local_section_names[[stage_name]] %||% stage_name

          # Create a partial results UI for the current stage
          return(tags$div(
            class = "partial-results",
            style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
            tags$h4(paste0("Verifying: ", stage_display_name)),
            tags$div(
              class = "alert alert-info",
              phosphoricons::ph_i("info-circle"),
              tags$span(style = "margin-left: 5px;", "Verification in progress... Showing partial results for this stage.")
            ),
            # Only show the section for the current stage
            check_section_ui(
              data = stage_data,
              check_title = stage_display_name,
              value_title = toupper(stage_display_name),
              description = paste("Checking", tolower(stage_display_name)),
              card_header_text = "VERIFICATION IN PROGRESS",
              check_list = tags$div(
                phosphoricons::ph_i("check-circle", weight = "fill", class = "text-success"),
                style = "font-size: 1.1em; flex-shrink: 0; margin-top: 0.05em;"
              ),
              check_message = "Verification in progress...",
              fieldset_title = toupper(stage_display_name),
              ns = session$ns,
              session = session
            )
          ))
        }
      }

      if (is.null(rv$verification_results)) {
        cat("verification_results is NULL\n")
        return(NULL)
      }

      # Check for error message
      if (!is.null(rv$verification_results$error_message)) {
        return(tags$div(
          class = "alert alert-danger",
          phosphoricons::ph_i("warning-octagon"),
          rv$verification_results$error_message
        ))
      }

      # Safely extract results
      results <- if (!is.null(rv$verification_results$results)) {
        rv$verification_results$results
      } else {
        cat("WARNING: verification_results$results is NULL\n")
        list()
      }

      # Debug output
      cat("Results length:", length(results), "\n")

      if (length(results) == 0) {
        cat("No results to display\n")
        return(tags$div(class = "alert alert-info", "No verification results available."))
      }



      # Updated section mapping to match the verification stages
      section_names <- c(
        "Project Info" = "Project Information",
        "Load/Entry" = "Data Loading",
        "Analysis" = "Analysis Results",
        "Visualization" = "Visualizations"
      )

      section_order <- c("Project Info", "Load/Entry", "Analysis", "Visualization")

      get_section_results <- function(section) Filter(function(x) x$stage == section, results)

      stage_warnings <- if (!is.null(rv$verification_results$stage_warnings)) {
        rv$verification_results$stage_warnings
      } else {
        list()
      }

      section_tag <- function(section_results) {
        req_missing <- sum(vapply(section_results, function(x) x$required && x$status != "present", logical(1)))
        opt_missing <- sum(vapply(section_results, function(x) !x$required && x$status != "present", logical(1)))

        if (req_missing == 0 && opt_missing == 0) {
          tags$span("All Complete", class = "badge bg-success-subtle text-success-emphasis")
        } else if (req_missing > 0) {
          tags$span(
            sprintf("%d Required Missing", req_missing),
            class = "badge bg-danger-subtle text-danger-emphasis"
          )
        } else {
          tags$span(
            sprintf("%d Optional Missing", opt_missing),
            class = "badge bg-warning-subtle text-warning-emphasis"
          )
        }
      }

      # Create a container div for all sections
      container_div <- div(
        id = "verification_results_container",
        style = "margin: 20px 0;"
      )

      # Add sections to the container using check_section_ui
      for (section in section_order) {
        section_results <- get_section_results(section)
        if (length(section_results) == 0) next

        # Compose the check_list UI for this section using the reusable function
        check_list <- create_generate_list_taglist(section_results, ns = session$ns, session = session)

        value_descriptions <- list(
          "Project Information" = "Checks that all required project information fields are complete and up to date. Ensures your project has the foundational details needed for reporting and analysis.",
          "Data Loading" = "Verifies that all necessary datasets have been loaded and cleaned. Ensures your data is ready for analysis by checking for completeness and proper formatting.",
          "Analysis Results" = "Confirms that the loaded data has been successfully analyzed. Checks for the presence of key results and metrics needed for interpretation and reporting.",
          "Visualizations" = "Ensures that the analyzed data has been visualized. Checks for the presence and completeness of key plots and figures to support interpretation and communication."
        )
        # Enhanced section status message and color logic
        req_missing <- sum(vapply(section_results, function(x) x$required && x$status != "present", logical(1)))
        opt_missing <- sum(vapply(section_results, function(x) !x$required && x$status != "present", logical(1)))
        section_display <- toupper(section_names[[section]])
        if (req_missing == 0 && opt_missing == 0) {
          section_status <- paste(section_display, "VERIFIED")
          section_status_type <- "success"
        } else if (req_missing > 0) {
          section_status <- paste(section_display, "MISSING REQUIRED ITEMS")
          section_status_type <- "danger"
        } else {
          section_status <- paste(section_display, "MISSING OPTIONAL ITEMS")
          section_status_type <- "warning"
        }
        # Compose the check_message (badge/summary)
        check_message <- section_tag(section_results)

        # Use check_section_ui for this section
        section_ui <- check_section_ui(
          data = section_results,
          check_title = section_names[[section]],
          value_title = toupper(section_names[[section]]),
          description = value_descriptions[[section]],
          card_header_text = section_status,
          check_list = check_list,
          check_message = check_message,
          ns = session$ns,
          session = session,
          fieldset_title = toupper(section_names[section])
        )

        container_div <- tagAppendChild(container_div, section_ui)
      }

      # Add warnings if any
      if (length(stage_warnings) > 0) {
        warning_div <- div(
          style = "margin-top: 1.5rem;",
          lapply(names(stage_warnings), function(stage) {
            div(
              class = "alert alert-warning mt-3",
              tags$strong("Warning: "),
              stage_warnings[[stage]]
            )
          })
        )
        container_div <- tagAppendChild(container_div, warning_div)
      }

      container_div # Return the constructed UI
    }) # Close renderUI

    # Download handler for project data
    output$download_project_data <- downloadHandler(
      filename = function() {
        paste0(
          "project_data_", format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".rds"
        )
      },
      content = function(file) {
        # Show a modal dialog with a progress bar
        showModal(modalDialog(
          title = "Preparing Data Download",
          "Please wait while we prepare your data for download...",
          footer = NULL
        ))

        # Ensure we're working with a reactiveValues object
        data_to_save <- if (is.reactive(project_data)) {
          reactiveValuesToList(project_data)
        } else {
          project_data
        }

        # Save the data to a temporary file first
        temp_file <- tempfile(fileext = ".rds")
        on.exit(unlink(temp_file))

        tryCatch(
          {
            saveRDS(data_to_save, file = temp_file)
            # Copy the file to the download location
            file.copy(temp_file, file)
            removeModal()
          },
          error = function(e) {
            removeModal()
            showNotification(
              paste("Error preparing data for download:", e$message),
              type = "error"
            )
          }
        )
      },
      contentType = "application/octet-stream"
    )

    # Dismiss completion banner handler
    observeEvent(input$dismiss_banner, {
      shinyjs::runjs(paste0("var elem = document.getElementById('", ns("completion_banner"), "'); if(elem) elem.classList.remove('active');"))
    })

    # Handle navigation when a Fix button is clicked (Moved here from renderUI)
    shiny::observeEvent(input$nav_to,
      {
        req(input$nav_to)
        # Use nav_select to switch to the requested tab
        bslib::nav_select(
          id = "main_navbar",
          selected = input$nav_to,
          session = session
        )

        # Also update the accordion to show the verification panel
        bslib::accordion_panel_open(
          id = "generate_accordion",
          values = "verify_readiness",
          session = session
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Toggle generate button based on verification status (Moved here from renderUI)
    shiny::observe({
      req(rv$verification_status == "complete")
      shinyjs::toggleState("generate_report", condition = isTRUE(rv$all_checks_passed))
    })

    # When the generate_report panel is opened, update last_step
    observeEvent(input$generate_accordion, {
      if ("generate_report" %in% input$generate_accordion) {
        update_last_step()
      }
    })

    # Return module outputs
    return(list(
      report_data = shiny::reactive({
        NULL
      }),
      status = shiny::reactive({
        if (is.null(rv$verification_status)) {
          "Ready to verify"
        } else if (rv$verification_status == "complete") {
          if (rv$all_checks_passed) {
            "Ready to generate report"
          } else {
            "Verification failed - missing required components"
          }
        } else {
          "Verifying..."
        }
      })
    )) # Correctly closes the list AND the shiny::moduleServer function argument
  }) # Correctly closes shiny::moduleServer call
} # Correctly closes mod_generate_server function
