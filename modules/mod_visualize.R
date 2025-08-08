#!! ModName = mod_visualize
# !! ModDisplayName = Visualize Data
# !! ModDescription = Create visualizations for project data
# !! ModCitation = Price, Jeremy F. (2025). mod_visualize. [Source code].
# !! ModNotes = This module provides functionality to visualize project data.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for visualization !! reactive

# Load required libraries
#' @importFrom dplyr ungroup filter
#' @importFrom tidyr pivot_longer
#' @importFrom phosphoricons ph_i ph
#' @importFrom ggplot2 ggsave

# Read color_palette from _brand.yml using yaml package
if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml")
library(yaml)
# Color palette will be determined dynamically from project_data or fallback to YAML

# Ensure ggplot2 is available
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Font setup for Google Fonts (Lato and IBM Plex Mono)
if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")
if (!requireNamespace("sysfonts", quietly = TRUE)) install.packages("sysfonts")
library(showtext)
library(sysfonts)
font_add_google("Lato", "lato")
font_add_google("DM Mono", "ibmplexmono")
showtext_auto()

# Configuration for visualization types
VIZ_CONFIG <- list(
  indicators = list(
    name = "Indicators",
    icon = "compass-tool",
    color = "#3B6B35",
    description = "Shows raw counts of key project elements<U+2014>like partners, hours, outputs, and participants<U+2014>to quickly convey project scale and activity, useful for funders and administrators."
  ),
  alignment = list(
    name = "Alignment",
    icon = "target",
    color = "#3F5E78",
    description = "Compares how researchers and partners perceive alignment in values, goals, and roles. High alignment means strong collaboration; gaps highlight areas for improvement."
  ),
  dynamics = list(
    name = "Dynamics",
    icon = "gear",
    color = "#4E342E",
    description = "Visualizes project dynamics across core domains, helping identify strengths and areas needing balance or integration."
  ),
  cascade = list(
    name = "Cascade Effects",
    icon = "waveform",
    color = "#990000",
    description = "Shows how project effects cascade through network layers, revealing reach, flow, and equity across the community."
  ),
  dashboard = list(
    name = "Dashboard",
    icon = "intersect",
    color = "#6B46C1",
    description = "Combines all visualization preview plots into a comprehensive dashboard overview of the entire project analysis."
  )
)

# Descriptions for visualization_section_ui
visualization_descriptions <- list(
  indicators = "Shows raw counts of key project elements-like partners, hours, outputs, and participants-to quickly convey project scale and activity, useful for funders and administrators.",
  alignment = "Compares how researchers and partners perceive alignment in values, goals, and roles. High alignment means strong collaboration; gaps highlight areas for improvement.",
  dynamics = "Visualizes project dynamics across core domains, helping identify strengths and areas needing balance or integration.",
  cascade = "Shows how project effects cascade through network layers, revealing reach, flow, and equity across the community.",
  dashboard = "Combines all visualization preview plots into a comprehensive dashboard overview of the entire project analysis."
)

# UI Function
mod_visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # JavaScript for custom visualization messages
    bslib::accordion(
      id = ns("visualize_accordion"),
      multiple = FALSE,

      # Main visualization panel
      bslib::accordion_panel(
        value = "visualize_data",
        title = tagList(
          phosphoricons::ph("compass-tool"),
          HTML("&nbsp;"),
          "Visualize Data"
        ),
        create_visualization_controls_ui(ns)
      ),

      # Results panel
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        div(
          id = ns("results_container"),
          uiOutput(ns("visualization_results"))
        )
      )
    )
  )
}

# Helper function to create visualization controls UI
create_visualization_controls_ui <- function(ns) {
  fluidRow(
    column(
      width = 12,
      p("This module allows you to visualize your project data. Select the type of visualization you want to perform and run the visualization."),

      # Visualization type selector
      tags$fieldset(
        class = "custom-fieldset",
        tags$legend("Select", class = "custom-legend"),
        shinyWidgets::radioGroupButtons(
          inputId = ns("visualization_type"),
          label = "VISUALIZATION TYPE",
          choices = list(
            "All Metrics" = "full",
            "Indicators" = "indicators",
            "Alignment" = "alignment",
            "Dynamics" = "dynamics",
            "Cascade" = "cascade"
          ),
          selected = NULL,
          direction = "horizontal",
          justified = TRUE
        )
      ),

      # Run visualization controls
      tags$fieldset(
        class = "custom-fieldset",
        tags$legend("Visualize", class = "custom-legend"),
        div(
          class = "d-flex flex-column align-items-center gap-3",
          actionButton(
            inputId = ns("run_visualization"),
            label = tagList("Visualize Metrics ", ph("compass-tool", weight = "bold")),
            width = "50%",
            class = "btn btn-primary btn-lg"
          ),
          div(
            id = ns("progress_container"),
            style = "width: 50%; visibility: hidden;",
            shinyWidgets::progressBar(
              id = ns("progress_bar"),
              value = 0,
              total = 100,
              status = "success",
              title = "Ready to Visualize",
              striped = FALSE,
              display_pct = FALSE
            )
          )
        )
      )
    )
  )
}

# Server Function
mod_visualize_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get color palette from project_data or fallback to YAML
    get_color_palette <- function() {
      # First check project_data for branding information
      if (!is.null(project_data)) {
        if (is.reactive(project_data)) {
          pd <- project_data()
          if (!is.null(pd$brand_config) && !is.null(pd$brand_config$default$color_palette)) {
            return(unname(unlist(pd$brand_config$default$color_palette)))
          }
        } else {
          if (!is.null(project_data$brand_config) && !is.null(project_data$brand_config$default$color_palette)) {
            return(unname(unlist(project_data$brand_config$default$color_palette)))
          }
        }
      }

      # Fallback to YAML file
      brand <- yaml::read_yaml("www/_brand.yml")
      return(unname(unlist(brand$default$color_palette)))
    }





    # Reactive values for visualization state
    viz_state <- reactiveValues(
      current_type = NULL,
      message = NULL,
      status = NULL,
      results = list()
    )

    # Auto-restore visualizations on module initialization if they were previously completed
    observe({
      if (!is.null(project_data$visualization_status) && length(project_data$visualization_status) > 0) {
        # Check if we have analyzed data and no current viz_state results
        has_analyzed_data <- !is.null(project_data$analyzed_data)
        has_no_current_results <- length(viz_state$results) == 0

        if (has_analyzed_data && has_no_current_results) {
          logger::log_info("Auto-restoring {length(project_data$visualization_status)} previously completed visualizations")

          # Restore each completed visualization
          for (viz_type in names(project_data$visualization_status)) {
            tryCatch(
              {
                # Check if we have the necessary data for this viz type
                can_restore <- viz_type == "indicators" ||
                  !is.null(project_data$analyzed_data[[paste0(viz_type, "_analyzed")]])

                if (can_restore) {
                  # Get visualization data and recreate the result
                  viz_data <- get_visualization_data(viz_type)
                  if (!is.null(viz_data)) {
                    viz_result <- create_visualization_result(viz_type, viz_data)

                    # Store in viz_state for UI
                    viz_state$results[[viz_type]] <- viz_result

                    # Initialize visualizations list if it doesn't exist
                    if (is.null(project_data$visualizations)) {
                      project_data$visualizations <- list()
                    }

                    # Store in project_data for other modules (like generate)
                    project_data$visualizations[[viz_type]] <- viz_result

                    # Ensure we have at least a main plot
                    if (is.null(viz_result$plots$main) && !is.null(viz_result$plot)) {
                      project_data$visualizations[[viz_type]]$plots$main <- viz_result$plot
                    }

                    logger::log_info("Restored {viz_type} visualization")
                  }
                } else {
                  logger::log_warn("Cannot restore {viz_type} visualization - missing analyzed data")
                }
              },
              error = function(e) {
                logger::log_error("Error restoring {viz_type} visualization: {e$message}")
              }
            )
          }

          # If we had dashboard visualization, recreate it
          if ("dashboard" %in% names(project_data$visualization_status)) {
            tryCatch(
              {
                dashboard_data <- get_dashboard_data()
                if (!is.null(dashboard_data) && length(dashboard_data) > 0) {
                  dashboard_result <- create_visualization_result("dashboard", dashboard_data)

                  # Store in viz_state for UI
                  viz_state$results[["dashboard"]] <- dashboard_result

                  # Initialize visualizations list if it doesn't exist
                  if (is.null(project_data$visualizations)) {
                    project_data$visualizations <- list()
                  }

                  # Store in project_data for other modules (like generate)
                  project_data$visualizations[["dashboard"]] <- dashboard_result

                  # Ensure we have at least a main plot
                  if (is.null(dashboard_result$plots$main) && !is.null(dashboard_result$plot)) {
                    project_data$visualizations[["dashboard"]]$plots$main <- dashboard_result$plot
                  }

                  logger::log_info("Restored dashboard visualization")
                }
              },
              error = function(e) {
                logger::log_error("Error restoring dashboard visualization: {e$message}")
              }
            )
          }

          # Signal that auto-restore is complete
          logger::log_info("Auto-restore completed for all visualizations")

          # Ensure visualization complete status is properly set
          if (!is.null(project_data$status)) {
            project_data$status$visualization_complete <- TRUE
          }
        }
      }
    })

    # Main visualization handler
    observeEvent(input$run_visualization, {
      req(input$visualization_type)

      tryCatch(
        {
          viz_type <- input$visualization_type
          viz_state$current_type <- viz_type

          show_progress("Starting visualization...")

          if (viz_type == "full") {
            run_full_visualization()
          } else {
            run_single_visualization(viz_type)
          }

          hide_progress()
          logger::log_info("Visualization completed successfully")
        },
        error = function(e) {
          handle_visualization_error(e)
        }
      )
    })

    # Helper functions for visualization execution
    run_single_visualization <- function(viz_type) {
      show_progress(paste("Preparing", VIZ_CONFIG[[viz_type]]$name, "visualization..."), 30)

      # Get the appropriate data and create visualization
      viz_data <- get_visualization_data(viz_type)
      if (is.null(viz_data)) {
        stop(paste("No", viz_type, "data available. Please run analysis first."))
      }

      show_progress("Generating plots...", 70)
      viz_result <- create_visualization_result(viz_type, viz_data)

      # Store the visualization result in viz_state
      viz_state$results[[viz_type]] <- viz_result

      # Minimal debug output for visualization storage
      cat("Storing", viz_type, "visualization\n")

      # Initialize visualizations list if it doesn't exist
      if (is.null(project_data$visualizations)) {
        project_data$visualizations <- list()
      }

      # Check if this visualization was previously completed (from snapshot restore)
      # but the actual plot objects are missing - if so, recreate them
      restore_if_needed <- function(viz_type) {
        if (!is.null(project_data$visualization_status) &&
          !is.null(project_data$visualization_status[[viz_type]]) &&
          is.null(project_data$visualizations[[viz_type]])) {
          # Validate that we have the necessary analyzed data
          has_analyzed_data <- !is.null(project_data$analyzed_data) &&
            (viz_type == "indicators" || !is.null(project_data$analyzed_data[[paste0(viz_type, "_analyzed")]]))

          if (has_analyzed_data) {
            logger::log_info("Restoring {viz_type} visualization from analyzed data")
            return(TRUE)
          } else {
            logger::log_warn("Cannot restore {viz_type} visualization - missing analyzed data")
            return(FALSE)
          }
        }
        return(FALSE)
      }

      should_restore <- restore_if_needed(viz_type)

      # Store the entire viz_result
      project_data$visualizations[[viz_type]] <- viz_result

      # Verify storage
      if (is.null(project_data$visualizations[[viz_type]])) {
        message("ERROR: Failed to store visualization for ", viz_type)
      } else {
        cat("Stored", viz_type, "visualization\n")
      }

      # Ensure we have at least a main plot
      if (is.null(viz_result$plots$main) && !is.null(viz_result$plot)) {
        project_data$visualizations[[viz_type]]$plots$main <- viz_result$plot
      }

      show_progress("Completed!", 100)
      viz_state$status <- "success"
      viz_state$message <- paste(VIZ_CONFIG[[viz_type]]$name, "visualization completed successfully!")

      # Open the Results accordion panel
      bslib::accordion_panel_set(
        id = "visualize_accordion",
        values = "results"
      )
    }

    run_full_visualization <- function() {
      # First run all the main visualizations (excluding dashboard)
      viz_types <- c("indicators", "alignment", "dynamics", "cascade")
      progress_steps <- seq(10, 75, length.out = length(viz_types))

      # Initialize visualizations list if it doesn't exist
      if (is.null(project_data$visualizations)) {
        project_data$visualizations <- list()
      }

      # Check if visualizations were previously completed but plots are missing
      restore_visualizations <- function() {
        if (!is.null(project_data$visualization_status)) {
          for (viz_type in names(project_data$visualization_status)) {
            if (is.null(project_data$visualizations[[viz_type]])) {
              # Check if we have the necessary analyzed data for restoration
              has_analyzed_data <- !is.null(project_data$analyzed_data) &&
                (viz_type == "indicators" || !is.null(project_data$analyzed_data[[paste0(viz_type, "_analyzed")]]))

              if (has_analyzed_data) {
                logger::log_info("Will restore {viz_type} visualization during full run")
              } else {
                logger::log_warn("Cannot restore {viz_type} visualization - missing analyzed data")
              }
            }
          }
        }
      }
      restore_visualizations()

      for (i in seq_along(viz_types)) {
        viz_type <- viz_types[i]

        # Skip if already restored/completed
        if (!is.null(viz_state$results[[viz_type]])) {
          logger::log_info("Skipping {viz_type} - already restored")
          next
        }

        show_progress(paste("Processing", VIZ_CONFIG[[viz_type]]$name, "..."), progress_steps[i])

        viz_data <- get_visualization_data(viz_type)
        if (!is.null(viz_data)) {
          viz_result <- create_visualization_result(viz_type, viz_data)

          # Store in viz_state
          viz_state$results[[viz_type]] <- viz_result

          # Store in project_data with consistent structure
          project_data$visualizations[[viz_type]] <- viz_result

          # Ensure we have at least a main plot
          if (is.null(viz_result$plots$main) && !is.null(viz_result$plot)) {
            project_data$visualizations[[viz_type]]$plots$main <- viz_result$plot
          }

          # No debug output needed
        } else {
          message("No data available for visualization: ", viz_type)
        }
        Sys.sleep(0.2) # Small delay for UI feedback
      }

      # Now create the dashboard with all the completed visualizations
      # Skip if dashboard is already restored
      if (is.null(viz_state$results[["dashboard"]])) {
        show_progress("Creating Dashboard...", 85)
        dashboard_data <- get_dashboard_data()
        if (!is.null(dashboard_data)) {
          dashboard_result <- create_visualization_result("dashboard", dashboard_data)

          # Store dashboard results
          viz_state$results[["dashboard"]] <- dashboard_result
          project_data$visualizations[["dashboard"]] <- dashboard_result

          # Ensure we have at least a main plot
          if (is.null(dashboard_result$plots$main) && !is.null(dashboard_result$plot)) {
            project_data$visualizations[["dashboard"]]$plots$main <- dashboard_result$plot
          }
        } else {
          message("Could not create dashboard - no visualization data available")
        }
      } else {
        logger::log_info("Skipping dashboard - already restored")
      }

      show_progress("Full visualization completed!", 100)
      viz_state$status <- "success"
      viz_state$message <- "Full visualization completed successfully!"

      # Open the Results accordion panel
      bslib::accordion_panel_set(
        id = "visualize_accordion",
        values = "results"
      )
    }

    # Data retrieval function
    get_visualization_data <- function(viz_type) {
      switch(viz_type,
        "indicators" = project_data$cleaned_data$indicators,
        "alignment" = get_analysis_data("alignment"),
        "dynamics" = get_analysis_data("dynamics"),
        "cascade" = get_analysis_data("cascade"),
        "dashboard" = get_dashboard_data(),
        NULL
      )
    }

    get_dashboard_data <- function() {
      # Collect all available visualizations for dashboard
      dashboard_data <- list()

      # Check which visualizations are available in viz_state$results (not project_data)
      available_viz <- names(viz_state$results)

      if (is.null(available_viz) || length(available_viz) == 0) {
        message("No visualizations available for dashboard")
        return(NULL)
      }

      message("DEBUG: Available visualizations for dashboard: ", paste(available_viz, collapse = ", "))

      # Collect preview plots from available visualizations
      for (viz_type in c("indicators", "alignment", "dynamics", "cascade")) {
        if (viz_type %in% available_viz) {
          viz_result <- viz_state$results[[viz_type]]
          message("DEBUG: Checking ", viz_type, " - has plots: ", !is.null(viz_result$plots))
          if (!is.null(viz_result) && !is.null(viz_result$plots$preview)) {
            message("DEBUG: Adding ", viz_type, " preview plot to dashboard")
            dashboard_data[[viz_type]] <- viz_result$plots$preview
          } else {
            message("DEBUG: No preview plot found for ", viz_type)
          }
        }
      }

      message("DEBUG: Dashboard data contains: ", paste(names(dashboard_data), collapse = ", "))

      if (length(dashboard_data) == 0) {
        message("No preview plots available for dashboard")
        return(NULL)
      }

      return(dashboard_data)
    }

    get_analysis_data <- function(analysis_type) {
      message("\n===== DEBUG: get_analysis_data for ", analysis_type, " =====")
      analysis_data <- project_data$analysis

      if (is.null(analysis_data)) {
        message("WARNING: analysis_data is NULL in get_analysis_data")
        return(NULL)
      }

      message("analysis_data names: ", paste(names(analysis_data), collapse = ", "))
      message("analysis_data$dynamics_analyzed: ", analysis_data$dynamics_analyzed)

      result <- switch(analysis_type,
        "alignment" = if (isTRUE(analysis_data$alignment_analyzed)) {
          analysis_data$alignment_table
        } else {
          message("Alignment analysis not completed or data not available")
          NULL
        },
        "dynamics" = if (isTRUE(analysis_data$dynamics_analyzed)) {
          message("\n===== DEBUG: Processing dynamics data =====")

          # Check for dimension data in analysis_data
          dimension_data <- analysis_data$dynamics_dimensions_table
          message("Dimension data from analysis_data: ", !is.null(dimension_data))

          # Fall back to project_data$analyzed_data if needed
          if (is.null(dimension_data) && !is.null(project_data$analyzed_data)) {
            message("Falling back to project_data$analyzed_data for dimension data")
            dimension_data <- project_data$analyzed_data$dynamics_dimensions_table
            message("Dimension data from project_data: ", !is.null(dimension_data))
          }

          # Debug output for domain scores
          domain_scores <- analysis_data$dynamics_domains
          message("Domain scores available: ", !is.null(domain_scores))
          if (!is.null(domain_scores)) {
            message("Domain scores structure:")
            str(domain_scores)
          }

          # Create result list with debug info
          result <- list(
            domain_scores = domain_scores,
            dimension_scores = dimension_data
          )

          message("Final dynamics data structure:")
          str(result)

          result
        } else {
          message("Dynamics analysis not completed or data not available")
          NULL
        },
        "cascade" = if (isTRUE(analysis_data$cascade_analyzed)) {
          analysis_data$cascade_data
        } else {
          message("Cascade analysis not completed or data not available")
          NULL
        },
        {
          message("Unknown analysis type: ", analysis_type)
          NULL
        }
      )

      return(result)
    }

    # Visualization result creation
    create_visualization_result <- function(viz_type, viz_data) {
      message("\n===== DEBUG: create_visualization_result for ", viz_type, " =====")
      message("viz_type: ", viz_type)

      if (is.null(viz_data)) {
        message("ERROR: No data available for visualization: ", viz_type)
        return(NULL)
      }

      message("viz_data structure:")
      str(viz_data)

      # Generate plots
      plots <- tryCatch(
        {
          switch(viz_type,
            "indicators" = create_indicators_plots(viz_data, get_color_palette(), project_data),
            "alignment" = create_alignment_plots(viz_data, get_color_palette(), project_data),
            "dynamics" = create_dynamics_plots(viz_data, get_color_palette(), project_data),
            "cascade" = create_cascade_plots(viz_data, get_color_palette(), project_data),
            "dashboard" = create_dashboard_plots(viz_data, get_color_palette(), project_data),
            stop("Unknown visualization type: ", viz_type)
          )
        },
        error = function(e) {
          message("Error creating ", viz_type, " plot: ", conditionMessage(e))
          NULL
        }
      )

      # Check if plots were generated
      if (is.null(plots)) {
        message("No plots were generated for ", viz_type)
        return(NULL)
      }

      # Create result list
      result <- list(
        plots = plots,
        data = viz_data,
        config = VIZ_CONFIG[[viz_type]]
      )

      cat("Created visualization result for", viz_type, "with structure:", paste(names(result), collapse = ", "), "\n")
      result
    }

    create_dashboard_plots <- function(dashboard_data, color_palette, project_data) {
      message("\n===== DEBUG: create_dashboard_plots =====")
      message("Dashboard data structure:")
      str(dashboard_data)
      message("Number of plots in dashboard_data: ", length(dashboard_data))
      message("Plot names: ", paste(names(dashboard_data), collapse = ", "))

      if (is.null(dashboard_data) || length(dashboard_data) == 0) {
        message("ERROR: No data available for dashboard")
        return(NULL)
      }

      # Load brand fonts
      brand_config <- NULL
      if (!is.null(project_data)) {
        if (is.reactive(project_data)) {
          pd <- project_data()
          if (!is.null(pd$brand_config)) {
            brand_config <- pd$brand_config
          }
        } else {
          if (!is.null(project_data$brand_config)) {
            brand_config <- project_data$brand_config
          }
        }
      }
      load_brand_fonts(brand_config)

      # Check each plot in dashboard_data
      for (viz_name in names(dashboard_data)) {
        plot_obj <- dashboard_data[[viz_name]]
        message("Plot ", viz_name, " class: ", paste(class(plot_obj), collapse = ", "))
        message("Plot ", viz_name, " is ggplot: ", inherits(plot_obj, "ggplot"))
      }

      # Create real multi-plot dashboard with specified layout
      tryCatch(
        {
          message("Creating multi-plot dashboard...")

          # Check if we have the required plots
          required_plots <- c("alignment", "dynamics", "cascade", "indicators")
          available_plots <- names(dashboard_data)

          message("Required plots: ", paste(required_plots, collapse = ", "))
          message("Available plots: ", paste(available_plots, collapse = ", "))

          # Try installing and using patchwork for layout
          patchwork_available <- FALSE

          # First check if patchwork is available
          if (requireNamespace("patchwork", quietly = TRUE)) {
            patchwork_available <- TRUE
            message("patchwork package found")
          } else {
            message("patchwork package not found, attempting to install...")
            tryCatch(
              {
                install.packages("patchwork", quiet = TRUE)
                if (requireNamespace("patchwork", quietly = TRUE)) {
                  patchwork_available <- TRUE
                  message("patchwork package installed successfully")
                }
              },
              error = function(e) {
                message("Failed to install patchwork: ", e$message)
              }
            )
          }

          # Get individual plots and add section labels with enhanced grid structure
          message("*** DASHBOARD DEBUG: Modifying individual plots with section labels ***")
          message("*** DASHBOARD DEBUG: Available plots in dashboard_data: ", paste(names(dashboard_data), collapse = ", "))

          # Main dashboard plots with labels and styling
          alignment_plot <- dashboard_data[["alignment"]] +
            ggplot2::labs(tag = "project alignment") +
            ggplot2::theme(
              plot.tag = ggplot2::element_text(size = 9, face = "italic", color = "#666666", hjust = 0),
              plot.tag.position = c(0.02, 0.98),
              panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
              plot.margin = ggplot2::margin(8, 3, 3, 8),
              plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = "#D0D0D0", linewidth = 0.5)
            )
          message("*** DASHBOARD DEBUG: Alignment plot modified ***")

          dynamics_plot <- dashboard_data[["dynamics"]] +
            ggplot2::labs(tag = "project dynamics") +
            ggplot2::theme(
              plot.tag = ggplot2::element_text(size = 9, face = "italic", color = "#666666", hjust = 0),
              plot.tag.position = c(0.02, 0.98),
              panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
              plot.margin = ggplot2::margin(3, 3, 8, 8),
              plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = "#D0D0D0", linewidth = 0.5)
            )
          message("*** DASHBOARD DEBUG: Dynamics plot modified ***")

          cascade_plot <- dashboard_data[["cascade"]] +
            ggplot2::labs(tag = "cascade effects") +
            ggplot2::theme(
              plot.tag = ggplot2::element_text(size = 9, face = "italic", color = "#666666", hjust = 0),
              plot.tag.position = c(0.02, 0.98),
              panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
              plot.margin = ggplot2::margin(8, 8, 3, 3),
              plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = "#D0D0D0", linewidth = 0.5)
            )
          message("*** DASHBOARD DEBUG: Cascade plot modified ***")

          indicators_plot <- dashboard_data[["indicators"]] +
            ggplot2::labs(tag = "project indicators") +
            ggplot2::theme(
              plot.tag = ggplot2::element_text(size = 9, face = "italic", color = "#666666", hjust = 0),
              plot.tag.position = c(0.02, 0.98),
              panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
              plot.margin = ggplot2::margin(3, 8, 8, 3),
              plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = "#D0D0D0", linewidth = 0.5)
            )
          message("*** DASHBOARD DEBUG: Indicators plot modified ***")

          # Create preview versions with no text and transparent background
          alignment_plot_preview <- dashboard_data[["alignment"]] +
            ggplot2::labs(tag = NULL, title = NULL, subtitle = NULL, caption = NULL) +
            ggplot2::theme(
              axis.text = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              legend.position = "none",
              panel.grid = ggplot2::element_blank(),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.margin = ggplot2::margin(2, 1, 1, 2)
            )

          dynamics_plot_preview <- dashboard_data[["dynamics"]] +
            ggplot2::labs(tag = NULL, title = NULL, subtitle = NULL, caption = NULL) +
            ggplot2::theme(
              axis.text = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              legend.position = "none",
              panel.grid = ggplot2::element_blank(),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.margin = ggplot2::margin(1, 1, 2, 2)
            )

          cascade_plot_preview <- dashboard_data[["cascade"]] +
            ggplot2::labs(tag = NULL, title = NULL, subtitle = NULL, caption = NULL) +
            ggplot2::theme(
              axis.text = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              legend.position = "none",
              panel.grid = ggplot2::element_blank(),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.margin = ggplot2::margin(2, 2, 1, 1)
            )

          indicators_plot_preview <- dashboard_data[["indicators"]] +
            ggplot2::labs(tag = NULL, title = NULL, subtitle = NULL, caption = NULL) +
            ggplot2::theme(
              axis.text = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank(),
              legend.position = "none",
              panel.grid = ggplot2::element_blank(),
              plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
              panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
              plot.margin = ggplot2::margin(1, 2, 2, 1)
            )
          message("*** DASHBOARD DEBUG: All plots prepared, checking patchwork availability ***")

          if (patchwork_available) {
            message("*** DASHBOARD DEBUG: Using patchwork for dashboard layout ***")
            message("*** DASHBOARD DEBUG: Creating 2x2 layout with enhanced styling ***")

            # Create the main dashboard layout:
            # ROW 1: Alignment Plot (50%) + Cascade Plot (50%)
            # ROW 2: Dynamics Plot (50%) + Indicators Plot (50%)
            dashboard_plot <- (alignment_plot + cascade_plot) /
              (dynamics_plot + indicators_plot) +
              patchwork::plot_layout(heights = c(1, 1)) +
              patchwork::plot_annotation(
                title = "CEnTR*IMPACT Visualizations",
                subtitle = "A synthesis of project alignment, dynamics, indicators, and cascade effects",
                theme = ggplot2::theme(
                  plot.title = ggplot2::element_text(
                    hjust = 0.5, size = 18, face = "bold",
                    color = "#2C3E50", margin = ggplot2::margin(b = 5),
                    family = get_brand_sans_font()
                  ),
                  plot.subtitle = ggplot2::element_text(
                    hjust = 0.5, size = 12, color = "#666666",
                    face = "italic", margin = ggplot2::margin(b = 5),
                    family = get_brand_sans_font()
                  ),
                  panel.background = ggplot2::element_rect(fill = "#F8F8F8", color = "#CCCCCC", linewidth = 1),
                  plot.background = ggplot2::element_rect(fill = "#F8F8F8", color = NA)
                )
              )

            # Create the preview dashboard layout (no text, transparent background):
            dashboard_plot_preview <- (alignment_plot_preview + cascade_plot_preview) /
              (dynamics_plot_preview + indicators_plot_preview) +
              patchwork::plot_layout(heights = c(1, 1)) +
              patchwork::plot_annotation(
                theme = ggplot2::theme(
                  panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
                  plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
                )
              )

            message("*** DASHBOARD DEBUG: Patchwork dashboard created successfully ***")
          } else if (requireNamespace("gridExtra", quietly = TRUE)) {
            message("*** DASHBOARD DEBUG: Using gridExtra for dashboard layout ***")

            # Create the main layout using gridExtra:
            # ROW 1: Alignment Plot (50%) + Cascade Plot (50%)
            # ROW 2: Dynamics Plot (50%) + Indicators Plot (50%)
            dashboard_plot <- gridExtra::grid.arrange(
              gridExtra::arrangeGrob(alignment_plot, cascade_plot, ncol = 2),
              gridExtra::arrangeGrob(dynamics_plot, indicators_plot, ncol = 2),
              ncol = 1,
              heights = c(1, 1),
              top = gridExtra::arrangeGrob(
                grid::textGrob("CEnTR*IMPACT Visualizations",
                  gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#2C3E50")
                ),
                grid::textGrob("A synthesis of project alignment, dynamics, indicators, and cascade effects",
                  gp = grid::gpar(fontsize = 12, col = "#666666", fontface = "italic")
                ),
                heights = c(0.6, 0.4)
              )
            )

            # Create the preview layout (no text, transparent background)
            dashboard_plot_preview <- gridExtra::grid.arrange(
              gridExtra::arrangeGrob(alignment_plot_preview, cascade_plot_preview, ncol = 2),
              gridExtra::arrangeGrob(dynamics_plot_preview, indicators_plot_preview, ncol = 2),
              ncol = 1,
              heights = c(1, 1)
            )
          } else {
            message("*** DASHBOARD DEBUG: Neither patchwork nor gridExtra available, using fallback ***")

            # Create a simple custom dashboard using base R approach
            # For now, show the alignment plot with enhanced styling
            dashboard_plot <- alignment_plot +
              ggplot2::labs(
                title = "CEnTR*IMPACT Visualizations - Alignment Analysis",
                subtitle = paste("Also available: Dynamics, Cascade, Indicators |", length(dashboard_data), "visualizations completed"),
                caption = "Install 'patchwork' package to see all plots combined in one dashboard"
              ) +
              ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold", color = "#2C3E50"),
                plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, color = "#666666", face = "italic"),
                plot.caption = ggplot2::element_text(hjust = 0.5, size = 8, color = "#999999"),
                panel.border = ggplot2::element_rect(color = "#E0E0E0", fill = NA, linewidth = 0.5),
                plot.margin = ggplot2::margin(15, 15, 15, 15)
              )

            # Create preview version (no text, transparent background)
            dashboard_plot_preview <- alignment_plot_preview
          }

          message("Dashboard created successfully")

          # Create high-resolution version for report
          dashboard_plot_hires <- dashboard_plot
          attr(dashboard_plot_hires, "dpi") <- 300

          list(
            main = dashboard_plot,
            preview = dashboard_plot_preview,
            report = dashboard_plot_hires
          )
        },
        error = function(e) {
          message("ERROR creating dashboard plot: ", conditionMessage(e))
          message("Full error: ")
          print(e)
          # Fallback: return the first available plot if dashboard creation fails
          if (length(dashboard_data) > 0) {
            message("Using fallback - returning first available plot")
            first_plot <- dashboard_data[[1]]
            # Create a simple transparent preview version of the first plot
            first_plot_preview <- first_plot +
              ggplot2::labs(tag = NULL, title = NULL, subtitle = NULL, caption = NULL) +
              ggplot2::theme(
                axis.text = ggplot2::element_blank(),
                axis.title = ggplot2::element_blank(),
                legend.position = "none",
                panel.grid = ggplot2::element_blank(),
                plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
                panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
              )

            list(
              main = first_plot,
              preview = first_plot_preview,
              report = first_plot
            )
          } else {
            message("No fallback available - returning NULL")
            NULL
          }
        }
      )
    }

    # Progress management
    show_progress <- function(message, value = NULL) {
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').style.visibility = 'visible';",
        ns("progress_container")
      ))
      if (!is.null(value)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar"),
          value = value,
          title = message
        )
      }
      viz_state$message <- message
    }

    hide_progress <- function() {
      shinyjs::delay(1000, {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').style.visibility = 'hidden';",
          ns("progress_container")
        ))
      })
    }

    # Error handling
    handle_visualization_error <- function(e) {
      error_msg <- paste("Visualization error:", conditionMessage(e))
      viz_state$status <- "error"
      viz_state$message <- error_msg
      hide_progress()
      cat("ERROR:", error_msg, "\n")
    }

    # Results UI rendering
    output$visualization_results <- renderUI({
      req(viz_state$results)

      if (length(viz_state$results) == 0) {
        return(create_no_results_ui())
      }

      # Define the order for displaying results (dashboard should be last)
      viz_order <- c("indicators", "alignment", "dynamics", "cascade", "dashboard")
      available_viz <- names(viz_state$results)
      ordered_viz <- viz_order[viz_order %in% available_viz]

      # Create a section for each visualization result in the specified order
      result_uis <- lapply(ordered_viz, function(viz_type) {
        viz_data <- viz_state$results[[viz_type]]

        # Debug: Check what we have in viz_data
        cat("Rendering", viz_type, "with structure:", toString(names(viz_data)), "\n")

        # Get the plots for display
        preview_plot <- NULL
        main_plot <- NULL

        if (!is.null(viz_data$plots)) {
          preview_plot <- viz_data$plots$preview
          main_plot <- viz_data$plots$main
          report_plot <- viz_data$plots$report

          # Fallbacks: ensure both are set if either exists
          if (is.null(main_plot) && !is.null(preview_plot)) {
            main_plot <- preview_plot
          }
          if (is.null(preview_plot) && !is.null(main_plot)) {
            preview_plot <- main_plot
          }
          if (is.null(report_plot) && !is.null(main_plot)) {
            report_plot <- main_plot
          }
        }

        # Create a unique ID for the plot (without namespace for output ID)
        plot_id_base <- paste0("viz_plot_", gsub("[^A-Za-z0-9]", "_", tolower(viz_type)))

        # Debug: Print plot object info
        cat("\n--- DEBUG: Plot Object Info ---\n")
        cat("Plot ID (base):", plot_id_base, "\n")
        cat("Plot class:", class(preview_plot), "\n")
        cat("Is ggplot:", inherits(preview_plot, "gg"), "\n")

        # Set up the plot rendering with enhanced error handling
        if (!is.null(preview_plot)) {
          # Create a local copy of preview_plot for the renderPlot closure
          local_preview_plot <- preview_plot

          # Create the preview plot output
          output[[plot_id_base]] <- renderPlot(
            {
              tryCatch(
                {
                  if (inherits(local_preview_plot, "gg")) {
                    print(local_preview_plot)
                  } else if (is.function(local_preview_plot)) {
                    cat("Plot is a function, calling it...\n")
                    p <- local_preview_plot()
                    if (inherits(p, "gg")) {
                      print(p)
                    } else {
                      stop("Function did not return a ggplot object")
                    }
                  } else {
                    stop("Not a ggplot object or function")
                  }
                },
                error = function(e) {
                  cat("ERROR rendering plot:", conditionMessage(e), "\n")
                  plot(1, 1,
                    type = "n",
                    main = "Error rendering plot",
                    sub = conditionMessage(e)
                  )
                }
              )
            },
            res = 140
          )
        }

        # Set up full plot rendering for modal with enhanced error handling
        full_plot_id <- paste0("full_", plot_id_base)
        if (!is.null(main_plot) || !is.null(preview_plot)) {
          # Create local copies for the closure
          local_main_plot <- main_plot
          local_preview_plot <- preview_plot

          output[[full_plot_id]] <- renderPlot(
            {
              tryCatch(
                {
                  if (!is.null(local_main_plot) && inherits(local_main_plot, "gg")) {
                    print(local_main_plot)
                  } else if (!is.null(local_preview_plot) && inherits(local_preview_plot, "gg")) {
                    print(local_preview_plot)
                  } else if (is.function(local_main_plot)) {
                    cat("Main plot is a function, calling it...\n")
                    p <- local_main_plot()
                    if (inherits(p, "gg")) {
                      print(p)
                    } else {
                      stop("Main plot function did not return a ggplot object")
                    }
                  } else if (is.function(local_preview_plot)) {
                    cat("Preview plot is a function, calling it...\n")
                    p <- local_preview_plot()
                    if (inherits(p, "gg")) {
                      print(p)
                    } else {
                      stop("Preview plot function did not return a ggplot object")
                    }
                  } else {
                    stop("No valid plot object available")
                  }
                },
                error = function(e) {
                  cat("ERROR rendering main plot:", conditionMessage(e), "\n")
                  plot(1, 1,
                    type = "n",
                    main = "Error rendering main plot",
                    sub = conditionMessage(e)
                  )
                }
              )
            },
            res = 140
          )
        }

        # Set up view full plot observer
        observeEvent(input[[paste0("view_full_", gsub(" ", "_", viz_type))]], {
          showModal(modalDialog(
            title = paste("Full", tools::toTitleCase(gsub("_", " ", viz_type)), "Visualization"),
            size = "xl",
            easyClose = TRUE,
            footer = modalButton("Close"),
            plotOutput(ns(full_plot_id), height = "500px"),
            if (!is.null(viz_data$description)) {
              div(
                style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                h4("Description"),
                p(viz_data$description)
              )
            }
          ))
        })

        # Set up download handler
        # Fix: Use the same ID format as the button in visualization_section_ui
        viz_type_title <- tools::toTitleCase(gsub("_", " ", viz_type))
        download_id <- paste0("download_", gsub(" ", "_", tolower(viz_type_title)))

        # Debug info for download handler
        cat("\n--- DEBUG: Setting up download handler ---\n")
        cat("viz_type:", viz_type, "\n")
        cat("viz_type_title:", viz_type_title, "\n")
        cat("download_id:", download_id, "\n")

        output[[download_id]] <- downloadHandler(
          filename = function() {
            paste0(
              "visualization_", gsub(" ", "_", tolower(viz_type)),
              "_", format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".png"
            )
          },
          content = function(file) {
            # Debug info when download is triggered
            cat("\n--- DEBUG: Download triggered ---\n")
            cat("Download ID:", download_id, "\n")
            cat("Filename:", file, "\n")

            # Use the report plot for download if available, otherwise fallback to main or preview
            plot_to_save <- if (!is.null(report_plot)) {
              cat("Using report_plot for download\n")
              report_plot
            } else if (!is.null(main_plot)) {
              cat("Using main_plot for download\n")
              main_plot
            } else if (!is.null(preview_plot)) {
              cat("Using preview_plot for download\n")
              preview_plot
            } else {
              cat("ERROR: No plot available for download\n")
              return(NULL)
            }

            # Determine dimensions based on visualization type
            if (viz_type %in% c("indicators", "alignment")) {
              # 16:9 ratio plots (12in x 6.75in at 300 DPI)
              width <- 12
              height <- 6.75
            } else {
              # Square plots (9in x 9in at 300 DPI)
              width <- 9
              height <- 9
            }

            # Save the plot
            # Save the plot and log the result
            tryCatch(
              {
                cat("Saving plot with dimensions:", width, "x", height, "inches\n")
                # Ensure ggplot2 is loaded for ggsave
                if (!requireNamespace("ggplot2", quietly = TRUE)) {
                  stop("Package 'ggplot2' is required but not available")
                }
                ggplot2::ggsave(
                  file,
                  plot = plot_to_save,
                  device = "png",
                  width = width,
                  height = height,
                  units = "in",
                  dpi = 300,
                  bg = "transparent"
                )
                cat("Plot successfully saved to:", file, "\n")
              },
              error = function(e) {
                cat("ERROR saving plot:", conditionMessage(e), "\n")
              }
            )
          }
        )

        # Create visualization section using the visualization_section_ui function
        # Add debug log to help track button IDs
        viz_type_title <- tools::toTitleCase(gsub("_", " ", viz_type))
        button_id <- paste0("download_", gsub(" ", "_", tolower(viz_type_title)))
        cat("\n--- DEBUG: Creating UI components ---\n")
        cat("Creating UI for", viz_type, "with button ID:", button_id, "\n")
        cat("This should match download_id:", download_id, "\n")
        cat("Are IDs matching:", button_id == download_id, "\n")

        visualization_section_ui(
          data = viz_data$data,
          metric_title = viz_type_title,
          preview_plot = plotOutput(ns(plot_id_base), height = "200px"),
          main_plot = plotOutput(ns(full_plot_id), height = "500px"),
          ns = ns,
          session = session,
          fieldset_title = toupper(gsub("_", " ", viz_type)),
          description = visualization_descriptions[[viz_type]]
        )
      })

      # Wrap in a fluidRow with columns
      tagList(
        do.call(fluidRow, lapply(result_uis, function(ui) {
          column(width = 12, ui)
        }))
      )
    })

    # Return visualization state for external access
    list(
      results = reactive(viz_state$results),
      status = reactive(viz_state$status),
      message = reactive(viz_state$message)
    )
  })
} # Close moduleServer and mod_visualize_server

# Helper UI functions
create_no_results_ui <- function() {
  tags$div(
    class = "data-placeholder text-center p-4",
    tags$div(
      class = "d-flex align-items-center justify-content-center gap-2",
      phosphoricons::ph("warning-circle", weight = "bold", class = "warning-icon"),
      tags$div(
        tags$strong("No Visualization Results"),
        tags$br(),
        "Please select a visualization type and run the visualization to view results."
      )
    )
  )
}

create_result_section_ui <- function(viz_type, result, ns) {
  config <- result$config

  tags$fieldset(
    class = "custom-fieldset mb-4",
    tags$legend(class = "custom-legend", toupper(config$name)),
    layout_columns(
      col_widths = c(4, 8),
      gap = "1rem",

      # Left: Preview and controls
      div(
        class = "d-flex flex-column h-100",
        bslib::value_box(
          title = config$name,
          value = plotOutput(ns(paste0(viz_type, "_preview")), height = "120px"),
          showcase = phosphoricons::ph(config$icon, weight = "fill"),
          theme = value_box_theme(bg = config$color, fg = "#f5f1e8"),
          class = "mb-3"
        ),
        div(
          class = "btn-group w-100",
          actionButton(
            ns(paste0(viz_type, "_expand")),
            "Expand",
            icon = icon("expand"),
            class = "btn-sm btn-outline-primary"
          ),
          downloadButton(
            ns(paste0(viz_type, "_download")),
            "Download",
            icon = icon("download"),
            class = "btn-sm btn-outline-success"
          )
        )
      ),

      # Right: Description and status
      bslib::card(
        bslib::card_header(
          tags$div(
            class = "d-flex align-items-center justify-content-between",
            tags$span("READY FOR REPORT GENERATION",
              class = "fw-medium text-success text-uppercase font-monospace"
            ),
            phosphoricons::ph("check-circle",
              weight = "fill", size = "2x",
              style = "color: var(--bs-success);"
            )
          )
        ),
        bslib::card_body(
          HTML(paste0("<div style='font-size: 1.1em;'>", config$description, "</div>"))
        )
      )
    ),

    # Overlay UI
    uiOutput(ns(paste0(viz_type, "_overlay")))
  )
}

# Overlay management
create_overlay_handlers <- function(viz_type, ns, session, output) {
  overlay_active <- reactiveVal(FALSE)

  observeEvent(session$input[[paste0(viz_type, "_expand")]], {
    overlay_active(TRUE)
  })

  observeEvent(session$input[[paste0(viz_type, "_close")]], {
    overlay_active(FALSE)
  })

  output[[paste0(viz_type, "_overlay")]] <- renderUI({
    if (isTRUE(overlay_active())) {
      tagList(
        div(
          class = "overlay-bg",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random())",
            ns(paste0(viz_type, "_close"))
          )
        ),
        div(
          class = "overlay-card",
          style = "background: #edeae2; max-width: 90vw; max-height: 90vh;",
          tags$h4(VIZ_CONFIG[[viz_type]]$name, class = "mb-3"),
          plotOutput(ns(paste0(viz_type, "_main")), height = "70vh"),
          actionButton(
            ns(paste0(viz_type, "_close")),
            "Close",
            class = "btn-secondary float-end mt-3"
          )
        )
      )
    }
  })
}
