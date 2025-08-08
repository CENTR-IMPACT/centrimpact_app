# Reusable UI Components for shinymgr
# This file contains reusable UI components that can be used across different modules

# Import required functions from bslib
#' @importFrom bslib value_box_theme

#' Metric Section UI
#'
#' Generates a two-column layout with a value box/placeholder on the left and a details card on the right.
#' This function is designed to be flexible, allowing complete customization of all card sections
#' using tagList for maximum flexibility across different modules.
#'
#' @param data The data frame to check (e.g., rv$indicators). If NULL or has 0 rows, shows a placeholder.
#' @param title Title for the value box and fieldset legend
#' @param value The value to display in the value box (numeric or ggplot object)
#' @param card_header_text Text to display in the card header
#' @param card_body UI elements to display in the card body
#' @param card_footer_button_text Text for the footer button
#' @param card_footer_button_id ID for the footer button (will be namespaced)
#' @param placeholder_title Title for the placeholder (shown when no data)
#' @param placeholder_text Text for the placeholder (shown when no data)
#' @param placeholder_icon Icon for the placeholder (shown when no data)
#' @param ns Namespace function for Shiny module
#' @param fieldset_title Optional title for the fieldset (defaults to title if not provided)
#' @return A fieldset containing a two-column layout with a value box and details card
# Helper function to get icon and descriptor based on score and type
get_score_metrics <- function(score, type = c("alignment", "dynamics", "cascade")) {
  if (is.null(score) || is.na(score) || !is.numeric(score)) {
    return(list(icon = NULL, descriptor = NULL))
  }

  type <- match.arg(type)

  if (type == "alignment") {
    if (score < 0.40) {
      list(icon = "caret-circle-double-down", descriptor = "LOW")
    } else if (score >= 0.40 && score <= 0.59) {
      list(icon = "caret-circle-down", descriptor = "FAIR")
    } else if (score >= 0.60 && score <= 0.74) {
      list(icon = "caret-circle-up", descriptor = "GOOD")
    } else {
      list(icon = "caret-circle-double-up", descriptor = "EXCELLENT")
    }
  } else { # dynamics or cascade
    if (score < 0.50) {
      list(icon = "caret-circle-double-down", descriptor = "Very Low")
    } else if (score >= 0.50 && score <= 0.59) {
      list(icon = "caret-circle-down", descriptor = "Low")
    } else if (score >= 0.60 && score <= 0.69) {
      list(icon = "caret-circle-up-down", descriptor = "Moderate")
    } else if (score >= 0.70 && score <= 0.79) {
      list(icon = "caret-circle-up", descriptor = "High")
    } else {
      list(icon = "caret-circle-double-up", descriptor = "Very High")
    }
  }
}

metric_section_ui <- function(data,
                              # Value box parameters
                              title,
                              value,
                              round_to = 2,
                              value_subtitle = NULL, # Optional content below value
                              # Card parameters
                              card_header_text,
                              card_body,
                              card_footer_button_text,
                              card_footer_button_id,
                              # Placeholder parameters
                              placeholder_title,
                              placeholder_text,
                              placeholder_icon,
                              # Namespace function
                              ns,
                              # Session object for server-side rendering
                              session = shiny::getDefaultReactiveDomain(),
                              # Fieldset title (defaults to title if not provided)
                              fieldset_title = NULL,
                              # Whether to show metrics (icon and descriptor)
                              show_metrics = FALSE) {
  # Validate data parameter
  data_is_valid <- !is.null(data) && is.data.frame(data) && nrow(data) > 0

  # Check if value is a plot
  is_plot <- inherits(value, "ggplot")
  icon_color <- rgb(245, 241, 232, maxColorValue = 255, alpha = 125)
  title_lower <- tolower(title)
  if (grepl("indicators", title_lower)) {
    bgcolor <- "#7E8480"
    icon <- ph_i("gauge", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("alignment", title_lower)) {
    bgcolor <- "#A08E6F"
    icon <- ph_i("flower-lotus", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("dynamics", title_lower)) {
    bgcolor <- "#88707E"
    icon <- ph_i("pulse", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("cascade", title_lower)) {
    bgcolor <- "#B49291"
    icon <- ph_i("waveform", weight = "thin", size = "8x", color = icon_color)
  } else {
    # Default fallback
    bgcolor <- "#6c757d"
    icon <- ph_i("circle", weight = "thin", size = "8x", color = icon_color)
  }

  # Handle NULL or NA value - use any() to handle vectors
  if (is.null(value) || any(is.na(value))) {
    value <- "N/A"
  } else if (length(value) > 1) {
    # If value is a vector with length > 1, take the first element
    value <- value[1]
  }

  # Use title for fieldset if fieldset_title not provided
  if (is.null(fieldset_title)) {
    fieldset_title <- title
  }

  cols <- list(
    # LEFT SIDE: Value Box or Placeholder
    if (!data_is_valid && !is_plot) {
      # Placeholder when no data - updated to match standard look
      bslib::value_box(
        style = "
          padding: 0.25em !important;
          border-radius: 10px 0 0 10px !important;
          border: 1px 0 1px 1px !important;
          border-style: solid !important;
          border-color: #d4c5b9 !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
        title = tags$div(
          "",
          class = "text-uppercase fw-semibold",
          style = "font-size: 0rem;"
        ),
        value = tags$div(
          style = "
            font-size: 3rem;
            font-weight: 600;
            line-height: 1;
            color: #6c5a47;
            text-align: center;",
          placeholder_icon,
          tags$div(
            style = "font-size: 0.9rem; margin-top: 0.5rem; color: #6c5a47;",
            placeholder_title
          )
        ),
        theme = bslib::value_box_theme(bg = "#f5f0e8", fg = "#6c5a47"),
        tags$div(
          class = "text-uppercase fw-normal",
          style = "
            font-size: 0.8rem;
            margin-top: 0.25rem;
            color: #6c5a47;
            text-align: center;",
          placeholder_text
        )
      )
    } else {
      # Value box when data exists
      bslib::value_box(
        style = "
          padding: 0.25em !important;
          border-radius: 10px 0 0 10px !important;
          border: 1px 0 1px 1px !important;
          border-style: solid !important;
          border-color: #d4c5b9 !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
        title = tags$div(
          class = "text-uppercase fw-normal",
          style = "
            font-size: 1.1rem;
            margin-top: 0.25rem;
            color: #f5f1e8;
            text-align: left;
            text-transform: uppercase;",
          title
        ),
        value = if (is_plot) {
          # For plots, show a minimal indicator in the value area
          tags$div(
            style = "
              font-size: 1.5rem;
              font-weight: 600;
              line-height: 1;
              color: #f5f1e8;
              text-align: center;
              margin-top: 1.5rem;",
            "View"
          )
        } else {
          # Display numeric value with optional icon and descriptor
          tagList(
            tags$div(
              round(as.numeric(value), round_to),
              class = "font-monospace",
              style = "
                font-size: 5rem;
                font-weight: 600;
                line-height: 1;
                color: #f5f1e8;"
            ),
            # Add icon and descriptor below the score if show_metrics is TRUE
            if (isTRUE(show_metrics)) {
              # Determine score type from title
              score_type <- tolower(title)
              score_type <- if (grepl("alignment", score_type)) {
                "alignment"
              } else if (grepl("dynamics", score_type)) {
                "dynamics"
              } else if (grepl("cascade", score_type)) {
                "cascade"
              } else {
                NULL
              }

              if (!is.null(score_type)) {
                metrics <- get_score_metrics(as.numeric(value), score_type)

                if (!is.null(metrics$icon) && !is.null(metrics$descriptor)) {
                  tags$div(
                    class = "metric-container",
                    style = "display: flex; align-items: center; justify-content: center; margin-top: 0.5rem; gap: 0.5rem;",
                    ph(metrics$icon, weight = "bold", class = "metric-icon", style = "font-size: 1.25rem;"),
                    tags$span(
                      class = "metric-descriptor",
                      metrics$descriptor,
                      style = "font-size: 1rem; font-weight: 500; font-family: var(--bs-body-font-family); text-transform: uppercase;"
                    )
                  )
                }
              }
            }
          )
        },
        showcase = if (is_plot) {
          # Create a unique ID for the plot
          plot_id <- paste0("plot_", gsub("[^A-Za-z0-9]", "_", title), "_", sample(1:10000, 1))

          # Store the plot in the session's userData for rendering
          session$userData[[plot_id]] <- value

          # Set up server-side rendering for the plot
          output[[plot_id]] <- renderPlot({
            req(session$userData[[plot_id]])
            session$userData[[plot_id]]
          })

          # Return the plot output for the showcase
          tags$div(
            style = "height: 100px; width: 100%; padding: 0.25rem;",
            plotOutput(
              outputId = ns(plot_id),
              height = "100%",
              width = "100%"
            )
          )
        } else {
          icon
        },
        showcase_layout = "left center",
        theme = bslib::value_box_theme(bg = bgcolor, fg = "#f5f1e8"),
        # Optional subtitle content below the title
        if (!is.null(value_subtitle)) {
          tags$div(
            class = "text-uppercase font-monospace",
            style = "
              font-size: 0.75rem;
              margin-top: 0.1rem;
              color: #f5f1e8;
              opacity: 0.9;",
            value_subtitle
          )
        }
      )
    },

    # RIGHT SIDE: Details Card (only show if we have valid data)
    if (data_is_valid) {
      bslib::card(
        style = "
          padding: 0 !important;
          border-radius: 0 8px 8px 0 !important;
          border: 1px 1px 1px 0;
          border-style: solid;
          border-color: #d4c5b9 !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;
          background: #f9f5f0;",

        # Header
        bslib::card_header(
          style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
          tags$div(
            class = "fw-medium text-left",
            style = "font-size: 1rem; color: var(--bs-body-color); text-transform: uppercase;",
            card_header_text
          )
        ),

        # Body (passed through directly)
        bslib::card_body(
          style = "padding: 0.5rem; background: var(--bs-body-bg); overflow-x: hidden;",
          card_body
        ),

        # Footer with button
        bslib::card_footer(
          style = "background: var(--bs-body-bg); border-top: 1px solid #e8ddd4; padding: 0.5rem 1rem; text-align: center;",
          actionButton(
            inputId = ns(card_footer_button_id),
            label = tagList(
              ph("table", weight = "fill", style = "margin-right: 0.45rem;"),
              card_footer_button_text
            ),
            class = "text-uppercase fw-semibold",
            style = "
              background-color: #8A7A8F;
              color: var(--bs-body-bg);
              border: none;
              padding: 0.44rem 1.1rem;
              font-size: 0.7rem;
              letter-spacing: 0.4px;
              border-radius: 6px;"
          )
        )
      )
    }
  )

  # Filter out NULL columns and create layout
  cols <- Filter(Negate(is.null), cols)

  # Create the fieldset with the columns
  fieldset <- tags$fieldset(
    class = "metric-section",
    if (!is.null(fieldset_title) || !is.null(title)) {
      tags$legend(
        class = "metric-legend",
        fieldset_title %||% title
      )
    },
    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "0px",
      !!!cols
    )
  )

  # Add server-side rendering for any plots
  if (inherits(value, "ggplot")) {
    plot_id <- paste0("plot_", gsub("[^A-Za-z0-9]", "_", title), "_")
    plot_ids <- grep(plot_id, names(session$userData), value = TRUE)

    if (length(plot_ids) > 0) {
      # Get the most recent plot ID
      plot_id <- plot_ids[length(plot_ids)]

      # Create an observer to render the plot
      observe({
        output[[plot_id]] <- renderPlot(
          {
            session$userData[[plot_id]]
          },
          height = 120,
          width = "100%"
        )
      })
    }
  }

  # Return the fieldset
  fieldset
}

visualization_section_ui <- function(data,
                                     # Value box parameters
                                     metric_title,
                                     description,
                                     # Card parameters
                                     card_header_text = "READY FOR FINAL CHECK",
                                     preview_plot,
                                     main_plot,
                                     # Namespace function
                                     ns,
                                     # Session object for server-side rendering
                                     session = shiny::getDefaultReactiveDomain(),
                                     # Fieldset title (defaults to title if not provided)
                                     fieldset_title = NULL) {
  icon_color <- rgb(245, 241, 232, maxColorValue = 255, alpha = 125)
  title_lower <- tolower(metric_title)
  if (grepl("indicators", title_lower)) {
    bgcolor <- "#7E8480"
    icon <- ph_i("gauge", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("alignment", title_lower)) {
    bgcolor <- "#A08E6F"
    icon <- ph_i("flower-lotus", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("dynamics", title_lower)) {
    bgcolor <- "#88707E"
    icon <- ph_i("pulse", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("cascade", title_lower)) {
    bgcolor <- "#B49291"
    icon <- ph_i("waveform", weight = "thin", size = "8x", color = icon_color)
  } else {
    # Default fallback
    bgcolor <- "#6c757d"
    icon <- ph_i("circle", weight = "thin", size = "8x", color = icon_color)
  }

  # Custom fieldset titles for each viz_type
  custom_titles <- list(
    indicators = "Project Indicators Plot",
    alignment = "Project Alignment Plot",
    dynamics = "Project Dynamics Plot",
    cascade = "Cascade Effects Plot"
  )

  # Set fieldset_title from metric_title with custom mapping and uppercase
  metric_key <- tolower(gsub(" ", "_", metric_title))
  fieldset_titles <- list(
    indicators = "Project Indicators",
    alignment = "Project Alignment",
    dynamics = "Project Dynamics",
    cascade = "Cascade Effects"
  )
  if (!is.null(fieldset_titles[[metric_key]])) {
    fieldset_title <- toupper(fieldset_titles[[metric_key]])
  } else {
    fieldset_title <- toupper(metric_title)
  }
  value_title <- paste0(fieldset_title, " PLOT")



  cols <- list(
    # LEFT SIDE: Value Box or Placeholder
    bslib::value_box(
      style = "
          padding: 0.25em !important;
          border-radius: 10px 0 0 10px !important;
          border: 1px 0 1px 1px !important;
          border-style: solid !important;
          border-color: #d4c5b9 !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
      title = tags$div(
        class = "text-uppercase fw-normal",
        style = "
            font-size: 1.1rem;
            margin-top: 0.25rem;
            color: #f5f1e8;",
        value_title
      ),
      value = "",
      showcase = icon,
      showcase_layout = "left center",
      description,
      theme = bslib::value_box_theme(bg = bgcolor, fg = "#f5f1e8"),
    ),

    # RIGHT SIDE: Details Card (only show if we have valid data)
    bslib::card(
      style = "
          padding: 0 !important;
          border-radius: 0 8px 8px 0 !important;
          border: 1px 1px 1px 0;
          border-style: solid;
          border-color: #d4c5b9 !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;
          background: #f9f5f0;",

      # Header
      bslib::card_header(
        style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
        tags$div(
          class = "fw-medium text-center",
          style = "font-size: 1rem; color: var(--bs-body-color);",
          card_header_text
        )
      ),

      # Body (passed through directly)
      bslib::card_body(
        style = "padding: 0.5rem; background: var(--bs-body-bg); overflow: hidden; display: flex; align-items: center; justify-content: center;",
        preview_plot
      ),

      # Footer with button
      bslib::card_footer(
        style = "background: #f9f5f0; border-top: 1px solid #e8ddd4; padding: 0.5rem 1rem; display: flex; justify-content: space-evenly; align-items: center;",
        actionButton(
          ns(paste0("view_full_", gsub(" ", "_", tolower(metric_title)))),
          label = tagList(
            ph("corners-out", weight = "regular", style = "margin-right: 0.45rem;"),
            "View Full Plot"
          ),
          class = "text-uppercase fw-semibold",
          style = "
              background-color: #8A7A8F;
              color: var(--bs-body-bg);
              border: none;
              padding: 0.44rem 1.1rem;
              font-size: 0.7rem;
              letter-spacing: 0.4px;
              border-radius: 6px;"
        ),
        downloadButton(
          ns(paste0("download_", gsub(" ", "_", tolower(metric_title)))),
          label = "Download Plot",
          icon = icon("download"),
          class = "btn btn-custom text-uppercase fw-semibold",
          style = "
              background-color: #8A7A8F;
              color: var(--bs-body-bg);
              border: none;
              padding: 0.44rem 1.1rem;
              font-size: 0.7rem;
              letter-spacing: 0.4px;
              border-radius: 6px;
              cursor: pointer;"
        )
      )
    )
  )

  # Filter out NULL columns and create layout
  cols <- Filter(Negate(is.null), cols)

  # Create the fieldset with the columns
  fieldset <- tags$fieldset(
    class = "metric-section",
    tags$legend(
      class = "metric-legend",
      fieldset_title %||% metric_title
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "0px",
      !!!cols
    )
  )

  # Return the fieldset
  fieldset
}

check_section_ui <- function(data,
                             # Value box parameters
                             check_title,
                             value_title,
                             description,
                             # Card parameters
                             card_header_text = "READY FOR REPORT GENERATION",
                             check_list,
                             check_message,
                             # Namespace function
                             ns,
                             # Session object for server-side rendering
                             session = shiny::getDefaultReactiveDomain(),
                             # Fieldset title (defaults to title if not provided)
                             fieldset_title = NULL) {
  icon_color <- rgb(245, 241, 232, maxColorValue = 255, alpha = 125)
  check_title <- toupper(check_title)
  if (grepl("PROJECT INFORMATION", check_title)) {
    bgcolor <- "#8B4B73"
    icon <- ph_i("toolbox", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("DATA LOADING", check_title)) {
    bgcolor <- "#5A7C7C"
    icon <- ph_i("pencil-ruler", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("ANALYSIS RESULTS", check_title)) {
    bgcolor <- "#B8860B"
    icon <- ph_i("calculator", weight = "thin", size = "8x", color = icon_color)
  } else if (grepl("VISUALIZATIONS", check_title)) {
    bgcolor <- "#6B4C93"
    icon <- ph_i("compass-tool", weight = "thin", size = "8x", color = icon_color)
  } else {
    # Default fallback
    bgcolor <- "#6c757d"
    icon <- ph_i("circle", weight = "thin", size = "8x", color = icon_color)
  }

  cols <- list(
    # LEFT SIDE: Value Box or Placeholder
    bslib::value_box(
      style = "
          padding: 0.25em !important;
          border-radius: 10px 0 0 10px !important;
          border: 1px 0 1px 1px !important;
          border-style: solid !important;
          border-color: #d4c5b9 !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
      title = "",
      # title = tags$div(
      #   class = "text-uppercase fw-normal",
      #   style = "
      #       font-size: 1.1rem;
      #       margin-top: 0.25rem;
      #       color: #f5f1e8;",
      #   check_title
      # ),
      value = tags$div(
        style = "font-size: 0.95em; color: #f5f1e8; margin-top: 0.25em;",
        description
      ),
      showcase = icon,
      # showcase_layout = "bottom",
      theme = bslib::value_box_theme(bg = bgcolor, fg = "#f5f1e8"),
    ),

    # RIGHT SIDE: Details Card (only show if we have valid data)
    bslib::card(
      style = "
          padding: 0 !important;
          border-radius: 0 8px 8px 0 !important;
          border: 1px 1px 1px 0;
          border-style: solid;
          border-color: #d4c5b9 !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;
          background: #f9f5f0;",

      # Header
      bslib::card_header(
        style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
        tags$div(
          class = "fw-medium text-center d-flex align-items-center justify-content-between",
          style = "font-size: 1rem; color: var(--bs-body-color);",
          card_header_text,
          check_message
        )
      ),

      # Body (passed through directly)
      bslib::card_body(
        style = "padding: 0.5rem; background: var(--bs-body-bg); overflow: hidden; display: flex; align-items: center; justify-content: center;",
        check_list
      )
    )
  )

  # Filter out NULL columns and create layout
  cols <- Filter(Negate(is.null), cols)

  # Create the fieldset with the columns
  fieldset <- tags$fieldset(
    class = "metric-section",
    tags$legend(
      class = "metric-legend",
      fieldset_title %||% check_title
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "0px",
      !!!cols
    )
  )

  # Return the fieldset
  fieldset
}

generate_section_ui <- function(data,
                                # Value box parameters
                                generate_title,
                                # Card parameters
                                card_header_text = "READY FOR DOWNLOAD",
                                generate_list,
                                # Namespace function
                                ns,
                                # Session object for server-side rendering
                                session = shiny::getDefaultReactiveDomain(),
                                # Fieldset title (defaults to title if not provided)
                                fieldset_title = NULL) {
  icon_color <- rgb(245, 241, 232, maxColorValue = 255, alpha = 125)
  generate_title <- toupper(generate_title)
  if (grepl("COLLECT", generate_title)) {
    bgcolor <- "#7B4397"
    icon <- ph_i("stack", weight = "thin", size = "8x", color = icon_color)
    generate_id <- "collect"
  } else if (grepl("COMPILE", generate_title)) {
    bgcolor <- "#4A6741"
    icon <- ph_i("code", weight = "thin", size = "8x", color = icon_color)
    generate_id <- "compile"
  } else if (grepl("STITCH", generate_title)) {
    bgcolor <- "#8B6914"
    icon <- ph_i("yarn", weight = "thin", size = "8x", color = icon_color)
    generate_id <- "stitch"
  } else if (grepl("PACKAGE", generate_title)) {
    bgcolor <- "#2D4A6B"
    icon <- ph_i("package", weight = "thin", size = "8x", color = icon_color)
    generate_id <- "package"
  } else {
    # Default fallback
    bgcolor <- "#6c757d"
    icon <- ph_i("circle", weight = "thin", size = "8x", color = icon_color)
  }

  cols <- list(
    # LEFT SIDE: Value Box or Placeholder
    bslib::value_box(
      style = "
          padding: 0.25em !important;
          border-radius: 10px 0 0 10px !important;
          border: 1px 0 1px 1px !important;
          border-style: solid !important;
          border-color: #d4c5b9 !important;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.06) !important;",
      title = "",
      value = "",
      showcase = icon,
      theme = bslib::value_box_theme(bg = bgcolor, fg = "#f5f1e8"),
    ),

    # RIGHT SIDE: Details Card (only show if we have valid data)
    bslib::card(
      style = "
          padding: 0 !important;
          border-radius: 0 8px 8px 0 !important;
          border: 1px 1px 1px 0;
          border-style: solid;
          border-color: #d4c5b9 !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;
          background: #f9f5f0;",

      # Header
      bslib::card_header(
        style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
        tags$div(
          class = "fw-medium",
          style = "font-size: 1rem; color: var(--bs-body-color);",
          card_header_text
        )
      ),

      # Body (passed through directly)
      bslib::card_body(
        div(
          id = paste0(generate_id, "_list_container"),
          create_generate_list_taglist(generate_list, ns = ns, session = session)
        )
      )
    )
  )

  # Filter out NULL columns and create layout
  cols <- Filter(Negate(is.null), cols)

  # Create the fieldset with the columns
  fieldset <- tags$fieldset(
    class = "metric-section",
    tags$legend(
      class = "metric-legend",
      fieldset_title %||% generate_title
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      gap = "0px",
      !!!cols
    )
  )

  # Return the fieldset
  fieldset
}

create_info_card <- function(ns, title, icon, status = "test", content, style = NULL) {
  # Build the style string safely
  base_style <- "width: 100% !important;
            border-radius: 10px !important;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    border: 1px solid #8e8380 !important;"

  final_style <- if (!is.null(style)) {
    paste0(base_style, " ", style)
  } else {
    base_style
  }

  bslib::card(
    style = final_style,
    bslib::card_header(
      style = "color: #F2ECD7; background: linear-gradient(135deg, rgba(255, 255, 255, 0.1) 0%, rgba(255, 255, 255, 0.2) 100%); text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);",
      fluidRow(
        column(
          width = 8,
          class = "infocard",
          h1(
            title,
            phosphoricons::ph(icon, weight = "bold", class = "sidebar-icon")
          )
        ),
        column(
          width = 4,
          div(
            class = "status-line",
            column(
              width = 1,
              div(
                style = "color: #3F5E78;",
                id = ns(paste0(status, "_status_icon")),
                ph("check-circle", weight = "bold", class = "sidebar-icon")
              )
            ),
            column(
              width = 11,
              div(
                span(
                  style = "color: #3F5E78;",
                  id = ns(paste0(status, "_status_message")),
                  "Ready"
                )
              )
            )
          )
        )
      )
    ),
    bslib::card_body(
      class = "infocard",
      style = "padding: 1.5em 2.5em 0em 2.5em !important;
      border-radius: 0px 0px 10px 10px !important;
      box-shadow: inset 0 4px 10px #8e838044 !important;",
      content
    )
  )
}


#' Create Generate List TagList
#'
#' Creates a tagList containing formatted generate list items with sophisticated layout.
#' Handles both simple character vectors and complex objects with status/label/action properties.
#'
#' @param generate_list List of items to be formatted. Can be character vector or list of objects with status/label/action
#' @param ns Namespace function (optional, for action buttons)
#' @param session Shiny session (optional, for action buttons)
#'
#' @return A shiny.tag.list object containing the formatted list display
create_generate_list_taglist <- function(generate_list, ns = NULL, session = NULL) {
  if (is.null(generate_list) || length(generate_list) == 0) {
    return(tagList())
  }

  # Helper functions for rendering
  clean_label <- function(label) gsub("\\(.*\\)$", "", label)

  status_icon_tag <- function(status) {
    switch(status,
      "present" = phosphoricons::ph_i("check-circle", weight = "fill", class = "text-success"),
      "complete" = phosphoricons::ph_i("check-circle", weight = "fill", class = "text-success"),
      "ready" = phosphoricons::ph_i("check-circle", weight = "fill", class = "text-success"),
      "processing" = phosphoricons::ph_i("question", weight = "regular", class = "text-info"),
      "missing_required" = phosphoricons::ph_i("x-circle", weight = "fill", class = "text-danger"),
      "missing_optional" = phosphoricons::ph_i("minus-circle", weight = "fill", class = "text-warning"),
      "warning" = phosphoricons::ph_i("warning", weight = "fill", class = "text-warning"),
      "pending" = phosphoricons::ph_i("clock", weight = "fill", class = "text-info"),
      # Default for simple character items - start as processing
      phosphoricons::ph_i("question", weight = "regular", class = "text-info")
    )
  }

  # Helper function to create action buttons
  action_tag <- function(item) {
    if (is.null(item$action) || is.null(ns) || is.null(session)) {
      return(NULL)
    }

    # Create the action button
    actionButton(
      inputId = ns(paste0("action_", gsub(" ", "_", tolower(item$label)))),
      label = item$action$label,
      class = "btn-sm",
      onclick = item$action$onclick
    )
  }

  # Convert simple character vector to object format if needed
  formatted_items <- if (is.character(generate_list)) {
    lapply(generate_list, function(item) {
      list(
        label = item,
        status = "processing",
        required = NULL,
        action = NULL
      )
    })
  } else {
    generate_list
  }

  # Create the layout based on number of items
  n_items <- length(formatted_items)
  if (n_items == 1) {
    # Center single item
    check_list <- fluidRow(
      column(
        width = 6, offset = 3,
        tags$div(
          class = "section-row",
          style = "padding: 0.4em 0.2em; display: flex; align-items: center; justify-content: center;",
          tags$div(
            style = "display: flex; align-items: flex-start; gap: 0.5em; width: 100%;",
            tags$div(
              status_icon_tag(formatted_items[[1]]$status),
              style = "font-size: 1.1em; flex-shrink: 0; margin-top: 0.05em;"
            ),
            tags$div(
              style = "flex-grow: 1; min-width: 0;",
              tags$div(
                style = "font-weight: 600; text-transform: uppercase; margin-bottom: 0.10em; line-height: 1.1;",
                clean_label(formatted_items[[1]]$label)
              ),
              tags$div(
                style = "font-family: var(--bs-code-font); font-size: 0.7em; font-weight: 300; color: #666; line-height: 1.1; text-transform: lowercase;",
                if (grepl("^missing", formatted_items[[1]]$status)) {
                  if (!is.null(formatted_items[[1]]$required) && formatted_items[[1]]$required) "required" else "optional"
                }
              )
            ),
            tags$div(
              style = "flex-shrink: 0; margin-left: 0.4em;",
              action_tag(formatted_items[[1]])
            )
          )
        )
      )
    )
  } else {
    # Two columns for multiple items
    check_list <- fluidRow(
      lapply(seq_len(n_items), function(i) {
        item <- formatted_items[[i]]
        column(
          width = 6,
          tags$div(
            class = "section-row",
            style = paste(
              "padding: 0.4em 0.2em; display: flex; align-items: center;",
              if (i > 2) "border-top: 1px solid rgba(0,0,0,0.08);" else ""
            ),
            tags$div(
              style = "display: flex; align-items: flex-start; gap: 0.5em; width: 100%;",
              tags$div(
                status_icon_tag(item$status),
                style = "font-size: 1.1em; flex-shrink: 0; margin-top: 0.05em;"
              ),
              tags$div(
                style = "flex-grow: 1; min-width: 0;",
                tags$div(
                  style = "font-weight: 600; text-transform: uppercase; margin-bottom: 0.10em; line-height: 1.1;",
                  clean_label(item$label)
                ),
                tags$div(
                  style = "font-family: var(--bs-code-font); font-size: 0.7em; font-weight: 300; color: #666; line-height: 1.1; text-transform: lowercase;",
                  if (grepl("^missing", item$status)) {
                    if (!is.null(item$required) && item$required) "required" else "optional"
                  }
                )
              ),
              tags$div(
                style = "flex-shrink: 0; margin-left: 0.4em;",
                action_tag(item)
              )
            )
          )
        )
      })
    )
  }

  return(check_list)
}

create_flat_info_card <- function(ns, title, icon, status = "test", content, style = NULL) {
  # Build the style string safely
  base_style <- "width: 100% !important;
            height: 100% !important;"

  final_style <- if (!is.null(style)) {
    paste0(base_style, " ", style)
  } else {
    base_style
  }

  bslib::card(
    style = final_style,
    bslib::card_header(
      h1(
        title,
        phosphoricons::ph(icon, weight = "bold", class = "sidebar-icon")
      )
    ),
    bslib::card_body(
      content
    )
  )
}

create_pill_info_card <- function(ns, title, icon, tabs, style = NULL, header_style = NULL, ...) {
  # Build the style string safely
  base_style <- "width: 100% !important;
            border-radius: 10px !important;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2) !important;
    border: 1px solid #8e8380 !important;"

  final_style <- if (!is.null(style)) {
    paste0(base_style, " ", style)
  } else {
    base_style
  }

  header_base_style <- "color: #F2ECD7; background: linear-gradient(135deg, rgba(255, 255, 255, 0.1) 0%, rgba(255, 255, 255, 0.2) 100%); text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);"
  final_header_style <- if (!is.null(header_style)) {
    paste0(header_base_style, " ", header_style)
  } else {
    header_base_style
  }

  # Build the tab panels
  tab_panels <- lapply(tabs, function(tab) {
    if (!is.null(tab$type) && tab$type == "actions") {
      bslib::nav_panel(
        title = tab$title,
        tagList(
          if (!is.null(tab$description)) p(tab$description),
          # Use the same button + progress bar layout as the standard actions card
          if (length(tab$action_buttons) > 0) {
            mapply(
              function(btn, prog_bar, index) {
                if (!is.null(btn)) {
                  # Create container for button and progress bar
                  fluidRow(
                    class = "action-row",
                    style = "min-height: 60px; position: relative; margin-bottom: 10px;",
                    column(
                      class = "d-flex flex-column justify-content-center align-items-center",
                      width = 12,
                      # Action button
                      div(
                        style = "width: 100%; margin-bottom: 5px; display: flex; justify-content: center;",
                        btn
                      ),
                      # Progress bar container (initially hidden)
                      if (!is.null(prog_bar)) {
                        div(
                          id = if (!is.null(tab$ns)) tab$ns(paste0("progress_container_", index)) else paste0("progress_container_", index),
                          class = "progress-bar-hidden",
                          style = "width: 100%; margin-top: 5px;",
                          div(
                            class = "progress custom-progressbar-style",
                            prog_bar
                          )
                        )
                      }
                    )
                  )
                }
              }, tab$action_buttons,
              if (!is.null(tab$progress_bars)) tab$progress_bars else vector("list", length(tab$action_buttons)),
              seq_along(tab$action_buttons),
              SIMPLIFY = FALSE
            )
          }
        )
      )
    } else {
      bslib::nav_panel(
        title = tab$title,
        tab$content
      )
    }
  })

  # Use a standard info card with navs_pill inside the card body for full theming control
  bslib::card(
    style = final_style,
    bslib::card_header(
      style = final_header_style,
      class = "infocard",
      h1(title),
      phosphoricons::ph(icon, weight = "bold")
    ),
    bslib::card_body(
      bslib::navset_card_pill(
        id = paste0(ns("pilltabs")),
        !!!tab_panels,
        ...
      )
    )
  )
}



create_status_line <- function(ns) {
  #' Create a standardized status line component
  #'
  #' This function creates a consistent status line component that looks and operates
  #' the same across all modules in the application.
  #'
  #' @param ns The namespace function for the current module
  #'
  #' @return A div element containing the standardized status line component
  #'
  #' @examples
  #' # Basic usage - will create a status line with id "status_line"
  #' create_status_line(ns)
  #'
  #' # In your server function, create the corresponding output:
  #' # output$status_line <- renderUI({ ... })

  div(
    class = "status-line",
    style = "width: 100%",
    column(
      width = 3,
      div(
        "Status",
        phosphoricons::ph("terminal", weight = "bold"),
        style = "font-weight = 'bold'; color: var(--bs-info) !important;",
        class = "d-flex justify-content-evenly align-items-center"
      )
    ),
    column(
      width = 9,
      style = "padding-left: 0.5em; padding-right: 2em;",
      div(
        uiOutput(ns("status_line")),
        style = "font-size: 1em; text-align: left; position: relative; min-width: 300px;",
        class = "d-flex justify-content-start align-items-center gap-3"
      )
    )
  )
}

create_status_output <- function(ns, status_state, session = NULL) {
  #' Create a standardized status output renderUI function
  #'
  #' This function creates a consistent status output that can be used across
  #' different modules to display status information with consistent styling.
  #'
  #' @param ns The namespace function for the current module
  #' @param status_state A reactive expression or value containing the current status
  #' @param session The session object (optional, for toast animations)
  #'
  #' @return A renderUI function for the status output
  #'
  #' @examples
  #' # In your server function:
  #' output$status_line <- create_status_output(ns, current_status_state, session)

  function() {
    status <- if (is.reactive(status_state)) status_state() else status_state

    if (status == "no_details" || status == "not_started") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #D3D3D3; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("empty", weight = "bold"),
        tags$span("Not Started")
      )
    } else if (status == "saved" || status == "complete") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #3B6B35; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("floppy-disk", weight = "bold"),
        tags$span("Complete")
      )
    } else if (status == "editing" || status == "in_progress") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #3F5E78; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("keyboard", weight = "bold"),
        tags$span("In Progress")
      )
    } else if (status == "error") {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #990000; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("x-circle", weight = "bold"),
        tags$span("Error")
      )
    } else {
      tags$div(
        id = ns("status-toast"),
        class = "toast-message",
        style = "color: #D3D3D3; display: flex; align-items: center; gap: 8px;",
        phosphoricons::ph("question", weight = "bold"),
        tags$span("Unknown Status")
      )
    }
  }
}

# Usage Examples for create_status_line and create_status_output:
#
# 1. In your UI function, add the status line:
#    create_status_line(ns)
#
# 2. In your server function, create the status output:
#    output$status_line <- create_status_output(ns, current_status_state, session)
#
# 3. The status system will automatically handle:
#    - "no_details" / "not_started" <U+2192> Gray "Not Started"
#    - "saved" / "complete" <U+2192> Green "Complete"
#    - "editing" / "in_progress" <U+2192> Blue "In Progress"
#    - "error" <U+2192> Red "Error"
#    - Unknown status <U+2192> Gray "Unknown Status"
#
# 4. Benefits:
#    - Consistent styling across all modules
#    - Standardized status messages
#    - Automatic toast animations
#    - Easy to maintain and update
#
# 5. Complete Example for a new module:
#    # In UI function:
#    mod_example_ui <- function(id) {
#      ns <- NS(id)
#      tagList(
#        fluidRow(
#          column(width = 8, h1("Example Module")),
#          column(width = 4, create_status_line(ns))
#        )
#      )
#    }
#
#    # In server function:
#    mod_example_server <- function(id) {
#      moduleServer(id, function(input, output, session) {
#        ns <- session$ns
#
#        # Track status state
#        current_status <- reactiveVal("not_started")
#
#        # Create status output
#        output$status_line <- create_status_output(ns, current_status, session)
#
#        # Update status based on actions
#        observeEvent(input$some_action, {
#          current_status("in_progress")
#          # ... do work ...
#          current_status("complete")
#        })
#      })
#    }

# End of file
