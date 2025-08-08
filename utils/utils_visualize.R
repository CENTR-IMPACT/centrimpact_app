# Utility functions for visualization plots
#
# Required packages for Google Fonts:
# - sysfonts (for font_add_google)
# - showtext (for showtext_auto)
# These should be loaded in the main application

# Text scaling constants for different plot types
REPORT_TEXT_SCALE_FACTOR <- 300 / 140 # 2.14 multiplier for 300 DPI vs 140 DPI

# Default brand fonts (fallbacks)
DEFAULT_SANS_FONT <- "Source Sans 3"
DEFAULT_MONO_FONT <- "Fira Code"

#' Scale text size for report plots
#' @param base_size Base text size for main plot
#' @return Scaled text size for report plot
scale_text_for_report <- function(base_size) {
  base_size * REPORT_TEXT_SCALE_FACTOR
}

#' Generate standardized title for plots
#' @param project_title The project title, or NULL for default
#' @param report_date The report date, or NULL for today's date
#' @return A formatted title string
get_plot_title <- function(project_title = NULL, report_date = NULL) {
  if (is.null(project_title)) {
    project_title <- "Project"
  }

  if (is.null(report_date)) {
    report_date <- format(Sys.Date(), "%Y-%m-%d")
  } else if (inherits(report_date, "Date")) {
    report_date <- format(report_date, "%Y-%m-%d")
  }

  return(paste(project_title, report_date))
}

load_brand_fonts <- function(brand_config = NULL) {
  tryCatch(
    {
      # Get font names from brand config or use defaults
      sans_font <- get_font_name_from_config(brand_config, "sans_font", DEFAULT_SANS_FONT)
      mono_font <- get_font_name_from_config(brand_config, "mono_font", DEFAULT_MONO_FONT)

      # Debug logging
      cat("Loading brand fonts - Sans:", sans_font, "Mono:", mono_font, "\n")

      # Only load if not already loaded
      if (!"brand_sans" %in% sysfonts::font_families()) {
        sysfonts::font_add_google(sans_font, "brand_sans")
        cat("<U+2713> Loaded brand sans font:", sans_font, "\n")
      } else {
        cat("<U+2713> Brand sans font already loaded:", sans_font, "\n")
      }
      if (!"brand_mono" %in% sysfonts::font_families()) {
        sysfonts::font_add_google(mono_font, "brand_mono")
        cat("<U+2713> Loaded brand mono font:", mono_font, "\n")
      } else {
        cat("<U+2713> Brand mono font already loaded:", mono_font, "\n")
      }
      showtext::showtext_auto()
    },
    error = function(e) {
      cat("Warning: Could not load brand fonts:", conditionMessage(e), "\n")
      cat("Falling back to system fonts\n")
    }
  )
}

#' Helper function to extract font name from brand config
#' @param brand_config Brand configuration list
#' @param font_type Type of font ("sans_font" or "mono_font")
#' @param default_font Default font name to use if not found
#' @return Font name string
get_font_name_from_config <- function(brand_config, font_type, default_font) {
  tryCatch(
    {
      # Navigate through the brand config structure
      if (!is.null(brand_config) &&
        !is.null(brand_config$default) &&
        !is.null(brand_config$default$typography) &&
        !is.null(brand_config$default$typography$fonts)) {
        fonts <- brand_config$default$typography$fonts

        # Find the font with the matching family type
        for (font in fonts) {
          if (!is.null(font$family) && font$family == font_type &&
            !is.null(font$name) && !is.null(font$source) &&
            font$source == "google") {
            return(font$name)
          }
        }
      }

      # Return default if not found
      return(default_font)
    },
    error = function(e) {
      return(default_font)
    }
  )
}

#' Get brand font family name for sans font
#' @return Font family name for sans font
get_brand_sans_font <- function() {
  # Check if brand font is available, fallback to system sans
  tryCatch(
    {
      if ("brand_sans" %in% sysfonts::font_families()) {
        return("brand_sans")
      } else {
        return("sans")
      }
    },
    error = function(e) {
      return("sans")
    }
  )
}

#' Get brand font family name for monospace font
#' @return Font family name for monospace font
get_brand_mono_font <- function() {
  # Check if brand font is available, fallback to system mono
  tryCatch(
    {
      if ("brand_mono" %in% sysfonts::font_families()) {
        return("brand_mono")
      } else {
        return("mono")
      }
    },
    error = function(e) {
      return("mono")
    }
  )
}

#' Create indicators plot
#' @param indicators_data Data frame of indicators
#' @param color_palette Vector of colors for the indicators
#' @param project_data Project data containing brand configuration (optional)
#' @return List with preview, main, and report ggplot objects
create_indicators_plots <- function(indicators_data, color_palette, project_data = NULL) {
  req(indicators_data)

  # Load brand fonts with project configuration
  brand_config <- NULL
  cat("DEBUG[indicators]: Checking project_data structure...\n")
  if (!is.null(project_data)) {
    cat("DEBUG[indicators]: project_data is not NULL\n")
    cat("DEBUG[indicators]: project_data class:", class(project_data), "\n")
    if (is.reactive(project_data)) {
      cat("DEBUG[indicators]: project_data is reactive, getting value...\n")
      pd <- project_data()
      cat("DEBUG[indicators]: reactive project_data brand_config:", !is.null(pd$brand_config), "\n")
      if (!is.null(pd$brand_config)) {
        brand_config <- pd$brand_config
        cat("Found brand configuration in reactive project_data for indicators plots\n")
      }
    } else {
      cat("DEBUG[indicators]: project_data is not reactive\n")
      cat("DEBUG[indicators]: direct project_data brand_config:", !is.null(project_data$brand_config), "\n")
      if (!is.null(project_data$brand_config)) {
        brand_config <- project_data$brand_config
        cat("Found brand configuration in project_data for indicators plots\n")
      }
    }
  } else {
    cat("DEBUG[indicators]: project_data is NULL\n")
  }

  if (is.null(brand_config)) {
    cat("No brand configuration found, using defaults for indicators plots\n")
  }
  load_brand_fonts(brand_config)

  # Handle empty or NULL data
  if (is.null(indicators_data) || nrow(indicators_data) == 0) {
    stop("No indicator data provided")
  }

  # Convert to data frame if it's not already
  if (!is.data.frame(indicators_data)) {
    indicators_data <- as.data.frame(indicators_data, stringsAsFactors = FALSE)
  }

  # Find the first numeric column to use as values
  numeric_cols <- sapply(indicators_data, is.numeric)
  value_col <- names(which(numeric_cols)[1])

  if (is.na(value_col)) {
    stop("No numeric column found in indicators data")
  }

  # Use row names or first non-numeric column as indicator names
  non_numeric_cols <- !sapply(indicators_data, is.numeric)
  if (any(non_numeric_cols)) {
    indicator_col <- names(which(non_numeric_cols)[1])
    plot_data <- data.frame(
      indicator = as.character(indicators_data[[indicator_col]]),
      value = as.numeric(indicators_data[[value_col]]),
      stringsAsFactors = FALSE
    )
  } else {
    plot_data <- data.frame(
      indicator = rownames(indicators_data),
      value = as.numeric(indicators_data[[value_col]]),
      stringsAsFactors = FALSE
    )
  }

  # Create base plot
  base_plot <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = indicator,
      r = value,
      fill = indicator
    )
  ) +
    centrimpactvis::geom_indicators(
      show.legend = FALSE,
      show_reference_circles = FALSE,
      show_reference_lines = TRUE,
      scale_factor = 30
    ) +
    ggplot2::scale_fill_manual(values = color_palette)

  # Create preview version
  preview_plot <- base_plot +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      )
    )

  # Create main version (140 DPI, 1920x1080 for 16:9 ratio)
  main_plot <- base_plot +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Project Indicators"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 14,
        family = get_brand_mono_font()
      ),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      )
    )


  # Create report version (300 DPI, 12in x 6.75in)
  report_plot <- base_plot +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Project Indicators"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = scale_text_for_report(14),
        family = get_brand_mono_font()
      ),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = scale_text_for_report(16),
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = scale_text_for_report(14),
        family = get_brand_sans_font()
      )
    )


  # Add metadata for rendering
  attr(main_plot, "dpi") <- 140
  attr(main_plot, "width") <- 1920 / 140 # pixels to inches
  attr(main_plot, "height") <- 1080 / 140
  attr(main_plot, "plot_type") <- "main"

  attr(report_plot, "dpi") <- 300
  attr(report_plot, "width") <- 12
  attr(report_plot, "height") <- 6.75
  attr(report_plot, "plot_type") <- "report"

  return(list(
    preview = preview_plot,
    main = main_plot,
    report = report_plot
  ))
}

#' Create alignment plots
#' @param analysis_results Alignment analysis results
#' @param color_palette Vector of colors for the alignment plot
#' @param project_data Project data containing brand configuration (optional)
#' @return List with preview and main ggplot objects
create_alignment_plots <- function(analysis_results, color_palette, project_data = NULL) {
  # Load brand fonts with project configuration
  brand_config <- NULL
  cat("DEBUG[alignment]: Checking project_data structure...\n")
  if (!is.null(project_data)) {
    cat("DEBUG[alignment]: project_data is not NULL\n")
    cat("DEBUG[alignment]: project_data class:", class(project_data), "\n")
    if (is.reactive(project_data)) {
      cat("DEBUG[alignment]: project_data is reactive, getting value...\n")
      pd <- project_data()
      cat("DEBUG[alignment]: reactive project_data brand_config:", !is.null(pd$brand_config), "\n")
      if (!is.null(pd$brand_config)) {
        brand_config <- pd$brand_config
        cat("Found brand configuration in reactive project_data for alignment plots\n")
      }
    } else {
      cat("DEBUG[alignment]: project_data is not reactive\n")
      cat("DEBUG[alignment]: direct project_data brand_config:", !is.null(project_data$brand_config), "\n")
      if (!is.null(project_data$brand_config)) {
        brand_config <- project_data$brand_config
        cat("Found brand configuration in project_data for alignment plots\n")
      }
    }
  } else {
    cat("DEBUG[alignment]: project_data is NULL\n")
  }

  if (is.null(brand_config)) {
    cat("No brand configuration found, using defaults for alignment plots\n")
  }
  load_brand_fonts(brand_config)

  # Handle different input structures
  if (is.data.frame(analysis_results)) {
    # Direct data frame input
    alignment_data <- analysis_results
  } else if (is.list(analysis_results) && !is.null(analysis_results$alignment) && !is.null(analysis_results$alignment$table)) {
    # Nested structure
    alignment_data <- as.data.frame(analysis_results$alignment$table)
  } else if (is.list(analysis_results) && !is.null(analysis_results$table)) {
    # Alternative structure
    alignment_data <- as.data.frame(analysis_results$table)
  } else {
    stop("No alignment analysis results available")
  }

  # Pivot to long format for plotting
  median_frame_long <- tidyr::pivot_longer(
    alignment_data,
    cols = c("partner", "researcher", "overall"),
    names_to = "role",
    values_to = "value"
  )

  # Ensure value column is numeric and handle NAs
  median_frame_long$value <- as.numeric(median_frame_long$value)
  median_frame_long <- median_frame_long[!is.na(median_frame_long$value), ]

  # Set factor levels for proper ordering
  median_frame_long$role <- factor(median_frame_long$role,
    levels = c("partner", "researcher", "overall")
  )

  first_group <- "partner"
  last_group <- "overall"

  # Create labels for left and right sides
  left_labels <- median_frame_long[median_frame_long$role == first_group, ]
  right_labels <- median_frame_long[median_frame_long$role == last_group, ]

  # Create base plot
  base_plot <- ggplot2::ggplot(
    data = median_frame_long,
    ggplot2::aes(x = role, y = value, group = alignment, color = alignment)
  ) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.border = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  # Main plot: add colored lines and points
  # Create main plot (140 DPI, 1920x1080 for 16:9 ratio)
  main_plot <- base_plot +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_palette) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Stakeholder Alignment",
      caption = paste0("Sa = ", ifelse(!is.null(project_data$analyzed_data) && !is.null(project_data$analyzed_data$alignment_score),
        round(as.numeric(project_data$analyzed_data$alignment_score), 2),
        "N/A"
      ))
    )

  preview_plot <- base_plot +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_palette) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  main_plot <- main_plot +
    ggrepel::geom_text_repel(
      data = left_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = get_brand_sans_font(),
      fontface = "italic",
      size = 2.5,
      nudge_x = -0.5,
      direction = "y",
      hjust = 1,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    ggrepel::geom_text_repel(
      data = right_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = get_brand_sans_font(),
      fontface = "italic",
      size = 2.5,
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    ggplot2::geom_text(
      data = median_frame_long,
      ggplot2::aes(label = round(value, 2)),
      size = 2,
      nudge_y = 0.05,
      nudge_x = 0.03,
      family = get_brand_mono_font()
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(30, 30, 30, 30),
      text = ggplot2::element_text(family = get_brand_sans_font()),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        family = get_brand_sans_font(),
        hjust = 1
      )
    )

  # Create report plot (300 DPI, 12in x 6.75in)
  report_plot <- base_plot +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_palette) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Stakeholder Alignment",
      caption = paste0("Sa = ", ifelse(!is.null(project_data$analyzed_data) && !is.null(project_data$analyzed_data$alignment_score),
        round(as.numeric(project_data$analyzed_data$alignment_score), 2),
        "N/A"
      ))
    ) +
    ggrepel::geom_text_repel(
      data = left_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = get_brand_sans_font(),
      fontface = "italic",
      size = scale_text_for_report(3.75),
      nudge_x = -0.5,
      direction = "y",
      hjust = 1,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    ggrepel::geom_text_repel(
      data = right_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = get_brand_sans_font(),
      fontface = "italic",
      size = scale_text_for_report(3.75),
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    ggplot2::geom_text(
      data = median_frame_long,
      ggplot2::aes(label = round(value, 2)),
      size = scale_text_for_report(2),
      nudge_y = 0.05,
      nudge_x = 0.03,
      family = get_brand_mono_font()
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::margin(30, 30, 30, 30),
      text = ggplot2::element_text(family = get_brand_sans_font()),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = scale_text_for_report(16),
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = scale_text_for_report(14),
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = scale_text_for_report(12),
        family = get_brand_sans_font(),
        hjust = 1
      )
    )

  # Add metadata for rendering
  attr(main_plot, "dpi") <- 140
  attr(main_plot, "width") <- 1920 / 140 # pixels to inches
  attr(main_plot, "height") <- 1080 / 140
  attr(main_plot, "plot_type") <- "main"

  attr(report_plot, "dpi") <- 300
  attr(report_plot, "width") <- 12
  attr(report_plot, "height") <- 6.75
  attr(report_plot, "plot_type") <- "report"

  return(list(preview = preview_plot, main = main_plot, report = report_plot))
}

#' Create dynamics plots
#' @param dynamics_data List containing dimension_scores and domain_scores
#' @param color_palette Vector of colors for the plot
#' @param project_data Project data containing brand configuration (optional)
#' @return List with preview, main, and report ggplot objects
create_dynamics_plots <- function(dynamics_data, color_palette, project_data = NULL) {
  # Debug: Print structure of input data
  message("\n===== DEBUG: create_dynamics_plots =====")
  message("Structure of dynamics_data:")
  str(dynamics_data)

  # Debug: Check if required components exist
  if (is.null(dynamics_data)) {
    message("ERROR: dynamics_data is NULL")
    return(NULL)
  }

  if (!is.list(dynamics_data)) {
    message("ERROR: dynamics_data is not a list")
    return(NULL)
  }

  message("\n===== DEBUG: dynamics_data components =====")
  message("Names in dynamics_data: ", paste(names(dynamics_data), collapse = ", "))

  # Check for required components
  if (is.null(dynamics_data$dimension_scores)) {
    message("WARNING: dimension_scores is missing from dynamics_data")
  } else {
    message("\n===== DEBUG: dimension_scores structure =====")
    str(dynamics_data$dimension_scores)
    message(
      "Column names in dimension_scores: ",
      if (!is.null(dynamics_data$dimension_scores)) {
        paste(names(dynamics_data$dimension_scores), collapse = ", ")
      } else {
        "NULL"
      }
    )
  }

  if (is.null(dynamics_data$domain_scores)) {
    message("WARNING: domain_scores is missing from dynamics_data")
  } else {
    message("\n===== DEBUG: domain_scores structure =====")
    str(dynamics_data$domain_scores)
    message(
      "Column names in domain_scores: ",
      if (!is.null(dynamics_data$domain_scores)) {
        paste(names(dynamics_data$domain_scores), collapse = ", ")
      } else {
        "NULL"
      }
    )
  }

  # Load brand fonts with project configuration
  brand_config <- NULL
  cat("DEBUG[dynamics]: Checking project_data structure...\n")
  if (!is.null(project_data)) {
    cat("DEBUG[dynamics]: project_data is not NULL\n")
    cat("DEBUG[dynamics]: project_data class:", class(project_data), "\n")
    if (is.reactive(project_data)) {
      cat("DEBUG[dynamics]: project_data is reactive, getting value...\n")
      pd <- project_data()
      cat("DEBUG[dynamics]: reactive project_data brand_config:", !is.null(pd$brand_config), "\n")
      if (!is.null(pd$brand_config)) {
        brand_config <- pd$brand_config
        message("Found brand configuration in reactive project_data for dynamics plots")
      }
    } else {
      cat("DEBUG[dynamics]: project_data is not reactive\n")
      cat("DEBUG[dynamics]: direct project_data brand_config:", !is.null(project_data$brand_config), "\n")
      if (!is.null(project_data$brand_config)) {
        brand_config <- project_data$brand_config
        message("Found brand configuration in project_data for dynamics plots")
      }
    }
  } else {
    cat("DEBUG[dynamics]: project_data is NULL\n")
  }

  if (is.null(brand_config)) {
    message("No brand configuration found, using defaults for dynamics plots")
  }
  load_brand_fonts(brand_config)

  # Validate input data
  if (is.null(dynamics_data) || !is.list(dynamics_data) ||
    is.null(dynamics_data$dimension_scores) || is.null(dynamics_data$domain_scores)) {
    stop("Invalid dynamics data provided")
  }

  dimension_scores <- dynamics_data$dimension_scores
  domain_scores <- dynamics_data$domain_scores

  if (!is.data.frame(dimension_scores) || !is.data.frame(domain_scores) ||
    nrow(dimension_scores) == 0 || nrow(domain_scores) == 0) {
    stop("Dynamics data frames are empty or invalid")
  }

  # Debug: Print structure of dimension_scores
  message("Structure of dimension_scores:")
  str(dimension_scores)
  message("Column names in dimension_scores:", paste(names(dimension_scores), collapse = ", "))

  # Check if the required columns exist
  if (!"dimension_value" %in% names(dimension_scores)) {
    stop(
      "Required column 'dimension_value' not found in dimension_scores. Available columns: ",
      paste(names(dimension_scores), collapse = ", ")
    )
  }
  if (!"domain_score" %in% names(domain_scores)) {
    stop(
      "Required column 'domain_score' not found in domain_scores. Available columns: ",
      paste(names(domain_scores), collapse = ", ")
    )
  }

  # Scale the scores (commented out)
  # dimension_scores$dimension_value <- dimension_scores$dimension_value * 2
  # domain_scores$domain_score <- domain_scores$domain_score * 2

  # Filter dimension_scores to include only the highest salience for each dimension
  dimension_scores <- do.call(rbind, lapply(split(dimension_scores, list(dimension_scores$domain, dimension_scores$dimension)), function(x) {
    x[x$salience == max(x$salience), , drop = FALSE]
  }))

  # Use custom solid fill colors for petals
  custom_fills <- c(
    "#8e8380", # Light brown
    "#8ba086", # Light green
    "#8c99a7", # Light blue
    "#b26f6b", # Light red
    "#bd908b" # Light rust
  )
  domain_fill_colors <- setNames(custom_fills[1:length(unique(domain_scores$domain))], unique(domain_scores$domain))
  # Use original color_palette for stamen (dimension points/labels)
  domain_stamen_colors <- setNames(color_palette[1:length(unique(domain_scores$domain))], unique(domain_scores$domain))

  # Prepare domain data
  n_domains <- length(unique(domain_scores$domain))
  domain_plot_data <- domain_scores
  domain_plot_data$domain <- factor(domain_plot_data$domain, levels = unique(domain_plot_data$domain))
  domain_plot_data$domain_index <- as.integer(domain_plot_data$domain)
  domain_plot_data$angle <- (domain_plot_data$domain_index - 0.5) * 2 * pi / n_domains
  domain_plot_data$x <- domain_plot_data$domain_score * cos(domain_plot_data$angle)
  domain_plot_data$y <- domain_plot_data$domain_score * sin(domain_plot_data$angle)

  # --- BEGIN: Wide, curved petals for domains ---

  arc_points <- 100
  petal_width <- 0.9 * 2 * pi / n_domains

  petal_polygons <- do.call(rbind, lapply(1:n_domains, function(i) {
    domain_name <- as.character(domain_plot_data$domain[i])
    score <- domain_plot_data$domain_score[i]
    angle_center <- domain_plot_data$angle[i]
    angle_start <- angle_center - petal_width / 2
    angle_end <- angle_center + petal_width / 2
    angles_outer <- seq(angle_start, angle_end, length.out = arc_points)
    angles_inner <- rev(angles_outer)
    x_outer <- score * cos(angles_outer)
    y_outer <- score * sin(angles_outer)
    x_inner <- 0 * cos(angles_inner)
    y_inner <- 0 * sin(angles_inner)
    data.frame(
      x = c(x_outer, x_inner),
      y = c(y_outer, y_inner),
      domain = domain_name,
      group = i
    )
  }))

  # --- END: Wide, curved petals for domains ---

  # Prepare dimension data
  # For spreading stamen equally, get angle_start and angle_end for each domain
  domain_angle_ranges <- data.frame(
    domain = domain_plot_data$domain,
    angle_center = domain_plot_data$angle,
    angle_start = domain_plot_data$angle - petal_width / 2,
    angle_end = domain_plot_data$angle + petal_width / 2
  )

  # Join dimension_scores with domain_angle_ranges
  dimension_plot_data <- merge(dimension_scores, domain_angle_ranges, by = "domain")

  # Calculate dim_count, dim_index, and other variables by domain
  dimension_plot_data <- do.call(rbind, lapply(split(dimension_plot_data, dimension_plot_data$domain), function(x) {
    x$dim_count <- nrow(x)
    x$dim_index <- seq_len(nrow(x))
    # Spread stamen equally across the petal's angular width
    x$dim_angle <- ifelse(x$dim_count == 1,
      x$angle_center,
      x$angle_start + (x$dim_index - 1) * (x$angle_end - x$angle_start) / (x$dim_count - 1)
    )
    x$x <- x$dimension_value * 0.5 * cos(x$dim_angle)
    x$y <- x$dimension_value * 0.5 * sin(x$dim_angle)
    return(x)
  }))

  # Create lines from center to dimensions
  dimension_lines <- dimension_plot_data
  dimension_lines$x_start <- 0
  dimension_lines$y_start <- 0
  dimension_lines$x_end <- dimension_lines$x
  dimension_lines$y_end <- dimension_lines$y

  # Find maximum for plot limits
  max_score <- max(c(
    sqrt(dimension_plot_data$x^2 + dimension_plot_data$y^2),
    sqrt(domain_plot_data$x^2 + domain_plot_data$y^2)
  ), na.rm = TRUE)
  plot_limit <- ceiling(max_score * 1.2)

  # Base plot without reference circles (for preview)
  base_plot <- ggplot2::ggplot() +
    # Draw wide, curved petals for domains
    ggplot2::geom_polygon(
      data = petal_polygons,
      ggplot2::aes(x = x, y = y, fill = domain, group = group),
      alpha = 1, show.legend = FALSE
    )

  # Base plot with reference circles (for main and report)
  base_plot_with_circles <- base_plot +
    # Circle guides at 0.25, 0.5, 0.75, 1.0 (background layer)
    ggforce::geom_circle(
      data = data.frame(r = c(0.25, 0.5, 0.75, 1.0)),
      ggplot2::aes(x0 = 0, y0 = 0, r = r),
      color = "gray80", linewidth = 0.3
    ) +
    # Circle guide labels (background layer)
    ggplot2::annotate(
      "text",
      x = c(0.25, 0.5, 0.75, 1.0), y = 0,
      label = c("0.25", "0.50", "0.75", "1.00"),
      color = "gray50", size = 3
    )

  # Preview version (without reference circles)
  preview_plot <- base_plot +
    # Add lines from center to dimensions
    ggplot2::geom_segment(
      data = dimension_lines,
      ggplot2::aes(
        x = x_start, y = y_start,
        xend = x_end, yend = y_end,
        color = domain
      ),
      linewidth = 0.5, show.legend = FALSE
    ) +
    # Add dimension points ("stamen")
    ggplot2::geom_point(
      data = dimension_plot_data,
      ggplot2::aes(x = x, y = y, color = domain),
      size = 1, show.legend = FALSE
    ) +
    # Set colors
    ggplot2::scale_color_manual(values = domain_stamen_colors) +
    ggplot2::scale_fill_manual(values = domain_fill_colors) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        family = get_brand_sans_font(),
        hjust = 1
      ),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.position = "bottom"
    )

  # Main version (140 DPI, 1080x1080 square)
  # Main plot uses version with reference circles
  main_plot <- base_plot_with_circles +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Project Dynamics",
      caption = paste0("Sd = ", ifelse(!is.null(project_data$analyzed_data) && !is.null(project_data$analyzed_data$dynamics_score),
        round(as.numeric(project_data$analyzed_data$dynamics_score), 2),
        "N/A"
      ))
    ) +
    # Add lines from center to dimensions (reduced by 50%)
    ggplot2::geom_segment(
      data = dimension_lines,
      ggplot2::aes(
        x = x_start, y = y_start,
        xend = x_end, yend = y_end,
        color = domain
      ),
      linewidth = 1, show.legend = FALSE
    ) +
    # Add dimension points ("stamen") (reduced by 50%)
    ggplot2::geom_point(
      data = dimension_plot_data,
      ggplot2::aes(x = x, y = y, color = domain),
      size = 1.5, show.legend = FALSE
    ) +
    # Set colors
    ggplot2::scale_color_manual(values = domain_stamen_colors) +
    ggplot2::scale_fill_manual(values = domain_fill_colors) +
    # Fixed aspect ratio and limits
    ggplot2::coord_fixed(
      xlim = c(-plot_limit, plot_limit),
      ylim = c(-plot_limit, plot_limit)
    ) +
    ggplot2::theme_minimal() +
    # Add labels to stamen points (all uppercase, #666, ibmplexmono, smart alignment)
    ggplot2::geom_text(
      data = {
        temp_data <- dimension_plot_data
        temp_data$dimension_label <- toupper(temp_data$dimension)
        temp_data$is_left <- (temp_data$dim_angle > pi / 2 & temp_data$dim_angle < 3 * pi / 2)
        temp_data$x_label <- temp_data$x + 0.12 * temp_data$x
        temp_data$y_label <- temp_data$y + 0.12 * temp_data$y
        temp_data$hjust_label <- ifelse(temp_data$is_left, 1, 0)
        temp_data
      },
      ggplot2::aes(
        x = x_label,
        y = y_label,
        label = dimension_label,
        hjust = hjust_label
      ),
      size = 2.5,
      vjust = 0.5,
      family = get_brand_sans_font(),
      color = "#666",
      show.legend = FALSE
    ) +
    # Add domain labels
    ggplot2::geom_text(
      data = {
        temp_data <- domain_plot_data
        temp_data$domain_label <- temp_data$domain
        temp_data
      },
      ggplot2::aes(
        x = 1.1 * plot_limit * cos(angle),
        y = 1.1 * plot_limit * sin(angle),
        label = domain_label,
        color = domain
      ),
      hjust = 0.5, vjust = 0.5, size = 2.5, family = get_brand_sans_font(), fontface = "italic", show.legend = FALSE
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        family = get_brand_sans_font(),
        hjust = 1
      )
    )

  # Create report version (300 DPI, 9in x 9in square)
  report_plot <- base_plot_with_circles +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Project Dynamics",
      caption = paste0("Sd = ", ifelse(!is.null(project_data$analyzed_data) && !is.null(project_data$analyzed_data$dynamics_score),
        round(as.numeric(project_data$analyzed_data$dynamics_score), 2),
        "N/A"
      ))
    ) +
    # Add lines from center to dimensions
    ggplot2::geom_segment(
      data = dimension_lines,
      ggplot2::aes(
        x = x_start, y = y_start,
        xend = x_end, yend = y_end,
        color = domain
      ),
      linewidth = 1.5, show.legend = FALSE
    ) +
    # Add dimension points ("stamen")
    ggplot2::geom_point(
      data = dimension_plot_data,
      ggplot2::aes(x = x, y = y, color = domain),
      size = 3, show.legend = FALSE
    ) +
    # Set colors
    ggplot2::scale_color_manual(values = domain_stamen_colors) +
    ggplot2::scale_fill_manual(values = domain_fill_colors) +
    # Fixed aspect ratio and limits
    ggplot2::coord_fixed(
      xlim = c(-plot_limit, plot_limit),
      ylim = c(-plot_limit, plot_limit)
    ) +
    ggplot2::theme_minimal() +
    # Add labels to stamen points (all uppercase, #666, ibmplexmono, smart alignment)
    ggplot2::geom_text(
      data = {
        temp_data <- dimension_plot_data
        temp_data$dimension_label <- toupper(temp_data$dimension)
        temp_data$is_left <- (temp_data$dim_angle > pi / 2 & temp_data$dim_angle < 3 * pi / 2)
        temp_data$x_label <- temp_data$x + 0.12 * temp_data$x
        temp_data$y_label <- temp_data$y + 0.12 * temp_data$y
        temp_data$hjust_label <- ifelse(temp_data$is_left, 1, 0)
        temp_data
      },
      ggplot2::aes(
        x = x_label,
        y = y_label,
        label = dimension_label,
        hjust = hjust_label
      ),
      size = scale_text_for_report(2.5),
      vjust = 0.5,
      family = get_brand_sans_font(),
      color = "#666",
      show.legend = FALSE
    ) +
    # Add domain labels
    ggplot2::geom_text(
      data = {
        temp_data <- domain_plot_data
        temp_data$domain_label <- temp_data$domain
        temp_data
      },
      ggplot2::aes(
        x = 1.1 * plot_limit * cos(angle),
        y = 1.1 * plot_limit * sin(angle),
        label = domain_label,
        color = domain
      ),
      hjust = 0.5, vjust = 0.5, size = scale_text_for_report(2.5), family = get_brand_sans_font(), fontface = "italic", show.legend = FALSE
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = scale_text_for_report(16),
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = scale_text_for_report(14),
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = scale_text_for_report(12),
        family = get_brand_sans_font(),
        hjust = 1
      )
    )

  # Add metadata for rendering
  attr(main_plot, "dpi") <- 140
  attr(main_plot, "width") <- 1080 / 140 # pixels to inches (square)
  attr(main_plot, "height") <- 1080 / 140
  attr(main_plot, "plot_type") <- "main"

  attr(report_plot, "dpi") <- 300
  attr(report_plot, "width") <- 9
  attr(report_plot, "height") <- 9
  attr(report_plot, "plot_type") <- "report"

  return(list(
    preview = preview_plot,
    main = main_plot,
    report = report_plot
  ))
}







#' Create cascade effects plots
#' @param cascade_data Data frame with cascade analysis results
#' @param color_palette Vector of colors for the cascade plot
#' @param project_data Project data containing brand configuration (optional)
#' @return List with preview, main, and report ggplot objects
create_cascade_plots <- function(cascade_data, color_palette, project_data = NULL) {
  # Load brand fonts with project configuration
  brand_config <- NULL
  cat("DEBUG[cascade]: Checking project_data structure...\n")
  if (!is.null(project_data)) {
    cat("DEBUG[cascade]: project_data is not NULL\n")
    cat("DEBUG[cascade]: project_data class:", class(project_data), "\n")
    if (is.reactive(project_data)) {
      cat("DEBUG[cascade]: project_data is reactive, getting value...\n")
      pd <- project_data()
      cat("DEBUG[cascade]: reactive project_data brand_config:", !is.null(pd$brand_config), "\n")
      if (!is.null(pd$brand_config)) {
        brand_config <- pd$brand_config
        cat("Found brand configuration in reactive project_data for cascade plots\n")
      }
    } else {
      cat("DEBUG[cascade]: project_data is not reactive\n")
      cat("DEBUG[cascade]: direct project_data brand_config:", !is.null(project_data$brand_config), "\n")
      if (!is.null(project_data$brand_config)) {
        brand_config <- project_data$brand_config
        cat("Found brand configuration in project_data for cascade plots\n")
      }
    }
  } else {
    cat("DEBUG[cascade]: project_data is NULL\n")
  }

  if (is.null(brand_config)) {
    cat("No brand configuration found, using defaults for cascade plots\n")
  }
  load_brand_fonts(brand_config)

  # Handle different input structures
  if (is.data.frame(cascade_data)) {
    cascade_df <- cascade_data
  } else if (is.list(cascade_data) && !is.null(cascade_data$cascade)) {
    cascade_df <- cascade_data$cascade
  } else {
    stop("No cascade data available for visualization")
  }

  # Validate and standardize column names
  if (!all(c("Degree", "Score") %in% names(cascade_df))) {
    if (all(c("layer_number", "layer_score") %in% names(cascade_df))) {
      # Extract numeric part from layer_number if it contains text
      if (is.character(cascade_df$layer_number)) {
        cascade_df$Degree <- as.numeric(gsub("[^0-9]", "", cascade_df$layer_number))
      } else {
        cascade_df$Degree <- cascade_df$layer_number
      }
      cascade_df$Score <- cascade_df$layer_score
    } else {
      stop("Cascade results must have columns 'Degree' and 'Score', or 'layer_number' and 'layer_score'.")
    }
  }

  # Sort degrees and prepare proportions
  # Extract numeric part from Degree if it contains text
  if (is.character(cascade_df$Degree)) {
    numeric_degrees <- as.numeric(gsub("[^0-9]", "", cascade_df$Degree))
  } else {
    numeric_degrees <- as.numeric(cascade_df$Degree)
  }
  cascade_df <- cascade_df[order(numeric_degrees), ]
  n_total <- 200
  scores <- as.numeric(cascade_df$Score)
  n_layers <- length(scores)

  # Distribute total points across layers proportionally
  n_points <- round(n_total * scores / sum(scores))
  n_points[length(n_points)] <- n_total - sum(n_points[-length(n_points)]) # adjust remainder
  layer_vec <- rep(1:n_layers, times = n_points)

  # Generate Vogel spiral coordinates
  t <- seq(1, n_total)
  r <- sqrt(t)
  golden_angle <- pi * (3 - sqrt(5))
  theta <- t * golden_angle
  x <- r * cos(theta)
  y <- r * sin(theta)

  vogel_df <- data.frame(x = x, y = y, layer = layer_vec)

  # Assign colors
  if (!is.null(color_palette)) {
    vogel_df$color <- color_palette[vogel_df$layer]
  } else {
    default_cols <- c("#8A7A8F", "#B49291", "#E0D0A6") # fallback colors
    vogel_df$color <- default_cols[vogel_df$layer]
  }

  # Calculate max radius for each layer
  layer_radii <- stats::aggregate(sqrt(vogel_df$x^2 + vogel_df$y^2),
    by = list(layer = vogel_df$layer),
    FUN = max
  )
  names(layer_radii)[2] <- "radius"

  # Create concentric ring data
  circle_df <- data.frame(
    x0 = 0,
    y0 = 0,
    r = layer_radii$radius + 2 # slight padding
  )

  # Optional: Degree labels at top of each ring
  label_df <- circle_df
  label_df$label <- paste("Degree", 1:nrow(label_df))
  label_df$x <- 0
  label_df$y <- label_df$r

  # --- Degree label and connecting line logic for main plot ---
  # Identify unique degrees (layers)
  degree_levels <- sort(unique(vogel_df$layer))
  degree_labels <- paste("Degree", degree_levels)

  # For each degree, find the first point
  first_points <- do.call(rbind, lapply(degree_levels, function(lvl) {
    vogel_df[vogel_df$layer == lvl, ][1, ]
  }))
  # For Degree 3, find the max radius
  deg3_points <- vogel_df[vogel_df$layer == 3, ]
  max_r <- if (nrow(deg3_points) > 0) {
    max(sqrt(deg3_points$x^2 + deg3_points$y^2))
  } else {
    max(sqrt(vogel_df$x^2 + vogel_df$y^2))
  }
  label_radius <- max_r * 1.08

  # Compute label positions (same angle as first point, radius = label_radius)
  label_angles <- atan2(first_points$y, first_points$x)
  label_x <- label_radius * cos(label_angles)
  label_y <- label_radius * sin(label_angles)
  label_df2 <- data.frame(
    x = label_x,
    y = label_y,
    label = degree_labels
  )
  # Connecting lines: from label to first point
  seg_df <- data.frame(
    x = label_x,
    y = label_y,
    xend = first_points$x,
    yend = first_points$y
  )

  # Build base plot
  # Main plot base (with degree labels and connecting lines, size 5) - 140 DPI, 1080x1080 square
  base_plot_main <- ggplot2::ggplot(vogel_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(layer)),
      size = 5, alpha = 0.9, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = unique(vogel_df$color)) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE,
      color = "#4A4A4A", linetype = "dotted"
    ) +
    ggplot2::geom_text(
      data = label_df2,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      vjust = -0.5, size = 2.5,
      color = "#4A4A4A", fontface = "bold"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

  # Preview plot base (no reference lines, reduced dot size)
  base_plot_preview <- ggplot2::ggplot(vogel_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(layer)),
      size = 1.5, alpha = 0.9, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = unique(vogel_df$color)) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

  # Lightweight preview version
  preview_plot <- base_plot_preview +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        family = get_brand_sans_font(),
        hjust = 1
      ),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  # Full main version with minimal theme (140 DPI, 1080x1080 square)
  main_plot <- base_plot_main +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Cascade Effects",
      caption = paste0("Sc = ", ifelse(!is.null(project_data$analyzed_data) && !is.null(project_data$analyzed_data$cascade_score),
        round(as.numeric(project_data$analyzed_data$cascade_score), 2),
        "N/A"
      ))
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 2.5, family = get_brand_sans_font()),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = 16,
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = 14,
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = 12,
        family = get_brand_sans_font(),
        hjust = 1
      )
    )

  # Report plot base (300 DPI, 9in x 9in square)
  base_plot_report <- ggplot2::ggplot(vogel_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(layer)),
      size = 5, alpha = 0.9, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = unique(vogel_df$color)) +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE,
      color = "#4A4A4A", linetype = "dotted"
    ) +
    ggplot2::geom_text(
      data = label_df2,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      vjust = -0.5, size = scale_text_for_report(2.5),
      color = "#4A4A4A", fontface = "bold",
      family = get_brand_sans_font()
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

  # Create report version
  report_plot <- base_plot_report +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = get_plot_title(
        project_data$project_title,
        project_data$report_date
      ),
      subtitle = "Cascade Effects",
      caption = paste0("Sc = ", ifelse(!is.null(project_data$analyzed_data) && !is.null(project_data$analyzed_data$cascade_score),
        round(as.numeric(project_data$analyzed_data$cascade_score), 2),
        "N/A"
      ))
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.title = ggplot2::element_text(
        size = scale_text_for_report(16),
        face = "bold",
        family = get_brand_sans_font()
      ),
      plot.subtitle = ggplot2::element_text(
        size = scale_text_for_report(14),
        family = get_brand_sans_font()
      ),
      plot.caption = ggplot2::element_text(
        size = scale_text_for_report(12),
        family = get_brand_sans_font(),
        hjust = 1
      )
    )

  # Add metadata for rendering
  attr(main_plot, "dpi") <- 140
  attr(main_plot, "width") <- 1080 / 140 # pixels to inches (square)
  attr(main_plot, "height") <- 1080 / 140
  attr(main_plot, "plot_type") <- "main"

  attr(report_plot, "dpi") <- 300
  attr(report_plot, "width") <- 9
  attr(report_plot, "height") <- 9
  attr(report_plot, "plot_type") <- "report"

  return(list(preview = preview_plot, main = main_plot, report = report_plot))
}
