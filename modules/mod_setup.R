#!! ModName = mod_setup
# !! ModDisplayName = Project Setup
# !! ModDescription = Set up project information and configuration
# !! ModCitation = Price, Jeremy F. (2025). mod_setup. [Source code].
# !! ModNotes = This module provides functionality to set up project information.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for setup !! reactive

# Load required libraries
#' @importFrom phosphoricons ph
#' @importFrom shinyAce updateAceEditor
#' @importFrom logger log_info log_warn log_error log_trace

# Utilities are loaded in global.R
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom shinyToastify showToast
#' @importFrom shinyvalidate InputValidator sv_required sv_between sv_in_set




# !! ModName = mod_setup
# !! ModDisplayName = Project Setup
# !! ModDescription = This module allows you to set up your project by selecting a directory, entering project details, and creating the necessary project structure. You can also edit author information and project configuration at any time.
# !! ModCitation = Price, Jeremy.  (2025). mod_setup. [Source code].
# !! ModNotes = Enter your module notes here.
# !! ModActive = 1/0
# !! FunctionArg = argName1 !! argDescription !! argClass
# !! FunctionArg = argName2 !! argDescription !! argClass
# !! FunctionReturn = returnName1 !! returnDescription !! returnClass
# !! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_setup_ui <- function(id) {
  # Load required libraries
  require(shinyjs)
  ns <- NS(id)
  tagList(

    # UI utilities are loaded in global.R
    tags$head(
      tags$style(HTML(paste0(
        "#", ns("details_status_message, "),
        "#", ns("author_status_message"), " {
          display: block;
          padding-left: 0.5em;
          z-index: 10;
          width: 100%;
          font-family: 'Share Tech Mono' !important;
        }"
      )))
    ),
    bslib::accordion(
      id = ns("setup_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "project_details",
        title = tagList(
          ph("clipboard-text"),
          HTML("&nbsp;"),
          "Project Details"
        ),
        fluidRow(
          column(
            width = 12,
            p("This module allows you to set up your project by selecting a directory, entering project details, and creating the necessary project structure. You can also edit author information and project configuration at any time."),
            p("Please follow the steps in the accordions below to complete the project setup.")
          )
        ),
        # Status line UI rendered reactively
        uiOutput(ns("status_line_ui")),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            textInput(
              ns("project_title"),
              "Project Title",
              placeholder = "Enter project title",
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            textInput(
              ns("project_subtitle"),
              "Project Subtitle",
              placeholder = "Enter project subtitle",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-left: 1em;",
            dateInput(
              ns("report_date"),
              "Report Date",
              value = Sys.Date(),
              width = "100%",
              format = "yyyy-mm-dd",
              startview = "month"
            )
          ),
          column(
            width = 6,
            style = "padding-right: 1em;",
            textInput(
              ns("project_funding"),
              "Project Funding Information",
              placeholder = "Enter project funding information",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            textAreaInput(
              ns("project_description"),
              "Project Description",
              placeholder = "Enter a concise description of your project",
              rows = 3,
              resize = "vertical",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            style = "padding-right: 1em;",
            shinyWidgets::prettyCheckboxGroup(
              ns("report_formats"),
              "Report Formats",
              width = "100%",
              inline = FALSE,
              choices = list("HTML", "PDF", "Word", "LaTeX", "Typst")
            )
          ),
          column(
            width = 3,
            style = "padding-left: 1em;",
            shinyWidgets::prettyRadioButtons(
              ns("report_license"),
              "Report License",
              inline = FALSE,
              choices = c("Public Domain", "CC BY", "CC BY-SA", "CC BY-NC", "CC BY-NC-SA", "CC BY-NC-ND"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            selectizeInput(
              inputId = ns("project_keywords"),
              label = "Project Keywords",
              width = "100%",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = TRUE,
                delimiter = ",",
                placeholder = "Enter keywords separated by commas"
              )
            )
          )
        ),
        fluidRow(
          div(
            class = "d-flex justify-content-evenly align-items-center",
            actionButton(
              ns("initiate_project"),
              tagList(
                ph("floppy-disk", weight = "bold"),
                HTML("&nbsp;"),
                "Save Project Details"
              ),
              class = "btn-lrg btn btn-primary",
              onclick = sprintf("$('#%s').show();", ns("status_line_container"))
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "author_information",
        title = tagList(ph("user-list"), HTML("&nbsp;"), "Author Information"),
        fluidRow(
          column(
            width = 12,
            p("You may edit the author information. This information will be used in the project configuration and can be updated at any time. The author information is stored in a YAML file called `authors.yml` located in the `config` directory of your project.")
          )
        ),
        # Status line UI for author information (rendered reactively)
        uiOutput(ns("author_status_line_ui")),
        fluidRow(
          column(
            width = 12,
            shinyWidgets::textInputIcon(
              inputId = ns("author_name"),
              label = tagList("Author Name", ph("asterisk", weight = "bold")),
              placeholder = "Enter author name",
              icon = icon("user"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_affiliation"),
              label = "Author Affiliation",
              placeholder = "Enter author affiliation",
              icon = icon("building-columns"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_orcid"),
              label = "Author ORCID",
              placeholder = "XXXX-XXXX-XXXX-XXXX",
              icon = icon("orcid"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            style = "padding-right: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_email"),
              label = "Author Email",
              placeholder = "Enter author email",
              icon = icon("at"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            style = "padding-left: 1em;",
            shinyWidgets::textInputIcon(
              inputId = ns("author_url"),
              label = "Author URL",
              placeholder = "https://",
              icon = icon("link"),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 2em; text-align: center;",
            div(
              class = "d-flex justify-content-evenly align-items-center",
              actionButton(
                ns("add_author_btn"),
                tagList(
                  ph("user-circle-plus"),
                  HTML("&nbsp;"),
                  "Add Author"
                ),
                class = "btn-lrg btn btn-primary"
              ),
              actionButton(
                ns("download_authors"),
                tagList(
                  ph("file-arrow-down"),
                  HTML("&nbsp;"),
                  "Download authors.yml"
                ),
                class = "btn-lrg btn btn-primary"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            style = "margin-top: 2em;",
            h4("YAML Preview"),
            textAreaInput(
              inputId = ns("author_yaml_code"),
              label = NULL,
              value = "",
              placeholder = "Author YAML will appear here...",
              rows = 15,
              resize = "vertical",
              width = "100%"
            )
          )
        )
      ),
      bslib::accordion_panel(
        value = "branding",
        title = tagList(ph("palette"), HTML("&nbsp;"), "Branding"),
        fluidRow(
          column(
            width = 12,
            h4(tagList("Edit Project Branding ", ph("palette", style = "margin-left: 10px;")))
          )
        ),
        fluidRow(
          column(
            width = 12,
            p("You may edit the project configuration. This information will be used in the project configuration and can be updated at any time. The project configuration is stored in a YAML file called `config.yml` located in the `config` directory of your project.")
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin: 15px 0;",
              uiOutput(ns("branding_status_line_ui"))
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: center; margin: 15px 0;",
              actionButton(
                inputId = ns("save_design"),
                label = "Save Design",
                class = "btn-setup btn-primary",
                icon = icon("save"),
                onclick = "console.log('Save Design onclick triggered!'); console.log('Button element:', this);"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            textAreaInput(
              inputId = ns("branding_yaml_code"),
              label = NULL,
              value = "",
              placeholder = "Enter YAML configuration here...",
              rows = 15,
              resize = "vertical",
              width = "100%"
            )
          )
        )
      )
    )
  )
}


# the server function
mod_setup_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track original content for proper change detection (moved to top)
    original_branding_content <- reactiveVal("")

    # Reactive to check if branding config is saved (moved to top)
    branding_config_saved <- reactive({
      !is.null(project_data$brand_config) && !is.null(project_data$brand_yaml)
    })

    # Reactive value to store status message and type
    status_line <- reactiveValues(
      message = "Project ready",
      type = "success",
      visible = TRUE,
      timestamp = Sys.time()
    )

    # Reactive value for author status message and type
    author_status_line <- reactiveValues(
      message = "",
      type = "success",
      visible = FALSE,
      timestamp = Sys.time()
    )

    # Reactive value for branding status message and type
    branding_status_line <- reactiveValues(
      message = "BRANDING READY FOR EDITING",
      type = "info",
      visible = TRUE,
      timestamp = Sys.time()
    )

    # Helper to update status line
    show_status_line <- function(msg, type = c("success", "warning"), show = TRUE) {
      type <- match.arg(type)
      status_line$message <- msg
      status_line$type <- type
      status_line$visible <- show
      status_line$timestamp <- Sys.time()
      logger::log_info("Status line updated: ", msg, " (Type: ", type, ")")
    }

    # Helper to update author status line
    show_author_status_line <- function(msg, type = c("success", "warning"), show = TRUE) {
      type <- match.arg(type)
      author_status_line$message <- msg
      author_status_line$type <- type
      author_status_line$visible <- show
      author_status_line$timestamp <- Sys.time()
      logger::log_info("Author status line updated: ", msg, " (Type: ", type, ")")
    }

    # Helper to update branding status line
    show_branding_status_line <- function(msg, type = c("success", "warning", "info"), show = TRUE) {
      type <- match.arg(type)
      branding_status_line$message <- msg
      branding_status_line$type <- type
      branding_status_line$visible <- show
      branding_status_line$timestamp <- Sys.time() # This triggers reactivity
      logger::log_info("Branding status line updated: ", msg, " (Type: ", type, ")")
    }

    # Reactive moved to top of function for proper scoping

    # Render status line UI
    output$status_line_ui <- renderUI({
      # Force dependency on the timestamp to ensure updates
      force(status_line$timestamp)

      # Only render if visible is TRUE or NULL
      if (is.null(status_line$visible) || isTRUE(status_line$visible)) {
        # Get icon based on message type
        status_icon <- if (status_line$type == "success") {
          phosphoricons::ph("check-circle", weight = "fill", class = "me-2")
        } else {
          phosphoricons::ph("warning", weight = "fill", class = "me-2")
        }

        # Create the status line div
        div(
          id = ns("status_line_container"),
          class = paste0(
            "alert ",
            ifelse(status_line$type == "success", "alert-success", "alert-warning"),
            " d-flex align-items-center"
          ),
          style = "margin-bottom: 1.25em; display: block !important;",
          status_icon,
          tags$span(status_line$message, style = "text-transform: uppercase;")
        )
      }
    })

    # Render branding status line UI
    output$branding_status_line_ui <- renderUI({
      # React to all branding status properties
      req(branding_status_line$timestamp)

      msg <- branding_status_line$message
      type <- branding_status_line$type

      logger::log_info("Branding status UI rendering - Message:", msg, " Type:", type)

      # Always render the status line
      status_icon <- switch(type,
        "success" = ph("check-circle", weight = "fill", style = "color: #5D7359; margin-right: 8px;"),
        "warning" = ph("warning-circle", weight = "fill", style = "color: #C5A259; margin-right: 8px;"),
        "info" = ph("info", weight = "fill", style = "color: #5B9BD5; margin-right: 8px;")
      )

      alert_class <- switch(type,
        "success" = "alert-success",
        "warning" = "alert-warning",
        "info" = "alert-info"
      )

      div(
        class = paste0("alert ", alert_class, " d-flex align-items-center"),
        style = "margin: 0; padding: 8px 12px; font-size: 13px;",
        status_icon,
        msg
      )
    })

    # Reactive moved to top of function for proper scoping

    # Render author status line UI
    output$author_status_line_ui <- renderUI({
      # Force dependency on the timestamp to ensure updates
      force(author_status_line$timestamp)

      # Only render if visible is TRUE or NULL
      if (is.null(author_status_line$visible) || isTRUE(author_status_line$visible)) {
        # Get icon based on message type
        status_icon <- if (author_status_line$type == "success") {
          phosphoricons::ph("check-circle", weight = "fill", class = "me-2")
        } else {
          phosphoricons::ph("warning", weight = "fill", class = "me-2")
        }

        # Create the status line div
        div(
          id = ns("author_status_line_container"),
          class = paste0(
            "alert ",
            ifelse(author_status_line$type == "success", "alert-success", "alert-warning"),
            " d-flex align-items-center"
          ),
          style = "margin-bottom: 1.25em; display: block !important;",
          status_icon,
          tags$span(author_status_line$message, style = "text-transform: uppercase;")
        )
      }
    })

    # Initialize shinyjs - only need to call this once
    shinyjs::useShinyjs()

    # Helper function to safely run JavaScript without errors
    safe_runjs <- function(js_code) {
      tryCatch(
        {
          shinyjs::runjs(paste0("try { ", js_code, " } catch(e) { console.log('JS error caught:', e); }"))
        },
        error = function(e) {
          logger::log_trace("JavaScript execution failed safely:", e$message)
        }
      )
    }

    logger::log_info("Initiating project setup module")

    # Debug observer to track all inputs
    observe({
      logger::log_info("=== INPUT DEBUG ===")
      logger::log_info("Available inputs:", paste(names(input), collapse = ", "))
      if ("save_design" %in% names(input)) {
        logger::log_info("save_design value:", input$save_design)
      } else {
        logger::log_warn("save_design NOT in input list")
      }
      logger::log_info("=== END INPUT DEBUG ===")
    })

    # Track button clicks with reactiveVal
    save_design_clicked <- reactiveVal(0)

    # Alternative observer to catch save_design changes - with save logic
    observe({
      val <- input$save_design
      if (!is.null(val) && val > 0) {
        logger::log_info("*** ALTERNATIVE OBSERVER: Save Design clicked! Value:", val)

        # Prevent multiple executions by using isolate
        isolate({
          # Put save logic directly here since this observer is working
          tryCatch(
            {
              # Get the YAML content from the editor
              yaml_content <- input$branding_yaml_code
              logger::log_info("YAML content retrieved, length:", ifelse(is.null(yaml_content), "NULL", nchar(yaml_content)))

              # Save regardless of content - store even empty content
              if (!is.null(yaml_content)) {
                yaml_content <- trimws(yaml_content)

                if (nzchar(yaml_content)) {
                  logger::log_info("Attempting to save YAML content of length:", nchar(yaml_content))

                  # Validate YAML syntax first
                  brand_config <- tryCatch(
                    {
                      parsed_config <- yaml::yaml.load(yaml_content)
                      logger::log_info("YAML parsed successfully")
                      parsed_config
                    },
                    error = function(e) {
                      logger::log_error("YAML parsing error:", e$message)
                      show_branding_status_line(paste("Invalid YAML format:", e$message), "warning")
                      return(NULL)
                    }
                  )

                  if (!is.null(brand_config)) {
                    # Store in project_data for later use
                    project_data$brand_config <- brand_config
                    project_data$brand_yaml <- yaml_content # Store raw YAML too

                    logger::log_info("Brand configuration stored in project_data")

                    # Update the original content tracker so change detection works correctly
                    original_branding_content(yaml_content)

                    # Show success message
                    show_branding_status_line("BRANDING SAVED", "success")

                    logger::log_info("Save Design completed successfully")
                  }
                } else {
                  # Save empty content too
                  logger::log_info("Saving empty YAML content")
                  project_data$brand_config <- list()
                  project_data$brand_yaml <- ""
                  original_branding_content("")
                  show_branding_status_line("BRANDING SAVED", "info")
                }
              } else {
                logger::log_warn("YAML content is NULL")
                show_branding_status_line("NO CONTENT TO SAVE", "warning")
              }
            },
            error = function(e) {
              logger::log_error("Error in Save Design handler:", e$message)
              show_branding_status_line("ERROR SAVING BRANDING", "warning")
            }
          )
        })
      }
    })

    # DOM inspection to verify button exists
    observe({
      shinyjs::runjs(sprintf("
        setTimeout(function() {
          var btn = document.getElementById('%s');
          console.log('Button with ID %s exists:', btn !== null);
          if (btn) {
            console.log('Button element:', btn);
            console.log('Button onclick:', btn.onclick);
            console.log('Button addEventListener check:', typeof btn.addEventListener);
          } else {
            console.log('Available elements with save in ID:',
              Array.from(document.querySelectorAll('[id*=\"save\"]')).map(el => el.id));
          }
        }, 1000);
      ", ns("save_design"), ns("save_design")))
    })

    # Initialize input validation
    iv <- InputValidator$new()

    # Helper function to get namespaced ID
    ns_id <- function(id) {
      session$ns(id)
    }

    # Project Title validation - required
    iv$add_rule("project_title", sv_required("Project title is required"))
    iv$add_rule("project_title", ~ if (nchar(.) > 200) "Project title must be 200 characters or less")
    iv$add_rule("project_title", ~ if (grepl("[<>]", .)) "Project title cannot contain < or > characters")

    # Project Subtitle validation - optional
    iv$add_rule("project_subtitle", ~ if (nchar(.) > 200) "Project subtitle must be 200 characters or less")
    iv$add_rule("project_subtitle", ~ if (grepl("[<>]", .)) "Project subtitle cannot contain < or > characters")

    # Project Description validation - optional
    iv$add_rule("project_description", ~ if (nchar(.) > 1000) "Project description must be 1000 characters or less")

    # Author Name validation - only required when adding an author
    # This rule is moved to the author addition section since it's not required for initial form submission

    # Email validation - optional but must be valid if provided
    iv$add_rule("author_email", ~ if (nzchar(.) && !grepl("^[^@]+@[^@]+\\.[^@]+", .)) "Please enter a valid email address")
    iv$add_rule("author_email", ~ if (nzchar(.) && nchar(.) > 100) "Email must be 100 characters or less")

    # URL validation - optional but must be valid if provided
    iv$add_rule("author_url", ~ if (nzchar(.) && !grepl("^https?://", .)) "Please enter a valid URL (include http:// or https://)")
    iv$add_rule("author_url", ~ if (nzchar(.) && nchar(.) > 200) "URL must be 200 characters or less")

    # ORCID validation - optional but must be valid if provided
    iv$add_rule("author_orcid", ~ if (nzchar(.) && !grepl("^\\d{4}-\\d{4}-\\d{4}-(\\d{3}X|\\d{4})$", .)) "Please enter a valid ORCID (e.g., 0000-0002-1825-0097)")

    # Report Date validation - required
    iv$add_rule("report_date", sv_required("Report date is required"))
    iv$add_rule("report_date", function(value) {
      # First check for NULL, NA, or empty string
      if (is.null(value) || is.na(value) || identical(value, "")) {
        return("Report date is required")
      }

      # Initialize date_val
      date_val <- NULL

      # Try to convert to Date if it's not already
      if (inherits(value, "Date")) {
        date_val <- value
      } else if (is.character(value)) {
        # Try to parse the date with multiple possible formats
        date_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%m-%d-%Y")

        for (fmt in date_formats) {
          parsed <- try(as.Date(value, format = fmt), silent = TRUE)
          if (inherits(parsed, "Date") && !is.na(parsed)) {
            date_val <- parsed
            break
          }
        }
      }

      # Check if we have a valid date
      if (is.null(date_val) || is.na(date_val)) {
        return("Please enter a valid date (YYYY-MM-DD, MM/DD/YYYY, or MM-DD-YYYY)")
      }

      # Check date bounds - convert to Date objects for comparison
      min_date <- as.Date("1900-01-01")
      max_date <- as.Date("2100-12-31")

      if (!is.na(date_val) && !is.null(date_val)) {
        if (date_val < min_date) {
          return("Date cannot be before 1900-01-01")
        }

        if (date_val > max_date) {
          return("Date cannot be after 2100-12-31")
        }
      }

      # If we get here, the date is valid
      return(NULL)
    })

    # Report Formats validation - at least one must be selected
    iv$add_rule("report_formats", ~ if (length(.) == 0) "Please select at least one report format")

    # Validate Report Formats - at least one format must be selected
    iv$add_rule("report_formats", sv_required())
    # iv$add_rule("report_formats", function(value) {
    #   if (is.null(value) || length(value) == 0) {
    #     "Please select at least one report format"
    #   }
    # })

    # Enable validation
    iv$enable()

    # Helper to robustly check for blank/empty/NA values
    is_blank <- function(x) {
      # Handle vectors by checking if all elements are blank
      if (length(x) > 1) {
        return(all(sapply(x, is_blank)))
      }

      if (is.null(x)) {
        return(TRUE)
      }
      if (is.na(x)) {
        return(TRUE)
      }
      if (is.character(x)) {
        if (length(x) == 0) {
          return(TRUE)
        }
        if (all(is.na(x))) {
          return(TRUE)
        }
        # Safely handle trimws by checking length first
        if (length(x) > 0 && all(nchar(trimws(x)) == 0)) {
          return(TRUE)
        }
      }
      if (is.list(x) && length(x) == 0) {
        return(TRUE)
      }
      if (is.atomic(x) && length(x) == 0) {
        return(TRUE)
      }
      FALSE
    }

    # Track saved state for status line
    details_saved <- reactiveVal(FALSE)
    status_message <- reactiveVal("Ready")

    # Track the previously selected tab
    # Tab navigation is now handled by the main server logic

    authors <- reactiveValues(list = list())


    # Status tracking removed



    # Reactive expression to determine current status
    current_status <- reactive({
      # Safely check if inputs exist before calling is_blank
      title_blank <- is.null(input$project_title) || is_blank(input$project_title)
      desc_blank <- is.null(input$project_description) || is_blank(input$project_description)
      formats_blank <- is.null(input$report_formats) || is_blank(input$report_formats)
      keywords_blank <- is.null(input$project_keywords) || is_blank(input$project_keywords)
      # Special handling for date values
      date_blank <- tryCatch(
        {
          is.null(input$report_date) || is_blank(input$report_date)
        },
        error = function(e) {
          logger::log_warn("Error checking date blank status: {conditionMessage(e)}")
          TRUE # Assume blank if there's an error
        }
      )

      if (
        !details_saved() &&
          title_blank &&
          desc_blank &&
          formats_blank &&
          keywords_blank &&
          date_blank
      ) {
        "no_details"
      } else if (details_saved()) {
        "saved"
      } else {
        "ready"
      }
    })

    # We no longer need the ready_icon and ready_message variables as we're using uiOutput

    # This handler has been moved and combined with the one at line ~990

    # Load _brand.yml into the branding editor on initialization
    observe({
      req(!is.null(input$setup_accordion))

      if ("branding" %in% input$setup_accordion) {
        tryCatch(
          {
            # Check if we have saved config in project_data first (prioritize saved content)
            if (!is.null(project_data$brand_yaml)) {
              # Load from project_data - this is saved content
              updateTextAreaInput(
                session = session,
                inputId = "branding_yaml_code",
                value = project_data$brand_yaml
              )
              original_branding_content(project_data$brand_yaml)

              # Check if we just saved (don't immediately overwrite save success message)
              if (branding_status_line$message == "BRANDING SAVED") {
                # Delay showing ready message to let user see save confirmation
                shinyjs::delay(2000, {
                  show_branding_status_line("BRANDING READY FOR EDITING", "info")
                })
              } else {
                show_branding_status_line("BRANDING READY FOR EDITING", "info")
              }
              logger::log_info("Successfully loaded saved brand configuration from project_data")
            } else {
              # Fall back to default file only if no saved content exists
              brand_file <- file.path("www", "_brand.yml")

              if (file.exists(brand_file)) {
                brand_content <- paste(readLines(brand_file, warn = FALSE), collapse = "\n")

                # Update editor
                updateTextAreaInput(
                  session = session,
                  inputId = "branding_yaml_code",
                  value = brand_content
                )

                # Set as original content
                original_branding_content(brand_content)
                show_branding_status_line("BRANDING READY FOR EDITING", "info")
                logger::log_info("Successfully loaded _brand.yml into editor")
              } else {
                # No file and no saved config
                original_branding_content("")
                show_branding_status_line("BRANDING READY FOR EDITING", "info")
              }
            }
          },
          error = function(e) {
            logger::log_error("Error loading brand configuration:", e$message)
            show_branding_status_line("ERROR LOADING BRANDING", "warning")
          }
        )
      }
    })

    # Update project title when changed
    observeEvent(input$project_title,
      {
        req(input$project_title)
        # Update the project data reactive value
        project_data$project_info$title <- input$project_title
        # Show a status message
        show_status_line("Project title updated", "success")
        logger::log_info("Project title updated to:", input$project_title)
      },
      ignoreInit = TRUE
    )

    # Consolidated input observers - no autosave/editing functionality as requested
    observeEvent(input$project_title,
      { },
      ignoreInit = TRUE
    )
    observeEvent(input$project_description,
      { },
      ignoreInit = TRUE
    )
    observeEvent(input$report_formats,
      { },
      ignoreInit = TRUE
    )
    observeEvent(input$project_keywords,
      { },
      ignoreInit = TRUE
    )

    # Handle report date
    # Handle report date changes
    observeEvent(input$report_date,
      {
        # Safely handle report date
        tryCatch(
          {
            if (!is.null(input$report_date)) {
              # Ensure we have a valid date value
              date_val <- as.Date(input$report_date)
              # Log the date change for debugging
              logger::log_info("Report date updated to:", as.character(date_val))
            }
          },
          error = function(e) {
            logger::log_warn("Error processing report date: {conditionMessage(e)}")
          }
        )
      },
      ignoreInit = TRUE
    )


    # Reset form and status on Reset Form button
    observeEvent(input$reset_form,
      {
        details_saved(FALSE)
        logger::log_info("Setting editing_active to FALSE in reset logic")
        editing_active(FALSE)
        last_edit_time(NULL)
        updateTextInput(session, "project_title", value = "")
        updateTextAreaInput(session, "project_description", value = "")
        updateCheckboxGroupInput(session, "report_formats", selected = character(0))
        updateSelectizeInput(session, "project_keywords", selected = character(0))
        updateDateInput(session, "report_date", value = Sys.Date())
      },
      ignoreInit = TRUE
    )

    observeEvent(input$add_author_btn, {
      # Safely get author input values with NULL checks and trim whitespace
      author_name <- if (!is.null(input$author_name)) trimws(input$author_name) else ""
      author_affiliation <- if (!is.null(input$author_affiliation)) trimws(input$author_affiliation) else ""
      author_orcid <- if (!is.null(input$author_orcid)) trimws(input$author_orcid) else ""
      author_email <- if (!is.null(input$author_email)) trimws(input$author_email) else ""
      author_url <- if (!is.null(input$author_url)) trimws(input$author_url) else ""

      # Require author name
      if (author_name == "") {
        logger::log_warn("Author name is required but was not provided")
        status_message("Author name is required")
        details_saved(FALSE)
        show_status_line("Author name is required", "warning")
        return()
      }

      logger::log_info("Adding new author with name:", author_name)
      # Save only non-empty author fields
      new_author <- list(name = author_name)
      if (author_affiliation != "") new_author$affiliation <- author_affiliation
      if (author_orcid != "") new_author$orcid <- author_orcid
      if (author_email != "") new_author$email <- author_email
      if (author_url != "") new_author$url <- author_url
      authors$list <- append(authors$list, list(new_author))

      # Update project_data directly to ensure it contains authors
      project_data$project_info$authors <- authors$list

      logger::log_info("Added new author: ", author_name, ", total authors: ", length(authors$list))

      # Clear form fields
      updateTextInput(session, "author_name", value = "")
      updateTextInput(session, "author_affiliation", value = "")
      updateTextInput(session, "author_orcid", value = "")
      updateTextInput(session, "author_email", value = "")
      updateTextInput(session, "author_url", value = "")

      # Update the yaml display
      tryCatch(
        {
          if (length(authors$list) > 0) {
            yaml_lines <- lapply(authors$list, function(author) {
              lines <- c(paste0("- name: ", author$name))
              if (!is.null(author$affiliation)) {
                lines <- c(lines, paste0("  affiliation: ", author$affiliation))
              }
              if (!is.null(author$orcid)) {
                lines <- c(lines, paste0("  orcid: ", author$orcid))
              }
              if (!is.null(author$email)) {
                lines <- c(lines, paste0("  email: ", author$email))
              }
              if (!is.null(author$url)) {
                lines <- c(lines, paste0("  url: ", author$url))
              }
              paste(lines, collapse = "\n")
            })
            yaml_text <- paste(yaml_lines, collapse = "\n")
            updateTextAreaInput(session, "author_yaml_code", value = yaml_text)
          }
        },
        error = function(e) {
          logger::log_error("Error updating YAML display: ", e$message)
        }
      )

      # Build YAML from all authors, only including non-empty fields and always using 'affiliations' in YAML
      yaml_lines <- lapply(authors$list, function(author) {
        lines <- c(paste0("- name: ", author$name))
        if (!is.null(author$affiliation) && author$affiliation != "") {
          lines <- c(lines, paste0("  affiliations: ", author$affiliation))
        }
        if (!is.null(author$orcid) && author$orcid != "") {
          lines <- c(lines, paste0("  orcid: ", author$orcid))
        }
        if (!is.null(author$email) && author$email != "") {
          lines <- c(lines, paste0("  email: ", author$email))
        }
        if (!is.null(author$url) && author$url != "") {
          lines <- c(lines, paste0("  url: ", author$url))
        }
        paste(lines, collapse = "\n")
      })
      yaml_text <- paste(yaml_lines, collapse = "\n")
      logger::log_info("Updating authors YAML with ", length(authors$list), " author(s)")
      updateTextAreaInput(session, "author_yaml_code", value = yaml_text)

      # Authors updated - no need to mark Project Setup as complete automatically
      logger::log_info("Authors YAML updated successfully")
      # Show success message
      status_message("Author added successfully")
      details_saved(TRUE)

      # Update project status - removed invalid shinyjs calls to non-existent elements
      # Status line is already handled by the reactive UI

      # Show success message for author addition
      show_author_status_line(
        msg = paste0(author_name, " added as author"),
        type = "success",
        show = TRUE
      )
    })

    # Tab navigation is now handled by the main server logic

    logger::log_info("Project Setup module initialized")


    # Remove the automatic title update on input change

    observe({
      # Update all form fields when project_data changes (e.g., when loading from RDS)
      updateTextInput(session, "project_title", value = project_data$project_info$title %||% "")
      updateTextInput(session, "project_subtitle", value = project_data$project_info$subtitle %||% "")
      updateTextAreaInput(session, "project_description", value = project_data$project_info$description %||% "")
      updateDateInput(session, "report_date", value = project_data$project_info$report_date %||% Sys.Date())
      updateTextInput(session, "project_funding", value = project_data$project_info$funding %||% "")

      # Update report formats if they exist
      if (!is.null(project_data$project_info$report_formats)) {
        updateCheckboxGroupInput(session, "report_formats", selected = project_data$project_info$report_formats)
      }

      # Update keywords if they exist
      if (!is.null(project_data$project_info$report_keywords)) {
        updateSelectizeInput(session, "project_keywords",
          selected = project_data$project_info$report_keywords
        )
      }

      # Update license if it exists
      if (!is.null(project_data$project_info$license)) {
        updateRadioButtons(session, "report_license",
          selected = project_data$project_info$license
        )
      }

      # Update authors if they exist in project_data
      if (!is.null(project_data$project_info$authors) && length(project_data$project_info$authors) > 0) {
        if (!is.null(project_data$project_info$authors) && length(project_data$project_info$authors) > 0) {
          # Update the authors reactive value
          authors$list <- lapply(project_data$project_info$authors, function(author) {
            # Ensure all fields are present with empty strings as defaults
            list(
              name = author$name %||% "",
              affiliation = author$affiliation %||% "",
              orcid = author$orcid %||% "",
              email = author$email %||% "",
              url = author$url %||% ""
            )
          })

          # Build YAML from all authors
          yaml_lines <- lapply(authors$list, function(author) {
            lines <- c(paste0("- name: ", author$name))
            if (nzchar(author$affiliation)) {
              lines <- c(lines, paste0("  affiliations: ", author$affiliation))
            }
            if (nzchar(author$orcid)) {
              lines <- c(lines, paste0("  orcid: ", author$orcid))
            }
            if (nzchar(author$email)) {
              lines <- c(lines, paste0("  email: ", author$email))
            }
            if (nzchar(author$url)) {
              lines <- c(lines, paste0("  url: ", author$url))
            }
            paste(lines, collapse = "\n")
          })

          yaml_text <- paste(yaml_lines, collapse = "\n")
          updateTextAreaInput(session, "author_yaml_code", value = yaml_text)
        }
      }
    })

    # Update project data reactively when inputs change
    # Track previous values to prevent unnecessary updates
    previous_values <- reactiveValues(
      project_title = NULL,
      project_subtitle = NULL,
      project_description = NULL,
      report_formats = NULL,
      project_keywords = NULL,
      report_date = NULL,
      project_funding = NULL,
      report_license = NULL
    )

    # Only update when values actually change
    observe({
      # Check if any values have changed
      current_values <- list(
        project_title = input$project_title,
        project_subtitle = input$project_subtitle,
        project_description = input$project_description,
        report_formats = input$report_formats,
        project_keywords = input$project_keywords,
        report_date = input$report_date,
        project_funding = input$project_funding,
        report_license = input$report_license
      )

      # Compare with previous values
      has_changes <- FALSE
      for (name in names(current_values)) {
        if (!identical(current_values[[name]], previous_values[[name]])) {
          has_changes <- TRUE
          previous_values[[name]] <- current_values[[name]]
        }
      }

      # Only proceed if there are actual changes
      if (has_changes) {
        # Log the change for debugging
        logger::log_info("Detected changes in project data, updating...")

        details_saved(FALSE)

        # Create a local copy of project_info to modify
        project_info <- isolate({
          if (is.reactivevalues(project_data$project_info)) {
            reactiveValuesToList(project_data$project_info)
          } else if (is.list(project_data$project_info)) {
            project_data$project_info
          } else {
            list() # Return empty list if structure is unexpected
          }
        })

        # Update report date
        project_info$report_date <- tryCatch(
          {
            if (!is.null(input$report_date) && !is.na(input$report_date)) {
              as.character(as.Date(input$report_date))
            } else {
              ""
            }
          },
          error = function(e) {
            ""
          }
        )

        # Update title if not null
        if (!is.null(input$project_title)) {
          project_info$title <- input$project_title
        }

        # Update other fields
        if (!is.null(input$project_subtitle)) project_info$subtitle <- input$project_subtitle
        if (!is.null(input$project_description)) project_info$description <- input$project_description
        if (!is.null(input$project_keywords)) project_info$keywords <- input$project_keywords
        if (!is.null(input$project_funding)) project_info$funding <- input$project_funding
        if (!is.null(input$report_license)) project_info$license <- input$report_license

        # Handle report formats
        if (!is.null(input$report_formats)) {
          project_info$report_formats <- input$report_formats
        }

        # Handle keywords specifically as they need special processing
        if (!is.null(input$project_keywords)) {
          # Split keywords by comma and trim whitespace
          keywords <- trimws(unlist(strsplit(input$project_keywords, ",")))
          project_info$report_keywords <- keywords[keywords != ""]
        }

        # Update the project_data reactively
        isolate({
          for (name in names(project_info)) {
            project_data$project_info[[name]] <- project_info[[name]]
          }
          # Force a reactive update by modifying a dummy value
          project_data$last_updated <- Sys.time()

          # Log the update for debugging
          logger::log_info(
            "Project data updated. Title:",
            project_data$project_info$title,
            "Date:",
            project_data$project_info$report_date
          )
        })
      }
    })
    # This observer has been moved and combined with form validation above

    # Create a reactive value to trigger UI updates
    trigger_ui_update <- reactiveVal(0)

    # Observer for the initiate project button
    observeEvent(input$initiate_project, {
      logger::log_info("=== Initiate Project Button Clicked ===")

      # Always make sure the status container is visible when this button is clicked
      show_status_line("Initiating project...", "success")

      # Update report formats
      if (!is.null(input$report_formats)) {
        report_formats(input$report_formats)
      }

      # Debug: Log all input values
      logger::log_info("Validating form with values:")
      logger::log_info(sprintf("Project Title: %s", input$project_title))
      logger::log_info(sprintf("Report Date: %s", input$report_date))
      logger::log_info(sprintf("Report Formats: %s", paste(input$report_formats, collapse = ", ")))

      # Use shinyvalidate to check all validation rules
      validation_result <- iv$validate()

      if (!iv$is_valid()) {
        # Get all validation failures
        failures <- Filter(Negate(is.null), validation_result)
        logger::log_warn(sprintf("Form validation failed with %d errors", length(failures)))

        # Log each failure
        for (i in seq_along(failures)) {
          logger::log_info(sprintf("Validation failure %d: %s", i, failures[[i]]))
        }

        # Get the first validation error message
        first_error <- if (length(failures) > 0) {
          # Get the first non-NULL error message
          msg <- failures[[1]]

          if (is.character(msg) && length(msg) > 0) {
            msg[1]
          } else if (is.list(msg) && "message" %in% names(msg)) {
            msg$message
          } else {
            "Please check the form for errors"
          }
        } else {
          "Please check the form for errors"
        }

        # Update status line with specific error message
        show_status_line(first_error, "warning")
        details_saved(FALSE)

        return()
      } else {
        logger::log_info("Form validation passed")
        show_status_line("Project initiated successfully!", "success")
      }

      logger::log_info("Form validation passed - all fields are valid")
      logger::log_info("Project Title: ", input$project_title)
      logger::log_info("Project Description: ", input$project_description)
      logger::log_info("Report Formats: ", paste(input$report_formats, collapse = ", "))
      logger::log_info("Report Keywords: ", paste(input$project_keywords, collapse = ", "))
      logger::log_info("Report Date: ", input$report_date)

      # Explicitly update all project info fields when saving
      project_data$project_info$title <- input$project_title
      project_data$project_info$description <- input$project_description
      project_data$project_info$report_formats <- input$report_formats

      # Process keywords - split by comma and trim whitespace
      if (!is.null(input$project_keywords) &&
        is.character(input$project_keywords) &&
        nzchar(trimws(input$project_keywords)[1])) {
        keywords <- trimws(unlist(strsplit(input$project_keywords, ",")))
        project_data$project_info$report_keywords <- keywords[keywords != ""]
      } else {
        project_data$project_info$report_keywords <- character(0)
      }

      # Update report date
      project_data$project_info$report_date <- tryCatch(
        {
          if (!is.null(input$report_date) && !is.na(input$report_date)) {
            as.character(as.Date(input$report_date))
          } else {
            ""
          }
        },
        error = function(e) {
          ""
        }
      )

      # Mark setup as complete
      project_data$status$setup_complete <- TRUE
      details_saved(TRUE)

      # Trigger a UI update to refresh the sidebar
      logger::log_info("Triggering sidebar update after project initialization")
      session$sendCustomMessage("update_sidebar", "")

      # Log the updated project data for debugging
      logger::log_info("Updated project_data$project_info: ")
      logger::log_info("Title: ", project_data$project_info$title)
      logger::log_info("Description: ", project_data$project_info$description)
      logger::log_info("Report Formats: ", paste(project_data$project_info$report_formats, collapse = ", "))
      logger::log_info("Report Keywords: ", paste(project_data$project_info$report_keywords, collapse = ", "))
      logger::log_info("Report Date: ", project_data$project_info$report_date)
      logger::log_info("Authors count: ", length(project_data$project_info$authors))
      if (length(project_data$project_info$authors) > 0) {
        logger::log_info("First author: ", project_data$project_info$authors[[1]]$name)
      }

      # Show success message in status bar and make it visible
      status_message("Project details saved successfully!")
      details_saved(TRUE)

      # Status display is handled by the reactive UI
      # Removed invalid shinyjs calls to non-existent elements

      # Force UI refresh
      trigger_ui_update(trigger_ui_update() + 1)
    })

    output$download_authors <- downloadHandler(
      filename = function() "authors.yml",
      content = function(file) {
        # Get the YAML content from the Ace editor with NULL check
        yaml_content <- if (!is.null(input$author_yaml_code)) input$author_yaml_code else ""
        # Write the content to the file
        writeLines(yaml_content, file)
      },
      contentType = "text/yaml"
    )



    # Reactive value to store report formats
    report_formats <- reactiveVal(character(0))

    # Report formats are now updated in the main initiate_project handler

    # Create a reactive value to store the icons UI
    report_icons_ui <- reactive({
      # Use the stored report formats instead of direct input
      formats <- report_formats()
      if (length(formats) == 0) {
        return(NULL)
      }

      # Map each format to a Font Awesome icon
      icons_map <- list(
        "html" = ph("file-html", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "pdf" = ph("file-pdf", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "docx" = ph("file-doc", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "pptx" = ph("file-ppt", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "latex" = ph("file-tsx", weight = "light", class = "sidebar-icon", style = "color: #ffffff;"),
        "typst" = ph("file-ts", weight = "light", class = "sidebar-icon", style = "color: #ffffff;")
      )

      # Create a span for each selected icon
      spans <- lapply(formats, function(choice) {
        if (choice %in% names(icons_map)) {
          tags$span(icons_map[[choice]])
        }
      })
      tagList(Filter(Negate(is.null), spans))
    })

    # Create reactives to return project data
    project_title <- reactive({
      if (!is.null(input$project_title) && nchar(trimws(input$project_title)) > 0) {
        input$project_title
      } else {
        ""
      }
    })

    project_description <- reactive({
      if (!is.null(input$project_description) && nchar(trimws(input$project_description)) > 0) {
        input$project_description
      } else {
        ""
      }
    })

    project_keywords <- reactive({
      if (!is.null(input$project_keywords) && length(input$project_keywords) > 0) {
        input$project_keywords
      } else {
        character(0)
      }
    })

    project_report_formats <- reactive({
      if (!is.null(input$report_formats) && length(input$report_formats) > 0) {
        input$report_formats
      } else {
        character(0)
      }
    })

    # Author information reactives
    author_name <- reactive({
      if (!is.null(input$author_name) && nchar(trimws(input$author_name)) > 0) {
        input$author_name
      } else {
        ""
      }
    })

    author_affiliation <- reactive({
      if (!is.null(input$author_affiliation) && nchar(trimws(input$author_affiliation)) > 0) {
        input$author_affiliation
      } else {
        ""
      }
    })

    author_orcid <- reactive({
      if (!is.null(input$author_orcid) && nchar(trimws(input$author_orcid)) > 0) {
        input$author_orcid
      } else {
        ""
      }
    })

    author_email <- reactive({
      if (!is.null(input$author_email) && nchar(trimws(input$author_email)) > 0) {
        input$author_email
      } else {
        ""
      }
    })

    author_url <- reactive({
      if (!is.null(input$author_url) && nchar(trimws(input$author_url)) > 0) {
        input$author_url
      } else {
        ""
      }
    })

    # Combined author information
    author_info <- reactive({
      list(
        name = author_name(),
        affiliation = author_affiliation(),
        orcid = author_orcid(),
        email = author_email(),
        url = author_url()
      )
    })

    # Combined authors list
    authors_list <- reactive({
      # Ensure we have a list, even if empty
      if (length(authors$list) == 0) {
        list()
      } else {
        authors$list
      }
    })

    # Add missing project_report_date function
    project_report_date <- reactive({
      if (!is.null(input$report_date)) {
        input$report_date
      } else {
        Sys.Date()
      }
    })

    # Combined project information
    project_info <- reactive({
      # Get the current authors list
      current_authors <- authors$list
      logger::log_info("Project info authors count: ", length(current_authors))

      # Update project_data directly to ensure it contains authors
      project_data$project_info$authors <- current_authors

      list(
        title = project_title(),
        description = project_description(),
        report_date = project_report_date(),
        report_keywords = project_keywords(),
        report_formats = project_report_formats(),
        authors = current_authors # Use the complete list of authors with proper structure
      )
    })

    # Toast notification code has been removed

    # Return the reactive UI and data for use in the main app
    return(
      list(
        icons = report_icons_ui,
        # Individual project data
        title = reactive({
          if (is.reactive(project_title)) project_title() else project_title
        }),
        description = reactive({
          if (is.reactive(project_description)) project_description() else project_description
        }),
        report_date = reactive({
          if (shiny::is.reactivevalues(project_data) &&
            !is.null(project_data$project_info) &&
            !is.null(project_data$project_info$report_date)) {
            project_data$project_info$report_date
          } else {
            NULL
          }
        }),
        keywords = reactive({
          if (is.reactive(project_keywords)) project_keywords() else project_keywords
        }),
        report_formats = reactive({
          if (is.reactive(project_report_formats)) project_report_formats() else project_report_formats
        }),
        # Individual author data
        author_name = reactive({
          if (is.reactive(author_name)) author_name() else author_name
        }),
        author_affiliation = reactive({
          if (is.reactive(author_affiliation)) author_affiliation() else author_affiliation
        }),
        author_orcid = reactive({
          if (is.reactive(author_orcid)) author_orcid() else author_orcid
        }),
        author_email = reactive({
          if (is.reactive(author_email)) author_email() else author_email
        }),
        author_url = reactive({
          if (is.reactive(author_url)) author_url() else author_url
        }),
        # Combined data
        author_info = reactive({
          if (is.reactive(author_info)) author_info() else author_info
        }),
        authors_list = reactive({
          if (is.reactive(authors_list)) authors_list() else authors_list
        }),
        project_info = reactive({
          if (is.reactive(project_info)) project_info() else project_info
        }),
        # Branding configuration status
        branding_saved = branding_config_saved
      )
    )

    # Use a reactive expression to update the status line
    output$project_status_ui <- renderUI({
      msg <- status_message()
      if (!is.null(msg)) {
        # Determine the alert class based on saved state
        alert_class <- if (details_saved()) "alert-success" else "alert-warning"

        # Create the status line UI
        div(
          id = ns("status_line_container"),
          class = paste("alert", alert_class, "d-flex align-items-center"),
          style = "margin-bottom: 1.25em; display: block !important;",
          if (details_saved()) {
            phosphoricons::ph("check-circle", weight = "fill", class = "me-2")
          } else {
            phosphoricons::ph("warning", weight = "fill", class = "me-2")
          },
          tags$span(msg, style = "text-transform: uppercase;")
        )
      }
    })

    # Improved change detection observer
    observeEvent(input$branding_yaml_code,
      {
        req(!is.null(input$branding_yaml_code))

        # Compare with original content
        current_content <- trimws(input$branding_yaml_code)
        original_content <- trimws(original_branding_content())

        logger::log_trace("Branding YAML change detected")
        logger::log_trace("Current length:", nchar(current_content))
        logger::log_trace("Original length:", nchar(original_content))

        # Only show warning if content has actually changed
        if (!identical(current_content, original_content)) {
          show_branding_status_line("UNSAVED CHANGES - CLICK SAVE DESIGN TO SAVE", "warning")
          logger::log_trace("Content changed - showing warning")
        } else {
          # Content matches original - check saved state
          if (branding_config_saved()) {
            show_branding_status_line("BRANDING SAVED", "success")
          } else {
            show_branding_status_line("BRANDING READY FOR EDITING", "info")
          }
          logger::log_trace("Content unchanged - showing saved/ready state")
        }
      },
      ignoreInit = TRUE
    )



    # Removed - save logic moved to alternative observer above

    # Debugging helper observer
    observe({
      logger::log_trace(
        "Branding status line state - Message:", branding_status_line$message,
        "Type:", branding_status_line$type,
        "Timestamp:", branding_status_line$timestamp
      )
    })
  })
}
