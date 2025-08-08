#!! ModName = mod_load_clean

# Debug message to verify module is loaded
logger::log_info("mod_load_clean module is being loaded")

# Import necessary functions
#' @importFrom dplyr filter select mutate rename case_when everything left_join group_by summarise ungroup arrange
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_starts
#' @importFrom shiny showNotification HTML tagList tags NS icon showModal modalDialog modalButton
#' @importFrom shinyjs useShinyjs delay html runjs
#' @importFrom utils str
#' @importFrom logger log_info log_error log_warn log_debug
#' @importFrom stats setNames
#' @importFrom phosphoricons ph
#' @importFrom bslib card_header card_body card accordion accordion_panel
#' @importFrom yaml yaml.load as.yaml
#' @importFrom visNetwork visNetwork visOptions visPhysics visLayout visLegend
#' @importFrom DT datatable
#' @importFrom igraph graph_from_data_frame V E layout_with_fr
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom shinyWidgets progressBar updateProgressBar
# Note: utils functions are sourced in global.R, so they are available globally
# !! ModDisplayName = Load and Clean Data
# !! ModDescription = Load and clean project data
# !! ModCitation = Price, Jeremy F. (2025). mod_load_clean. [Source code].
# !! ModNotes = This module provides functionality to load and clean project data.
# !! ModActive = 1
# !! FunctionArg = project_data !! Project data for loading and cleaning !! reactive

# Utilities are loaded in global.R

# =================================================================================================
# UI Function
# =================================================================================================

mod_load_clean_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(), # Initialize shinyjs
    tags$head(
      tags$style(HTML("
        /* Custom modal styling */
        .modal-content {
          border-radius: 8px;
          box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        }
        .modal-header {
          background-color: #f5f1e8;
          border-bottom: 1px solid #e8ddd4;
          border-radius: 8px 8px 0 0;
        }
        .modal-title {
          color: #48443C;
          font-weight: 600;
        }
        .modal-body {
          background-color: #ffffff;
          padding: 15px;
        }
        .modal-footer {
          background-color: #f5f1e8;
          border-top: 1px solid #e8ddd4;
          border-radius: 0 0 8px 8px;
        }
        .modal-backdrop.in {
          opacity: 0.3;
        }
        /* DataTables styling */
        .dataTables_wrapper {
          padding: 10px 0;
        }
        table.dataTable thead th {
          background-color: #f5f1e8;
          color: #48443C;
          font-weight: 600;
        }
        table.dataTable tbody tr:hover {
          background-color: rgba(245, 241, 232, 0.5);
        }
        /* Progress bar styling */
      ")),
      tags$style(HTML(paste0(
        "#", ns("progress_container"), " {
          margin: 10px 0;
          width: 100%;
        }",
        "#", ns("progress_bar_clean"), " {
          height: 30px !important;
          font-size: 14px !important;
          line-height: 30px !important;
        }",
        ".overlay-container {
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          z-index: 9999;
          display: flex;
          justify-content: center;
          align-items: center;
        }",
        ".overlay-bg {
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background: rgba(0,0,0,0.3); /* More transparent background */
        }",
        ".overlay-card {
          background: #edeae2;
          padding: 20px;
          border-radius: 8px;
          max-width: 90%;
          max-height: 90%;
          overflow-y: auto;
          position: relative;
          z-index: 10000;
        }"
      ))),
      # Add custom JavaScript for progress bar updates
      tags$script(HTML(
        "Shiny.addCustomMessageHandler('updateProgressBar', function(message) {
          var $bar = $('#' + message.id);
          if ($bar.length) {
            $bar.css('width', message.value + '%')
               .attr('aria-valuenow', message.value)
               .find('.progress-bar')
                 .css('width', message.value + '%')
                 .text(message.text || '');
          }
        });"
      ))
    ),
    bslib::accordion(
      id = ns("load_clean_accordion"),
      multiple = FALSE,
      bslib::accordion_panel(
        value = "overview",
        title = tagList(
          ph("pencil-ruler"),
          HTML("&nbsp;"),
          "Upload & Clean",
          div(
            style = "position: absolute; right: 50px; top: 50%; transform: translateY(-50%);",
            img(
              src = "load.png",
              alt = "Load and Clean Image",
              style = "height: 54px; width: auto; max-width: 100%;"
            )
          )
        ),
        fluidRow(
          width = 12,
          p(
            "This module allows you to load and clean your project data
              efficiently. Follow the steps below to prepare your data for
              analysis."
          ),
          tags$fieldset(
            class = "custom-fieldset",
            tags$legend(
              "Select",
              class = "custom-legend"
            ),
            layout_columns(
              col_widths = c(6, 6),
              shiny::fileInput(
                ns("alignment_data_file"),
                "Alignment Data File",
                accept = c(".csv", "text/csv"),
                buttonLabel = tagList(
                  phosphoricons::ph("flower-lotus", weight = "light"),
                  " Choose File"
                ),
                placeholder = "No file selected",
                width = "100%"
              ),
              shiny::fileInput(
                ns("main_data_file"),
                "Main Data File",
                accept = c(".csv", "text/csv"),
                buttonLabel = tagList(
                  phosphoricons::ph("database", weight = "light"),
                  " Choose File"
                ),
                placeholder = "No file selected",
                width = "100%"
              )
            )
          ),
          tags$fieldset(
            class = "custom-fieldset",
            tags$legend(
              "Clean",
              class = "custom-legend"
            ),
            fluidRow(
              class = "d-flex justify-content-center align-items-center",
              actionButton(
                inputId = ns("clean_data"),
                label = tagList(
                  "Clean Data ",
                  ph("spray-bottle", weight = "bold")
                ),
                width = "50%",
                class = "btn btn-primary btn-lg"
              )
            ),
            fluidRow(
              class = "d-flex justify-content-center",
              shinyjs::hidden(
                div(
                  id = ns("progress_container"),
                  style = "width: 50%; margin-top: 1em;",
                  shinyWidgets::progressBar(
                    id = ns("progress_bar_clean"),
                    value = 0,
                    total = 100,
                    status = "success",
                    title = "Ready to Clean"
                  )
                )
              )
            )
          )
          # shiny::column(
          #   width = 1,
          #   shiny::img(
          #     src = "load.png", alt = "Data Load and Clean Image",
          #     style = "width: auto; max-width: 100%; max-height: 7em; margin-top: 0.5em; padding-left: auto; padding-right: 0.5em;"
          #   )
          # )
        )
      ),
      bslib::accordion_panel(
        value = "results",
        title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
        uiOutput(ns("results_ui"))
      )
    )
  )
}

# =================================================================================================
# Server Function
# =================================================================================================
mod_load_clean_server <- function(id, project_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    rv <- reactiveValues(
      main = NULL,
      alignment = NULL,
      indicators = NULL,
      dynamics = NULL,
      cascade = NULL
    ) # closes reactiveValues

    # Check if we have restored raw data from a snapshot and simulate file uploads
    observe({
      # Debug current state
      logger::log_info("=== DATA RESTORATION CHECK ===")
      logger::log_info("Raw main data present: {!is.null(project_data$raw_main_data)}")
      logger::log_info("Raw alignment data present: {!is.null(project_data$raw_alignment_data)}")
      logger::log_info("Cleaned main data present: {!is.null(project_data$cleaned_data$indicators) || !is.null(project_data$cleaned_data$dynamics) || !is.null(project_data$cleaned_data$cascade)}")
      logger::log_info("Cleaned alignment data present: {!is.null(project_data$cleaned_data$alignment)}")
      logger::log_info("Current rv$main_data_file: {!is.null(rv$main_data_file)}")
      logger::log_info("Current rv$alignment_data_file: {!is.null(rv$alignment_data_file)}")
      logger::log_info("Data loaded status: {project_data$status$data_loaded}")

      # Check for restored main data
      if (!is.null(project_data$raw_main_data) && is.null(rv$main_data_file)) {
        # Create a mock file input object to simulate uploaded file
        rv$main_data_file <- list(
          name = "restored_main_data.csv",
          datapath = NULL, # Will use raw data directly
          type = "text/csv"
        )
        logger::log_info("<U+2713> Restored main data from snapshot: {nrow(project_data$raw_main_data)} rows")
      }

      # Check for restored alignment data
      if (!is.null(project_data$raw_alignment_data) && is.null(rv$alignment_data_file)) {
        # Create a mock file input object to simulate uploaded file
        rv$alignment_data_file <- list(
          name = "restored_alignment_data.csv",
          datapath = NULL, # Will use raw data directly
          type = "text/csv"
        )
        logger::log_info("<U+2713> Restored alignment data from snapshot: {nrow(project_data$raw_alignment_data)} rows")
      }

      logger::log_info("=== END DATA RESTORATION CHECK ===")
    })

    # Observer for main data file upload
    observeEvent(input$main_data_file, {
      req(input$main_data_file)
      rv$main_data_file <- input$main_data_file
      # Don't update progress bar here, only on Clean Data button click
    }) # closes observeEvent (main_data_file)

    # Observer for alignment data file upload
    observeEvent(input$alignment_data_file, {
      req(input$alignment_data_file)
      rv$alignment_data_file <- input$alignment_data_file
      # Don't update progress bar here, only on Clean Data button click
    }) # closes observeEvent (alignment_data_file)

    # Observer for the clean data button
    observeEvent(input$clean_data, {
      logger::log_info("Clean Data button clicked")

      # Show the progress bar container
      shinyjs::show(id = "progress_container", anim = TRUE, animType = "fade")

      # Check if any files are uploaded
      if (is.null(rv$main_data_file) && is.null(rv$alignment_data_file)) {
        logger::log_warn("No files uploaded for cleaning")
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar_clean"),
          value = 0,
          status = "warning",
          title = "No files uploaded! Please upload files first."
        )
        # Hide the progress bar container after 3 seconds
        shinyjs::delay(3000, {
          shinyjs::hide(id = "progress_container", anim = TRUE, animType = "fade")
          shinyWidgets::updateProgressBar(
            session = session,
            id = ns("progress_bar_clean"),
            value = 0,
            status = "success",
            title = "Ready to Clean"
          )
        })
        return()
      }

      # --- PROGRESS BAR LOGIC ---

      # 1. Show progress bar and set up update function
      shinyjs::show(id = "progress_container", anim = TRUE, animType = "fade")
      update_progress <- function(value, text) {
        logger::log_info(sprintf("Progress: %d%%, Status: %s", value, text))
        shinyWidgets::updateProgressBar(
          session = session,
          id = ns("progress_bar_clean"),
          value = value,
          title = text
        )
      } # closes update_progress

      # 2. Initialize progress bar with starting message
      update_progress(0, "Starting data cleaning...")

      tryCatch(
        {
          # Clean main data if uploaded
          if (!is.null(rv$main_data_file)) {
            update_progress(20, "Reading main data file...")
            # Check if this is restored data (no datapath) or a fresh upload
            if (is.null(rv$main_data_file$datapath) && !is.null(project_data$raw_main_data)) {
              # Use restored raw data
              main_data_raw <- project_data$raw_main_data
              logger::log_info("Using restored main data from snapshot")
            } else {
              # Read from uploaded file
              main_data_raw <- readr::read_csv(rv$main_data_file$datapath)
            }
            logger::log_info("Main data file read successfully: {rv$main_data_file$name}")
            logger::log_info("Main data raw dimensions: {nrow(main_data_raw)} rows, {ncol(main_data_raw)} columns")

            # Save raw data to project_data for snapshot functionality
            project_data$raw_main_data <- main_data_raw

            update_progress(40, "Cleaning main data...")
            logger::log_info("Starting clean_data function...")
            main_cleaned <- clean_data(main_data_raw)
            logger::log_info("Main data cleaned successfully")
            logger::log_info("Cleaned data structure: {paste(names(main_cleaned), collapse = ', ')}")

            # Store cleaned data in reactive values
            rv$indicators <- main_cleaned$indicators
            rv$dynamics <- main_cleaned$dynamics
            rv$cascade <- main_cleaned$cascade
            qualtrics_metadata_removed <- isTRUE(main_cleaned$qualtrics_metadata_removed)

            # Log what was stored in local reactive values
            logger::log_info("Local reactive values updated:")
            logger::log_info("  - indicators: {if(!is.null(rv$indicators)) nrow(rv$indicators) else 'NULL'} rows")
            logger::log_info("  - dynamics: {if(!is.null(rv$dynamics)) nrow(rv$dynamics) else 'NULL'} rows")
            logger::log_info("  - cascade: {if(!is.null(rv$cascade)) if(is.list(rv$cascade)) paste(names(rv$cascade), collapse=', ') else class(rv$cascade)}")

            # Update project data with all cleaned data for other modules to access
            if (!is.null(main_cleaned$indicators)) {
              project_data$cleaned_data$indicators <- main_cleaned$indicators
              logger::log_info("Indicators data updated in project data: {nrow(main_cleaned$indicators)} rows, {ncol(main_cleaned$indicators)} columns")
            }
            if (!is.null(main_cleaned$dynamics)) {
              project_data$cleaned_data$dynamics <- main_cleaned$dynamics
              logger::log_info("Dynamics data updated in project data: {nrow(main_cleaned$dynamics)} rows, {ncol(main_cleaned$dynamics)} columns")
            }
            if (!is.null(main_cleaned$cascade)) {
              project_data$cleaned_data$cascade <- main_cleaned$cascade
              logger::log_info("Cascade data updated in project data: {if(is.list(main_cleaned$cascade)) paste(names(main_cleaned$cascade), collapse=', ') else class(main_cleaned$cascade)}")
            }
          }
          project_data$qualtrics_metadata_removed <- isTRUE(main_cleaned$qualtrics_metadata_removed)
          # Clean alignment data if uploaded
          if (!is.null(rv$alignment_data_file)) {
            update_progress(80, "Cleaning alignment data...")
            # Check if this is restored data (no datapath) or a fresh upload
            if (is.null(rv$alignment_data_file$datapath) && !is.null(project_data$raw_alignment_data)) {
              # Use restored raw data
              alignment_data_raw <- project_data$raw_alignment_data
              logger::log_info("Using restored alignment data from snapshot")
            } else {
              # Read from uploaded file
              alignment_data_raw <- readr::read_csv(rv$alignment_data_file$datapath)
            }
            logger::log_info("Alignment data file read successfully: {rv$alignment_data_file$name}")
            logger::log_info("Alignment data raw dimensions: {nrow(alignment_data_raw)} rows, {ncol(alignment_data_raw)} columns")

            # Save raw alignment data to project_data for snapshot functionality
            project_data$raw_alignment_data <- alignment_data_raw

            update_progress(80, "Cleaning alignment data...")
            logger::log_info("Starting clean_alignment_data function...")
            alignment_cleaned <- clean_alignment_data(alignment_data_raw)
            # alignment_cleaned is now a list: $data, $qualtrics_metadata_removed
            rv$alignment <- alignment_cleaned$data
            alignment_qualtrics_metadata_removed <- isTRUE(alignment_cleaned$qualtrics_metadata_removed)
            logger::log_info("Alignment data cleaned successfully")
            logger::log_info("Cleaned alignment data dimensions: {nrow(rv$alignment)} rows, {ncol(rv$alignment)} columns")

            # Update project data with alignment data for other modules to access
            project_data$cleaned_data$alignment <- rv$alignment
            logger::log_info("Alignment data updated in project data: {nrow(rv$alignment)} rows, {ncol(rv$alignment)} columns")
          }

          update_progress(100, "Finalizing...")

          # Log what data was shared with the project data
          logger::log_info("=== DATA SHARING SUMMARY ===")
          if (!is.null(project_data$cleaned_data$alignment)) {
            logger::log_info("Alignment data shared: {nrow(project_data$cleaned_data$alignment)} rows, {ncol(project_data$cleaned_data$alignment)} columns")
          } else {
            logger::log_info("No alignment data shared")
          }
          if (!is.null(project_data$cleaned_data$dynamics)) {
            logger::log_info("Dynamics data shared: {nrow(project_data$cleaned_data$dynamics)} rows, {ncol(project_data$cleaned_data$dynamics)} columns")
          } else {
            logger::log_info("No dynamics data shared")
          }
          if (!is.null(project_data$cleaned_data$cascade)) {
            if (is.list(project_data$cleaned_data$cascade)) {
              logger::log_info("Cascade data shared: list with elements: {paste(names(project_data$cleaned_data$cascade), collapse = ', ')}")
            } else {
              logger::log_info("Cascade data shared: {class(project_data$cleaned_data$cascade)}")
            }
          } else {
            logger::log_info("No cascade data shared")
          }
          logger::log_info("=== END DATA SHARING SUMMARY ===")

          # Mark data as loaded in project status
          project_data$status$data_loaded <- TRUE
          logger::log_info("<U+2713> Data loading status set to TRUE")
          logger::log_info("<U+2713> Data cleaning completed successfully")

          # Check if this was a restoration from snapshot
          is_restoration <- (!is.null(rv$main_data_file) && is.null(rv$main_data_file$datapath)) ||
            (!is.null(rv$alignment_data_file) && is.null(rv$alignment_data_file$datapath))

          if (is_restoration) {
            logger::log_info("<U+2713> Data restored from snapshot - cleaning state verified")
          }

          # On success, update status and reset progress bar after 3 seconds
          update_progress(100, "Data cleaning complete!")
          # Add a small delay before resetting the progress bar
          shinyjs::delay(3000, {
            update_progress(0, "Ready to clean")
            shinyjs::hide(id = "progress_container", anim = TRUE, animType = "fade")
          })

          # Switch to the results accordion panel
          tryCatch(
            {
              bslib::accordion_panel_set(
                id = "load_clean_accordion",
                value = "results",
                session = session
              )
            },
            error = function(e) {
              logger::log_error(sprintf("Error switching to results accordion panel: %s", conditionMessage(e)))
            }
          )
        },
        error = function(e) {
          logger::log_error("Error during data cleaning: {conditionMessage(e)}")

          # On error, update status and reset progress bar
          update_progress(100, "Error during data cleaning!")
          shinyjs::delay(3000, {
            update_progress(0, "Ready to clean")
            shinyjs::hide(id = "progress_container", anim = TRUE, animType = "fade")
          })
        }
      ) # closes tryCatch
    }) # closes observeEvent (clean_data)

    # --- NEW RENDERUI LOGIC FOR INLINE ALERTS ---
    # (REMOVED)

    # Auto-hide logic for success/info/warning messages
    # (REMOVED)

    output$cascade_ui <- renderUI({
      if (is.null(project_data$cleaned_data$cascade)) {
        return(
          tags$div(
            class = "data-placeholder",
            tags$div(
              class = "d-flex align-items-center justify-content-center gap-2",
              ph("warning-circle", weight = "bold", class = "warning-icon"),
              tags$div(
                tags$strong("No Cascade Effects Data"),
                tags$br(),
                "Upload and clean your data to view cascade effects here."
              )
            )
          ) # closes return (no cascade data)
        )
      }
      shiny::fluidRow(
        shiny::column(
          width = 6,
          create_flat_info_card(
            ns,
            title = "Network Parameters",
            icon = "sliders-horizontal",
            content = tagList(
              div(
                shiny::downloadButton(ns("download_cascade_yaml"),
                  label = "Download Parameters",
                  icon = shiny::icon("download"),
                  class = "btn-sm",
                  style = "background-color: #3B6B35; color: white;\n                          border: none;"
                )
              ),
              div(
                shinyAce::aceEditor(
                  outputId = ns("cascade_yaml_editor"),
                  mode = "yaml",
                  theme = "github",
                  height = "320px",
                  fontSize = 14,
                  debounce = 750,
                  autoScrollEditorIntoView = TRUE,
                  highlightActiveLine = TRUE
                )
              )
            )
          ) # closes create_flat_info_card (parameters)
        ), # closes shiny::column (parameters)
        shiny::column(
          width = 6,
          create_flat_info_card(
            ns,
            title = "Network Plot",
            icon = "graph",
            content = tagList(
              plotOutput(ns("static_network_plot"), height = "300px"),
              # Add legend
              shiny::div(
                style = "text-align: center;",
                shiny::h5("Legend"),
                shiny::div(
                  style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 15px;",
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #1E325C; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("Researcher")
                  ),
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #3B6B35; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("Community Member")
                  ),
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #A64B42; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("2nd Degree")
                  ),
                  shiny::div(
                    style = "display: flex; align-items: center; margin: 5px;",
                    shiny::div(
                      style = "width: 15px; height: 15px; background-color:\n                    #E0D0A6; margin-right: 5px; border-radius: 3px;"
                    ),
                    shiny::span("3rd Degree")
                  )
                )
              )
            )
          ) # closes create_flat_info_card (network plot)
        ) # closes shiny::column (network plot)
      ) # closes shiny::fluidRow
    }) # closes renderUI (cascade_ui)

    # --- Cascade Effects YAML and Network Plot ---
    # Load default YAML content
    observe({
      # Only proceed if we have cascade data
      req(!is.null(project_data$cleaned_data$cascade))
      tryCatch(
        {
          # Use the YAML model from cascade analysis if available
          if (!is.null(project_data$cleaned_data$cascade$model)) {
            yaml_content <- yaml::as.yaml(project_data$cleaned_data$cascade$model)
            logger::log_info("Cascade YAML editor populated from cascade analysis model.")
          } else {
            yaml_content <- generate_cascade_yaml(project_data$cleaned_data$cascade)
            logger::log_info("Cascade YAML editor populated with default template.")
          }
          shinyAce::updateAceEditor(session, "cascade_yaml_editor", value = yaml_content)
        },
        error = function(e) {
          logger::log_error("Error generating or updating cascade YAML: {conditionMessage(e)}")
          # Removed shiny::showNotification("Failed to generate cascade parameters.", type = "error")
        }
      ) # closes tryCatch (cascade YAML)
    }) # closes observe (cascade YAML)

    # Download handler for YAML
    output$download_cascade_yaml <- downloadHandler(
      filename = function() {
        paste0("cascade-parameters-", Sys.Date(), ".yml")
      },
      content = function(file) {
        writeLines(input$cascade_yaml_editor, file)
      }
    ) # closes downloadHandler (download_cascade_yaml)

    # Render the static network plot
    output$static_network_plot <- renderPlot({
      req(ns_project$cascade_data)
      tryCatch(
        {
          create_static_network_plot(ns_project$cascade_data)
        },
        error = function(e) {
          logger::log_error("Error rendering static network plot: {conditionMessage(e)}")
          plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
          text(1, 1, "Error creating network plot", col = "red")
        }
      ) # closes tryCatch (static_network_plot)
    }) # closes renderPlot (static_network_plot)

    # --- Overlay Card UI and Server Logic for Results Panel ---
    # Helper for overlay card UI
    overlay_card_ui <- function(id, title, content_ui) {
      ns <- NS(id)
      uiOutput(ns("overlay_ui"))
    } # closes overlay_card_ui

    # Add overlay UI outputs to the UI (already in UI via uiOutput)

    # We're replacing the overlay_card_server with direct button handlers above

    # Indicators overlay - Direct button handler
    observeEvent(input$view_indicators_overlay, {
      logger::log_info("Indicators view button clicked")

      # Create a modal dialog instead of shinyalert
      showModal(modalDialog(
        title = "Cleaned Indicators Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto; padding: 0;", # Constrain height with no padding
        DT::dataTableOutput(ns("indicators_modal_table")),
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Render the data table inside the modal
      output$indicators_modal_table <- DT::renderDataTable({
        req(rv$indicators)
        DT::datatable(rv$indicators,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "ftip",
            scrollY = "50vh", # Constrain table height
            autoWidth = TRUE
          ),
          rownames = FALSE
        )
      })
    })
    output$indicators_overlay_table <- DT::renderDataTable({
      req(rv$indicators)
      DT::datatable(rv$indicators, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (indicators_overlay_table)
    output$indicators_overlay_ui <- renderUI({
      overlay_card_ui("indicators_overlay", "Cleaned Indicators Data", DT::dataTableOutput(ns("indicators_overlay_table")))
    }) # closes renderUI (indicators_overlay_ui)

    # Alignment overlay - Direct button handler
    observeEvent(input$view_alignment_overlay, {
      logger::log_info("Alignment view button clicked")

      # Create a modal dialog instead of shinyalert
      showModal(modalDialog(
        title = "Cleaned Alignment Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto; padding: 0;", # Constrain height with no padding
        DT::dataTableOutput(ns("alignment_modal_table")),
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Render the data table inside the modal
      output$alignment_modal_table <- DT::renderDataTable({
        req(rv$alignment)
        DT::datatable(rv$alignment,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "ftip",
            scrollY = "50vh", # Constrain table height
            autoWidth = TRUE
          ),
          rownames = FALSE
        )
      })
    })
    output$alignment_overlay_table <- DT::renderDataTable({
      req(rv$alignment)
      DT::datatable(rv$alignment, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (alignment_overlay_table)
    output$alignment_overlay_ui <- renderUI({
      overlay_card_ui("alignment_overlay", "Cleaned Alignment Data", DT::dataTableOutput(ns("alignment_overlay_table")))
    }) # closes renderUI (alignment_overlay_ui)

    # Dynamics overlay - Direct button handler
    observeEvent(input$view_dynamics_overlay, {
      logger::log_info("Dynamics view button clicked")

      # Create a modal dialog instead of shinyalert
      showModal(modalDialog(
        title = "Cleaned Dynamics Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto; padding: 0;", # Constrain height with no padding
        DT::dataTableOutput(ns("dynamics_modal_table")),
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Render the data table inside the modal
      output$dynamics_modal_table <- DT::renderDataTable({
        req(rv$dynamics)
        DT::datatable(rv$dynamics,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "ftip",
            scrollY = "50vh", # Constrain table height
            autoWidth = TRUE
          ),
          rownames = FALSE
        )
      })
    })
    output$dynamics_overlay_table <- DT::renderDataTable({
      req(rv$dynamics)
      DT::datatable(rv$dynamics, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (dynamics_overlay_table)
    output$dynamics_overlay_ui <- renderUI({
      overlay_card_ui("dynamics_overlay", "Cleaned Dynamics Data", DT::dataTableOutput(ns("dynamics_overlay_table")))
    }) # closes renderUI (dynamics_overlay_ui)

    # Cascade overlay - Direct button handler
    observeEvent(input$view_cascade_overlay, {
      logger::log_info("Cascade view button clicked")

      # Create a modal dialog instead of shinyalert
      showModal(modalDialog(
        title = "Cleaned Cascade Effects Data",
        size = "l",
        easyClose = TRUE,
        style = "max-height: 90vh; overflow-y: auto; padding: 0;", # Constrain height with no padding
        DT::dataTableOutput(ns("cascade_modal_table")),
        footer = tagList(
          modalButton("Close", icon = icon("circle-xmark"))
        )
      ))

      # Render the data table inside the modal
      output$cascade_modal_table <- DT::renderDataTable({
        req(project_data$cleaned_data$cascade)
        req(project_data$cleaned_data$cascade$edges)
        DT::datatable(project_data$cleaned_data$cascade$edges,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = "ftip",
            scrollY = "50vh", # Constrain table height
            autoWidth = TRUE
          ),
          rownames = FALSE
        )
      })
    })
    output$cascade_overlay_table <- DT::renderDataTable({
      req(project_data$cleaned_data$cascade)
      req(project_data$cleaned_data$cascade$edges)
      DT::datatable(project_data$cleaned_data$cascade$edges, options = list(scrollX = TRUE, pageLength = 10, dom = "ftip"), rownames = FALSE)
    }) # closes renderDataTable (cascade_overlay_table)
    output$cascade_overlay_ui <- renderUI({
      overlay_card_ui("cascade_overlay", "Cleaned Cascade Effects Data", DT::dataTableOutput(ns("cascade_overlay_table")))
    }) # closes renderUI (cascade_overlay_ui)

    output$results_ui <- renderUI({
      ns <- session$ns

      # Check if we have any cleaned data or if data loading is complete
      has_any_data <- !is.null(project_data$cleaned_data$indicators) ||
        !is.null(project_data$cleaned_data$alignment) ||
        !is.null(project_data$cleaned_data$dynamics) ||
        !is.null(project_data$cleaned_data$cascade) ||
        project_data$status$data_loaded

      # If no data at all, show a simple message
      if (!has_any_data) {
        return(
          div(
            class = "text-center mt-5",
            style = "padding: 40px;",
            ph("upload", weight = "light", size = "xl", style = "color: #dee2e6; margin-bottom: 20px;"),
            h4("No Data Available", style = "color: #6c757d; margin-bottom: 10px;"),
            p("Upload and clean your data to view results here.", style = "color: #6c757d;")
          )
        )
      }

      # Helper function to safely calculate completeness metrics
      calculate_completeness <- function(df) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) {
          return(list(data_completeness = 0, case_completeness = 0))
        }
        # Calculate data completeness
        data_completeness <- round(mean(!is.na(df)) * 100, 1)
        # Calculate case completeness
        case_completeness <- round(mean(complete.cases(df)) * 100, 1)
        list(
          data_completeness = data_completeness,
          case_completeness = case_completeness
        )
      }

      # Calculate completeness for each dataset
      indicators_metrics <- calculate_completeness(project_data$cleaned_data$indicators)
      alignment_metrics <- calculate_completeness(project_data$cleaned_data$alignment)
      dynamics_metrics <- calculate_completeness(project_data$cleaned_data$dynamics)
      cascade_metrics <- calculate_completeness(project_data$cleaned_data$cascade$edges)

      # Extract metrics
      indicators_data_completeness <- indicators_metrics$data_completeness
      indicators_case_completeness <- indicators_metrics$case_completeness
      alignment_data_completeness <- alignment_metrics$data_completeness
      alignment_case_completeness <- alignment_metrics$case_completeness
      dynamics_data_completeness <- dynamics_metrics$data_completeness
      dynamics_case_completeness <- dynamics_metrics$case_completeness
      cascade_data_completeness <- cascade_metrics$data_completeness
      cascade_case_completeness <- cascade_metrics$case_completeness

      # Check if data was restored from snapshot
      restoration_banner <- if ((!is.null(rv$main_data_file) && is.null(rv$main_data_file$datapath)) ||
        (!is.null(rv$alignment_data_file) && is.null(rv$alignment_data_file$datapath))) {
        div(
          class = "alert alert-info",
          style = "margin-bottom: 20px; border-left: 4px solid #5D7359;",
          div(
            class = "d-flex align-items-center",
            ph("check-circle", weight = "fill", size = "lg", class = "me-2", style = "color: #5D7359;"),
            div(
              tags$strong("Data Restored from Snapshot"),
              tags$br(),
              "Your uploaded and cleaned data has been successfully restored. Data is ready for analysis."
            )
          )
        )
      } else {
        NULL
      }

      # Create data status summary (only show when there are issues or during restoration)
      is_restoration <- (!is.null(rv$main_data_file) && is.null(rv$main_data_file$datapath)) ||
        (!is.null(rv$alignment_data_file) && is.null(rv$alignment_data_file$datapath))

      has_issues <- !project_data$status$data_loaded && !has_any_data

      data_status_summary <- if (is_restoration || has_issues) {
        div(
          class = "mb-3",
          style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 15px;",
          h5("Data Processing Status", class = "mb-3"),
          if (is_restoration) {
            div(
              class = "d-flex align-items-center mb-2",
              ph("database", weight = "fill", size = "sm", class = "me-2", style = "color: #17a2b8;"),
              span("Data restored from snapshot and ready for analysis", class = "text-info")
            )
          },
          if (has_issues) {
            div(
              class = "d-flex align-items-center",
              ph("clock", weight = "fill", size = "sm", class = "me-2", style = "color: #ffc107;"),
              span("Upload and clean your data to view results", class = "text-warning")
            )
          }
        )
      } else {
        NULL
      }

      # Use a single tagList to build the entire UI at once.
      # This is cleaner and avoids the errors from the previous structure.
      tagList(
        restoration_banner,
        data_status_summary,
        # -- INDICATORS SECTION --
        metric_section_ui(
          data = project_data$cleaned_data$indicators,

          # Value box parameters
          title = "Indicators Data",
          value = nrow(project_data$cleaned_data$indicators),
          round_to = 0,
          value_subtitle = "Records Processed",

          # Card parameters
          card_header_text = "Ready for Analysis",
          card_body = create_load_clean_taglist(
            data_completeness = indicators_data_completeness,
            case_completeness = indicators_case_completeness,
            qualtrics_metadata_removed = project_data$qualtrics_metadata_removed
          ),
          card_footer_button_text = "VIEW Data Table",
          card_footer_button_id = "view_indicators_overlay",

          # Placeholder parameters (shown when data is NULL/empty)
          placeholder_title = "No Project Indicators Data",
          placeholder_text = "Upload data to begin",
          placeholder_icon = ph("upload", weight = "fill", size = "lg"),

          # Required namespace function
          ns = ns,

          # Custom fieldset title (defaults to 'title' if not provided)
          fieldset_title = "Project Indicators"
        ),

        # -- ALIGNMENT SECTION --
        metric_section_ui(
          data = project_data$cleaned_data$alignment,

          # Value box parameters
          title = "Project Alignment Data",
          value = nrow(project_data$cleaned_data$alignment),
          round_to = 0,
          value_subtitle = "Records Processed",

          # Card parameters
          card_header_text = "Ready for Analysis",
          card_body = create_load_clean_taglist(
            data_completeness = alignment_data_completeness,
            case_completeness = alignment_case_completeness,
            qualtrics_metadata_removed = project_data$qualtrics_metadata_removed
          ),
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
          data = project_data$cleaned_data$dynamics,

          # Value box parameters
          title = "Project Dynamics Data",
          value = nrow(project_data$cleaned_data$dynamics),
          round_to = 0,
          value_subtitle = "Records Processed",

          # Card parameters
          card_header_text = "Ready for Analysis",
          card_body = create_load_clean_taglist(
            data_completeness = dynamics_data_completeness,
            case_completeness = dynamics_case_completeness,
            qualtrics_metadata_removed = project_data$qualtrics_metadata_removed
          ),
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
          data = project_data$cleaned_data$cascade$edges,

          # Value box parameters
          title = "Cascade Effects Data",
          value = nrow(project_data$cleaned_data$cascade$edges),
          round_to = 0,
          value_subtitle = "Records Processed",

          # Card parameters
          card_header_text = "Ready for Analysis",
          card_body = create_load_clean_taglist(
            data_completeness = cascade_data_completeness,
            case_completeness = cascade_case_completeness,
            qualtrics_metadata_removed = project_data$qualtrics_metadata_removed
          ),
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
        ),
        # No need for overlay UI outputs with direct shinyalert approach
      ) # closes the main tagList
    }) # closes renderUI
    # Return reactive values for use in other modules
    return(rv)
  }) # closes moduleServer
} # closes mod_load_clean_server

# =================================================================================================
# Helper Functions
# =================================================================================================

#' Create Load Clean TagList
#'
#' Creates a tagList containing data completeness, case completeness, and cleaning status indicators
#' using the reusable create_generate_list_taglist function.
#'
#' @param data_completeness Numeric value (0-100) representing data completeness percentage
#' @param case_completeness Numeric value (0-100) representing case completeness percentage
#' @param qualtrics_metadata_removed Logical indicating if Qualtrics metadata was removed
#'
#' @return A shiny.tag.list object containing the metrics display
#' @export
create_load_clean_taglist <- function(data_completeness, case_completeness, qualtrics_metadata_removed = NULL) {
  # Convert to format expected by create_generate_list_taglist
  metrics_list <- list(
    list(
      label = paste0("Data Completeness: ", data_completeness, "%"),
      status = "complete",
      required = NULL,
      action = NULL
    ),
    list(
      label = paste0("Case Completeness: ", case_completeness, "%"),
      status = "complete",
      required = NULL,
      action = NULL
    ),
    list(
      label = paste0(
        "Cleaned: ",
        if (!is.null(qualtrics_metadata_removed) && isTRUE(qualtrics_metadata_removed)) {
          "Qualtrics Metadata Removed, "
        } else {
          ""
        },
        "Data Structure Verified"
      ),
      status = "complete",
      required = NULL,
      action = NULL
    )
  )

  create_generate_list_taglist(metrics_list)
}


#' Generate Cascade YAML
#'
#' Generates YAML configuration for the cascade effects network visualization.
#'
#' @param cascade_data A data frame with 'from' and 'to' columns for the network edges.
#' @return A string containing the YAML configuration.
generate_cascade_yaml <- function(cascade_data) {
  # Default parameters
  params <- list(
    network_layout = "layout_with_fr",
    network_colors = list(
      researcher = "#1E325C",
      community = "#3B6B35",
      second_degree = "#A64B42",
      third_degree = "#E0D0A6"
    ),
    network_physics = list(
      enabled = TRUE,
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -50,
        centralGravity = 0.01,
        springLength = 100,
        springConstant = 0.08
      )
    )
  )
  # Convert to YAML string
  yaml::as.yaml(params)
} # closes generate_cascade_yaml


#' Create Static Network Plot
#'
#' Creates a static network visualization using igraph that matches the dynamic
#' visNetwork plot in styling and layout.
#'
#' @param cascade_data A list containing 'nodes' and 'edges' data frames from cascade analysis.
#' @return A ggplot object representing the network visualization.
create_static_network_plot <- function(cascade_data) {
  tryCatch(
    {
      # Extract nodes and edges
      nodes <- cascade_data$nodes
      edges <- cascade_data$edges

      # Check if data is available
      if (is.null(nodes) || is.null(edges) || nrow(nodes) == 0 || nrow(edges) == 0) {
        logger::log_warn("No valid network data available for static plot")
        return(NULL)
      }

      # Create igraph object
      g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

      # Define color scheme to match visNetwork
      color_map <- c(
        "Researcher" = "#1E325C",
        "Community Member" = "#3B6B35",
        "2nd Degree" = "#A64B42",
        "3rd Degree" = "#E0D0A6"
      )

      # Get node colors based on roles
      node_colors <- color_map[nodes$role]

      # Calculate layout (similar to visNetwork forceAtlas2Based)
      layout_coords <- igraph::layout_with_fr(g, niter = 1000)

      # Create the plot
      par(mar = c(0, 0, 0, 0)) # Remove margins
      plot(g,
        layout = layout_coords,
        vertex.color = node_colors,
        vertex.size = 3,
        vertex.label = NA, # No labels for cleaner look
        vertex.frame.color = "#4a4a4a",
        vertex.frame.width = 0.5,
        edge.color = "#888888",
        edge.width = 0.5,
        edge.curved = 0.2,
        asp = 0
      ) # Maintain aspect ratio
      logger::log_info("Static network plot created successfully with {nrow(nodes)} nodes and {nrow(edges)} edges")
    },
    error = function(e) {
      logger::log_error("Error creating static network plot: {conditionMessage(e)}")
      # Return a simple error plot
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error creating network plot", col = "red", cex = 1.2)
    }
  ) # closes tryCatch
} # closes create_static_network_plot

# Helper: create_score_value_box (local copy)

create_score_value_box2 <- function(title, score, type, bgcolor, icon_choice = ph_i("circle")) {
  score_display <- round(score, 0)

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
      score_display,
      class = "font-monospace",
      style = "
        font-size: 5rem;
        font-weight: 600;
        line-height: 1;
        color: #f5f1e8;"
    ),
    showcase = icon_choice,
    showcase_layout = "top right",
    theme = value_box_theme(bg = bgcolor, fg = "#f5f1e8"),
    tags$div(
      class = "text-uppercase fw-normal",
      style = "
        font-size: 0.9rem;
        margin-top: 0.25rem;
        color: #f5f1e8;",
      "records processed"
    )
  ) # closes bslib::value_box
} # closes create_score_value_box

#' Metric Section UI
#'
#' Generates the two-column layout for a metric (value box/placeholder, details card, overlay button).
#' @param data The data frame to check (e.g., rv$indicators)
#' @param value_box_title Title for the value box
#' @param score The score to display in the value box (usually nrow(data))
#' @param type The type for the value box (for styling)
#' @param bgcolor Background color for the value box
#' @param icon_choice Icon for the value box
#' @param input_id Input ID for the overlay button
#' @param extra_info Extra info line in the details card (e.g., "Qualtrics Metadata Removed")
#' @param ns Namespace function
#' @param section_name Section name for the legend/title
#' @param placeholder_title Title for the placeholder (if no data)
#' @param placeholder_text Text for the placeholder (if no data)
#' @param placeholder_icon Icon for the placeholder (if no data)
#' @param qualtrics_metadata_removed Logical indicating if qualtrics metadata was removed.
#' @return The inner layout (layout_columns(...)) for the metric section
metric_section_ui2 <- function(data, value_box_title, score, type, bgcolor, icon_choice, input_id, extra_info = NULL, ns, section_name, placeholder_title, placeholder_text, placeholder_icon, qualtrics_metadata_removed = NULL) {
  # If qualtrics_metadata_removed is TRUE, override extra_info (except for Alignment)
  if (!is.null(qualtrics_metadata_removed) && isTRUE(qualtrics_metadata_removed)) {
    extra_info <- "Qualtrics Metadata Removed"
  }
  cols <- list(
    # LEFT SIDE: Value Box or Placeholder
    if (is.null(data) || nrow(data) == 0) {
      tags$div(
        class = "data-placeholder",
        style = "\n        background: #f5f0e8;\n        border: 2px dashed #d4c5b9;\n        border-radius: 8px 0 0 8px;\n        padding: 1.1rem 0.5rem;\n        text-align: center;\n        min-height: 75px;\n        display: flex;\n        align-items: center;\n        justify-content: center;",
        tags$div(
          placeholder_icon,
          tags$div(
            style = "color: #6c5a47;",
            tags$strong(placeholder_title,
              style = "font-size:0.98rem; display:block; margin-bottom: 0.1rem;"
            ),
            tags$div(
              style = "font-size:0.85rem; line-height:1.2;",
              placeholder_text
            )
          )
        )
      )
    } else {
      create_score_value_box(
        title = value_box_title,
        score = score,
        type = type,
        bgcolor = bgcolor,
        icon_choice = icon_choice
      )
    },
    # RIGHT SIDE: Details Card (if data)
    if (!is.null(data) && nrow(data) > 0) {
      data_completeness <- round(sum(!is.na(data)) / (nrow(data) * ncol(data)) * 100, 1)
      case_completeness <- round(sum(complete.cases(data)) / nrow(data) * 100, 1)
      tagList(
        bslib::card(
          style = "\n        padding:0 !important;\n        border-radius:0 8px 8px 0 !important;\n        border: 1px 1px 1px 0; border-style: solid; border-color: #d4c5b9 !important;\n        box-shadow: 0 2px 4px rgba(0,0,0,0.08) !important;\n        background: #f9f5f0;",
          bslib::card_header(
            style = "background-color: #f9f5f0; border-bottom: 1px solid #e8ddd4; padding: 0.5rem 1rem;",
            tags$div(
              class = "d-flex align-items-center justify-content-between mb-0",
              tags$div("Status",
                class = "fw-medium",
                style = "font-size:0.75rem;color: var(--bs-success);text-transform:uppercase;"
              ),
              tags$div(
                "Ready for Analysis",
                class = "fw-bold",
                style = "font-size:1.1rem; text-transform: uppercase; color: var(--bs-success);font-family:var(--bs-font-monospace);"
              ),
              ph("check-circle", weight = "fill", size = "xl", style = "color: var(--bs-success);")
            )
          ),
          bslib::card_body(
            style = "padding:0.22rem .5rem; background: var(--bs-body-bg); overflow-x: hidden;",
            tagList(
              tags$div(
                class = "row",
                tags$div(
                  class = "col-6",
                  style = "font-size:0.8rem;line-height:1.14;color: var(--bs-primary);",
                  tags$div(
                    class = "d-flex align-items-start mb-1",
                    ph("check-circle",
                      weight = "regular", size = "1.5x",
                      style = "color: var(--bs-primary); margin-right:0.36rem; margin-top:0.05rem;"
                    ),
                    tags$div(
                      tags$strong("Data Completeness", style = "color: var(--bs-primary); text-transform: uppercase;"),
                      tags$br(),
                      tags$span(
                        style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace);",
                        paste0(data_completeness, "%")
                      )
                    )
                  )
                ),
                tags$div(
                  class = "col-6",
                  style = "font-size:0.8rem;line-height:1.14;color: var(--bs-primary);",
                  tags$div(
                    class = "d-flex align-items-start",
                    ph("check-circle",
                      weight = "regular", size = "1.5x",
                      style = "color: var(--bs-primary); margin-right:0.36rem; margin-top:0.05rem;"
                    ),
                    tags$div(
                      tags$strong("Case Completeness", style = "color: var(--bs-primary); text-transform: uppercase;"),
                      tags$br(),
                      tags$span(
                        style = "color: var(--bs-primary); text-transform: uppercase; font-family: var(--bs-font-monospace);",
                        paste0(case_completeness, "%")
                      )
                    )
                  )
                )
              ),
              tags$div(
                class = "row",
                style = "font-size:0.8rem;line-height:1.14;color:#8A7A8F;",
                tags$div(
                  class = "d-flex align-items-start mb-1",
                  ph("check-circle",
                    weight = "regular", size = "1.5x",
                    style = "color: var(--bs-primary); margin-right:0.36rem; margin-top:0.05rem;"
                  ),
                  tags$div(
                    tags$strong("Cleaned", style = "color: var(--bs-primary); text-transform: uppercase;"),
                    tags$br(),
                    tags$span(
                      style = "color: var(--bs-primary); text-transform: uppercase;",
                      extra_info,
                      if (!is.null(extra_info)) ", ",
                      "Data Structure Verified"
                    )
                  )
                )
              )
            )
          ),
          bslib::card_footer(
            style = "\n          background: var(--bs-body-bg);\n          border-top:1px solid #e8ddd4;\n          padding:0.5rem 1rem; text-align:center;",
            actionButton(
              inputId = ns(input_id),
              label = tagList(
                ph("table", weight = "fill", style = "margin-right:0.45rem;"),
                "VIEW CLEANED DATA"
              ),
              class = "text-uppercase fw-semibold",
              style = paste0("background-color: #8A7A8F; color: var(--bs-body-bg); border: none;\n                  padding:0.44rem 1.1rem;font-size:0.7rem;letter-spacing:0.4px;border-radius:6px;")
            )
          )
        )
      )
    }
  )
  do.call(layout_columns, c(list(col_widths = c(5, 7), gap = 0), cols))
}
