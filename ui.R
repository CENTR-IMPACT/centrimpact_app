library(bslib)
library(thematic)
library(shiny)
library(shinyjs)
thematic::thematic_shiny()

# Define a custom theme for the application
custom_theme <- bslib::bs_theme(
  version = 5,
  base_font = bslib::font_google("Work Sans", wght = c(400, 600)),
  heading_font = bslib::font_google("Bitter", wght = c(400, 700)),
  code_font = bslib::font_google("IBM Plex Mono", wght = c(300, 400, 600, 700)),
  bg = "#EDEAE2", # Main app background (lightest warm neutral)
  fg = "#2A2A2A", # Default text color (dark soft grey)

  primary = "#5C6470",
  secondary = "#7A734D",
  info = "#5A6C75",
  success = "#5D7359",
  warning = "#C5A259",
  danger = "#8F524E",
  "sidebar-bg" = "#5C5A58", # **Crucial: Darker, rich warm grey/brown for sidebar background**
  "sidebar-fg" = "#F5F1E8", # **Crucial: Creamy white for text/icons on dark sidebar**

  "navbar-bg" = "#5C5A58", # Rich brown (darkest, strong anchor)
  "navbar-fg" = "#F5F1E8", # Creamy white for navbar text/links

  "card-bg" = "#E2D8C3", # For accordions, cards (slightly darker than main bg)
  "accordion-bg" = "#E2D8C3", # Explicitly set accordion panel bg if different from card-bg
  "accordion-active-bg" = "#B8A989", # Darker highlight for active accordion header
  "input-bg" = "#FDF5E6", # A very light cream for input fields, stands out from card-bg
  "border-color" = "#C8BFA6" # A general border color for consistency
)


ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* CSS will be added to handle snapshot banner positioning */
    "))
  ),
  tags$script(HTML("
    // Debug function to inspect workflow steps
    window.debugWorkflowSteps = function() {
      const steps = document.querySelectorAll('.step');
      console.log('Current workflow step states:');
      steps.forEach(step => {
        const id = step.id;
        const classes = Array.from(step.classList).join(', ');
        const status = step.querySelector('.step-status');
        const statusText = status ? status.innerText : 'no status';
        console.log(`${id}: ${classes} - ${statusText}`);
      });
    };

    // Force refresh workflow status display
    function refreshWorkflowSteps() {
      const steps = ['setup', 'upload', 'analyze', 'visualize', 'generate'];
      steps.forEach(step => {
        const stepElement = document.getElementById(`step-${step}`);
        if (stepElement && stepElement.classList.contains('completed')) {
          console.log(`Refreshing completed step: ${step}`);
          // Briefly remove and add the class to trigger animations
          stepElement.classList.remove('completed');
          setTimeout(() => {
            stepElement.classList.add('completed');
          }, 50);
        }
      });
    }

    // Check if Phosphor font is loaded
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(function() {
        // Create a test element to check if the font loaded properly
        const testEl = document.createElement('span');
        testEl.style.fontFamily = 'Phosphor, Arial';
        testEl.style.visibility = 'hidden';
        testEl.style.position = 'absolute';
        testEl.innerHTML = '\\uE184'; // Use a character from the font
        document.body.appendChild(testEl);

        // Get computed style of the test element
        const computedStyle = window.getComputedStyle(testEl);
        const fontFamily = computedStyle.getPropertyValue('font-family');

        // Check if the character renders at the expected width
        if (!fontFamily.includes('Phosphor') || testEl.offsetWidth === 0) {
          console.warn('Phosphor icon font may not be loaded properly. Fallback icons will be used.');
          document.documentElement.classList.add('phosphor-font-missing');

          // Notify R about the font loading issue
          if (Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('js_warning', {
              type: 'font_not_loaded',
              font: 'Phosphor',
              timestamp: Date.now()
            });
          }
        } else {
          console.log('Phosphor icon font loaded successfully');
        }

        // Clean up
        document.body.removeChild(testEl);
      }, 500); // Check after a delay to ensure font had a chance to load
    });

    Shiny.addCustomMessageHandler('snapshotLoaded', function(message) {
      var btn = document.querySelector('#load_snapshot_button .btn-file > span');
      if(btn) {
        btn.innerHTML = 'Snapshot Loaded';
        setTimeout(function() {
          btn.innerHTML = 'Load Snapshot <i class=\"ph ph-camera-plus\"></i>';
        }, 10000);
      } else {
        console.log('Snapshot button not found!');
      }
    });

    // Monitor for snapshot download and show success banner after download starts
    document.addEventListener('DOMContentLoaded', function() {
      var snapshotButton = document.getElementById('save_snapshot');
      if (snapshotButton) {
        snapshotButton.addEventListener('click', function() {
          // Show success banner after a delay to ensure download has started
          setTimeout(function() {
            Shiny.setInputValue('snapshot_download_completed', Math.random());
          }, 2000);
        });
      }
    });

    // Handle data mode selector clicks
    document.addEventListener('DOMContentLoaded', function() {
      const uploadOption = document.getElementById('upload_mode_option');
      const enterOption = document.getElementById('enter_mode_option');

      if (uploadOption && enterOption) {
        uploadOption.addEventListener('click', function() {
          // Toggle to upload mode
          uploadOption.classList.remove('inactive');
          uploadOption.classList.add('active');
          enterOption.classList.remove('active');
          enterOption.classList.add('inactive');

          // Send event to Shiny
          Shiny.setInputValue('data_mode_selector', 'upload', {priority: 'event'});
        });

        enterOption.addEventListener('click', function() {
          // Toggle to enter mode
          enterOption.classList.remove('inactive');
          enterOption.classList.add('active');
          uploadOption.classList.remove('active');
          uploadOption.classList.add('inactive');

          // Send event to Shiny
          Shiny.setInputValue('data_mode_selector', 'enter', {priority: 'event'});
        });
      }
    });

    // Initialize data mode selector state
    Shiny.addCustomMessageHandler('setDataModeSelector', function(message) {
      const uploadOption = document.getElementById('upload_mode_option');
      const enterOption = document.getElementById('enter_mode_option');

      if (uploadOption && enterOption) {
        if (message.mode === 'upload') {
          uploadOption.classList.remove('inactive');
          uploadOption.classList.add('active');
          enterOption.classList.remove('active');
          enterOption.classList.add('inactive');
        } else if (message.mode === 'enter') {
          enterOption.classList.remove('inactive');
          enterOption.classList.add('active');
          uploadOption.classList.remove('active');
          uploadOption.classList.add('inactive');
        }
      }
    });
  ")),
  bslib::page_navbar(
    id = "main_navbar",
    theme = custom_theme,
    title = "CEnTR*IMPACT Toolkit",
    header = tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "custom.css"
      )
    ),
    sidebar = bslib::sidebar(
      position = "left",
      class = "enhanced-sidebar",
      style = "background: linear-gradient(145deg, #5C5A58 0%, #4A4845 100%); color: #F5F1E8; height: 100%;",

      # Project Section
      div(
        class = "project-section",
        div(
          class = "section-header",
          "Project Info"
        ),
        div(
          class = "project-title",
          uiOutput("project_title_text")
        )
      ),

      # Scores Section
      div(
        class = "scores-section",
        div(
          class = "section-header",
          "Scores"
        ),
        div(
          class = "score-item",
          div(class = "score-label", "Project Alignment"),
          div(
            class = "d-flex align-items-center justify-content-between",
            div(
              class = "score-icon alignment",
              ph("flower-lotus", weight = "bold")
            ),
            div(
              class = "score-value d-flex align-items-center",
              uiOutput("alignment_score_icon"),
              uiOutput("alignment_score_display")
            )
          )
        ),
        div(
          class = "score-item",
          div(class = "score-label", "Project Dynamics"),
          div(
            class = "d-flex align-items-center justify-content-between",
            div(
              class = "score-icon dynamics",
              ph("pulse", weight = "bold")
            ),
            div(
              class = "score-value d-flex align-items-center",
              div(id = "dynamics_score_icon", class = "shiny-html-output"),
              div(id = "dynamics_score_display", class = "shiny-html-output")
            )
          )
        ),
        div(
          class = "score-item",
          div(class = "score-label", "Cascade Effects"),
          div(
            class = "d-flex align-items-center justify-content-between",
            div(
              class = "score-icon cascade",
              ph("waveform", weight = "bold")
            ),
            div(
              class = "score-value d-flex align-items-center",
              div(id = "cascade_score_icon", class = "shiny-html-output"),
              div(id = "cascade_score_display", class = "shiny-html-output")
            )
          )
        )
      ),

      # Tools Section
      div(
        class = "tools-section",
        div(
          class = "section-header",
          "Tools"
        ),
        # div(
        #   class = "data-mode-selector",
        #   div(
        #     id = "upload_mode_option",
        #     class = "mode-option active",
        #     "Upload Data"
        #   ),
        #   div(
        #     id = "enter_mode_option",
        #     class = "mode-option inactive",
        #     "Enter Data"
        #   )
        # ),
        div(
          class = "d-flex flex-column gap-2",
          tags$div(
            class = "snapshot-button",
            div(class = "button-icon", ph("download", weight = "fill")),
            downloadButton(
              outputId = "save_snapshot",
              label = " Save Snapshot",
              class = "btn btn-file"
            )
          ),
          div(
            class = "d-flex flex-column w-100",
            style = "gap: 4px;",
            div(
              class = "snapshot-button",
              tags$label(
                class = "btn btn-file w-100",
                tagList(
                  div(class = "button-icon", ph("upload", weight = "fill")),
                  "Load Snapshot",
                  fileInput(
                    inputId = "load_snapshot",
                    label = NULL,
                    accept = ".rds",
                    buttonLabel = NULL,
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Navigation panels (comma-separated)
    bslib::nav_panel(
      title = "Home",
      value = "home",
      icon = ph("map-pin-area", weight = "bold"),
      mod_home_ui("home_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      title = "Project Setup",
      icon = ph("toolbox", weight = "fill"),
      mod_setup_ui("setup_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      value = "Upload Data",
      title = "Upload Data",
      icon = ph("pencil-ruler", weight = "fill"),
      mod_load_clean_ui("load_clean_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      value = "Enter Data",
      title = "Enter Data",
      icon = ph("pencil-ruler", weight = "fill"),
      mod_enter_data_ui("enter_data_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      title = "Analyze Data",
      icon = ph("calculator", weight = "fill"),
      mod_analyze_ui("analyze_data_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      title = "Visualize Data",
      icon = ph("compass-tool", weight = "fill"),
      mod_visualize_ui("visualize_1"),
      class = "main_content"
    ),
    bslib::nav_panel(
      title = "Generate Report",
      icon = ph("pen-nib", weight = "fill"),
      mod_generate_ui("generate_1"),
      class = "main_content"
    ),
    # Right-aligned navigation items
    bslib::nav_spacer(),
    bslib::nav_menu(
      title = "About",
      icon = ph("info", weight = "bold"),
      align = "right",
      bslib::nav_item(
        style = "font-family: var(--bs-font-sans-serif); font-size: 0.9em; padding: 5px !important;",
        tags$p("CEnTR*IMPACT was developed by Jeremy F. Price, PhD, at Indiana University Indianapolis through a 2024 CUMU-Collaboratory Fellowship, with contributions from community-engaged scholars across the IU system."),
        tags$p("This toolkit provides an ensemble of metrics designed by and for community-engaged scholars to evaluate and communicate the impact of their work through quantitative measures that complement qualitative narratives."),
        tags$p(
          tags$strong("Important:"),
          " CEnTR*IMPACT should never be used as a single-value indicator for consequential decisions like promotion, tenure, or funding."
        ),
        tags$hr(),
        tags$p(
          tags$strong("Disclaimer:"),
          " This web application is provided as-is without warranty. Users are responsible for backing up their data. Session timeouts or technical issues may result in data loss."
        ),
        tags$hr(),
        tags$p(
          tags$strong("Licensing:"),
          " This web application is available under the MIT License. The underlying CEnTR*IMPACT methodology and framework remain under CC BY-NC-SA 4.0."
        )
      ),
      bslib::nav_item(
        style = "font-family: var(--bs-font-code); font-size: 0.75em; padding: 5px !important;",
        bslib::nav_item(
          style = "display: flex; align-items: center; gap: 10px; padding: 0 15px !important;",
          tags$div(
            style = "display: flex; gap: 15px; align-items: center;",
            tags$a(
              style = "display: flex; align-items: center; gap: 5px; color: inherit; text-decoration: none; font-family: var(--bs-font-code); font-size: 0.75em; padding: 10px 0;",
              phosphoricons::ph("article"),
              "Report",
              href = "https://cumuonline.org/wp-content/uploads/2024-CUMU-Collaboratory-Fellowship-Report.pdf",
              target = "_blank"
            ),
            tags$span(style = "border-left: 1px solid rgba(255,255,255,0.2); height: 24px;"),
            tags$a(
              style = "display: flex; align-items: center; gap: 5px; color: inherit; text-decoration: none; font-family: var(--bs-font-code); font-size: 0.75em; padding: 10px 0;",
              shiny::icon("github"),
              "Source Code",
              href = "https://github.com/yourusername/centrimpactgolem",
              target = "_blank"
            ),
            tags$span(style = "border-left: 1px solid rgba(255,255,255,0.2); height: 24px;"),
            tags$button(
              id = "showDocsModal",
              style = "background: none; border: none; cursor: pointer; display: flex; align-items: center; gap: 5px; color: inherit; font-family: var(--bs-font-code); font-size: 0.75em; padding: 10px 0; margin: 0;",
              shiny::icon("book"),
              "Documentation"
            ),
            tags$script(HTML(
              "$(document).on('click', '#showDocsModal', function() {
              var modal = '<div class=\"modal fade\" id=\"docsModal\" tabindex=\"-1\" aria-labelledby=\"docsModalLabel\" aria-hidden=\"true\">' +
                '<div class=\"modal-dialog modal-dialog-centered\">' +
                '<div class=\"modal-content\">' +
                '<div class=\"modal-header\">' +
                '<h5 class=\"modal-title\" id=\"docsModalLabel\">Documentation</h5>' +
                '<button type=\"button\" class=\"btn-close\" data-bs-dismiss=\"modal\" aria-label=\"Close\"></button>' +
                '</div>' +
                '<div class=\"modal-body\">' +
                '<p>Documentation is currently in progress. Please check back soon for comprehensive guides and references.</p>' +
                '</div>' +
                '<div class=\"modal-footer\">' +
                '<button type=\"button\" class=\"btn btn-secondary\" data-bs-dismiss=\"modal\">Close</button>' +
                '</div>' +
                '</div>' +
                '</div>' +
                '</div>';

              $('body').append(modal);
              var modalElement = new bootstrap.Modal(document.getElementById('docsModal'));
              modalElement.show();

              // Clean up the modal after it's closed
              $('#docsModal').on('hidden.bs.modal', function () {
                $(this).remove();
              });
            });"
            ))
          )
        )
      )
    )
  )
)
