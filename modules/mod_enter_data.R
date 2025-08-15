#!! ModName = mod_enter_data
# !! ModDisplayName = Enter your module shiny display name here.
# !! ModDescription = Enter your module description here.
# !! ModCitation = Price, Jeremy.  (2025). mod_home. [Source code].
# !! ModNotes = Enter your module notes here.
# !! ModActive = 1/0
# !! FunctionArg = argName1 !! argDescription !! argClass
# !! FunctionArg = argName2 !! argDescription !! argClass
# !! FunctionReturn = returnName1 !! returnDescription !! returnClass
# !! FunctionReturn = returnName2 !! returnDescription !! returnClass


# the ui function
mod_enter_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
        bslib::accordion(
          id = ns("data_entry_accordion"),
          multiple = FALSE,
          
          # Main visualization panel
          bslib::accordion_panel(
            value = "intro_data",
            title = tagList(
              phosphoricons::ph("compass-tool"),
              HTML("&nbsp;"),
              "Which Data"
            ),
            # Do UI here, ask which data sets they want to enter: Indicators, Dynamics, Cascade
            shinyWidgets::radioGroupButtons(
              inputId = ns("analysis_type"),
              label = "ANALYSIS TYPE",
              choices = list(
                "All Data" = "full",
                "Indicators Data" = "indicators",
                "Dynamics Data" = "dynamics",
                "Cascade Data" = "cascade"
              ),
              selected = "full",
              direction = "horizontal",
              justified = TRUE
            )
          ),
          
          # One panel for each option
          bslib::accordion_panel(
            value = "indicators",
            title = tagList(ph("flag-checkered", weight = "fill"), HTML("&nbsp;"), "Results"),
            div(
             # Indicator Name: [Textbox for Numbers] [ ] Not Applicable (checkbox) (See "Main" Qualtrics Survey)
            )
            # Save Button -> See mod_setup.R status_line_ui function for example See mod_load_clean for project_data structure for storing responses
          ),
          bslib::accordion_panel(
            value = "dynamics",
            title = tagList(ph("arrows-clockwise", weight = "fill"), HTML("&nbsp;"), "Dynamics"),
            div(
              # Choose Domains
              # Go through each select domain and do the modified rank ordering
            )
            # Save Button -> See mod_setup.R status_line_ui function for example
          )
        ),
 
        bslib::card(
      bslib::card_header(
        style = "background-color: #F2ECD7; border-radius: 0px 0px 10px 10px !important;
      box-shadow: inset 0 4px 10px #8e838066 !important;",
        img(
          src = "centr_logo.png",
          class = "header-logo",
          style = "max-width: 80%; height: auto; max-height: 120px; margin: 0 auto 20px; display: block;"
        )
      ),
      bslib::card_body(
        class = "main_content",
        fluidRow(
          column(
            width = 6,
            create_flat_info_card(
              ns,
              title = "Welcome",
              icon = "hand-waving",
              content = tagList(
                p("Welcome to the CEnTR*IMPACT Toolkit. Use the tabs above to navigate through the application.")
              )
            )
          ),
          column(
            width = 6,
            create_flat_info_card(
              ns,
              title = "Process Map",
              icon = "map-pin-area",
              content = img(
                src = "blueprint-wf.png",
                alt = "CEnTR*IMPACT Logo",
                style = "max-width: 100%; border-radius: 18px !important; box-shadow: 4px 4px 8px rgba(0, 0, 0, 0.2); height: auto;"
              )
            )
          )
        )
      )
    )
  )
}


# the server function
mod_enter_data_server <- function(id, argName1, argName2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      reactiveValues(
        returnName1 = reactive(returnName1()),
        returnName2 = reactive(returnName2())
      )
    )
  })
}
