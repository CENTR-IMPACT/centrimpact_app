#!! ModName = mod_home
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
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    img(
      src = "centr_logo.png",
      class = "header-logo",
      style = "max-width: 80%; height: auto; max-height: 120px; margin: 0 auto 20px; display: block;"
    ),
    fluidRow(
        create_flat_info_card(
          ns,
          title = "Welcome",
          icon = "hand-waving",
          content = tagList(
            p("Welcome to the CEnTR*IMPACT Toolkit. Use the tabs above to navigate through the application."),
            downloadButton(ns("download_qsf"), "Download QSF Templates", class = "btn-primary"),
            p("To collect data, download the Qualtrics survey files.")
          )
        )
      ),
    fluidRow(
      column(
        width = 6,
        create_flat_info_card(
          ns,
          title = "About CEnTR*IMPACT",
          icon = "info",
          content = tagList(
            p("CEnTR*IMPACT is an ensemble of metrics designed by and for community-engaged scholars to evaluate and communicate the impact of their work. Unlike traditional academic metrics that focus solely on publications and citations, CEnTR*IMPACT captures the unique qualities of community-engaged research through multiple dimensions."),
            p("This toolkit is intended for use by researchers, community partners, and other stakeholders involved in CEnR projects. It aims to streamline the data collection process, enhance data quality, and support the evaluation of CEnR efforts."),
            h2(
              "What CEnTR*IMPACT Measures ",
              ph("ruler", weight = "bold")
              ),
            p(
              strong("Direct Indicators"),
              " - Raw counts of project elements like community partners, engagement hours, individuals served, and outputs created."
            ),
            p(
              strong("Project Alignment"),
              " - How well researchers and community partners agree on project goals, values, roles, and outcomes across eight key factors."
            ),
            p(
              strong("Project Dynamics"),
              " - The balance of effort across five domains (Contexts, Processes, Interventions & Research, Engaged Learning, and Outcomes) using the Community-Based Participatory Research framework."
            ),
            p(
              strong("Cascade Effects"),
              " - The potential for information and impact to spread across three degrees of social network connections, measuring how well the project supports equitable distribution of knowledge and power."
            ),
            h2(
              "Our Approach ",
              ph("compass", weight = "bold")
              ),
            p("CEnTR*IMPACT was developed through a collaborative process with community-engaged scholars across the Indiana University system. The metrics reflect the values identified by practitioners in the field: prioritizing community needs, building sustainable partnerships, and ensuring mutual transformation."),
            p("These metrics are designed to complement, not replace, qualitative narratives. They provide a quantitative \"backbone\" that helps scholars and communities tell their stories more effectively to colleagues, administrators, and funders while maintaining the authentic complexity of community-engaged work."),
            h2(
              "Key Principles ",
              ph("shield-check", weight = "bold")
              ),
            tags$ul(
              tags$li(
                tags$strong("Values-Based:"),
                " Built from what community-engaged scholars and communities actually value"
                ),
              tags$li(
                tags$strong("Balanced:"),
                " Measures equity and balance rather than just \"more\" or \"bigger\""
                ),
              tags$li(
                tags$strong("Complementary:"),
                " Designed to support, not replace, narrative evaluation"),
              tags$li(
                tags$strong("Responsible:"),
                " Aligned with frameworks for ethical metrics use"
                )
            ),
            h2(
              "Important Note ",
              ph("warning", weight = "fill"),
              class = "text-danger"
              ),
            p(
              tags$strong("CEnTR*IMPACT should never be used as a single-value indicator for consequential decisions"),
              " like promotion, tenure, or funding. These metrics are meant to enrich understanding and facilitate conversation, not to reduce complex work to simple numbers."
            )
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
}


# the server function
mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Download handler for QSF template
    output$download_qsf <- downloadHandler(
      filename = function() {
        "centrimpact_qsf.zip"
      },
      content = function(file) {
        file.copy("www/centrimpact_qsf.zip", file)
      },
      contentType = "application/zip"
    )
  })
}
