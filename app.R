# Load required packages
library(shiny)

# Setup the user interface ---------
ui <- fluidPage(
    title = "Corporate Plan",
    lang = "en-GB", # set the language of the page - important for accessibility
    tags$head(includeCSS("www/css/main.css")), # put the CSS in the head section rather than in the body - for HTML5 conformity

    HTML('<header class="themeDarkBlueBIU">
                <a href="https://www.trafford.gov.uk" aria-label="Go to Trafford Council website"><img src="images/biu_logo_white_on_transparent_large.png" alt="Trafford Council | Business Intelligence Unit" id="logoBIU"/></a>
                <h1>Corporate Plan Dashboard</h1>
          </header>
          <main>'),

    # titlePanel(
    #
    # ),
    # tagList(
    #     navbarPage(
    #         title = "",
    #         collapsible = TRUE,
    #         id = "tabs",
    #         tabPanel("Home"
    #             # This is for the home/landing page
    #         ),
    #         # Pull in all the ui fragments for each of the priorities
    #         source("priorities/accessible/ui_fragment.R", local = TRUE)$value,
    #         source("priorities/climate/ui_fragment.R", local = TRUE)$value,
    #         source("priorities/health/ui_fragment.R", local = TRUE)$value,
    #         source("priorities/poverty/ui_fragment.R", local = TRUE)$value,
    #         source("priorities/services/ui_fragment.R", local = TRUE)$value
    #     )
    # ),

    HTML('</main>
          <footer>

          </footer>')
)

# Declare the server code to supply objects to the user interface ---------
server <- function(input, output, session) {

    # Pull in all the server fragments for each of the priorities
    source("priorities/accessible/server_fragment.R", local = TRUE)$value
    source("priorities/climate/server_fragment.R", local = TRUE)$value
    source("priorities/health/server_fragment.R", local = TRUE)$value
    source("priorities/poverty/server_fragment.R", local = TRUE)$value
    source("priorities/services/server_fragment.R", local = TRUE)$value

    # Event listeners for the tabs
    observeEvent(input$accessible_tab, {
      updateTabsetPanel(session, "tabs", selected = "Open &amp; Accessible")
    })

    observeEvent(input$climate_tab, {
      updateTabsetPanel(session, "tabs", selected = "Climate Crisis")
    })

    observeEvent(input$health_tab, {
      updateTabsetPanel(session, "tabs", selected = "Health Inequalities")
    })

    observeEvent(input$poverty_tab, {
      updateTabsetPanel(session, "tabs", selected = "Poverty Support")
    })

    observeEvent(input$services_tab, {
      updateTabsetPanel(session, "tabs", selected = "Quality Services")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
