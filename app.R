# Load required packages
library(shiny)
library(bslib)

# Set SASS variables for theme ---------
bs_global_theme(version = "4", bootswatch = NULL)
tab_theme <- bs_theme(
  bg = "#e7e6e1",
  fg = "#00142e"
)

# Setup the user interface ---------
ui <- fluidPage(
    lang = "en-GB", # set the language of the page - important for accessibility
    tags$head(
        includeCSS("www/css/main.css"),
        includeCSS("www/css/tabs.css")), # put the CSS in the head section rather than in the body - for HTML5 conformity

    HTML('<header class="themeDarkBlueBIU">
                <a href="https://www.trafford.gov.uk" aria-label="Go to Trafford Council website"><img src="images/biu_logo_white_on_transparent_large.png" alt="Trafford Council | Business Intelligence Unit" id="logoBIU"/></a>
                <h1>Corporate Plan Dashboard</h1>
          </header>
          <main>'),

    navbarPage(
        id = "tabs",
        title = "",
        windowTitle = "Trafford Council Corporate Plan Dashboard", # This is the page title. Needs to be here otherwise an empty <title> is created.
        collapsible = TRUE,
        theme = tab_theme,
        
        tabPanel(
            # Home/landing page
            title = "Introduction",
            icon = icon("home"),
            HTML("<h2>Visualising the Council's priorities</h2>")
        ),
        # Pull in all the ui fragments for each of the priorities in the order we want the tabs to appear
        source("priorities/health/ui_fragment.R", local = TRUE)$value,
        source("priorities/poverty/ui_fragment.R", local = TRUE)$value,
        source("priorities/climate/ui_fragment.R", local = TRUE)$value,
        source("priorities/services/ui_fragment.R", local = TRUE)$value
        
    ),

    HTML('</main>
          <footer>

          </footer>')
)

# Declare the server code to supply objects to the user interface ---------
server <- function(input, output, session) {

    # Pull in all the server fragments for each of the priorities
    source("priorities/health/server_fragment.R", local = TRUE)$value
    source("priorities/poverty/server_fragment.R", local = TRUE)$value
    source("priorities/climate/server_fragment.R", local = TRUE)$value
    source("priorities/services/server_fragment.R", local = TRUE)$value

    # Event listeners for the tabs
    observeEvent(input$health_tab, {
      updateTabsetPanel(session, "tabs", selected = "Health Inequalities")
    })
    
    observeEvent(input$poverty_tab, {
      updateTabsetPanel(session, "tabs", selected = "Poverty Reduction")
    })

    observeEvent(input$climate_tab, {
      updateTabsetPanel(session, "tabs", selected = "Climate Crisis")
    })

    observeEvent(input$services_tab, {
      updateTabsetPanel(session, "tabs", selected = "Council Services")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
