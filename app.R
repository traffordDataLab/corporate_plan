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
        tags$link(rel = "stylesheet", href = "css/main.css"),
        tags$link(rel = "stylesheet", href = "css/tabs.css"),
        tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto")
    ),

    HTML('<header class="themeDarkBlueBIU">
              <a href="https://www.trafford.gov.uk" aria-label="Go to Trafford Council website"><img src="images/biu_logo_white_on_transparent_large.png" alt="Trafford Council | Business Intelligence Unit" id="logoBIU"/></a>
              <h1>Corporate Plan Dashboard</h1>
          </header>
          <main>'
    ),

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
            HTML("<div id='homePageContainer'>
                  <h2>Visualising the Council's priorities</h2>
                  <p>The Corporate Plan describes Trafford Council's vision and priorities for the borough and the priorities we have identified as an organisation as being key to the delivery of that vision:</p>"
            ),

            fluidRow(
                column(width = 6,
                    tags$button(
                        id = "health_btn",
                        class = "btn action-button homeMenuButton",
                        img(
                            src = "images/icon_heart.png",
                            alt = "",
                            role = "presentation"
                        ),
                        p("Reducing health inequalities")
                    )
                ),
                column(width = 6,
                    tags$button(
                        id = "poverty_btn",
                        class = "btn action-button homeMenuButton",
                        img(
                            src = "images/icon_money.png",
                            alt = "",
                            role = "presentation"
                        ),
                        p("Supporting people out of poverty")
                    )
                )
            ),
            fluidRow(
                column(width = 6,
                    tags$button(
                        id = "climate_btn",
                        class = "btn action-button homeMenuButton",
                        img(
                            src = "images/icon_plant.png",
                            alt = "",
                            role = "presentation"
                        ),
                        p("Addressing our climate crisis")
                    )
                ),
                column(width = 6,
                    tags$button(
                        id = "services_btn",
                        class = "btn action-button homeMenuButton",
                        img(
                            src = "images/icon_rosette.png",
                            alt = "",
                            role = "presentation"
                        ),
                        p("Council services")
                    )
                )
            ),

            HTML("<h3>About the dashboard</h3>
                  <p>This dashboard allows you to browse a range of indicators that relate to each of the strategic priorities. The indicators are provided at local authority level or in some cases lower, with many benchmarked against Trafford's <a href='https://www.cipfa.org/services/cipfastats/nearest-neighbour-model' target='_blank' aria-label='CIPFA nearest neighbours, (opens in new window)'>CIPFA nearest neighbours</a> (other councils with the most similar statistical characteristics in terms of social and economic features). Some of the visualisations can be filtered by variable, area and chart type. Information is provided about each indicator with a link to the original data source.</p>
                  </div>"
            )
        ),
        # Pull in all the ui fragments for each of the priorities in the order we want the tabs to appear
        source("priorities/health/ui_fragment.R", local = TRUE)$value,
        source("priorities/poverty/ui_fragment.R", local = TRUE)$value,
        source("priorities/climate/ui_fragment.R", local = TRUE)$value,
        source("priorities/services/ui_fragment.R", local = TRUE)$value

    ),

    HTML('</main>
          <footer>
              <div>Developed in <a href="https://cran.r-project.org/" target="_blank" aria-label="R, (opens in new window)">R</a> by the <a href="https://www.trafforddatalab.io">Trafford Data Lab</a> under the <a href="https://www.trafforddatalab.io/LICENSE.txt">MIT</a> licence</div>
          </footer>')
)

# Declare the server code to supply objects to the user interface ---------
server <- function(input, output, session) {

    # Pull in all the server fragments for each of the priorities
    source("priorities/health/server_fragment.R", local = TRUE)$value
    source("priorities/poverty/server_fragment.R", local = TRUE)$value
    source("priorities/climate/server_fragment.R", local = TRUE)$value
    source("priorities/services/server_fragment.R", local = TRUE)$value

    # Event listeners for the buttons on the "introduction" tab to select the relevant tabs
    observeEvent(input$health_btn, {
      updateTabsetPanel(session, "tabs", selected = "Health Inequalities")
    })

    observeEvent(input$poverty_btn, {
      updateTabsetPanel(session, "tabs", selected = "Poverty Reduction")
    })

    observeEvent(input$climate_btn, {
      updateTabsetPanel(session, "tabs", selected = "Climate Crisis")
    })

    observeEvent(input$services_btn, {
      updateTabsetPanel(session, "tabs", selected = "Council Services")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
