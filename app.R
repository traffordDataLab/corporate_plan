# Load required packages
library(shiny)
library(bslib)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(ggiraph)
library(sf)
library(leaflet)
library(scales)


# Set common plot colours, themes and options etc. used in the visualisations ---------
# NOTE: These are here to be in global scope - would've previously been placed in global.R

# ggplot2 theme
theme_x <- function () { 
  theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "#333333", size = 10, hjust = 1, margin = margin(t = 15)),
      axis.title.x = element_text(size = 11, hjust = 1, margin = margin(t = 10)),
      axis.title.y = element_text(size = 11, angle = 90, hjust = 1, margin = margin(r = 10)),
      axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)),
      legend.position = "none"
    )
}

# customisation of ggiraph interactive output, building on top of theme_x() to give a common appearance to the plots
lab_ggiraph_options <- list(opts_tooltip(use_fill = FALSE, opacity = 1, css = "background-color: #e7e6e1; color: #212121; padding: 0.5em; border-radius: 0.5em;"),
                            opts_hover(css = "fill-opacity: 1; stroke:white; stroke-opacity: 1; r: 2.5pt;"),
                            opts_selection(type = "single"),
                            opts_toolbar(saveaspng = FALSE))

plot_colour_trafford = "#00445e"
plot_colour_similar_authorities = "#009590"
plot_colour_england = "#ffcb00"
plot_colour_spinner = "#bdbdbd"


# Set SASS variables for main app theme ---------
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
                  <p>The corporate plan describes Trafford Council's strategic vision, outcomes and priorities for the borough, with the priorities being key to it's delivery. We will focus on three priorities to help us achieve these outcomes: <strong>reducing health inequalities</strong>, <strong>supporting people out of poverty</strong> and <strong>addressing our climate crisis</strong>.</p>"
            ),

            fluidRow(
                column(width = 6,
                    tags$button(
                        id = "health_btn",
                        class = "btn action-button homeMenuButton",
                        span(
                            img(
                                src = "images/icon_health.png",
                                alt = "",
                                role = "presentation"
                            )
                        ),
                        div(
                            h3("Reducing health inequalities"),
                            p("Working with people, communities and partners, particularly in deprived areas, to improve the physical and mental health of all our residents.")
                        )
                    )
                ),
                column(width = 6,
                    tags$button(
                        id = "poverty_btn",
                        class = "btn action-button homeMenuButton",
                        span(
                            img(
                                src = "images/icon_poverty.png",
                                alt = "",
                                role = "presentation"
                            )
                        ),
                        div(
                            h3("Supporting people out of poverty"),
                            p("Tackling the root causes to prevent people from falling into poverty, and raising people out of it.")
                        )
                    )
                )
            ),
            fluidRow(
                column(width = 6,
                    tags$button(
                        id = "climate_btn",
                        class = "btn action-button homeMenuButton",
                        span(
                            img(
                                src = "images/icon_climate.png",
                                alt = "",
                                role = "presentation"
                            )
                        ),
                        div(
                            h3("Addressing our climate crisis"),
                            p("Reducing our carbon footprint and tackling the impact of climate change.")
                        )
                    )
                ),
                column(width = 6,
                    tags$button(
                        id = "services_btn",
                        class = "btn action-button homeMenuButton",
                        span (
                            img(
                                src = "images/icon_services.png",
                                alt = "",
                                role = "presentation"
                            )
                        ),
                        div(
                            h3("Council services"),
                            p("Although not part of the corporate plan, this section provides information on a number of services and functions provided by the council.")
                        )
                    )
                )
            ),

            HTML("<h3>About the dashboard</h3>
                  <p>The dashboard visualises a range of indicators relating to each of the three strategic priorities, as well as an additional section covering council services. The indicators are provided at local authority level or in some cases lower, with many benchmarked against Trafford's <a href='https://www.cipfa.org/services/cipfastats/nearest-neighbour-model' target='_blank' aria-label='CIPFA nearest neighbours, (opens in new window)'>CIPFA nearest neighbours</a> (other councils with the most similar statistical characteristics in terms of social and economic features). Some of the visualisations have additional options, such as alternative chart types. Information is provided about each indicator, including links to download the data and to the original data source.</p>
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
