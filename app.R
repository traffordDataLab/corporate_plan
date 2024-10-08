##################################################################################################################
# 2024-08-30:
# THIS APP HAS NOW BEEN REPLACED BY THE TRAFFORD THEMES APP: https://trafforddatalab.shinyapps.io/trafford_themes/
# MUCH OF THE CODE HAS BEEN COMMENTED OUT AS THE PAGE NOW ONLY NEEDS TO REDIRECT USERS TO THE NEW APP
# NEW CODE FOR THE PURPOSE OF REDIRECTING IS CLEARLY MARKED
##################################################################################################################

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
library(markdown)


# Set common plot colours, themes and options etc. used in the visualisations ---------
# NOTE: These are here to be in global scope - would've previously been placed in global.R

# ggplot2 theme
# theme_x <- function () { 
#   theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
#     theme(
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       plot.caption = element_text(color = "#333333", size = 10, hjust = 1, margin = margin(t = 15)),
#       axis.title.x = element_text(size = 11, hjust = 1, margin = margin(t = 10)),
#       axis.title.y = element_text(size = 11, angle = 90, hjust = 1, margin = margin(r = 10)),
#       axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)),
#       legend.position = "none"
#     )
# }

# customisation of ggiraph interactive output, building on top of theme_x() to give a common appearance to the plots
# lab_ggiraph_options <- list(opts_tooltip(use_fill = FALSE, opacity = 1, css = "background-color: #e7e6e1; color: #212121; padding: 0.5em; border-radius: 0.5em;"),
#                             opts_hover(css = "fill-opacity: 1; stroke:white; stroke-opacity: 1; r: 2.5pt;"),
#                             opts_selection(type = "single"),
#                             opts_toolbar(saveaspng = FALSE))
# 
# plot_colour_trafford = "#00445e"
# plot_colour_similar_authorities = "#009590"
# plot_colour_england = "#ffcb00"
# plot_colour_spinner = "#bdbdbd"


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
        #tags$link(rel = "stylesheet", href = "css/tabs.css"),
        tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto"),
        
        
        # THE FOLLOWING IS NEW CODE FOR THE REDIRECTION ---
        tags$title('Trafford Council Corporate Plan Dashboard'),
        ###################################################
        
        
    ),

    HTML('<header class="themeDarkBlueBIU">
              <a href="https://www.trafford.gov.uk" aria-label="Go to Trafford Council website"><img src="images/biu_logo_white_on_transparent_large.png" alt="Trafford Council | Business Intelligence Unit" id="logoBIU"/></a>
              <h1>Corporate Plan Dashboard</h1>
          </header>
          <main>'
    ),
    
    
    # THE FOLLOWING IS NEW CODE FOR THE REDIRECTION ---
    HTML('<h2>App change notification</h2>
          <p>The Corporate Plan Dashboard has been replaced by a new app called <strong>Trafford Themes</strong> published at the following location:</p>
          <p><a href="https://trafforddatalab.shinyapps.io/trafford_themes/">https://trafforddatalab.shinyapps.io/trafford_themes</a></p>
          <p>Please visit the link provided to use the new app.</p>'
    ),
    ###################################################

    
    # navbarPage(
    #     id = "tabs",
    #     title = "",
    #     windowTitle = "Trafford Council Corporate Plan Dashboard", # This is the page title. Needs to be here otherwise an empty <title> is created.
    #     collapsible = TRUE,
    #     theme = tab_theme,
    # 
    #     tabPanel(
    #         # Home/landing page
    #         title = "Introduction",
    #         icon = icon("house"),
    #         HTML("<div id='homePageContainer'>
    #               <h2>Visualising the Council's priorities</h2>
    #               <p>The corporate plan describes Trafford Council's strategic vision, outcomes and priorities for the borough, with the priorities being key to it's delivery. We will focus on three priorities to help us achieve these outcomes: <strong>reducing health inequalities</strong>, <strong>supporting people out of poverty</strong> and <strong>addressing our climate crisis</strong>.</p>"
    #         ),
    # 
    #         fluidRow(
    #             column(width = 6,
    #                 tags$button(
    #                     id = "health_btn",
    #                     class = "btn action-button homeMenuButton",
    #                     span(
    #                         img(
    #                             src = "images/icon_health.png",
    #                             alt = "",
    #                             role = "presentation"
    #                         )
    #                     ),
    #                     div(
    #                         h3("Reducing health inequalities"),
    #                         p("Working with people, communities and partners, particularly in deprived areas, to improve the physical and mental health of all our residents.")
    #                     )
    #                 )
    #             ),
    #             column(width = 6,
    #                 tags$button(
    #                     id = "poverty_btn",
    #                     class = "btn action-button homeMenuButton",
    #                     span(
    #                         img(
    #                             src = "images/icon_poverty.png",
    #                             alt = "",
    #                             role = "presentation"
    #                         )
    #                     ),
    #                     div(
    #                         h3("Supporting people out of poverty"),
    #                         p("Tackling the root causes to prevent people from falling into poverty, and raising people out of it.")
    #                     )
    #                 )
    #             )
    #         ),
    #         fluidRow(
    #             column(width = 6,
    #                 tags$button(
    #                     id = "climate_btn",
    #                     class = "btn action-button homeMenuButton",
    #                     span(
    #                         img(
    #                             src = "images/icon_climate.png",
    #                             alt = "",
    #                             role = "presentation"
    #                         )
    #                     ),
    #                     div(
    #                         h3("Addressing our climate crisis"),
    #                         p("Reducing our carbon footprint and tackling the impact of climate change.")
    #                     )
    #                 )
    #             )#,
                # column(width = 6,
                #     tags$button(
                #         id = "services_btn",
                #         class = "btn action-button homeMenuButton",
                #         span (
                #             img(
                #                 src = "images/icon_services.png",
                #                 alt = "",
                #                 role = "presentation"
                #             )
                #         ),
                #         div(
                #             h3("Council services"),
                #             p("Although not part of the corporate plan, this section provides information on a number of services and functions provided by the council.")
                #         )
                #     )
                # )
    #        ),

    #         HTML("<h3>About the dashboard</h3>
    #               <p>The dashboard visualises a range of indicators relating to each of the three strategic priorities. These show data for Trafford compared to the average of other similar Local Authorities (in terms of statistical characteristics) and also, where possible, to England. Similar Local Authorities for indicators relating to children are defined within the <a href='https://www.gov.uk/government/publications/local-authority-interactive-tool-lait' target='_blank' aria-label=\"Children's Services Statistical Neighbour Benchmarking Tool, (opens in a new window)\">Children's Services Statistical Neighbour Benchmarking Tool</a>. For all other indicators the <a href='https://www.cipfa.org/services/cipfastats/nearest-neighbour-model' target='_blank' aria-label='CIPFA nearest neighbours, (opens in new window)'>CIPFA Nearest Neighbours</a> definition is used.</p>
    #               <p>
    #                   <details style='font-size: 0.85em'>
    #                       <summary><strong>Trafford's Children's Services Statistical Neighbours</strong></summary>
    #                       <ul>
    #                           <li>Bracknell Forest</li>
    #                           <li>Bromley</li>
    #                           <li>Buckinghamshire</li>
    #                           <li>Central Bedfordshire</li>
    #                           <li>Cheshire East</li>
    #                           <li>Hampshire</li>
    #                           <li>Hertfordshire</li>
    #                           <li>Solihull</li>
    #                           <li>Stockport</li>
    #                           <li>York</li>
    #                       </ul>
    #                   </details>
    #                   
    #                   <details style='font-size: 0.85em'>
    #                       <summary><strong>Trafford's CIPFA Nearest Neighbours</strong></summary>
    #                       <ul>
    #                           <li>Bedford</li>
    #                           <li>Bury</li>
    #                           <li>Cheshire West and Chester</li>
    #                           <li>Derby</li>
    #                           <li>Medway</li>
    #                           <li>Milton Keynes</li>
    #                           <li>Peterborough</li>
    #                           <li>Reading</li>
    #                           <li>Solihull</li>
    #                           <li>South Gloucestershire</li>
    #                           <li>Stockport</li>
    #                           <li>Swindon</li>
    #                           <li>Thurrock</li>
    #                           <li>Warrington</li>
    #                           <li>York</li>
    #                       </ul>
    #                   </details>
    #               </p>
    #               <p>The visualisations are interactive, displaying the values of the data presented. Some of the indicators have multiple visualisations showing different aspects of the data, which can be selected using the relevant tabs below them. Further information is also provided below each indicator, including links to download the data used in the visualisation(s) and to the original source of the data.</p>
    #               </div>"
    #         )
    #     ),
    #     # Pull in all the ui fragments for each of the priorities in the order we want the tabs to appear
    #     source("priorities/health/ui_fragment.R", local = TRUE)$value,
    #     source("priorities/poverty/ui_fragment.R", local = TRUE)$value,
    #     source("priorities/climate/ui_fragment.R", local = TRUE)$value,
    #     #source("priorities/services/ui_fragment.R", local = TRUE)$value
    # 
    # ),

    HTML('</main>
          <footer>
              <div>Developed in <a href="https://cran.r-project.org/" target="_blank" aria-label="R, (opens in new window)">R</a> by the <a href="https://www.trafforddatalab.io">Trafford Data Lab</a> under the <a href="https://www.trafforddatalab.io/LICENSE.txt">MIT</a> licence</div>
          </footer>'
    )
    #      
    #       <script>
    #           /*
    #               Receive call from Shiny server to make a given ggraph plot accessible.
    #               Despite the fact that the SVGs produced by ggraph are navigatable by screen readers, it is not a good user experience.
    #               This call handler and function add features to the SVG to improve this using the accessibility pattern:
    #               <svg> + role="img" + <title> + <desc> + aria-labelledby="[ID]" (https://www.smashingmagazine.com/2021/05/accessible-svg-patterns-comparison/#pattern-11-svg-role-img-title-desc-aria-labelledby-id)
    #           */
    #           Shiny.addCustomMessageHandler("a11yPlotSVG", function(message) {
    #               var a11yCallback = setInterval(function() {  // Setup a call to the update function every 500 milliseconds in case the plot does not exist yet
    #                   try {
    #                       // Split out the components of the message parameter passed to the function.
    #                       // These contain the id of the plot SVG we are manipulating, the title of the plot and the alt text
    #                       arrMsg = message.split("|")
    #                       svgId = arrMsg[0];
    #                       titleText = arrMsg[1]
    #                       altText = arrMsg[2];
    #                       
    #                       // Create a <title> element for the SVG containing the plot title
    #                       svgTitle = document.createElement("title");
    #                       svgTitle.setAttribute("id", svgId + "_title");
    #                       svgTitle.appendChild(document.createTextNode(titleText + ".")); // Add a full stop at the end (usually the titles do not have any) to add a pause between the title and the alt text when it is read aloud.
    #                       
    #                       // Create a <desc> element for the SVG containing the plot alt text
    #                       svgDesc = document.createElement("desc");
    #                       svgDesc.setAttribute("id", svgId + "_desc");
    #                       svgDesc.appendChild(document.createTextNode(altText));
    #                       
    #                       // Get the plot SVG DOM element, add the <title> and <desc> elements and set the required attributes
    #                       svg = document.getElementById(arrMsg[0]);
    #                       svg.appendChild(svgTitle);
    #                       svg.appendChild(svgDesc);
    #                       svg.setAttribute("role", "img");
    #                       svg.setAttribute("aria-labelledby", svgTitle.id + " " + svgDesc.id);
    #                       
    #                       clearInterval(a11yCallback);
    #                   }
    #                   catch(e) {
    #                       // An error occurred, try again in 500ms
    #                   }
    #               }, 500);
    #           });
    #       </script>')
)

# Declare the server code to supply objects to the user interface ---------
server <- function(input, output, session) {

    # Pull in all the server fragments for each of the priorities
    # source("priorities/health/server_fragment.R", local = TRUE)$value
    # source("priorities/poverty/server_fragment.R", local = TRUE)$value
    # source("priorities/climate/server_fragment.R", local = TRUE)$value
    #source("priorities/services/server_fragment.R", local = TRUE)$value

    # Event listeners for the buttons on the "introduction" tab to select the relevant tabs
    # observeEvent(input$health_btn, {
    #   updateTabsetPanel(session, "tabs", selected = "Health Inequalities")
    # })
    # 
    # observeEvent(input$poverty_btn, {
    #   updateTabsetPanel(session, "tabs", selected = "Poverty Reduction")
    # })
    # 
    # observeEvent(input$climate_btn, {
    #   updateTabsetPanel(session, "tabs", selected = "Climate Crisis")
    # })

    # observeEvent(input$services_btn, {
    #   updateTabsetPanel(session, "tabs", selected = "Council Services")
    # })
}

# Run the app
shinyApp(ui = ui, server = server)
