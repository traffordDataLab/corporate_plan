# User Interface code for Priority: Addressing our climate crisis
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Climate Crisis",
    HTML('<div id="climatePageContainer">
          <h2>Addressing our climate crisis</h2>'
    ),
    includeHTML("help.html"),
    fluidRow(
        # indicator visualisations go here
    ),

    HTML('</div>')
)
