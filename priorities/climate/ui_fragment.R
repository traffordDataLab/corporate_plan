# User Interface code for Priority: Addressing our climate crisis
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Climate Crisis",
    h2("Addressing our climate crisis"),
    includeHTML("help.html"),
    fluidRow(
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Licensed vehicles"),
            uiOutput("licensed_vehicles_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "licensed_vehicles_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
                    includeMarkdown("data/climate/metadata/licensed_vehicles.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Household waste"),
            uiOutput("household_waste_recycling_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "household_waste_recycling_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/household_waste_recycling.md"),
            HTML('</details>')
        )
    )
)