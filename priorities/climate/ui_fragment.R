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
            h3("Vehicle miles"),
            uiOutput("vehicle_miles_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "vehicle_miles_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/vehicle_miles_travelled.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Electric Vehicle Charging"),
            uiOutput("ev_charging_points_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "ev_charging_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/electric_vehicle_charging_points.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>Household waste <small>(1)</small></h3>"),
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
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>Household waste <small>(2)</small></h3>"),
            uiOutput("household_waste_not_recycled_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "household_waste_not_recycled_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/household_waste_not_recycled.md"),
            HTML('</details>')
        )
    )
)