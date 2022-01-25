# User Interface code for Priority: Reducing health inequalities
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Health Inequalities",
    h2("Reducing health inequalities"),
    includeHTML("help.html"),
    fluidRow(
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("4-5 year olds with obesity"),
            uiOutput("obese_reception_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "obese_reception_selection",
                choiceNames = c("Trend", "Boxplot", "Deprivation", "Map"),
                choiceValues = c("Trend", "Boxplot", "Deprivation", "Map"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/obese_reception.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("10-11 year olds with obesity"),
            uiOutput("obese_year6_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "obese_year6_selection",
              choiceNames = c("Trend", "Boxplot", "Deprivation", "Map"),
              choiceValues = c("Trend", "Boxplot", "Deprivation", "Map"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/obese_year6.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Overweigth or obese adults"),
            uiOutput("overweight_adult_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "overweight_adult_selection",
              choiceNames = c("Trend", "Boxplot"),
              choiceValues = c("Trend", "Boxplot"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/overweight_adult.md"),
            HTML('</details>')
        )
    )
)
