# User Interface code for Priority: Reducing health inequalities
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Health Inequalities",
    h2("Reducing health inequalities"),
    includeHTML("help.html"),
    fluidRow(
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("4-5 year olds with excess weight"),
            uiOutput("obese_reception_box"),
            radioGroupButtons(
                inputId = "obese_reception_selection",
                choiceNames = c("Trend", "Boxplot"),
                choiceValues = c("Trend", "Boxplot"),
                selected = "Trend",
                direction = "horizontal",
                justified = TRUE,
                individual = FALSE
            ),
        )
    )
)
