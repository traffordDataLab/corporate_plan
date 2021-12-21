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
            HTML('<details>
                    <summary>Further information</summary>'),
                    includeMarkdown("data/climate/metadata/licensed_vehicles.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Indicator #2 title"),
            p("Indicator #2")
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Indicator #3 title"),
            p("Indicator #3")
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Indicator #4 title"),
            p("Indicator #4")
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Indicator #5 title"),
            p("Indicator #5")
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Indicator #6 title"),
            p("Indicator #6")
        )
    )
)