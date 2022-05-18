# Server code for Priority: Addressing our climate crisis

# Licensed vehicles (all vehicle types) ---------

# Load in data and create mean of similar neighbours - data for England is excluded as the counts can't be compared to individual LAs
df_licensed_vehicles <- read_csv("data/climate/licensed_vehicles.csv") %>%
  filter(area_name != "England") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average"),
         period = as.character(period)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value_all_vehicles)))

# Plot
output$licensed_vehicles_plot <- renderggiraph({
  gg <- ggplot(df_licensed_vehicles,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
               geom_line(size = 1) +
               geom_point_interactive(
                 aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                                      '<span class="plotTooltipMain">', area_name, '</span><br />',
                                      '<span class="plotTooltipPeriod">', period, '</span>')),
                 shape = 21, size = 2.5, colour = "white"
               ) +
               scale_colour_manual(values = if_else(df_licensed_vehicles$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
               scale_fill_manual(values = if_else(df_licensed_vehicles$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
               scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
               labs(title = "All Licensed vehicles",
                    subtitle = NULL,
                    caption = "Source: DfT and DVLA",
                    x = NULL,
                    y = "Count",
                    fill = NULL,
                    alt = "Line chart showing the number of vehicles registered to Trafford addresses has been consistently lower compared to the average of similar authorities since 2016. Trafford registrations between 2015 and 2016 dropped by approximately 20,000 and have since been around 130,000, whereas the average for similar authorities has continued to rise, reaching 202,502 in 2020.") +
               theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_licensed_vehicles_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_licensed_vehicles_plot")
})

# Render the output in the ui object
output$licensed_vehicles_box <- renderUI({
  withSpinner(
    ggiraphOutput("licensed_vehicles_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Licensed vehicles (ulev) ---------

# Load in data and create percentages as well as average of similar authorities
df_licensed_ulev <- read_csv("data/climate/licensed_vehicles.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average"),
         period = as.character(period),
         value = round((value_ulev/value_all_vehicles)*100, 2)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 2))

# Plot
output$licensed_ulev_plot <- renderggiraph({
  gg <- ggplot(df_licensed_ulev,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Proportion of Ultra Low Emission Vehicles",
         subtitle = NULL,
         caption = "Source: DfT and DVLA",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing the proportion of licensed vehicles registered to Trafford addresses which are ultra low emission vehicles has been consistently lower, and increasing at a slower rate, compared to the average for similar authorities and England from 2011 to 2020. In 2020 the average proportion for similar authorities was 2.38% compared with an average of 1.18% in England and 0.81% in Trafford.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_licensed_ulev_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_licensed_ulev_plot")
})

# Render the output in the ui object
output$licensed_ulev_box <- renderUI({
  withSpinner(
    ggiraphOutput("licensed_ulev_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Vehicle miles travelled ---------

# Load in data and create mean of similar neighbours
df_vehicle_miles <- read_csv("data/climate/vehicle_miles_travelled.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England LA average" ~ "England LA average",
                               TRUE ~ "Similar authorities average"),
         period = as.character(period)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value)))

# Plot
output$vehicle_miles_plot <- renderggiraph({
  gg <- ggplot(df_vehicle_miles,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma(accuracy = 1)(value), ' million</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England LA average" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England LA average" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
    labs(title = "Annual motor vehicle miles travelled",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "Miles (millions)",
         fill = NULL,
         alt = "Line chart showing the number of miles travelled annually within Trafford between 2010 and 2020 is consistently lower compared with the average for similar authorities and the average of all local authorities in England. Mileage has been increasing year on year for all up to 2019, before showing a considerable decrease in 2020: 1,085 million down to 887 million miles in Trafford, 1,448 million down to 1,140 million miles for the average of similar authorities and 2,030 million down to 1,603 million miles for the average of all local authorities in England.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_vehicle_miles_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_vehicle_miles_plot")
})

# Render the output in the ui object
output$vehicle_miles_box <- renderUI({
  withSpinner(
    ggiraphOutput("vehicle_miles_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Electric vehicle charging points ---------

# Load in data and create mean of similar neighbours
df_ev_charging_points_rate <- read_csv("data/climate/electric_vehicle_charging_points.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$ev_charging_points_plot <- renderggiraph({
  gg <- ggplot(df_ev_charging_points_rate,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Publicly available electric vehicle charging devices",
         subtitle = NULL,
         caption = "Source: DfT and OZEV",
         x = NULL,
         y = "Devices (per 100K)",
         fill = NULL,
         alt = "Line chart showing that there have been consistently fewer publicly available charging devices per 100,000 people in Trafford compared to the average of similar authorities and England between October 2019 and April 2022. Whilst its comparitors have shown an increasing trend, Trafford's rate between October 2019 and July 2021 has been static, averaging approximately 20 devices per 100,000 people. Between July 2021 and January 2022 data for Trafford showed a promising improvement, increasing at a faster rate than it's comparitors despite still being lower. However the latest data for April 2022 shows a decrease from the previous period to 24.4 devices per 100,000 people compared to 42.3 for the average of similar authorities and 45.8 for England.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_charging_points_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_charging_points_plot")
})

# Render the output in the ui object
output$ev_charging_points_box <- renderUI({
  withSpinner(
    ggiraphOutput("ev_charging_points_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Household waste recycling ---------

# Load in data for percentages and tonnes separately and create mean of similar neighbours
df_household_waste_recycling_percentage <- read_csv("data/climate/household_waste_recycling.csv") %>%
  filter(measure == "Percentage") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

df_household_waste_recycling_tonnes <- read_csv("data/climate/household_waste_recycling.csv") %>%
  filter(measure == "Frequency") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$household_waste_recycling_plot <- renderggiraph({
  
  if (input$household_waste_recycling_selection == "% Trend") {
  
    gg <- ggplot(df_household_waste_recycling_percentage,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                             '<span class="plotTooltipMain">', area_name, '</span><br />',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(title = "Household waste collected and sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing the percentage of household waste collected and sent for recycling in Trafford has been higher than the average of similar authorities and England since 2011/12. Whilst the trend for its comparitors has remained broadly consistent, Trafford showed a big increase between 2021/13 (47.9%) and 2014/15 (61.9%) before going on a downward trend. Although all lines show a decrease from 2019/20 to 2020/21, Trafford still collected and sent for recycling a higher percentage of waste (53.3%) compared to the average of similar authorities (44.5%) and the average for England (42.3%).") +
      theme_x()
    
  } else {
    
    gg <- ggplot(df_household_waste_recycling_tonnes,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                             '<span class="plotTooltipMain">', area_name, '</span><br />',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = if_else(df_household_waste_recycling_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_fill_manual(values = if_else(df_household_waste_recycling_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
      labs(title = "Household waste collected and sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Tonnes",
           fill = NULL,
           alt = "Line chart showing between 2014/15 and 2016/17 Trafford collected and sent for recycling around 50,000 tonnes of household waste, approximately 7,000 more than the average for similar authorities. Since then the amounts collected in Trafford have decreased to a similar level to its comparator. 2020/21 showed a slightly higher amount of 46,040 tonnes in Trafford compared to an average of 44,259 tonnes for similar authorities.") +
      theme_x()
    
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_household_waste_recycling_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_household_waste_recycling_plot")
})

# Render the output in the ui object
output$household_waste_recycling_box <- renderUI({
  withSpinner(
    ggiraphOutput("household_waste_recycling_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px",
  )
})


# Household waste not recycled ---------

# Load in data for percentages and tonnes separately and create mean of similar neighbours
df_household_waste_not_recycled_percentage <- read_csv("data/climate/household_waste_not_recycled.csv") %>%
  filter(measure == "Percentage") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

df_household_waste_not_recycled_tonnes <- read_csv("data/climate/household_waste_not_recycled.csv") %>%
  filter(measure == "Frequency") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))


# Plot
output$household_waste_not_recycled_plot <- renderggiraph({
  
  if (input$household_waste_not_recycled_selection == "% Trend") {
  
    gg <- ggplot(df_household_waste_not_recycled_percentage,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                             '<span class="plotTooltipMain">', area_name, '</span><br />',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(title = "Household waste collected not sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing that Trafford has had a lower percentage of household waste collected but not sent for recycling than the average of similar authorities and England between 2011/12 and 2020/21, although the gap has been narrowing since 2016/17. In 2020/21 the percentage for Trafford was 46.7% compared to 55.5% for the average of similar authorities and 57.7% for the England average.") +
      theme_x()
    
  } else {
  
    gg <- ggplot(df_household_waste_not_recycled_tonnes,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                             '<span class="plotTooltipMain">', area_name, '</span><br />',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = if_else(df_household_waste_not_recycled_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_fill_manual(values = if_else(df_household_waste_not_recycled_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
      labs(title = "Household waste collected not sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Tonnes",
           fill = NULL,
           alt = "Line chart showing Trafford has had a lower tonnage of household waste that was collected but not sent for recycling compared to the average of similar authorities between 2014/15 and 2020/21. Despite the gap narrowing since 2016/17, Trafford recorded 40,394 tonnes in 2020/21 compared to 52,042 tonnes for the average of similar authorities.") +
      theme_x()
  
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_household_waste_not_recycled_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_household_waste_not_recycled_plot")
})

# Render the output in the ui object
output$household_waste_not_recycled_box <- renderUI({
  withSpinner(
    ggiraphOutput("household_waste_not_recycled_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Domestic Energy Performance Certificates ---------

# Load in data
df_epc <- read_csv("data/climate/energy_performance_certificates.csv")

# function to calculate 10-year periods of data as this is the validity period for the certificates
calc10YearProportionEPC <- function(year_from) {
  # NOTE: +9 not +10 as year_from+9 = 10 years of data e.g. 2012 - 2021
  filter(df_epc, period >= year_from, period <= year_from+9) %>%
  group_by(area_code, area_name) %>%
  summarise(value_certificates_lodged = sum(value_certificates_lodged),
            value_rating_A = sum(value_rating_A),
            value_rating_B = sum(value_rating_B),
            value_rating_C = sum(value_rating_C)) %>%
  mutate(period = paste0(year_from, " - ", year_from+9),
         value_AC = sum(value_rating_A, value_rating_B, value_rating_C),
         value = (value_AC/value_certificates_lodged)*100) %>%
  select(area_code, area_name, period, value)
}

# Get the data for the 10 year periods
df_epc_2009 <- calc10YearProportionEPC(2009)
df_epc_2010 <- calc10YearProportionEPC(2010)
df_epc_2011 <- calc10YearProportionEPC(2011)
df_epc_2012 <- calc10YearProportionEPC(2012)

# Combine the datasets, then create the average of similar LAs
df_epc <- bind_rows(df_epc_2009,
                    df_epc_2010,
                    df_epc_2011,
                    df_epc_2012) %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$domestic_epc_plot <- renderggiraph({
  gg <- ggplot(df_epc,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Domestic EPC rated A, B or C over 10 years",
         subtitle = NULL,
         caption = "Source: DLUHC",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing that over 10 year periods Trafford has consistently lower percentages of domestic properties with Energy Performance Certificates (EPC) rated A, B or C than the average of similar authorities (10% fewer) or England (7% fewer). The latest time period available, 2012-2021 shows 33.4% of domestic properties in Trafford having EPCs with the most efficient ratings, compared to 43.3% for the average of similar authorities and 40.5% for England.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_domestic_epc_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_domestic_epc_plot")
})

# Render the output in the ui object
output$domestic_epc_box <- renderUI({
  withSpinner(
    ggiraphOutput("domestic_epc_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Borough wide CO2 emissions ---------

# Load in data and create mean of similar neighbours
df_borough_co2_emissions <- read_csv("data/climate/borough_wide_co2_emissions.csv") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average"),
         period = as.character(period)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value)))

# Plot
output$borough_co2_emissions_plot <- renderggiraph({
  gg <- ggplot(df_borough_co2_emissions,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = if_else(df_borough_co2_emissions$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
    scale_fill_manual(values = if_else(df_borough_co2_emissions$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
    labs(title = expression(paste("Territorial Carbon Dioxide (", CO[2], ") emission estimates")),
         subtitle = NULL,
         caption = "Source: BEIS",
         x = NULL,
         y = "Kilotonnes (kt)",
         fill = NULL,
         alt = "Line chart showing that territorial CO2 emissions in Trafford were higher than the average for similar authorities between 2010 and 2019. Although the amount has been decreasing since 2013, the average for similar authorities has been decreasing at the same rate. The latest data for 2019 shows 1,468 kilotonnes of CO2 emitted within the borough of Trafford compared to the similar authorities average of 1,238 kilotonnes.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_borough_co2_emissions_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_borough_co2_emissions_plot")
})

# Render the output in the ui object
output$borough_co2_emissions_box <- renderUI({
  withSpinner(
    ggiraphOutput("borough_co2_emissions_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Nitrogen Dioxide (NO2) Concentration ---------

# Load in data
df_no2_concentration <- read_csv("data/climate/no2_concentration.csv") %>%
  mutate(period = as.character(period))

# Plot
output$no2_concentration_plot <- renderggiraph({
  gg <- ggplot(df_no2_concentration,
               aes(x = period, y = value, colour = station_name, fill = station_name, group = station_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '</span><br />',
                           '<span class="plotTooltipMain">', station_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white",
      show.legend = FALSE
    ) +
    scale_colour_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458", "Trafford Wellacre Academy" = "#FA9209")) +
    scale_fill_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458", "Trafford Wellacre Academy" = "#FA9209")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = expression(paste("Annual mean Nitrogen Dioxide (", NO[2], ") concentration")),
         subtitle = NULL,
         caption = "Source: Trafford Council and Ricardo EE",
         x = NULL,
         y = expression(paste("µg/m"^3)),
         fill = NULL,
         colour = "Location: ",
         alt = "Line chart showing the annual mean of N.O.2 readings taken between 2013 and 2021 at 3 monitoring stations within Trafford: Trafford A56, Trafford Moss Park and Trafford Wellacre Academy. Readings from Trafford A56 are the highest, followed by Trafford Moss Park and then Trafford Wellacre Academy, with all 3 showing an overall gradual decreasing trend in the readings since 2016. All 3 stations showed decreases between 2019 to 2020 recording their lowest annual readings in the time period plotted. Readings from all showed slight increases in 2021 from the previous year, with Trafford A56 recording 23.1 microgrammes of N.O.2 per cubic metre compared with 15 microgrammes at Trafford Moss Park and 13.3 microgrammes at Trafford Wellacre Academy.") +
    theme_x() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_no2_concentration_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_no2_concentration_plot")
})

# Render the output in the ui object
output$no2_concentration_box <- renderUI({
  withSpinner(
    ggiraphOutput("no2_concentration_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Particulate Matter (PM10) Concentration ---------

# Load in data
df_pm10_concentration <- read_csv("data/climate/pm10_concentration.csv") %>%
  mutate(period = as.character(period))

# Plot
output$pm10_concentration_plot <- renderggiraph({
  gg <- ggplot(df_pm10_concentration,
               aes(x = period, y = value, colour = station_name, fill = station_name, group = station_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '</span><br />',
                           '<span class="plotTooltipMain">', station_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white",
      show.legend = FALSE
    ) +
    scale_colour_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458")) +
    scale_fill_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = expression(paste("Annual mean Particulate Matter (", PM[10], ") concentration")),
         subtitle = NULL,
         caption = "Source: Trafford Council and Ricardo EE",
         x = NULL,
         y = expression(paste("µg/m"^3)),
         fill = NULL,
         colour = "Location: ",
         alt = "Line chart showing the annual mean of PM10 readings taken between 2013 and 2021 at 2 monitoring stations within Trafford: Trafford A56 and Trafford Moss Park. The trend for both has been a generally stready decline, with the exception of 2018 and 2019. However levels in 2021 are around the lowest recorded within the timeframe plotted, with Trafford A56 recording 14.4 microgrammes of PM10 per cubic metre compared to 13.2 microgrammes at Trafford Moss Park.") +
    theme_x() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_pm10_concentration_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_pm10_concentration_plot")
})

# Render the output in the ui object
output$pm10_concentration_box <- renderUI({
  withSpinner(
    ggiraphOutput("pm10_concentration_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Adults who walk or cycle 5 times per week ---------

# Load in data and create percentages as well as average of similar authorities
df_adults_walk_cycle <- read_csv("data/climate/adults_walking_or_cycling.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$adults_walk_cycle_plot <- renderggiraph({
  gg <- ggplot(df_adults_walk_cycle,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(size = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Adults walking or cycling five times per week",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing the percentage of adults in Trafford regularly participating in walking or cycling activities compared with the average of similar authorities and England from 2015-16 to 2019-20. Up to 2018-19 Trafford was consistently showing around 1 percentage point lower participation than the average of similar authorities and 2 percentage points lower than the England average. However in 2019-20 a sharper participation increase in Trafford coupled with a decrease in its comparitors closed the gap, with both Trafford and the England average recording 34.5% participation compared with 34.2% for the average of similar authorities.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_adults_walk_cycle_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_adults_walk_cycle_plot")
})

# Render the output in the ui object
output$adults_walk_cycle_box <- renderUI({
  withSpinner(
    ggiraphOutput("adults_walk_cycle_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})
