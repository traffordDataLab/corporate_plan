# Server code for Priority: Addressing our climate crisis

# Licensed vehicles (all vehicle types) ---------

# Load in data and create mean of similar neighbours
df_licensed_vehicles <- read_csv("data/climate/licensed_vehicles.csv") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value)))

# Plot
output$licensed_vehicles_plot <- renderggiraph({
  gg <- ggplot(df_licensed_vehicles,
               aes(x = period, y = value, colour = area_name, fill = area_name,)) +
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
               labs(title = "Number of licensed vehicles",
                    subtitle = "All vehicle types",
                    caption = "Source: DfT and DVLA",
                    x = NULL,
                    y = "Count",
                    fill = NULL) +
               theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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
