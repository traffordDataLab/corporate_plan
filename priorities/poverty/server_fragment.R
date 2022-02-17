# Server code for Priority: Supporting people out of poverty

# Percentage receiving Universal Credit (UC) and the Claimant Count (CC) ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({
  
#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Number of people prevented from becoming homeless ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Improve the number of affordable housing completions ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Reduction in % of children in poverty ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Maintain the low level of 16-17 year olds who are NEET and NEET plus unknown ---------

# Load in data
df_neet <- read_csv("data/poverty/neet.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(indicator, period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$neet_plot <- renderggiraph({
  
  if (input$neet_selection == "Trend") {
    
    gg <- ggplot(df_neet %>% filter(indicator == "16-17 year olds not in education, employment or training (NEET)"),
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
      labs(title = "Academic age 16-17 year olds NEET",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL) +
      theme_x()
    
  } else {
    
    gg <- ggplot(df_neet %>% filter(indicator == "16-17 year olds not in education, employment or training (NEET) or whose activity is not known"),
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
      labs(title = "Academic age 16-17 year olds NEET or activity not known",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL) +
      theme_x()
    
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

# Render the output in the ui object
output$neet_box <- renderUI({
  withSpinner(
    ggiraphOutput("neet_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Improve the number of people being re-housed (from Trafford’s housing waiting list) ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Reduce % of households fuel poverty levels ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Improve overall employment rate (aged 16-64) (%) ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Improve employees paid at/above the real living wage ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Improve school readiness all children and those with a free school meal status ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderggiraph({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     ggiraphOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })
