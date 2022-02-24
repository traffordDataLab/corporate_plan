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

children_poverty <- read_csv("data/poverty/children_poverty.csv") #%>%
#mutate(period = as_factor(period)) %>%

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

children_poverty_cssn_mean <- children_poverty %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period, indicator) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average"#,
         #period = as_factor(period)
  ) %>%
  filter(!is.na(value))

children_poverty_trend <- bind_rows(children_poverty %>% select(area_name, period,value,indicator) %>% filter(area_name %in% c("Trafford", "England")), children_poverty_cssn_mean) 


# Plot
output$children_poverty_plot <- renderggiraph({
  
  if (input$children_poverty_selection == "Rel. Trend") {
    
    gg <- ggplot(
      filter(children_poverty_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             indicator == "Children in relative low income families (under 16s)"),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Children in relative low income families",
        subtitle = NULL,
        caption = "Source: DWP",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
    
  } else {

    gg <- ggplot(
      filter(children_poverty_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             indicator == "Children in absolute low income families (under 16s)"),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Children in absolute low income families",
        subtitle = NULL,
        caption = "Source: DWP",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
    
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
  
  
})

# Render the output in the ui object
output$children_poverty_box <- renderUI({
  withSpinner(
    ggiraphOutput("children_poverty_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


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

employment_rate <- read_csv("data/poverty/employment_rate.csv") #%>%
  #mutate(period = as_factor(period)) %>%
  #filter(!is.na(value))

employment_rate_cipfa_mean <- employment_rate %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average"#,
         #period = as_factor(period)
         ) %>%
  filter(!is.na(value))

employment_rate_trend <- bind_rows(employment_rate %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), employment_rate_cipfa_mean) %>%
  mutate(period = str_sub(period, start = 10))

# Plot
output$employment_rate_plot <- renderggiraph({
  
  if (input$employment_rate_selection == "Trend") {
    
    gg <- ggplot(
      filter(employment_rate_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Employment rate - aged 16 to 64",
        subtitle = NULL,
        caption = "Source: Annual Polulation Survey",
        x = "12 months ending",
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
  

})

# Render the output in the ui object
output$employment_rate_box <- renderUI({
  withSpinner(
    ggiraphOutput("employment_rate_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Improve employees paid at/above the real living wage ---------

# Load in data
df_real_living_wage <- read_csv("data/poverty/real_living_wage.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$real_living_wage_plot <- renderggiraph({
  gg <- ggplot(df_real_living_wage,
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
    labs(title = "Employees paid at or above the real living wage",
         subtitle = NULL,
         caption = "Source: ONS",
         x = NULL,
         y = "Percentage",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

# Render the output in the ui object
output$real_living_wage_box <- renderUI({
  withSpinner(
    ggiraphOutput("real_living_wage_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Improve school readiness all children and those with a free school meal status ---------

# Load in data
df_school_readiness <- read_csv("data/poverty/school_readiness.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(indicator, period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$school_readiness_plot <- renderggiraph({
  
  if (input$school_readiness_selection == "Trend") {
    
    gg <- ggplot(df_school_readiness %>% filter(indicator == "School readiness: percentage of children achieving a good level of development at the end of Reception"),
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
      labs(title = "Reception pupils: good level of development",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL) +
      theme_x()
    
  } else {
    
    gg <- ggplot(df_school_readiness %>% filter(indicator == "School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception"),
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
      labs(title = "Reception pupils: good level of development (FSM)",
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
output$school_readiness_box <- renderUI({
  withSpinner(
    ggiraphOutput("school_readiness_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Percentage of pupils reaching the expected standard at the end of key stage 2 in reading, writing and mathematics ---------

# Load in data
df_expected_standard_ks2 <- read_csv("data/poverty/expected_standard_ks2.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$expected_standard_ks2_plot <- renderggiraph({
  gg <- ggplot(df_expected_standard_ks2,
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
    labs(title = "Pupils reaching expected standard at KS2 (RWM)",
         subtitle = NULL,
         caption = "Source: DfE",
         x = NULL,
         y = "Percentage",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

# Render the output in the ui object
output$expected_standard_ks2_box <- renderUI({
  withSpinner(
    ggiraphOutput("expected_standard_ks2_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

