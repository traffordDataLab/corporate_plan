# Health inequalities #

#  4-5 year old children classified as obese --------------------------------------------------

obese_r <- read_csv("data/health/obese_reception.csv") 

obese_reception <- obese_r %>%
  filter(indicator == "Reception: Prevalence of obesity (including severe obesity)") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

obese_reception_cssn_mean <- obese_reception %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

obese_reception_trend <- bind_rows(obese_reception %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), obese_reception_cssn_mean)

obese_reception_quintiles <- obese_r %>%
  filter(indicator == "Reception: Prevalence of obesity (including severe obesity), 5-years data combined") %>% 
  mutate(inequality = as_factor(inequality))

obese_r_quintiles_cssn_mean <- obese_reception_quintiles %>%
  filter(area_code %in% cssn$area_code) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value)) 

obese_r_quintiles_plot <- obese_reception_quintiles %>%
  filter(area_name %in% c("England", "Trafford")) %>%
  select(area_name,period,inequality,value) %>%
  bind_rows(obese_r_quintiles_cssn_mean) %>%
  mutate(area_name = factor(area_name, levels = c("Trafford","Similar Authorities average","England")))

obese_reception_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(obese_r %>% filter(area_type == "Ward") %>% select(area_code, indicator, value), by = "area_code")

output$obese_reception_plot <- renderggiraph({
  
  if (input$obese_reception_selection == "Trend") {
    
    gg <- ggplot(
      filter(obese_reception_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Obese children aged 4-5 years",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else if (input$obese_reception_selection == "Boxplot"){
    
    gg <- ggplot(data = filter(obese_reception, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(obese_reception, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(obese_reception, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(obese_reception, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(obese_reception, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = " Obese children aged 4-5 years",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  } else if (input$obese_reception_selection == "Deprivation"){
    gg <-
      ggplot(obese_r_quintiles_plot, aes(x = inequality, y = value, fill = area_name, group = area_name)) +
      geom_bar_interactive(aes(tooltip =
                                 paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                        '<span class="plotTooltipMain">', area_name, '</span><br />',
                                        '<span class="plotTooltipPeriod">', inequality, '</span><br />')), 
                           stat = "identity", width = 0.5, position = position_dodge(width=0.6)) +
      scale_fill_manual(
        values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      
      scale_x_discrete(labels = wrap_format(13)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = label_percent(scale = 1, accuracy = 1)) +
      labs(
        title = "Obese children aged 4-5 years by deprivation",
        subtitle = "2015/16 - 2019/20",
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, MHCLG",
        x = NULL,
        y = NULL
      ) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 0)))
  } else {
    gg <-
      ggplot(obese_reception_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(obese_reception_wards$value, na.rm=T),max(obese_reception_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)
      ) +
      labs(
        title = "Obese children aged 4-5 years by ward",
        subtitle = "2017/18 - 2019/20",
        caption = "Source: National Child Measurement Programme, NHS Digital",
        x = NULL,
        y = NULL
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$obese_reception_box <- renderUI({
  withSpinner(
    ggiraphOutput("obese_reception_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  10-11 year old children classified as obese --------------------------------------------------

obese_y6 <- read_csv("data/health/obese_year6.csv") 

obese_year6 <- obese_y6 %>%
  filter(indicator == "Year 6: Prevalence of obesity (including severe obesity)") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

obese_year6_cssn_mean <- obese_year6 %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

obese_year6_trend <- bind_rows(obese_year6 %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), obese_year6_cssn_mean)

obese_year6_quintiles <- obese_y6 %>%
  filter(indicator == "Year 6: Prevalence of obesity (including severe obesity), 5-years data combined") %>% mutate(inequality = as_factor(inequality))

obese_y6_quintiles_cssn_mean <- obese_year6_quintiles %>%
  filter(area_code %in% cssn$area_code) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value)) 

obese_y6_quintiles_plot <- obese_year6_quintiles %>%
  filter(area_name %in% c("England", "Trafford")) %>%
  select(area_name,period,inequality,value) %>%
  bind_rows(obese_y6_quintiles_cssn_mean) %>%
  mutate(area_name = factor(area_name, levels = c("Trafford","Similar Authorities average","England")))

obese_year6_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(obese_y6 %>% filter(area_type == "Ward") %>% select(area_code, indicator, value), by = "area_code")


output$obese_year6_plot <- renderggiraph({
  
  if (input$obese_year6_selection == "Trend") {
    
    gg <- ggplot(
      filter(obese_year6_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Obese children aged 10-11 years",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else if (input$obese_year6_selection == "Boxplot"){
    
    gg <- ggplot(data = filter(obese_year6, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(obese_year6, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(obese_year6, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(obese_year6, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(obese_year6, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = " Obese children aged 10-11 years",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  } else if (input$obese_year6_selection == "Deprivation"){
    gg <-
      ggplot(obese_y6_quintiles_plot, aes(x = inequality, y = value, fill = area_name, group = area_name)) +
      geom_bar_interactive(aes(tooltip =
                                 paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                        '<span class="plotTooltipMain">', area_name, '</span><br />',
                                        '<span class="plotTooltipPeriod">', inequality, '</span><br />')),
                           stat = "identity", width = 0.5, position = position_dodge(width=0.6)) +
      scale_fill_manual(
        values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_x_discrete(labels = wrap_format(13)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = label_percent(scale = 1, accuracy = 1)) +
      labs(
        title = "Obese children aged 10-11 years by deprivation",
        subtitle = "2015/16 - 2019/20",
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, MHCLG",
        x = NULL,
        y = NULL
      ) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 0)))
  } else {
    gg <-
      ggplot(obese_year6_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(obese_year6_wards$value, na.rm=T),max(obese_year6_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)) +
      labs(
        title = "Obese children aged 10-11 years by ward",
        subtitle = "2017/18 - 2019/20",
        caption = "Source: National Child Measurement Programme, NHS Digital",
        x = NULL,
        y = NULL
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$obese_year6_box <- renderUI({
  withSpinner(
    ggiraphOutput("obese_year6_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Adults classified as overweight or obese--------------------------------------------------

cipfa <- read_csv("data/cipfa2019.csv") %>%
  select(area_code)

overweight_adult <- read_csv("data/health/overweight_adult.csv") %>%
  filter(indicator == "Percentage of adults (aged 18+) classified as overweight or obese") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

overweight_adult_cipfa_mean <- overweight_adult %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

overweight_adult_trend <- bind_rows(overweight_adult %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), overweight_adult_cipfa_mean)


output$overweight_adult_plot <- renderggiraph({
  
  if (input$overweight_adult_selection == "Trend") {
    
    gg <- ggplot(
      filter(overweight_adult_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Adults classified as overweight or obese",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(overweight_adult, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(overweight_adult, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(overweight_adult, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(overweight_adult, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(overweight_adult, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Adults classified as overweight or obese",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$overweight_adult_box <- renderUI({
  withSpinner(
    ggiraphOutput("overweight_adult_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Active Adults --------------------------------------------------

active_adults <- read_csv("data/health/active_adults.csv") %>%
  filter(indicator == "Percentage of physically active adults") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

active_adults_cipfa_mean <- active_adults %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

active_adults_trend <- bind_rows(active_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), active_adults_cipfa_mean)


output$active_adults_plot <- renderggiraph({
  
  if (input$active_adults_selection == "Trend") {
    
    gg <- ggplot(
      filter(active_adults_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Physically active adults",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(active_adults, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(active_adults, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(active_adults, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(active_adults, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(active_adults, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Physically active adults",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$active_adults_box <- renderUI({
  withSpinner(
    ggiraphOutput("active_adults_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Inactive Adults --------------------------------------------------

inactive_adults <- read_csv("data/health/inactive_adults.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

inactive_adults_cipfa_mean <- inactive_adults %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

inactive_adults_trend <- bind_rows(inactive_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), inactive_adults_cipfa_mean)


output$inactive_adults_plot <- renderggiraph({
  
  if (input$inactive_adults_selection == "Trend") {
    
    gg <- ggplot(
      filter(inactive_adults_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Physically inactive adults",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(inactive_adults, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(inactive_adults, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(inactive_adults, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(inactive_adults, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(inactive_adults, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Physically inactive adults",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$inactive_adults_box <- renderUI({
  withSpinner(
    ggiraphOutput("inactive_adults_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Fairly Active Adults --------------------------------------------------

fairly_active_adults <- read_csv("data/health/fairly_active_adults.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

fairly_active_adults_cipfa_mean <- fairly_active_adults %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

fairly_active_adults_trend <- bind_rows(fairly_active_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), fairly_active_adults_cipfa_mean)


output$fairly_active_adults_plot <- renderggiraph({
  
  if (input$fairly_active_adults_selection == "Trend") {
    
    gg <- ggplot(
      filter(fairly_active_adults_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Physically fairly active adults",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$fairly_active_adults_box <- renderUI({
  withSpinner(
    ggiraphOutput("fairly_active_adults_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Active Children --------------------------------------------------

active_children <- read_csv("data/health/active_children.csv") %>%
  filter(indicator == "Percentage of physically active children and young people") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

active_children_cssn_mean <- active_children %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

active_children_trend <- bind_rows(active_children %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), active_children_cssn_mean)


output$active_children_plot <- renderggiraph({
  
  if (input$active_children_selection == "Trend") {
    
    gg <- ggplot(
      filter(active_children_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Physically active children",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(active_children, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(active_children, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(active_children, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(active_children, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(active_children, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Physically active children",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$active_children_box <- renderUI({
  withSpinner(
    ggiraphOutput("active_children_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Under 75 mortality rate --------------------------------------------------

mortality_rate <- read_csv("data/health/mortality_rate.csv") %>%
  #filter(unit == "Persons") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

mortality_rate_cipfa_mean <- mortality_rate %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period, unit) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

mortality_rate_trend <- bind_rows(mortality_rate %>% select(area_name, period,value,unit) %>% filter(area_name %in% c("Trafford", "England")), mortality_rate_cipfa_mean) 

mortality_rate_persons <- mortality_rate %>%
  filter(unit == "Persons")


output$mortality_rate_plot <- renderggiraph({
  
  if (input$mortality_rate_selection == "Trend") {
    
    gg <- ggplot(
      filter(mortality_rate_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             unit == "Persons"),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Under 75 mortality rate from preventable causes",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS",
        x = NULL,
        y = "per 100,000 polulation",
        colour = NULL
      ) +
      theme_x()
  }
  else if (input$mortality_rate_selection == "Boxplot"){
    
    gg <- ggplot(data = filter(mortality_rate_persons, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(mortality_rate_persons, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(mortality_rate_persons, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(mortality_rate_persons, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(mortality_rate_persons, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Under 75 mortality rate from preventable causes",
           subtitle = NULL,
           caption = "Source: Annual Mortality Extracts, ONS",
           x = NULL, y = "per 100,000 polulation",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  } else {
    
    gg <- ggplot(
      filter(mortality_rate_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             unit != "Persons", period %in% c("2015":"2020")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      facet_wrap(~unit, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Under 75 mortality rate from preventable causes",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS",
        x = NULL,
        y = "per 100,000 polulation",
        colour = NULL
      ) +
      theme_x()
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$mortality_rate_box <- renderUI({
  withSpinner(
    ggiraphOutput("mortality_rate_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Healthy life expectancy at birth --------------------------------------------------

healthy_life_expectancy <- read_csv("data/health/healthy_life_expectancy.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

healthy_life_expectancy_cipfa_mean <- healthy_life_expectancy %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

healthy_life_expectancy_trend <- bind_rows(healthy_life_expectancy %>% select(area_name, period,value,inequality) %>% filter(area_name %in% c("Trafford", "England")), healthy_life_expectancy_cipfa_mean)


output$healthy_life_expectancy_plot <- renderggiraph({
  
  if (input$healthy_life_expectancy_selection == "Sex") {
    
    gg <- ggplot(healthy_life_expectancy_trend,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Healthy life expectancy at birth by sex",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS, Annual Population Survey",
        x = NULL,
        y = "years",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(healthy_life_expectancy, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(healthy_life_expectancy, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(healthy_life_expectancy, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(healthy_life_expectancy, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(healthy_life_expectancy, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Healthy life expectancy at birth by sex",
           subtitle = NULL,
           caption = "Source: Annual Mortality Extracts, ONS, Annual Population Survey",
           x = NULL,
           y = "years",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$healthy_life_expectancy_box <- renderUI({
  withSpinner(
    ggiraphOutput("healthy_life_expectancy_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Inequality life expectancy at birth --------------------------------------------------

inequality_life_expectancy <- read_csv("data/health/inequality_life_expectancy.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

inequality_life_expectancy_cipfa_mean <- inequality_life_expectancy %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

inequality_life_expectancy_trend <- bind_rows(inequality_life_expectancy %>% select(area_name, period,value,inequality) %>% filter(area_name %in% c("Trafford", "England")), inequality_life_expectancy_cipfa_mean)


output$inequality_life_expectancy_plot <- renderggiraph({
  
  if (input$inequality_life_expectancy_selection == "Sex") {
    
    gg <- ggplot(inequality_life_expectancy_trend,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Inequality life expectancy at birth by sex",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS, Annual Population Survey",
        x = NULL,
        y = "years",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(inequality_life_expectancy, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(inequality_life_expectancy, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(inequality_life_expectancy, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(inequality_life_expectancy, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(inequality_life_expectancy, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Inequality life expectancy at birth by sex",
           subtitle = NULL,
           caption = "Source: Annual Mortality Extracts, ONS, Annual Population Survey",
           x = NULL,
           y = "years",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$inequality_life_expectancy_box <- renderUI({
  withSpinner(
    ggiraphOutput("inequality_life_expectancy_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Children Dental Decay--------------------------------------------------

children_dental_decay <- read_csv("data/health/children_dental_decay.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

children_dental_decay_cssn_mean <- children_dental_decay %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

children_dental_decay_trend <- bind_rows(children_dental_decay %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), children_dental_decay_cssn_mean)


output$children_dental_decay_plot <- renderggiraph({
  
  if (input$children_dental_decay_selection == "Trend") {
    
    gg <- ggplot(
      filter(children_dental_decay_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "5 year olds with obvious dental decay",
        subtitle = NULL,
        caption = "Source: Dental Public Health Epidemiology Programme for England",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(children_dental_decay, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(children_dental_decay, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(children_dental_decay, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(children_dental_decay, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(children_dental_decay, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "5 year olds with obvious dental decay",
           subtitle = NULL,
           caption = "Source: Dental Public Health Epidemiology Programme for England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$children_dental_decay_box <- renderUI({
  withSpinner(
    ggiraphOutput("children_dental_decay_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Smoking adults in manual occupations --------------------------------------------------

adults_smoking_manual <- read_csv("data/health/adults_smoking_manual.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

adults_smoking_manual_cipfa_mean <- adults_smoking_manual %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

adults_smoking_manual_trend <- bind_rows(adults_smoking_manual %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), adults_smoking_manual_cipfa_mean)


output$adults_smoking_manual_plot <- renderggiraph({
  
  if (input$adults_smoking_manual_selection == "Trend") {
    
    gg <- ggplot(
      filter(adults_smoking_manual_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Smoking adults in routine and manual occupations",
        subtitle = NULL,
        caption = "Source: Annual Population Survey, ONS",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(adults_smoking_manual, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(adults_smoking_manual, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(adults_smoking_manual, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(adults_smoking_manual, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(adults_smoking_manual, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(
        title = "Smoking adults in routine and manual occupations",
        subtitle = NULL,
        caption = "Source: Annual Population Survey, ONS",
        x = NULL, y = "Percentage",
        fill = "Compared with England:") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

output$adults_smoking_manual_box <- renderUI({
  withSpinner(
    ggiraphOutput("adults_smoking_manual_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})