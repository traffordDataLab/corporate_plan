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
  } else {
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
        subtitle = NULL,
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, ONS",
        x = NULL,
        y = NULL
      ) +
      theme_x() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 0)))
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

#10-11 year old children classified as obese --------------------------------------------------

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
  } else {
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
        subtitle = NULL,
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, ONS",
        x = NULL,
        y = NULL
      ) +
      theme_x() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 0)))
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

