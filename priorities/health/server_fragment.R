# Health inequalities #

# Child classified as obese in 4-5 year olds --------------------------------------------------

obese_reception <- read_csv("data/health/obese_reception.csv") %>%
  filter(indicator == "Reception: Prevalence of obesity (including severe obesity)") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

obese_reception_cssn_mean <- read_csv("data/health/obese_reception.csv") %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "CSSN mean",
         period = as_factor(period)) %>%
  filter(!is.na(value))

obese_reception_trend <- bind_rows(obese_reception %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), obese_reception_cssn_mean)

output$obese_reception_plot <- renderggiraph({

  if (input$obese_reception_selection == "Trend") {

    gg <- ggplot(
      filter(obese_reception_trend, area_name %in% c("Trafford", "CSSN mean", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(size = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "CSSN mean" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "CSSN mean" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Children aged 4-5 years who have excess weight",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL
      ) +
      theme_x()
  }
  else {

    gg <- ggplot(data = filter(obese_reception, area_name != "England"),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(obese_reception, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =  paste0(
                                   "<strong>", value, "</strong>", "%", "<br/>",
                                   "<em>", area_name, "</em><br/>",
                                   period)),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(obese_reception, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =  paste0(
                                     "<strong>", filter(obese_reception, area_name == "England")$value, "</strong>", "%", "<br/>",
                                     "<em>", "England", "</em><br/>",
                                     filter(obese_reception, area_name == "England")$period)),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Children aged 4-5 years who have excess weight",
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
