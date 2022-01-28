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
    labs(title = "Percentage of Ultra Low Emission Vehicles",
         subtitle = "Compared to all licensed vehicles",
         caption = "Source: DfT and DVLA",
         x = NULL,
         y = "Percentage",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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
    labs(title = "Annual motor vehicle traffic",
         subtitle = "Miles travelled (millions)",
         caption = "Source: DfT",
         x = NULL,
         y = "Miles (millions)",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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
         subtitle = "Per 100,000 population",
         caption = "Source: DfT and OZEV",
         x = NULL,
         y = "Devices (per 100K)",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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

# Load in data and create mean of similar neighbours
df_household_waste_recycling <- read_csv("data/climate/household_waste_recycling.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$household_waste_recycling_plot <- renderggiraph({
  gg <- ggplot(df_household_waste_recycling,
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
    labs(title = "Household waste sent for recycling",
         subtitle = "Percentage collected",
         caption = "Source: DEFRA",
         x = NULL,
         y = "Percentage",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
})

# Render the output in the ui object
output$household_waste_recycling_box <- renderUI({
  withSpinner(
    ggiraphOutput("household_waste_recycling_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Household waste not recycled ---------

# Load in data and create mean of similar neighbours
df_household_waste_not_recycled <- read_csv("data/climate/household_waste_not_recycled.csv") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$household_waste_not_recycled_plot <- renderggiraph({
  gg <- ggplot(df_household_waste_not_recycled,
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
    labs(title = "Household waste not sent for recycling",
         subtitle = "Tonnage collected",
         caption = "Source: DEFRA",
         x = NULL,
         y = "Tonnes",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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
    labs(title = "Domestic Energy Performance Certificates (EPC)",
         subtitle = "Proportion rated A, B or C over 10 year periods",
         caption = "Source: DLUHC",
         x = NULL,
         y = "Percentage",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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
    labs(title = "Local Authority territorial CO2 emissions estimates",
         subtitle = "Carbon dioxide emissions within region borders",
         caption = "Source: BEIS",
         x = NULL,
         y = "Kilotonnes (kt)",
         fill = NULL) +
    theme_x()
  
  girafe(ggobj = gg, options = lab_ggiraph_options)
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
