### PLOT PREP -----------------------------------------------------------------
who_plot_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))

who_plotly_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 8, family = "arial"),
        axis.title.y = element_text(size = 8, family = "arial"),
        plot.title = element_text(size = 11, face = "bold", family = "arial"),
        axis.text = element_text(size = 8, family = "arial"))

# Create caption to correctly source all plots
who_caption <- "Data from Johns Hopkins"
who_link <- "https://systems.jhu.edu/research/public-health/ncov/"
gapminder_link <- "https://www.gapminder.org/data/"

# Create colour palette
who_pal <- brewer.pal(n = 9, name = "YlOrRd")


### VECTORS FOR USER INPUT SELECTIONS IN SHINY APP ----------------------------
regions <- unique(who_clean$province_state)
countries <- sort(unique(who_clean$country_region))
plot_type <- c("Total cases", "New cases each day")
hundred_cases <- unique(who_hundred$country_region)  # countries with more than 100 cases
critical_countries <- c("China", "Italy", "Korea, South", "US")  # for growth plot
critical_countries_expanded <- c(critical_countries, "Germany", "France", "Spain", "United Kingdom") # for daily growth plot

### FUNCTIONS FOR PLOTS -------------------------------------------------------

growthPlot <- function(.df, .country_filter) {
  # Line plot of cases, with day 0 as day after 100 cases reported
  # Args: .df = dataframe
  #       .country_fiter = country name as string
  # Returns: plot
  
  # Locations for text labels
  suppressWarnings(label_location <- .df %>% 
                     filter(country_region %in% critical_countries | country_region == .country_filter) %>% 
                     filter(day_since_hdrd == max(day_since_hdrd)) %>% 
                     mutate(x_pos = day_since_hdrd, y_pos = cases, 
                            text_lab = glue(country_region, ": ", format(cases, big.mark = ","))))
  
  
  # Date for plot title
  plot_date <- format(max(.df$date), "%d-%b-%Y")
  
  .df %>% 
    filter(country_region %in% critical_countries | country_region == .country_filter) %>% 
    ggplot() +
    geom_line(aes(x = day_since_hdrd, y = cases, colour = country_region)) +
    geom_text(data = label_location, aes(x = x_pos, y = cases, 
                                         label = text_lab, colour = country_region), 
              nudge_x = 1, hjust = 0) +
    scale_y_log10(breaks=c(100, 1000, 10000, 100000, 1000000),
                  labels=c("100", "1,000", "10,000", "100,000", "1,000,000")) +
    scale_x_continuous(breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63), limits = c(0, 63)) +
    labs(title = glue("Confirmed COVID-19 cases: Data to ", plot_date),
         subtitle = "Growth since day after 100 cases reported, log scale",
         y = "Cases, log scale",
         x = "Days since 100 cases reported") +
    who_plot_theme +
    theme(legend.position = "None", 
          panel.grid.minor = element_blank())
}

growthPerCapitaPlot <- function(.df, .country_filter) {
  # Line plot of cases per capita, with day 0 as day after 100 cases reported
  # Args: .df = dataframe
  #       .country_fiter = country name as string
  # Returns: plot
  
  # Locations for text labels
  suppressWarnings(label_loc <- .df %>% 
                     filter(country_region %in% critical_countries | country_region == .country_filter) %>% 
                     filter(day_since_hdrd == max(day_since_hdrd)) %>% 
                     mutate(x_pos = day_since_hdrd, y_pos = cases_per_cap,
                            text_lab = glue(country_region, ": ", round(cases_per_cap, 0))))
  
  
  # Date for plot title
  plot_date <- format(max(.df$date), "%d-%b-%Y")
  
  .df %>% 
    filter(country_region %in% critical_countries | country_region == .country_filter) %>% 
    ggplot() +
    geom_line(aes(x = day_since_hdrd, y = cases_per_cap, colour = country_region)) +
    geom_text(data = label_loc, aes(x = x_pos, y = y_pos, 
                                    label = text_lab, colour = country_region), 
              nudge_x = 1, hjust = 0) +
    scale_y_log10(breaks=c(0.1, 1, 10, 100, 1000),
                  labels=c("0.1", "1", "10", "100", "1,000")) +
    scale_x_continuous(breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63), limits = c(0, 63)) +
    labs(title = glue("Confirmed COVID-19 cases per million: Data to ", plot_date),
         subtitle = "Growth since day after 100 cases reported, log scale",
         y = "Cases per million, log scale",
         x= "Days since 100 cases reported") +
    who_plot_theme +
    theme(legend.position = "None",
          panel.grid.minor = element_blank())
}

dailyCasesPerCapitaPlot <- function(.df, .country_filter) {
  # Line plot of new cases each day per capita, with day 0 as day after 100 cases reported
  # Args: .df = dataframe
  #       .country_fiter = country name as string
  # Returns: plot
  
  # Locations for text labels
  label_loc_ncpc <- .df %>% 
    filter(country_region %in% critical_countries_expanded | country_region == .country_filter) %>% 
    filter(day_since_hdrd == max(day_since_hdrd)) %>% 
    mutate(x_pos = day_since_hdrd, y_pos = new_cases_per_cap)
  
  plot_date <- format(max(.df$date), "%d-%b-%Y")
  plot_title <- glue("Confirmed COVID-19 cases per million: Data to ", plot_date)
  
  .df %>% 
    filter(country_region %in% critical_countries_expanded | country_region == .country_filter) %>% 
    filter(new_cases_per_cap > 0) %>% 
    ggplot() +
    geom_line(aes(x = day_since_hdrd, y = new_cases_per_cap, colour = country_region)) +
    geom_text(data = label_loc_ncpc, aes(x = x_pos, y = y_pos, 
                                         label = country_region, colour = country_region), 
              nudge_x = 1, hjust = 0) +
    facet_wrap(~country_region, nrow = 3, ncol = 3) +
    scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000),
                  labels=c("0.01", "0.1", "1", "10", "100", "1,000")) +
    scale_x_continuous(breaks = c(0, 7, 14, 21, 28, 35, 42, 49, 56, 63), limits = c(0, 63)) +
    labs(title = plot_title,
         subtitle = "Since day after 100 cases reported, log scale",
         y = "Daily cases per million, log scale",
         x= "Days since 100 cases reported") +
    who_plot_theme +
    theme(legend.position = "None",
          panel.grid.minor = element_blank())
}

countryPlot <- function(.df, .country_name, .y_var) {
  # Takes country names and creates col plot of cases
  # Args: .df = dataframe, .country_name = name of country as string
  #       .y_var = y axis variable as string
  # Returns: plot
  case_type <- case_when(.y_var == "cases" ~"Confirmed COVID-19 cases in ",
                         .y_var == "new_cases" ~"Daily confirmed COVID-19 cases in ")
  plot_date <- format(max(.df$date), "%d-%b-%Y")
  plot_title <- glue(case_type, .country_name, ", log scale\n", " Data to ", plot_date)
  
  
  country_plot <- .df %>% 
    group_by(country_region, date) %>% 
    summarise(cases = sum(cases, na.rm = T)) %>% 
    mutate(prev_cases = lag(cases, 1),
           new_cases = cases - prev_cases) %>% 
    drop_na() %>% 
    filter(country_region == .country_name & cases > 0) %>% 
    ggplot(aes(x = date)) +
    geom_col(aes_string(y = .y_var), fill = who_pal[7], width = 0.5, alpha = 0.8) +
    labs(title = plot_title,
         x = "",
         y = "") +
    who_plotly_theme +
    scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000, 1000000),
                  labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000"), 
                  limits=c(0.1, 1000000)) +
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%y")
  
  ggplotly(country_plot, tooltip = c("x", "y")) 
}

regionPlot <- function(.df, .state_name, .y_var) {
  # Takes country names and creates col plot of cases
  # Args: .df = dataframe, .country_name = name of country as string; 
  #       .y_var = y axis variable as string
  # Returns: plot
  case_type <- case_when(.y_var == "cases" ~"Confirmed COVID-19 cases in ",
                         .y_var == "new_cases" ~"Daily confirmed COVID-19 cases in ")
  plot_date <- format(max(.df$date), "%d-%b-%Y")
  plot_title <- glue(case_type, .state_name, ", linear scale\n", " Data to ", plot_date)
  
  region_plot <- .df %>% 
    group_by(province_state, date) %>% 
    summarise(cases = sum(cases, na.rm = T)) %>% 
    mutate(prev_cases = lag(cases, 1),
           new_cases = cases - prev_cases) %>% 
    drop_na() %>% 
    filter(province_state == .state_name & cases > 0) %>% 
    ggplot(aes(x = date)) +
    geom_col(aes_string(y = .y_var), fill = who_pal[7], width = 0.5, alpha = 0.8) +
    labs(title = plot_title,
         x = "",
         y = "") +
    who_plotly_theme +
    scale_y_continuous() +
    scale_x_date(date_breaks = "1 week", date_labels = "%d-%b-%y")
  
  ggplotly(region_plot, tooltip = c("x", "y"))
}