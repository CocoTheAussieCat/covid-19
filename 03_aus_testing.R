### IMPORT DATA ---------------------------------------------------------------
# From Guardian Australia JSON feed
# Guardian Australia https://www.theguardian.com/au
aus_guardian_html <- "https://interactive.guim.co.uk/docsdata/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE.json"
aus_org <- fromJSON(aus_guardian_html, flatten = T)
aus_df <- aus_org$sheets$updates

# Replace blanks with NA, remove commas in numbers
aus_df_clean <- aus_df %>%
  mutate_all(~na_if(., "")) %>% 
  mutate_all(~(gsub(",", "", .)))


### CLEAN DATA ----------------------------------------------------------------
# Convert data type, create date_time variable, rename
aus_clean <- suppressWarnings(aus_df_clean %>% 
                                mutate(State = fct_explicit_na(State, na_level = "Missing")) %>% 
                                mutate(Date = dmy(Date)) %>% 
                                mutate(Time = ifelse(!is.na(Time), paste0(Time, ":00"), "00:00:00")) %>% 
                                mutate(date_time = as.POSIXct(Date + lubridate:: hms(Time))) %>% 
                                select(-`Update Source`,-`Notes` ) %>% 
                                mutate_if(is_character, as.numeric) %>% 
                                select(state = State, 
                                       date = Date, 
                                       date_time = date_time,
                                       cases = `Cumulative case count`, 
                                       deaths = `Cumulative deaths`,
                                       tests =`Tests conducted (total)`,
                                       neg = `Tests conducted (negative)`, 
                                       icu = `Intensive care (count)`,
                                       hospital = `Hospitalisations (count)`, 
                                       recovered = `Recovered (cumulative)`)
)

aus_clean %>% 
  filter(state == "NSW") %>% 
  arrange(desc(date)) %>% 
  glimpse()
  

# Create dense dataframe
# For each day, select maximum value for all numeric variables
# If neg and cases exists but tests doesn't, tests = neg+cases
# Fill NA with previous value

aus_dense <- aus_clean %>% 
  mutate_if(is.numeric, ~(if_else(is.na(.), 0, .))) %>% 
  group_by(state, date) %>%
  arrange(date) %>% 
  summarise_if(is.numeric, max) %>%
  mutate_all(~na_if(., 0)) %>% 
  mutate(tests = if_else(is.na(tests), 
                         if_else(!is.na(neg), neg + cases, tests),
                         tests)) %>% 
  fill(cases, deaths, tests, icu, hospital, recovered) %>% 
  fill(cases, deaths, tests, icu, hospital, recovered, .direction = "up") %>% 
  select(-neg) %>% 
  ungroup()
  

# Add population stats
aus_pop <- tribble(~"province_state", ~"state", ~"population",
                   "New South Wales", "NSW", 8118000,
                   "Victoria", "VIC", 6229900,
                   "Queensland", "QLD", 5115500,
                   "Western Australia", "WA", 2630600,
                   "South Australia", "SA", 1756500,
                   "Tasmania", "TAS", 535500,
                   "Australian Capital Territory", "ACT", 428100,
                   "Northern Territory", "NT", 245600) %>% 
  mutate(state = as.factor(state))

aus_with_pop <- aus_dense %>% 
  left_join(aus_pop, by = "state")

# Add new cases and per capita variables
aus_test <- aus_with_pop %>% 
  group_by(state) %>% 
  mutate(prev_cases = lag(cases, 1),
         new_cases = cases - prev_cases,
         cases_per_cap = cases/population*10^6,
         new_cases_per_cap =  new_cases/population*10^6)%>% 
  mutate(prev_tests= lag(tests, 1),
         new_tests = tests - prev_tests)%>%
  ungroup() %>% 
  mutate(pos_test_ratio = cases/tests,
         test_per_cap = tests/population * 10^6) 


# Filter for more recent day's stats for plotting
aus_test_current <- aus_test %>%
  group_by(state) %>% 
  filter(date == max(date)) %>% 
  select(state, date, cases, tests, cases_per_cap, pos_test_ratio, test_per_cap) %>% 
  glimpse()


### PLOT PREP -----------------------------------------------------------------
aus_plot_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 12, family = "sans"),
        axis.title.y = element_text(size = 12, family = "sans"),
        axis.text = element_text(size = 12, family = "sans"),
        plot.title = element_text(size = 16, face = "bold", family = "sans"),
        plot.subtitle = element_text(size = 12, face = "bold", family = "sans"),
        plot.caption = element_text(size = 12, face = "italic", hjust = 0, family = "sans"),
        legend.position = "None",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

label_size <- 4

aus_date <- format(max(aus_test_current$date), "%d %B %Y")
aus_plot_title <- glue("Australian COVID-19 testing to ", aus_date)

# Create colour palette
aus_pal <- brewer.pal(n = 8, name = "Dark2")

# 1. Plot of total cases
total_cases_plot <- aus_test_current %>% 
  ggplot(aes(x = reorder(state, cases), y = cases)) +
  geom_point(aes(colour = reorder(state, cases))) +
  geom_segment(aes(yend = 0, xend = state, colour = reorder(state, cases))) +
  geom_text(aes(y = cases + 20, label = cases, 
                colour = reorder(state, cases)), hjust = "left", size = label_size) +
  labs(title = aus_plot_title,
    subtitle = "Confirmed cases",
       x = "",
       y = "") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 3000), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000)) +
  aus_plot_theme +
  scale_color_brewer(palette = "Dark2")

# 2. Plot of total tests
total_tests_plot <- aus_test_current %>% 
  ggplot(aes(x = reorder(state, cases), y = tests/10^3)) +
  geom_point(aes(colour = reorder(state, cases))) +
  geom_segment(aes(yend = 0, xend = state, colour = reorder(state, cases))) +
  geom_text(aes(y = tests/10^3 + 1, label = scales::comma(round(tests/10^3, digits = 1), suffix = "k"), 
                colour = reorder(state, cases)), hjust = "left", size = label_size) +
  labs(subtitle = "Tests administered",
       x = "",
       y = "") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 200), breaks = c(0, 50, 100, 150, 200), 
                     labels = scales::comma_format(suffix = "k")) +
  aus_plot_theme +
  scale_color_brewer(palette = "Dark2")

# 3. Plot of tests per million people
test_per_cap_plot <- aus_test_current %>% 
  ggplot(aes(x = reorder(state, cases), y = test_per_cap/10^3)) +
  geom_point(aes(colour = reorder(state, cases))) +
  geom_segment(aes(yend = 0, xend = state, colour = reorder(state, cases))) +
  geom_text(aes(y = test_per_cap/10^3 + 0.2, label = round(test_per_cap/10^3, digits = 1), 
                colour = reorder(state, cases)), 
            hjust = "left", size = label_size) +
  labs(subtitle = "Tests per thousand people",
       x = "",
       y = "") +
  coord_flip() +
  aus_plot_theme +
  scale_y_continuous(limits = c(0, 20), breaks = c(0, 5, 10, 15, 20)) +
  scale_color_brewer(palette = "Dark2")


# 4. Plot of positive case ratio
pos_test_plot <- aus_test_current %>% 
  ggplot(aes(x = reorder(state, cases), y = pos_test_ratio)) +
  geom_point(aes(colour = reorder(state, cases))) +
  geom_segment(aes(yend = 0, xend = state, colour = reorder(state, cases))) +
  geom_text(aes(y = pos_test_ratio + 0.0025, 
                label = percent(pos_test_ratio, accuracy = 0.1), colour = reorder(state, cases)), 
            hjust = "left", size = label_size) +
  labs(subtitle = "% Tests positive",
       x = "",
       y = "") +
  coord_flip() +
  aus_plot_theme +
  scale_y_continuous(labels = percent_format(0.1), limits = c(0, 0.05)) +
  scale_color_brewer(palette = "Dark2")

# Combine plots using {patchwork} to form 2x2
patch_1 <- total_cases_plot + total_tests_plot
patch_2 <- test_per_cap_plot + pos_test_plot
patch <- patch_1 / patch_2   # sent to Shiny App



