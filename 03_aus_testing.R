### IMPORT DATA ---------------------------------------------------------------

# Australian state populations (data taken from ABS Sep 2019)
aus_pop <- tribble(~"province_state", ~"state", ~"population",
                   "New South Wales", "NSW", 8118000,
                   "Victoria", "VIC", 6229900,
                   "Queensland", "QLD", 5115500,
                   "Western Australia", "WA", 2630600,
                   "South Australia", "SA", 1756500,
                   "Tasmania", "TAS", 535500,
                   "Australian Capital Territory", "ACT", 428100,
                   "Northern Territory", "NT", 245600)

aus_total_pop = sum(aus_pop$population) # Australia population

# Testing stats manually inputted from state health press releases each day
test_org <- read_csv("https://raw.githubusercontent.com/CocoTheAussieCat/covid-19/master/tests.csv")

test <- test_org %>% 
  mutate(date = dmy(date))

# Change names to match who_clean
names(test) <- c("country_region", "province_state", "date", "tests", "positive", "updated")

# Joined test to who_clean and aus_pop
who_test <- test %>% 
  left_join(who_clean, by = c("country_region", "province_state", "date")) %>% 
  select(-population) %>% 
  left_join(aus_pop, by = "province_state") %>% 
  select(country_region, province_state, date, tests, cases, state, population, positive, lat, long)

# Filter for just Aus countries, use "cases" if "positive" is NA, rerun previous, new case maths
who_aus <- who_test %>% 
  filter(country_region == "Australia") %>% 
  mutate(cases = if_else(!is.na(positive), positive, cases)) %>% 
  select(-positive)

# Add previous day's cases and new case column, calculate per capita numbers
aus_test <- who_aus %>% 
  group_by(province_state) %>% 
  mutate(prev_cases = lag(cases, 1),
         new_cases = cases - prev_cases,
         cases_per_cap = cases/population*10^6,
         new_cases_per_cap =  new_cases/population*10^6)%>% 
  ungroup() %>% 
  mutate(pos_test_ratio = cases/tests,
         test_per_cap = tests/population * 10^6)

# Filter for more recent day's stats 
aus_test_current <- aus_test %>%
  group_by(state) %>% 
  filter(date == max(date) & cases == max(cases))

aus_test_current %>% 
  select(province_state, pos_test_ratio)

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
  scale_y_continuous(limits = c(0, 1000), breaks = c(0, 200, 400, 600, 800, 1000)) +
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
  scale_y_continuous(limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70), 
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
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_brewer(palette = "Dark2")


# 4. Plot of positive case ratio
pos_test_plot <- aus_test_current %>% 
  ggplot(aes(x = reorder(state, cases), y = pos_test_ratio)) +
  geom_point(aes(colour = reorder(state, cases))) +
  geom_segment(aes(yend = 0, xend = state, colour = reorder(state, cases))) +
  geom_text(aes(y = pos_test_ratio + 0.0005, 
                label = percent(pos_test_ratio, accuracy = 0.1), colour = reorder(state, cases)), 
            hjust = "left", size = label_size) +
  labs(subtitle = "% Tests positive",
       x = "",
       y = "") +
  coord_flip() +
  aus_plot_theme +
  scale_y_continuous(labels = percent_format(0.1), limits = c(0, 0.03)) +
  scale_color_brewer(palette = "Dark2")

# Combine plots using {patchwork} to form 2x2
patch_1 <- total_cases_plot + total_tests_plot
patch_2 <- test_per_cap_plot + pos_test_plot
patch <- patch_1 / patch_2   # sent to Shiny App

