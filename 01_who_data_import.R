### IMPORT DATA ---------------------------------------------------------------
# Import country population rds file
population <- read_rds("population.rds")

# Import covid-19 data from Johns Hopkins
who_html <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

who_org <- read_csv(who_html)

who_data <- who_org

# Find number of columns to pivot_longer correctly
x = dim(who_data)[2]


### CLEAN DATA ----------------------------------------------------------------
# Pivot dates longer, change date to correct type, removes cases = NA
who_long <- who_data %>% 
  pivot_longer(cols = 5:x, names_to = "date", values_to = "cases") %>% 
  mutate(date = mdy(date)) %>% 
  filter(!is.na(cases))

# Change names
new_names <- c("province_state", "country_region", "lat", "long", "date", "cases")
names(who_long) <- new_names

# Join population df
who_long_pop <- who_long %>% 
  left_join(population, by = "country_region")

# Add worldwide totals
worldwide <- who_long_pop %>% 
  group_by(date) %>% 
  summarise(cases = sum(cases, na.rm = T),
            population = sum(population, na.rm = T)) %>% 
  mutate(province_state = "World",
         country_region = "World",
         lat = 0,
         long = 0) %>% 
  select(province_state, country_region, lat, long, date, cases, population)

who_long_worldwide <- who_long_pop %>%
  rbind(worldwide)

# Add previous day's cases, new cases columns
who_clean <- who_long_worldwide %>% 
  group_by(country_region, province_state) %>% 
  mutate(prev_cases = lag(cases, 1),
         new_cases = cases-prev_cases) %>% 
  ungroup()

# Create df for growth charts
who_country <- who_clean %>% 
  group_by(country_region, date) %>% 
  arrange(date) %>% 
  summarise(cases = sum(cases, na.rm = T),
            population = mean(population, na.rm = T)) %>% 
  mutate(prev_cases = lag(cases, 1),
         new_cases = cases - prev_cases,
         cases_per_cap = cases/population*10^6,
         new_cases_per_cap =  new_cases/population*10^6)

# Find countries with at least 100 cases
who_hundred <- who_country %>% 
  filter(cases > 99)

# Add days since 100 cases reported - USE FOR GROWTH PLOTS
who_day_zero <- who_hundred %>% 
  mutate(day_since_hdrd = as.numeric(date-min(date)))