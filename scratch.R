library(tidyverse)
library(lubridate)

# get raw data ####
base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
download.file(url = paste0(base_url, "time_series_19-covid-Confirmed.csv"),
              destfile = "data/covid19_confirmed.csv")
download.file(url = paste0(base_url, "time_series_19-covid-Deaths.csv"),
              destfile = "data/covid19_deaths.csv")
download.file(url = paste0(base_url, "time_series_19-covid-Recovered.csv"),
              destfile = "data/covid19_recovered.csv")

# combine data, tidy and calculate some useful quantities
df <- read_csv("data/covid19_confirmed.csv") %>%
  pivot_longer(cols = contains("/20"), names_to = "date", values_to = "n_cases") %>%
  filter(is.na(n_cases) == FALSE) %>%
  mutate(date = mdy(date)) %>%
  left_join(read_csv("data/covid19_deaths.csv") %>%
              pivot_longer(cols = contains("/20"), names_to = "date", values_to = "n_deaths") %>%
              filter(is.na(n_deaths) == FALSE) %>%
              mutate(date = mdy(date))) %>%
  left_join(read_csv("data/covid19_recovered.csv") %>%
              pivot_longer(cols = contains("/20"), names_to = "date", values_to = "n_recovered") %>%
              filter(is.na(n_recovered) == FALSE) %>%
              mutate(date = mdy(date))) %>%
  arrange(`Province/State`, `Country/Region`, Lat, Long) %>%
  mutate(day = min(date) %--% date / days(1)) %>%
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  mutate(n_cases_per_day = if_else(row_number() == 1, 0, (n_cases - lag(n_cases)) / ((lag(date) %--% date) / days(1))),
         n_deaths_per_day = if_else(row_number() == 1, 0,  (n_deaths - lag(n_deaths)) / ((lag(date) %--% date) / days(1))),
         n_recovered_per_day = if_else(row_number() == 1, 0,  (n_recovered - lag(n_recovered)) / ((lag(date) %--% date) / days(1))) ) %>%
  ungroup() %>%
  rename(sub_area = "Province/State", area = "Country/Region", lat = "Lat", long = "Long")

# latest date recorded
paste0("Most recent available data is from: ", df$date %>% max(), " (day ", max(df$day), ")")

# global mortality rate (%) of infected
paste0("Global average mortality rate is: ", round(100*sum(df$n_deaths, na.rm=T)/sum(df$n_cases, na.rm=T), 3), "%")

# mortality rate by country
mortality_country <- df %>%
  group_by(area) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_mortality = 100 * n_deaths / n_cases) %>%
  select(area, percent_mortality, n_cases, n_deaths) %>% arrange(-percent_mortality)
mortality_country %>% View()

# exploratory plots ####
# cases in mainland China
df %>%
  filter(area == "Mainland China") %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  ggplot(aes(x = date, y = n_cases, group=area)) +
  geom_point() + geom_step()

# deaths by area, y scales are relative
df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>% filter(sum(n_deaths) > 0) %>%
  ggplot(aes(x = date, y = n_deaths, group=area)) +
  geom_point() + geom_step() +
  facet_wrap(~area, scales = "free_y")

# number of confirmed cases per day by area, y scales are relative
df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  ggplot(aes(x = date, y = n_cases_per_day, group=area)) +
  geom_point() + geom_line() +
  facet_wrap(~area, scales = "free_y")

# global number of confirmed cases per day
df %>%
  group_by(date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  ggplot(aes(x = date, y = n_cases_per_day)) +
  geom_point() + geom_line()

# percent of current confirmed cases that have recovered, by area
df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_recovered = 100 * n_recovered / n_cases) %>%
  group_by(area) %>% filter(sum(percent_recovered, na.rm=T) > 0) %>%
  ggplot(aes(x = date, y = percent_recovered, group = area)) +
  geom_point() + geom_step() +
  facet_wrap(~area, scales = "fixed")

# percent of current confirmed cases who have died, by area
df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_deaths = 100 * n_deaths / n_cases) %>%
  group_by(area) %>% filter(sum(percent_deaths, na.rm=T) > 0) %>%
  ggplot(aes(x = date, y = percent_deaths, group = area)) +
  geom_point() + geom_step() +
  facet_wrap(~area, scales = "fixed")


# exploratory maps ####
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(mapview)

pal <- colorNumeric("Reds", domain = df$n_deaths %>% unique())

map <- df %>%
  # filter(day==28) %>%
  filter(day == max(day)) %>%
  leaflet() %>%
  # addTiles() %>%
  addProviderTiles(providers$Stamen.Watercolor) %>%
  # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  # addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = ~sqrt(n_cases)/5, stroke=T, 
                   opacity = .9, color="chocolate",
                   fillColor = ~pal(n_deaths), fillOpacity = .9)

mapshot(map, file = file.path(getwd(), "map.png"),
        vheight = 1200, vwidth = 1600, zoom = 3)

# scratch ####

# centre of mass (Euclidean for simplicity)
com_df <- df %>%
  group_by(lat, long, date) %>%
  summarise(n_cases = sum(n_cases, na.rm=T)) %>%
  group_by(date) %>%
  summarise(com_lat = sum(n_cases*lat, na.rm = T) / sum(n_cases, na.rm = T),
            com_long = sum(n_cases*long, na.rm = T) / sum(n_cases, na.rm = T),
            n_cases = sum(n_cases, na.rm=T)) %>%
  arrange(date) 

# world_map <- map_data("world")
# 
# ggplot(world_map, aes(x = long, y = lat, group=group)) +
#   geom_polygon(fill="lightgray", colour = "white") + 
#   geom_point(data = com_df %>% mutate(group=1628), aes(x=com_long, y=com_lat)) +
#   # ylim(-60, 85) +
#   xlim(111.5, 112.5) +
#   ylim(30.5, 31.5) 

com_df %>%
  ggplot(aes(x=com_long, y=com_lat)) +
  geom_point(aes(colour="red", size = n_cases)) +
  geom_path(arrow = arrow(type="closed")) +
  theme(legend.position = "none")

com_df %>%
  leaflet() %>%
  addTiles() %>%
  # addTerminator() %>%
  addCircleMarkers(lng = ~com_long, lat = ~com_lat, radius = ~sqrt(n_cases)/20, stroke=T, 
                   opacity = .9)
addp  
