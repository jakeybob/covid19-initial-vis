library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

#### GET DATA ####
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
  mutate_at(vars(starts_with("n_")), as.integer) %>%
  arrange(`Province/State`, `Country/Region`, Lat, Long) %>%
  mutate(day = min(date) %--% date / days(1)) %>%  #  numbers of days since first case
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  mutate(n_cases_per_day = if_else(row_number() == 1, 0, (n_cases - lag(n_cases)) / ((lag(date) %--% date) / days(1))),
         n_deaths_per_day = if_else(row_number() == 1, 0,  (n_deaths - lag(n_deaths)) / ((lag(date) %--% date) / days(1))),
         n_recovered_per_day = if_else(row_number() == 1, 0,  (n_recovered - lag(n_recovered)) / ((lag(date) %--% date) / days(1))) ) %>%
  ungroup() %>%
  rename(sub_area = "Province/State", area = "Country/Region", lat = "Lat", long = "Long") %>%
  mutate(area = recode(area, "Mainland China" = "China",
                       "United Arab Emirates" = "UAE",
                       "North Macedonia" = "N. Macedonia"))

# mortality rate by country
mortality_country <- df %>%
  group_by(area) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_mortality = 100 * n_deaths / n_cases) %>%
  select(area, percent_mortality, n_cases, n_deaths) %>% arrange(-percent_mortality)

# common plot elements
theme_custom <- theme_bw() +
  theme(plot.title = element_text(face="bold"),
        text=element_text(size=16, family = "Source Sans Pro"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 10, face = "bold"))
dot_size <- 3.5
line_size <- 1.75

#### CASES BY COUNTRY ####

p_cases_country <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>% ungroup() %>%
  mutate(area = if_else(area=="China", "China", "Everywhere Else")) %>%
  group_by(area, date) %>% summarise(n_cases = sum(n_cases, na.rm=T)) %>%
  ggplot(aes(x = date, y = n_cases, colour=area)) +
  geom_point(size=dot_size) + geom_step(size=line_size) +
  scale_y_continuous(labels=comma) + 
  ggtitle("COVID-19 cumulative cases") + xlab("") + ylab("") +
  theme_custom + theme(legend.position = "bottom")

p_cases_country
ggsave("pics/p_cases_country.png", device = "png", dpi="retina", width=300, height=200, units="mm")


p_cases_country_facet <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>% filter(sum(n_cases) > 0) %>%
  ggplot(aes(x = date, y = n_cases, group=area)) +
  geom_point(size=dot_size-2) + geom_step(size=line_size-1) +
  scale_y_continuous(labels=comma) + 
  facet_wrap(~area, scales = "free_y", labeller=label_wrap_gen(width = 20, multi_line = T)) +
  ggtitle("COVID-19 cumulative cases") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8))

p_cases_country_facet
ggsave("pics/p_cases_country_facet.png", device = "png", dpi="retina", width=300, height=200, units="mm")

#### RATE OF NEW CASES ####

p_cases_per_day_country <- df %>%
  mutate(area = if_else(area=="China", "China", "Everywhere Else")) %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>% filter(sum(n_cases_per_day) > 0) %>%
  ggplot(aes(x = date, y = n_cases_per_day, group=area, colour=area)) +
  geom_point(size=dot_size) + geom_line(size=line_size) +
  scale_y_continuous(labels=comma) + 
  facet_wrap(~area, scales = "free_y", labeller=label_wrap_gen(width = 20, multi_line = T)) +
  ggtitle("COVID-19 cases per day") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8),
                       legend.position = "none")
p_cases_per_day_country
ggsave("pics/p_cases_per_day_country.png", device = "png", dpi="retina", width=300, height=200, units="mm")


top_6_most_recent <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>% 
  filter(sum(n_cases_per_day) > 0,
         date == max(date)) %>%
  arrange(-n_cases_per_day) %>%
  head(6)

p_cases_per_day_country_facet <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>%
  filter(area %in% top_6_most_recent$area) %>%
  ggplot(aes(x = date, y = n_cases_per_day, group=area, colour=area)) +
  geom_point(size=dot_size-2) + geom_line(size=line_size-1) +
  scale_y_continuous(labels=comma) + 
  facet_wrap(~area, scales = "free_y", labeller=label_wrap_gen(width = 20, multi_line = T), ncol = 3) +
  ggtitle("COVID-19 cases per day") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8),
                       legend.position = "none")

p_cases_per_day_country_facet
ggsave("pics/p_cases_per_day_country_facet.png", device = "png", dpi="retina", width=300, height=200, units="mm")
# p_cases_per_day_country_facet$data <- p_cases_per_day_country_facet$data %>% ungroup()
# plotly::ggplotly(p_cases_per_day_country_facet)


#### RECOVERED / DEATHS ####

global_mortality <- sum(df$n_deaths, na.rm=T)/sum(df$n_cases, na.rm=T)
p_global_mortality <- df %>%
  group_by(date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_mortality = n_deaths / n_cases,
         percent_recovered = n_recovered / n_cases,) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = global_mortality, colour="red") +
  geom_point(aes(y = percent_mortality), size=dot_size) + geom_step(aes(y = percent_mortality), size=line_size) +
  scale_y_continuous(labels = percent_format(accuracy = .25)) +
  annotate("text", label=paste0("", round(100*global_mortality, 2), "%"), 
           x=max(df$date), y=global_mortality+5e-4, colour="red") +
  ggtitle("COVID-19 global mortality rate") + xlab("") + ylab("") +
  theme_custom
p_global_mortality
# ggsave("pics/p_global_mortality.png", device = "png", dpi="retina", width=300, height=200, units="mm")

p_global_mortality_recovered <- df %>%
  group_by(date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_mortality = n_deaths / n_cases,
         percent_recovered = n_recovered / n_cases) %>% 
  pivot_longer(cols = starts_with("percent")) %>%
  ggplot(aes(x = date, y = value, group=name)) +
  geom_point(aes(colour=name), size=dot_size) + geom_step(aes(colour=name), size=line_size) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_hue(labels = c("mortality", "recovery")) +
  ggtitle("COVID-19 global mortality & recovery rates") + xlab("") + ylab("") +
  theme_custom + theme(legend.position = "bottom")
p_global_mortality_recovered
# ggsave("pics/p_global_mortality_recovered.png", device = "png", dpi="retina", width=300, height=200, units="mm")

p_both_mortality_recovered <- p_global_mortality + p_global_mortality_recovered
ggsave("pics/p_both_mortality_recovered.png", device = "png", dpi="retina", width=300, height=200, units="mm")


p_mortality_facet <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_deaths = n_deaths / n_cases) %>%
  group_by(area) %>% filter(sum(percent_deaths, na.rm=T) > 0) %>%
  ggplot(aes(x = date, y = percent_deaths, group = area)) +
  geom_point(size=dot_size-2) + geom_step(size=line_size-1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~area, scales = "free_y") +
  ggtitle("COVID-19 mortality rates") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8),
                       legend.position = "none")
p_mortality_facet
ggsave("pics/p_mortality_facet.png", device = "png", dpi="retina", width=300, height=200, units="mm")

p_recovered_facet <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_recovered = n_recovered / n_cases) %>%
  group_by(area) %>% filter(sum(percent_recovered, na.rm=T) > 0) %>%
  ggplot(aes(x = date, y = percent_recovered, group = area)) +
  geom_point(size=dot_size-2) + geom_step(size=line_size-1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~area, scales = "free_y") +
  ggtitle("COVID-19 recovery rates") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8),
                       legend.position = "none")
p_recovered_facet
ggsave("pics/p_recovered_facet.png", device = "png", dpi="retina", width=300, height=200, units="mm")


####
library(gganimate)
df %>%
  filter(area == "China") %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_recovered = n_recovered / n_cases,
         percent_deaths = n_deaths / n_cases) %>%
  # filter(date == min(date)) %>%
  ggplot(aes(x = percent_recovered, y = percent_deaths)) +
  geom_point() +
    transition_time(date) +
    ease_aes('linear')
  
  