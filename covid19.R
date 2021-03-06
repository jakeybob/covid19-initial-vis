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
  mutate(day = min(date) %--% date / days(1)) %>%  #  numbers of days since first case
  group_by(`Province/State`, `Country/Region`, Lat, Long) %>%
  mutate(n_cases_per_day = if_else(row_number() == 1, 0, (n_cases - lag(n_cases)) / ((lag(date) %--% date) / days(1))),
         n_deaths_per_day = if_else(row_number() == 1, 0,  (n_deaths - lag(n_deaths)) / ((lag(date) %--% date) / days(1))),
         n_recovered_per_day = if_else(row_number() == 1, 0,  (n_recovered - lag(n_recovered)) / ((lag(date) %--% date) / days(1))) ) %>%
  ungroup() %>%
  rename(sub_area = "Province/State", area = "Country/Region", lat = "Lat", long = "Long") %>%
  mutate(area = recode(area, "Mainland China" = "China",
                       "United Arab Emirates" = "UAE",
                       "North Macedonia" = "N. Macedonia",
                       "Bosnia and Herzegovina" = "Bosnia/Herzegovina",
                       "Saint Barthelemy" = "St Barthelemy",
                       "Iran (Islamic Republic of)" = "Iran",
                       "occupied Palestinian territory" = "OPT",
                       "Taipei and environs" = "Taipei",
                       "Hong Kong SAR" = "Hong Kong",
                       "Macao SAR" = "Macao", 
                       "Saint Vincent and the Grenadines" = "St Vincent/Grenadines", 
                       "United Kingdom" = "UK",
                       "Republic of the Congo" = "Congo (Republic)",
                       "Dominican Republic" = "Dom. Republic",
                       "Equatorial Guinea" = "Eq. Guinea",
                       "Trinidad and Tobago" = "Trinidad/Tobago",
                       "Antigua and Barbuda" = "Antigua/Barbuda",
                       "Central African Republic" = "CAR",
                       "US" = "USA"))

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

# p_cases_country
ggsave("pics/p_cases_country.png", device = "png", dpi="retina", width=300, height=200, units="mm")


p_cases_country_facet <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>% filter(sum(n_cases) > 0) %>%
  ggplot(aes(x = date, y = n_cases, group=area)) +
  geom_point(size=dot_size-2) + geom_step(size=line_size-1) +
  scale_y_continuous(labels=comma) +
  facet_wrap(~area, scales = "free_y", labeller=label_wrap_gen(width = 20, multi_line = T),
             ncol = 9) +
  ggtitle("COVID-19 cumulative cases") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8),
                       strip.text = element_text(size = 9, face = "bold"))

# p_cases_country_facet
ggsave("pics/p_cases_country_facet.png", device = "png", dpi="retina", width=320, height=500, units="mm")


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
# p_cases_per_day_country
ggsave("pics/p_cases_per_day_country.png", device = "png", dpi="retina", width=300, height=200, units="mm")


top_6_most_recent <- df %>%
  filter(area != "Others") %>%
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

# p_cases_per_day_country_facet
ggsave("pics/p_cases_per_day_country_facet.png", device = "png", dpi="retina", width=300, height=200, units="mm")
# p_cases_per_day_country_facet$data <- p_cases_per_day_country_facet$data %>% ungroup()
# plotly::ggplotly(p_cases_per_day_country_facet)


#### RECOVERED / DEATHS ####

global_mortality <- sum(df$n_deaths, na.rm=T)/sum(df$n_cases, na.rm=T)
p_global_mortality <- df %>%
  group_by(date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_mortality = n_deaths / n_cases,
         percent_recovered = n_recovered / n_cases) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = global_mortality, colour="red") +
  geom_point(aes(y = percent_mortality), size=dot_size) + geom_step(aes(y = percent_mortality), size=line_size) +
  scale_y_continuous(labels = percent_format(accuracy = .1)) +
  annotate("text", label=paste0("", round(100*global_mortality, 2), "%"),
           x=max(df$date), y=global_mortality+5e-4, colour="red") +
  ggtitle("COVID-19 global mortality rate") + xlab("") + ylab("") +
  theme_custom
# p_global_mortality
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
# p_global_mortality_recovered
# ggsave("pics/p_global_mortality_recovered.png", device = "png", dpi="retina", width=300, height=200, units="mm")

p_both_mortality_recovered <- p_global_mortality + p_global_mortality_recovered
ggsave("pics/p_both_mortality_recovered.png", device = "png", dpi="retina", width=300, height=200, units="mm")


p_mortality_facet <- df %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_deaths = n_deaths / n_cases) %>%
  group_by(area) %>% filter(sum(percent_deaths, na.rm=T) > 0) %>% #View()
  ggplot(aes(x = date, y = percent_deaths, group = area)) +
  geom_point(size=dot_size-2) + geom_step(size=line_size-1) +
  scale_y_continuous(labels = percent_format(accuracy = .1)) +
  facet_wrap(~area, scales = "free_y") +
  ggtitle("COVID-19 mortality rates") + xlab("") + ylab("") +
  theme_custom + theme(axis.text.x = element_text(angle = 90),
                       axis.text.y = element_text(size=8),
                       legend.position = "none")
# p_mortality_facet
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
# p_recovered_facet
ggsave("pics/p_recovered_facet.png", device = "png", dpi="retina", width=300, height=200, units="mm")


#### MORTALITY / RECOVERED ANIMATION ####
library(gganimate)

top_6_total_cases <- df %>%
  filter(area != "Others") %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  group_by(area) %>%
  filter(sum(n_cases_per_day) > 0,
         date == max(date)) %>%
  arrange(-n_cases) %>%
  head(6)

plot_data <- df %>%
  filter(area %in% top_6_total_cases$area) %>%
  group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
  mutate(percent_recovered = n_recovered / n_cases,
         percent_deaths = n_deaths / n_cases)

anim <- plot_data %>%
  ggplot(aes(x = percent_recovered, y = percent_deaths)) +
  geom_point(aes(colour=area, size=n_cases)) +
  scale_size_continuous(range = c(4, 20), labels = comma) +
  theme_custom  + theme(legend.title=element_text(size=10, family = "Source Sans Pro")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0, .06)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits=c(0, .9)) +
  labs(title = 'COVID-19 Recovery/Mortality {frame_time}', x = 'recovered', y = 'mortality', 
       size="cases", colour="") +
  transition_time(date) +
  ease_aes('linear') 

animate(anim, width = 1600, height = 1600, res=200, fps=30, duration=10, end_pause = 5)
anim_save("pics/anim_mortality_recovered.gif")

# df_to_plot <- df %>%
#   filter(area %in% top_6_total_cases$area) %>%
#   group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
#   mutate(percent_recovered = n_recovered / n_cases,
#          percent_deaths = n_deaths / n_cases)
# 
# df_to_plot %>%
#   group_by(area, date) %>% summarise_at(vars(starts_with("n_")), sum, na.rm=T) %>%
#   mutate(percent_recovered = n_recovered / n_cases,
#          percent_deaths = n_deaths / n_cases) %>%
#   ggplot(aes(x = percent_recovered, y = percent_deaths)) +
#   geom_line(aes(colour=area), alpha=.7) +
#   geom_point(data=df_to_plot %>% filter(date == max(date)), aes(colour=area, size=n_cases)) +
#   ggtitle("something") + xlab("recovered") + ylab("mortality") + labs(colour="") +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   scale_x_continuous(labels = percent_format(accuracy = 1)) +
#   ylim(0, .1) + xlim(0, .6) +
#   theme_custom

#### SPATIAL EVOLUTION ####
# weighted average of lat/long over sphere
com_filter <- 48 # day beyond which c.o.m averaging breaks down
com_df <- df %>%
  mutate(lat = pi*lat/180, long = pi*long/180) %>%
  group_by(lat, long, date, day) %>%
  summarise(n_cases = sum(n_cases, na.rm=T)) %>%
  mutate(x = cos(lat)*cos(long), y = sin(lat)*cos(long), z = sin(long)) %>%
  group_by(date, day) %>%
  summarise(com_x = sum(n_cases*x, na.rm = T) / sum(n_cases, na.rm = T),
            com_y = sum(n_cases*y, na.rm = T) / sum(n_cases, na.rm = T),
            com_z = sum(n_cases*z, na.rm = T) / sum(n_cases, na.rm = T),
            n_cases = sum(n_cases, na.rm=T)) %>%
  mutate(com_lat = (180/pi)*atan2(com_y, com_x)+180,
         com_long = 180-(180/pi)*atan2(com_z, sqrt(com_x^2 + com_y^2)),
         r = sqrt(com_x^2 + com_y^2 + com_z^2),
         d = 1 -r,
         com_lat = if_else(com_lat >= 180, com_lat-360, com_lat)) %>%
  arrange(date)

p_com_1 <- com_df %>% filter(day <= com_filter) %>%
  ggplot(aes(x=com_long, y=com_lat)) +
  geom_point(aes(size = n_cases, colour=date)) +
  geom_path(arrow = arrow(type="closed")) +
  theme(legend.position = "none") +
  xlab("longitude") + ylab("latitude") + ggtitle("COVID-19 'centre of mass'") +
  labs(size="cases", colour="date") +
  scale_y_continuous(labels = number_format(accuracy = 1, suffix = "° N")) +
  scale_x_continuous(labels = number_format(accuracy = 1, suffix = "° E")) +
  scale_size_continuous(labels = comma) +
  theme_custom + theme(legend.position = "left",
                       legend.title=element_text(size=10, family = "Source Sans Pro"))
p_com_1
p_com_2 <- com_df %>%
  ggplot(aes(x=date, y=d)) +
  geom_point(aes(size = n_cases, colour=date)) +
  geom_line() +
  theme(legend.position = "none") +
  xlab("") + ylab("dispersion") + ggtitle("COVID-19 spatial dispersion") +
  theme_custom + theme(legend.position="none")
p_com_2
p_com <- p_com_1 + p_com_2
p_com

ggsave("pics/p_com.png", device = "png", dpi="retina", width=300, height=200, units="mm")

# log plot
# p_com_3 <- com_df %>%
#   ggplot(aes(x=date, y=d)) +
#   geom_point(aes(size = n_cases, colour=date)) +
#   geom_line() +
#   theme(legend.position = "none") +
#   xlab("") + ylab("dispersion") + ggtitle("COVID-19 spatial dispersion") +
#   coord_trans(y="log10") +
#   theme_custom + theme(legend.position="none")
# p_com_3

spread_base_date <- dmy("20-02-2020") # date exponential behaviour begins
exp_fit_df <- com_df %>% ungroup() %>%
  filter(date >= spread_base_date,
         date <= max(date, na.rm=T))

model <-lm(log(d) ~ day, exp_fit_df) # fit log model to this data (using day rather than date as is numeric)
m <- model$coefficients["day"] 
c <- model$coefficients["(Intercept)"]

day_disp <- floor(-c/m) # day on which d will be >= 1
date_disp <- spread_base_date + days(day_disp - min(exp_fit_df$day))

# df of model data, will include min date, max date and date where d => 1
exp_model_df <- exp_fit_df %>%
  select(date, day) %>%
  filter(date == min(date) | date == max(date)) %>%
  bind_rows(tibble(date = date_disp+days(1), day = day_disp+1)) %>%
  mutate(d = exp((m*day)+c )) %>%
  arrange(date)

p_com_4 <- com_df %>%
  filter(date >= dmy("15-02-2020")) %>%
  ggplot(aes(x=date, y=d)) +
  geom_line() +
  geom_point(aes(size = n_cases, colour=day)) +
  theme(legend.position = "none") +
  xlab("") + ylab("dispersion") + ggtitle("COVID-19 spatial dispersion projection") +
  geom_line(data = exp_model_df, colour="red", size=1) +
  scale_y_log10(breaks = c( .1, .5, 1)) +
  geom_hline(yintercept = 1) +
  annotate(geom = "rect", 
           xmin = date_disp, xmax = date_disp + days(1),
           ymin = 0, ymax = Inf, alpha = 0.3, fill = "red") +
  annotate(geom = "text",
           colour = "red", fontface = 2,
           label = format(date_disp, format="%b %d %Y → ") ,
           x = date_disp,
           y = .005,
           hjust = "right") +
  theme_custom + theme(legend.position = "none")
p_com_4  

ggsave("pics/p_com_proj.png", device = "png", dpi="retina", width=300, height=200, units="mm")


#### LEAFLET ####
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(mapview)

pal <- colorNumeric("Reds", domain = df$n_cases %>% unique())

map <- df %>%
  filter(day == max(day)) %>%
  arrange(-n_cases) %>%
  leaflet() %>%
  setView(zoom=3, lat=30, lng=5) %>%
  # addProviderTiles(providers$Stamen.Watercolor) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = ~sqrt(n_cases)/5, stroke=T,
                   opacity = .9, color="chocolate",
                   fillColor = ~pal(n_cases), fillOpacity = .9)
# maps
mapshot(map, file = file.path(getwd(), "pics/map.png"),
        vheight = 1000, vwidth = 2100)

mapshot(map %>% addProviderTiles(providers$Stamen.Watercolor), 
        file = file.path(getwd(), "pics/map2.png"),
        vheight = 1000, vwidth = 2100)

# geographical moving c.o.m

com_df <- com_df %>% filter(day <= com_filter)
map_com <- com_df %>%
  leaflet() %>%
  setView(zoom=5, lat=20, lng=110) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  # addProviderTiles(providers$Stamen.Terrain) %>%
  addPolylines(lat = ~com_lat, lng = ~com_long, color = "blue", weight = 5, opacity=.8) %>%
  addCircleMarkers(lng = ~com_long, lat = ~com_lat, stroke=F,
                   radius=~sqrt(n_cases)/30, fillColor = "blue", fillOpacity = .8) %>%
  addLabelOnlyMarkers(lng = filter(com_df %>% ungroup(), day==max(day))$com_long, 
                      lat = filter(com_df %>% ungroup(), day==max(day))$com_lat,
    label = format(filter(com_df %>% ungroup(), day==max(day))$date, format="%B %d %Y"),
    labelOptions = labelOptions(noHide = T, direction = "right",
                                # offset = c(8,4),
                                style=list("font-style" = "bold",
                                           "font-family" = "sans-serif",
                                           "font-size" = "20px",
                                           # "background" = "rgba(0,0,255,1)",
                                           "border-color" = "rgba(0,0,0,0.5)",
                                           "color" = "black"))) 

mapshot(map_com, 
        file = file.path(getwd(), "pics/map_com.png"),
        vheight = 1200, vwidth = 1000)
