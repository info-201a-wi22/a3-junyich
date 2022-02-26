library("tidyverse")
library("dplyr")
library("ggplot2")
library("reshape")
#library("maps")
#library("mapproj")

incarceration <- read.csv("https://github.com/vera-institute/incarceration-trends/raw/master/incarceration_trends.csv", stringsAsFactors = FALSE)

jail_pop <- incarceration %>% 
  select(year, total_jail_pop, female_adult_jail_pop, male_adult_jail_pop, female_juvenile_jail_pop, male_juvenile_jail_pop) %>% 
  filter(year >= 2000) %>% 
  group_by(year) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

jail_pop <- as.data.frame(jail_pop)
jail_pop <- melt(data = jail_pop, id.vars = "year")

jail_pop_growth <- ggplot(jail_pop, aes(x=year, y=value, color=variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Population") +
  ggtitle("Change of Jail Population from 2000 to 2018") +
  guides(color = guide_legend(title = "Types")) +
  scale_color_discrete(labels = c("Total Pop in Jail", "Female Adult Population", "Male Adult Population", "Female Juvenile Population", "Male Juvenile Population")) +
  scale_y_continuous(labels = scales::comma)

  
jail_pop_growth

prison_rate_wa <- incarceration %>% 
  filter(year >= 1999 & year <= 2016 & state == 'WA') %>% 
  select(year, total_pop, total_prison_pop) %>%
  group_by(year) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) %>% 
  mutate(total_pop_change = (total_pop - lag(total_pop)) / lag(total_pop)) %>% 
  mutate(total_prison_pop_change = (total_prison_pop - lag(total_prison_pop)) / lag(total_prison_pop)) %>% 
  select(year, total_pop_change, total_prison_pop_change) %>% 
  filter(year >= 2000)

prison_rate_wa <- as.data.frame(prison_rate_wa)
prison_rate_wa <- melt(data = prison_rate_wa, id.vars = "year")

prison_change_wa <- ggplot(prison_rate_wa, aes(x=year, y=value, color=variable)) +
  geom_point() +
  geom_smooth(method = "lm", linetype = "dashed") +
  labs(x = "Year", y = "Rate of Change in %", title = "Change in Prison&Total Population over years (WA)") +
  scale_y_continuous(labels = scales::percent) +
  guides(color = guide_legend(title = "Regression Line")) +
  scale_color_discrete(labels = c("Change in total population", "Change in prison population")) +
  scale_fill_continuous(name = "Point")


prison_change_wa



jail_pop_2016 <- incarceration %>% 
  filter(year == 2016) %>% 
  select(year, state, total_jail_pop) %>% 
  group_by(state) %>% 
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 
  mutate(state = state.name[match(state, state.abb)]) %>% 
  mutate(state = tolower(state)) %>% 
  mutate(state = replace_na(state, "district of columbia")) %>% 
  dplyr::rename(region = state)

state_shape <- map_data("state") %>% 
  left_join(jail_pop_2016, by="region")

jail_pop_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "white", 
    size = .1      
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Jail Population 2016", title = "Jailed Population per States in US") +
  theme_minimal()

jail_pop_map
