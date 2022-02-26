source("../source/analysis.R")

used_col <- list(
  "total_jail_pop", 
  "female_adult_jail_pop", 
  "male_adult_jail_pop", 
  "female_juvenile_jail_pop",
  "male_juvenile_jail_pop",
  "total_pop", 
  "total_prison_pop",
  "total_jail_pop"
)

max_grow_prison <- prison_rate_wa %>% 
  filter(variable == "total_prison_pop_change") %>% 
  filter(value == max(value))
max_pri_wa <- max_grow_prison$value
max_pri_wa_year <- max_grow_prison$year

max_grow_pop <- prison_rate_wa %>% 
  filter(variable == "total_pop_change") %>% 
  filter(value == max(value)) %>% 
  pull(value)

max_jail_state <- jail_pop_2016 %>% 
  filter(total_jail_pop == max(total_jail_pop)) %>% 
  mutate(region = toupper(region))

jail_pop_over_year <- jail_pop %>% 
  filter(variable == "total_jail_pop") %>% 
  filter(value == max(value))

year <- jail_pop_over_year$year
num <- jail_pop_over_year$value
