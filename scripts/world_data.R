library(tidyverse)
library(lubridate)
world_df <- read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
  janitor::clean_names()

write_csv(world_df, 
          path = paste0('data/owid/', max(world_df$date), '.csv'))

world_df <- world_df %>%
  mutate(
    # format the date as a date, instead of string
    date = ymd(date),
    # get the week beginning
    week_beginning = floor_date(date,
                                unit = "week",
                                # week starts on a Sunday
                                week_start = getOption("lubridate.week.start", 7)
    ))


europe_df <- filter(world_df, continent == 'Europe')

big_europe_countries <- europe_df %>% 
  filter(
    # select big countries only 
    population > 20000000,
    # remove the weird negative data points for spain
    new_deaths >= 0)
  

europe_deaths <- ggplot(big_europe_countries, aes(x = date, y = new_deaths_per_million)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ location, ncol = 2) +
  theme_bw() +
  labs(x = 'Date', y = 'New deaths per million')
europe_deaths
ggsave('figures/world_data/europe_deaths.jpg', europe_deaths)


europe_cases <- ggplot(filter(big_europe_countries, new_cases_per_million >=0), 
                       aes(x = date, y = new_cases_per_million)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ location, ncol = 2) +
  theme_bw() +
  labs(x = 'Date', y = 'New positive tests per million')
europe_cases
ggsave('figures/world_data/europe_cases.jpg', europe_cases)


# UK and Germany ----------------------------------------------------------

uk_and_germany <- big_europe_countries %>%
  filter(new_cases_per_million >=0, 
         location %in% c('United Kingdom', 'Germany'))

uk_germany_cases <- ggplot(uk_and_germany, aes(x = date, y = new_cases_per_million, colour = location)) + 
  geom_line()+
  scale_colour_viridis_d(name = 'Country')+
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Date', y = 'New positive tests per million')

ggsave('figures/world_data/uk_germany_cases.jpg', uk_germany_cases)

uk_germany_deaths <- ggplot(uk_and_germany, aes(x = date, y = new_deaths_per_million, colour = location)) + 
  geom_line()+
  scale_colour_viridis_d(name = 'Country')+
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Date', y = 'New deaths per million')

ggsave('figures/world_data/uk_germany_deaths.jpg', uk_germany_deaths)




# Focal countries ---------------------------------------------------------


countries_of_interest <- c('United Kingdom', 'Germany', 'Ghana', 'Malaysia')

focal_countries_df <- filter(world_df, location %in% countries_of_interest)


focal_deaths <- ggplot(focal_countries_df, aes(x = date, y = new_deaths_per_million)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ location) +
  theme_bw() +
  labs(x = 'Date', y = 'New deaths per million')
focal_deaths
ggsave('figures/world_data/focal_deaths.jpg', focal_deaths)



focal_cases <- ggplot(filter(focal_countries_df, new_cases_per_million >=0), 
                       aes(x = date, y = new_cases_per_million)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ location) +
  theme_bw() +
  labs(x = 'Date', y = 'New positive tests per million')
focal_cases
ggsave('figures/world_data/focal_cases.jpg', focal_cases)


# Continents --------------------------------------------------------------

continents <- world_df %>%
  filter(!is.na(continent)) %>%
  group_by(continent, date) %>%
  summarise(n_cases = sum(new_cases),
            n_deaths = sum(new_deaths))


continent_deaths <- ggplot(continents, aes(x = date, y = n_deaths)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ continent) +
  theme_bw() +
  labs(x = 'Date', y = 'New deaths')
continent_deaths
ggsave('figures/world_data/continent_deaths.jpg', continent_deaths)

continent_cases <- ggplot(continents, aes(x = date, y = n_cases)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ continent) +
  theme_bw() +
  labs(x = 'Date', y = 'New cases')
continent_cases
ggsave('figures/world_data/continent_cases.jpg', continent_cases)




# World -------------------------------------------------------------------

overall_df <- world_df %>%
  filter(location == 'World')


world_deaths <- ggplot(overall_df, aes(x = date, y = new_deaths)) + 
  geom_bar(stat = 'identity') +
  theme_bw() +
  labs(x = 'Date', y = 'New deaths')
world_deaths
ggsave('figures/world_data/world_deaths.jpg', world_deaths)

world_cases <- ggplot(overall_df, aes(x = date, y = new_cases)) + 
  geom_bar(stat = 'identity') +
  theme_bw() +
  labs(x = 'Date', y = 'New cases')
world_cases
ggsave('figures/world_data/world_cases.jpg', world_cases)

