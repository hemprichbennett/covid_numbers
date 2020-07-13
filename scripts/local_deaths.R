library(tidyverse)
library(lubridate)
library(here)


# data from https://coronavirus.data.gov.uk

fetch_data <- T

if (fetch_data == T) {

  # get latest data

  require(RCurl)


  csv_string <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv"

  # now read the file
  death_df <- read_csv(csv_string) %>%
    janitor::clean_names() %>%
    mutate(reporting_date = ymd(reporting_date))

  # pull the date of the latest data from the url the file was downloaded from

  # get rid of all characters before '2020' (the beginning of the date in the
  # string)
  date <- max(death_df$reporting_date)

  # get rid of all characters after 'COVID' (the end of the date in the
  # string)



  if (!dir.exists("data")) {
    dir.create("data")
  }
  if (!dir.exists("data/local_deaths")) {
    dir.create("data/local_deaths")
  }

  write_csv(
    death_df,
    paste0("data/local_deaths/", date, ".csv")
  )
} else {
  deaths_files <- list.files("data/local_deaths/", full.names = T)

  death_df <- read_csv(deaths_files[length(deaths_files)])
}



death_df <- death_df %>%
  # rearrange so that the rows are in chronological order
  arrange(reporting_date) %>%
  mutate(
    # this dataset has 'reporting date' as a variable, which is actually the 
    # date that the report was issued, rather than the date that a person died
    # on. So we need to make a new column to give the date of death
    death_record_date = reporting_date -1,
    
    # some of the values are NA instead of NA, replace them so that zero-values
    # are included in plots
    daily_change_in_deaths = replace_na(daily_change_in_deaths, 0),
    # sort some week values for plotting of days and week beginnings
    weekday = weekdays(death_record_date),
    Weekend = ifelse(weekday %in% c("Saturday", "Sunday"), "Weekend or bank holiday", "Weekday"),
    #week_no = week(reporting_date),
    week_beginning = floor_date(death_record_date,
      unit = "week",
      # week starts on a Sunday
      week_start = getOption("lubridate.week.start", 7)
    ),
    week_beginning = paste(
      month(week_beginning, label = T),
      day(week_beginning)
    ),
    week_beginning = factor(week_beginning, levels = unique(week_beginning))
  )



if (!dir.exists("figures")) {
  dir.create("figures")
}
if (!dir.exists("figures/local_deaths")) {
  dir.create("figures/local_deaths")
}



death_bars <- ggplot(filter(death_df, area_type == 'Nation'), 
                     aes(x = death_record_date,
                                   y = daily_change_in_deaths
),
)+
  geom_bar(stat = 'identity', aes(fill = filter(death_df, area_type == 'Nation')$Weekend))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d(option = 'E', name = 'Day')+
  xlab('Date')+
  ylab('Daily COVID-19 deaths')+
  facet_wrap(. ~ area_name, scales = 'free')

death_bars
ggsave('figures/local_deaths/country_death_bars.jpg', death_bars)  



death_boxes <- ggplot(filter(death_df, area_type == 'Nation'), 
                      aes(x = week_beginning, 
                                    y = daily_change_in_deaths))+
  geom_boxplot()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Week beginning (Sunday)')+
  ylab('Daily COVID-19 deaths')+
  facet_wrap(. ~ area_name, scales = 'free')

death_boxes
ggsave('figures/local_deaths/country_death_boxes.jpg', death_boxes)  





# Per-capita --------------------------------------------------------------

# some massively crude calculations of deaths per-capita, so that comparisons
# can be made between countries. Population numbers from the 2019 estimate
# on wikipedia

country_populations <- tibble(area_name = c('England', 'Northern Ireland',
                                            'Scotland', 'Wales'),
                              population = c(56286961, 1893667,
                                      5463300, 3152879))

nations_df <- death_df %>%
  filter(area_type == 'Nation') %>%
  # add the info from country_populations
  left_join(country_populations) %>%
  # calculate the death rate
  mutate(death_rate = daily_change_in_deaths / population)



rate_bars <- ggplot(nations_df, 
                    aes(x = death_record_date, y = death_rate))+
  geom_bar(stat = 'identity', aes(fill = nations_df$Weekend))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d(option = 'E', name = 'Day')+
  xlab('Date')+
  ylab('Daily deaths per capita')+
  facet_wrap(. ~ area_name)
rate_bars

ggsave('figures/local_deaths/country_deathrate_bars.jpg', rate_bars)  




rate_boxes <- ggplot(nations_df, 
                      aes(x = week_beginning, y = death_rate))+
  geom_boxplot()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Week beginning (Sunday)')+
  ylab('Daily deaths per capita')+
  facet_wrap(. ~ area_name)

rate_boxes
ggsave('figures/local_deaths/country_deathrate_boxes.jpg', rate_boxes)  



week_deaths <- death_df %>%
  filter(area_type == 'Nation') %>%
  # add the info from country_populations
  group_by(week_beginning, area_name) %>%
  summarise(n_death = sum(daily_change_in_deaths)) %>%
  left_join(country_populations) %>%
  # calculate the death rate
  mutate(death_rate = n_death / (population * 1000)) %>%
  ggplot(., aes(x = week_beginning, y = death_rate, colour = area_name)) +
  geom_point()+
  scale_colour_viridis_d(name = 'Nation')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom')+
  xlab('Week beginning (Sunday)')+
  ylab('Deaths per 1000 people')

ggsave('figures/local_deaths/country_totaldeaths_points.jpg', week_deaths)




daily_deaths <- death_df %>%
  filter(area_type == 'Nation') %>%
  # add the info from country_populations
  #group_by(week_beginning, area_name) %>%
  #summarise(n_death = sum(daily_change_in_deaths)) %>%
  left_join(country_populations) %>%
  # calculate the death rate
  mutate(death_rate = daily_change_in_deaths / (population * 1000)) %>%
  ggplot(., 
         aes(x = death_record_date, y = death_rate))+
  geom_bar(stat = 'identity', aes(fill = nations_df$Weekend))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d(option = 'E', name = 'Day')+
  xlab('Date')+
  facet_wrap(. ~ area_name) +
  ylab('Officially recorded COVID deaths per 1000 people')

daily_deaths
ggsave('figures/local_deaths/country_daily_deathrate_points.jpg', daily_deaths)


death_line <- death_df %>%
  filter(area_type == 'Nation') %>%
  # add the info from country_populations
  #group_by(week_beginning, area_name) %>%
  #summarise(n_death = sum(daily_change_in_deaths)) %>%
  left_join(country_populations) %>%
  # calculate the death rate
  mutate(death_rate = daily_change_in_deaths / (population * 1000)) %>%
  ggplot(., aes(x = death_record_date, y = death_rate, colour = area_name))+
  geom_line()+
  scale_color_viridis_d()+
  theme_bw()

death_line
