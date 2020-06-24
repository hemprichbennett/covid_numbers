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



  if (!exists("data")) {
    dir.create("data")
  }
  if (!exists("data/local_deaths")) {
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
    # some of the values are NA instead of NA, replace them so that zero-values
    # are included in plots
    daily_change_in_deaths = replace_na(daily_change_in_deaths, 0),
    # sort some week values for plotting of days and week beginnings
    weekday = weekdays(reporting_date),
    Weekend = ifelse(weekday %in% c("Saturday", "Sunday"), "Weekend or bank holiday", "Weekday"),
    #week_no = week(reporting_date),
    week_beginning = floor_date(reporting_date,
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



if (!exists("figures")) {
  dir.create("figures")
}
if (!exists("figures/local_deaths")) {
  dir.create("figures/local_deaths")
}



death_bars <- ggplot(filter(death_df, area_type == 'Nation'), aes(x = reporting_date,
                                   y = daily_change_in_deaths
),
)+
  geom_bar(stat = 'identity', aes(fill = filter(death_df, area_type == 'Nation')$Weekend))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d(option = 'E', name = 'Day')+
  xlab('Date')+
  ylab('Daily deaths')+
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
  ylab('Daily deaths')+
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
                    aes(x = reporting_date, y = death_rate))+
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
