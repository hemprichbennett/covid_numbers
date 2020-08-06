#### Header ####
## Project: COVID armchair stats
## Script purpose: local infection detection analysis
## Date: 2020-06-18
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes
##################################################

#### Setup ####

# Prevent partial-match errors 
options(warnPartialMatchArgs = TRUE)


library(tidyverse)
library(lubridate)


# data from https://coronavirus.data.gov.uk

fetch_data <- T


# Get/format the data -----------------------------------------------------

if (fetch_data == T) {
  
  # get latest data
  
  require(RCurl)
  
  
  csv_string <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  
  # now read the file
  detection_df <- read_csv(csv_string) %>%
    janitor::clean_names() %>%
    mutate(specimen_date = ymd(specimen_date))
  
  
  # the date of the last data upload
  date <- max(detection_df$specimen_date)
  
  
  
  
  if (!dir.exists("data")) {
    dir.create("data")
  }
  if (!dir.exists("data/detections")) {
    dir.create("data/detections")
  }
  
  write_csv(
    detection_df,
    paste0("data/detections/", date, ".csv")
  )
} else {
  detection_files <- list.files("data/detections/", full.names = T)
  
  detection_df <- read_csv(detection_files[length(detection_files)])
}

# basic formatting stuff to make the tibble nicer

detection_df <- detection_df %>%
  # rearrange so that the rows are in chronological order
  arrange(specimen_date) %>%
  # add some columns to deal with days of the week and the week's start date
  mutate(
    weekday = weekdays(specimen_date),
    Weekend = ifelse(weekday %in% c("Saturday", "Sunday"), "Weekend or bank holiday", "Weekday"),
    week_no = week(specimen_date),
    week_beginning = floor_date(specimen_date,
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

# out of interest, the number of different places for the available categories
# of geographic area
detection_df %>%
  group_by(area_type) %>%
  summarise(n = length(unique(area_name)))


# a subset of counties that are interesting to me specifically (as facetting by
# 150 counties would be a bit much)
interesting_counties <- c('Devon', 'Cornwall and Isles of Scilly', 'Somerset',
                          'Oxfordshire', 'Warwickshire', 'Northamptonshire', 
                          'Buckinghamshire', 'Berkshire', 'Wiltshire', 'Gloucestershire')


county_df <- detection_df %>%
  filter(area_name %in% interesting_counties)


# Make figures ------------------------------------------------------------



# check theres somewhere to save the figures
if (!dir.exists("figures")) {
  dir.create("figures")
}
if (!dir.exists("figures/county_cases")) {
  dir.create("figures/county_cases")
}

# make a barplot for the counties of interest
county_bars <- ggplot(county_df, 
                      aes(x = specimen_date, 
                          y = daily_lab_confirmed_cases))+
  geom_bar(stat = 'identity', aes(fill = county_df$Weekend))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  xlab('Date upon which test was completed')+
  ylab('Number of positive pillar 1 COVID-19 tests')+
  scale_fill_viridis_d(option = 'E', name = 'Day')+
  facet_wrap(. ~ area_name, scales = 'free')

county_bars
ggsave('figures/county_cases/county_case_bars.jpg', county_bars)  

# make a weekly boxplot of the counties of interest
case_boxes <- ggplot(county_df, 
                      aes(x = week_beginning, 
                          y = daily_lab_confirmed_cases))+
  geom_boxplot()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Week beginning (Sunday)')+
  ylab('Number of positive pillar 1 COVID-19 tests')+
  facet_wrap(. ~ area_name, scales = 'free')

case_boxes
ggsave('figures/county_cases/county_case_boxes.jpg', case_boxes)  


# look at regional trends
if (!dir.exists("figures/region_cases")) {
  dir.create("figures/region_cases")
}

# make a barplot of regional case detections
region_bars <- ggplot(filter(detection_df, area_type == 'region'),
                      aes(x = specimen_date, 
                          y = daily_lab_confirmed_cases))+
  geom_bar(stat = 'identity', 
           aes(fill = filter(detection_df, area_type == 'region')$Weekend))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  xlab('Date upon which test was completed')+
  ylab('Number of positive COVID-19 tests')+
  scale_fill_viridis_d(option = 'E', name = 'Day')+
  facet_wrap(. ~ area_name, scales = 'free')
region_bars

ggsave('figures/region_cases/region_case_bars.jpg', region_bars)  

# 
# mar_23rd <- detection_df %>%
#   filter(specimen_date == '2020-03-23' & area_name == 'England') %>%
#   pull(daily_lab_confirmed_cases)
# 
# ggplot(filter(detection_df, area_type == 'Nation'),
#        aes(x = specimen_date, 
#            y = daily_lab_confirmed_cases))+
#   geom_bar(stat = 'identity', 
#            aes(fill = filter(detection_df, area_type == 'Nation')$Weekend))+
#   geom_hline(yintercept = mar_23rd)+
#   theme_bw()+
#   theme(legend.position = 'bottom')+
#   xlab('Date upon which test was completed')+
#   ylab('Number of positive pillar 1 COVID-19 tests')+
#   scale_fill_viridis_d(option = 'E', name = 'Day')+
#   facet_wrap(. ~ area_name, scales = 'free')

region_week <- detection_df %>% 
  filter(area_type == 'region') %>% 
  group_by(week_beginning) %>%
  summarise(n_days = length(unique(specimen_date))) %>%
  filter(n_days == 7) %>%
  left_join(filter(detection_df, area_type =='region')) 

region_boxplot <- ggplot(region_week, 
                         aes(x = week_beginning, 
                             y = daily_lab_confirmed_cases)) + 
  geom_boxplot() +
  facet_wrap(. ~ area_name, scales = 'free') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

region_boxplot
ggsave('figures/region_cases/region_case_boxplot.pdf', region_boxplot)

region_week_bar <- region_week %>%
  group_by(area_name, week_beginning) %>%
  summarise(n_cases = sum(daily_lab_confirmed_cases)) %>%
  ggplot(., aes(x = week_beginning, y = n_cases)) +
  geom_bar(stat = 'identity')+
  facet_wrap(. ~ area_name, scales = 'free') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  labs(x = 'Week beginning', y = 'Number of cases per week')

ggsave('figures/region_cases/region_week_barplot.pdf', region_week_bar)
