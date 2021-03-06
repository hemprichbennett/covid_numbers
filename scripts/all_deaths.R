library(tidyverse)
library(lubridate)


# data from https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public

fetch_data <- T

if(fetch_data == T){
  
  # get latest data
  
  require(RCurl)
  
  # query the gov.uk website for the url of the most recent csv with 'deaths'
  # in it's name
  
  webpage <- getURL("https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public")
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  
  csv_string <- webpage[grep('deaths.+csv', webpage)]
  
  # string manipulation to get just the url
  if(length(csv_string) >1){csv_string <- csv_string[1]}
  
  csv_string <- gsub('.+https', 'https', csv_string)
  
  csv_string <- gsub('csv.+', 'csv', csv_string)
  
  # now read the file
  death_df <- read_csv(csv_string) #%>%
    #janitor::clean_names()
  
  # pull the date of the latest data from the url the file was downloaded from
  
  date <- Sys.Date()
  
  if(!dir.exists('data')){dir.create('data')}
  if(!dir.exists('data/all_deaths')){dir.create('data/all_deaths')}
    
  write_csv(death_df,
            paste0('data/all_deaths/', date, '-COVID-19-UK-deaths-time-series.csv'))
  
  
}else{
  deaths_files <- list.files('data/all_deaths/', full.names = T)
  
  death_df <- read_csv(deaths_files[length(deaths_files)])
}

death_df <- death_df %>%
  mutate(`UK Cumulative count of deaths in all settings` = gsub('N/A', NA, `UK Cumulative count of deaths in all settings`),
         `UK Daily count of deaths in all settings` = gsub('N/A', NA, `UK Daily count of deaths in all settings`),
         `UK Cumulative count of deaths in all settings` = as.numeric(`UK Cumulative count of deaths in all settings`),
         `UK Daily count of deaths in all settings` = as.numeric(`UK Daily count of deaths in all settings`))


death_df <- death_df %>%
  mutate(`Publicly confirmed as deceased as of 5pm this day` = 
           dmy(`Publicly confirmed as deceased as of 5pm this day`),
         weekday = weekdays(`Publicly confirmed as deceased as of 5pm this day`),
         Weekend = ifelse(weekday %in% c('Saturday', 'Sunday'), 'Weekend or bank holiday', 'Weekday'),
         Events = rep(NA, nrow(.)),
         week_no = week(`Publicly confirmed as deceased as of 5pm this day`),
         week_beginning = floor_date(`Publicly confirmed as deceased as of 5pm this day`, unit = 'week',
                                     # week starts on a Thursday as thats the date of our
                                     # first death
                                     week_start = getOption("lubridate.week.start", 4)),
         week_beginning = paste(month(week_beginning, label = T), 
                                day(week_beginning)),
         week_beginning = factor(week_beginning, levels = unique(week_beginning)))

death_df$week_beginning

# Add bank holidays as weekends
death_df$Weekend[which(death_df$`Publicly confirmed as deceased as of 5pm this day` == dmy('10/04/2020'))] <- 'Weekend or bank holiday'
death_df$Weekend[which(death_df$`Publicly confirmed as deceased as of 5pm this day` == dmy('13/04/2020'))] <- 'Weekend or bank holiday'
death_df$Weekend[which(death_df$`Publicly confirmed as deceased as of 5pm this day` == dmy('08/05/2020'))] <- 'Weekend or bank holiday'
death_df$Weekend[which(death_df$`Publicly confirmed as deceased as of 5pm this day` == dmy('25/05/2020'))] <- 'Weekend or bank holiday'

# 
# # Add landmark events
# death_df$Events[which(death_df$`Publicly confirmed as deceased as of 5pm this day` == dmy('23/03/2020'))] <- 'Lockdown started'
# death_df$Events[which(death_df$`Publicly confirmed as deceased as of 5pm this day` == dmy('01/06/2020'))] <- 'Restrictions lifted'
# 
# for_lines <- death_df %>%
#   filter(!is.na(Events))


if(!dir.exists('figures')){dir.create('figures')}
if(!dir.exists('figures/all_deaths')){dir.create('figures/all_deaths')}


death_bars <- ggplot(death_df, aes(x = `Publicly confirmed as deceased as of 5pm this day`,
                     y = `UK Daily count of deaths in all settings`
                     ),
       )+
  geom_bar(stat = 'identity', aes(fill = death_df$Weekend))+
  #geom_smooth()+
  ylim(min = 0, max = 1200)+ 
  theme_bw()+
  theme(legend.position = 'bottom')+
  scale_fill_viridis_d(option = 'E', name = 'Day')#+
  # geom_vline(xintercept = for_lines$`Publicly confirmed as deceased as of 5pm this day`,
  #           aes(colour = for_lines$Events),
  #           linetype = 'dashed')

death_bars
ggsave('figures/all_deaths/death_bars.jpg', death_bars)  


death_boxes <- death_df %>%
  # filter out any incomplete weeks as they're not a fair comparison
  # for the boxplots
  group_by(week_beginning) %>%
  summarise(n_days = n()) %>%
  left_join(death_df) %>%
  filter(n_days == 7) %>%
  ggplot(., aes(x = week_beginning, 
                     y = `UK Daily count of deaths in all settings`))+
  geom_boxplot()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Week beginning (Thursday)')

death_boxes
ggsave('figures/all_deaths/death_boxes.jpg', death_boxes, dpi = 600)




death_df %>%
  # filter out any incomplete weeks as they're not a fair comparison
  # for the boxplots
  group_by(week_beginning) %>%
  summarise(n_days = n()) %>%
  left_join(death_df) %>%
  filter(n_days == 7) %>%
  ggplot(., aes(y = forcats::fct_rev(week_beginning), 
                x = `UK Daily count of deaths in all settings`))+
  ggridges::geom_density_ridges()+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab('Week beginning (Thursday)')
