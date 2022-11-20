# Using readr package
if(!require('timetk')) {
  install.packages('timetk')
  library('timetk')
}

library(lubridate)
library(readr)
library(dplyr)

setwd('C:/Users/jparis/OneDrive - OPEN Health/Documents/Git/MFO/ModellingFootballOutcomes/ELO Data')
list_csv_files <- list.files()
Club_ELO <- readr::read_csv(list_csv_files, id = "file_name")
Club_ELO
setwd('C:/Users/jparis/OneDrive - OPEN Health/Documents/Git/MFO/ModellingFootballOutcomes')

PremierLeague_ELO <- filter(Club_ELO, To > '1992-08-01')
PremierLeague_dataset_ELO <- filter(PremierLeague_ELO, To > '2014-08-01' & To < '2020-08-01')

rm(Club_ELO)

ELO_vars <- c('Club', 'Elo', 'To')

ELO_join <- PremierLeague_dataset_ELO[ELO_vars]

ELO_join['Club'][ELO_join['Club'] == 'QPR'] <- 'Queens Park Rangers'
ELO_join['Club'][ELO_join['Club'] == 'Man City'] <- 'Manchester City'
ELO_join['Club'][ELO_join['Club'] == 'Newcastle'] <- 'Newcastle United'
ELO_join['Club'][ELO_join['Club'] == 'Man United'] <- 'Manchester United'
ELO_join['Club'][ELO_join['Club'] == 'West Brom'] <- 'West Bromwich Albion'
ELO_join['Club'][ELO_join['Club'] == 'Wolves'] <- 'Wolverhampton Wanderers'

names(ELO_join)[names(ELO_join) == 'Club'] <- 'team_home'
names(ELO_join)[names(ELO_join) == 'To'] <- 'date'
names(ELO_join)[names(ELO_join) == 'Elo'] <- 'elo_home'

ELO_join_home <- ELO_join
ELO_join_away <- ELO_join

names(ELO_join_away)[names(ELO_join_away) == 'team_home'] <- 'team_away'
names(ELO_join_away)[names(ELO_join_away) == 'elo_home'] <- 'elo_away'

EPL <- readr::read_csv('EPL_corrected.csv')
EPL <- mutate(EPL, date = as.Date(date, format = '%d/%m/%Y'))

EPL_ELO <- merge(EPL, ELO_join_home, by = c('team_home','date'))
EPL_ELO_away <- merge(EPL_ELO, ELO_join_away, by = c('team_away','date'))

EPL %>% count(season)
EPL_ELO %>% count(season)



