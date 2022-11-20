# Using readr package
library(readr)
setwd('C:/Users/jparis/OneDrive - OPEN Health/Documents/Git/MFO/ModellingFootballOutcomes/ELO Data')
list_csv_files <- list.files()
Club_ELO <- readr::read_csv(list_csv_files, id = "file_name")
Club_ELO
setwd('C:/Users/jparis/OneDrive - OPEN Health/Documents/Git/MFO/ModellingFootballOutcomes')

PremierLeague_ELO <- filter(Club_ELO, To > '1992-08-01')
PremierLeague_dataset_ELO <- filter(PremierLeague_ELO, To > '2014-08-01' & To < '2020-07-26')

rm(Club_ELO)

unique(PremierLeague_dataset_ELO$Club)
unique(EPL$team_home)