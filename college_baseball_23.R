library(shiny)
library(glue)
library(tidyverse)
library(dplyr)
library(formattable)
library(gtExtras)
library(rsconnect)
library(baseballr)
library(retrosheet)
library(gt)
library(stringr)
library(janitor)
library(DT)
library(furrr)
library(data.table)
library(readxl)
library(scales)
library(tidytable)
library(googlesheets4)
devtools::install_github("BillPetti/baseballr")

#all_teams <- ncaa_teams(2023)
# 
# 
# write.csv(all_teams, "/Users/mattoneil/Documents/MO/Lineups/college_teams", row.names=FALSE)

all_teams <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/college/college_teams')


matt <- c('Georgia Tech','Ole Miss','Texas','Stanford','Maryland','Creighton', 'UC Santa Barbara','UCLA')
ryan <- c('North Carolina','LSU','Oklahoma St.','Oregon','Michigan','UConn','Southern Miss.','Virginia')

matt_teams <- all_teams %>%
  filter(team_name %in% matt) %>%
  group_by(team_name, team_id) %>%
  summarize(year = max(year))

ryan_teams <- all_teams %>%
  filter(team_name %in% ryan) %>%
  group_by(team_name, team_id) %>%
  summarize(year = max(year))

all_teams <- matt_teams %>%
  bind_rows(ryan_teams)


#baseballr::load_ncaa_baseball_schedule()

# team_name <- all_teams %>%
#   filter(team_id == 169) %>%
#   select(team_name)


results <- function(team_id_number){
  
  team_name <- all_teams %>%
    filter(team_id == team_id_number) %>%
    select(team_name)
  
  matt <- c('Georgia Tech','Ole Miss','Texas','Stanford','Maryland','Creighton', 'UC Santa Barbara','UCLA')
  ryan <- c('North Carolina','LSU','Oklahoma St.','Oregon','Michigan','UConn','Southern Miss.','Virginia')
  
  matt_teams <- all_teams %>%
    filter(team_name %in% matt) %>%
    group_by(team_name, team_id) %>%
    summarize(year = max(year))
  
  ryan_teams <- all_teams %>%
    filter(team_name %in% ryan) %>%
    group_by(team_name, team_id) %>%
    summarize(year = max(year))
  
  all_teams <- matt_teams %>%
    bind_rows(ryan_teams)
  
  results <- baseballr::ncaa_schedule_info(team_id_number, year = 2023) %>%
    filter(!is.na(home_team_score) | !is.na(away_team_score)) %>%
    mutate(team = team_name$team_name) %>%
    mutate(winner = case_when(home_team_score > away_team_score ~ home_team,
                              away_team_score > home_team_score ~ away_team,
                              TRUE ~ 'Neither')) %>%
    mutate(result = case_when(winner == team ~ 'W',
                              TRUE ~ 'L'))
  
  return(results)

}



matt_results <- map_dfr(matt_teams$team_id, results) %>%
  #filter(!is.na(contest_id)) %>%
  select(team, date, home_team, home_team_conference_id, home_team_score, away_team, away_team_score, away_team_conference_id, winner, result)
ryan_results <- map_dfr(ryan_teams$team_id, results) %>%
  #filter(!is.na(contest_id)) %>%
  select(team, date, home_team, home_team_conference_id, home_team_score, away_team, away_team_score, away_team_conference_id, winner, result)

sheet_write(matt_results, ss = '1i2G5w_zqcuABeXnb0XY98pAsAUt361HwW6YkFuOSTrA', sheet = 'Matt Results')
sheet_write(ryan_results, ss = '1i2G5w_zqcuABeXnb0XY98pAsAUt361HwW6YkFuOSTrA', sheet = 'Ryan Results')



