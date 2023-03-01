#next steps
  #weighting the pitchers more heavily on strikeouts than on base
  #now value pitchers by points
  #get this on GitHub
  #replace the lineup tab with a table about hitters before and hitters after to compare indexes
  #get percentage better/worse for woba for each pitcher
  # $ per xWOBA for both pitchers and the all batters tab as a value
  # spitting out lineups that stay under a certain value and giving their ranks
  # using the implied totals to help with stacking on auto developed lineups (z-score)
  # pull weather in
  # pitcher props (can I scrape)
  # L/R splits
  # clean up code and mark it up
  # lineup optimizer, function based on amount of salary left over and how many total teams we want to pull from
  # ggplot three different pages, one on xBA, one on xSlg, and one on xWOBA to rank players
  
#pacman::p_load_current_gh("billpetti/baseballr")

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
library(shinyWidgets)
library(lubridate)
library(ggrepel)
library(rvest)
library(XML)
library(httr)
library(jsonlite)
library(lpSolve)


#only take balls put in play
valid_events <- c("single","double","triple","home_run","walk","strikeout","field_out","force_out","sac_fly","fielders_choice","grounded_into_double_play","fielders_choice_out","sac_bunt","field_error","hit_by_pitch","double_play","strikeout_double_play","other_out","triple_play","sac_bunt_double_play")



teams <- mlb_teams(season = 2022, sport_ids = c(1)) %>%
  select(team_id, team_full_name, team_abbreviation)


#fanduel results history

fd_history <- function(){
  old_results <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/fdhistory.csv') %>%
    janitor::clean_names() %>%
    filter(sport == 'mlb' & salary_cap == '$35k' & (!str_detect(link, 'voided') & !str_detect(link, 'endedunmatched')) & date >= '2022-01-01') %>%
    mutate(winnings = as.numeric(winnings), score = as.numeric(score)) %>%
    group_by(date,score) %>%
    summarize(total_entries = sum(entry), winnings = as.double(sum(winnings))) %>%
    arrange(desc(score))
  return(old_resu)
}



#betting lines for over/under
clean_totals <- function(url) {

  url <- 'https://sportsdata.usatoday.com/baseball/mlb/odds'

  WS <- read_html(url) %>%
    html_nodes(xpath = '//*[@id="__next"]/div[5]/div[3]/div/div[1]/div[2]/div[2]') %>%
    html_table()

  lines <- WS[[1]] %>%
    janitor::clean_names() %>%
    `colnames<-` (c('team','spread','money_line','total','garbage')) %>%
    filter(spread != 'Spread') %>%
    select(-c(garbage, spread)) %>%
    mutate(money_line = as.numeric(money_line)) %>%
    mutate(team = case_when(str_detect(team, 'Angels') ~ 'LAA',
                            str_detect(team, 'Astros') ~ 'HOU',
                            str_detect(team, 'Rangers') ~ 'TEX',
                            str_detect(team, 'Mariners') ~ 'SEA',
                            str_detect(team, 'Athletics') ~ 'OAK',
                            str_detect(team, 'Dodgers') ~ 'LAD',
                            str_detect(team, 'Padres') ~ 'SD',
                            str_detect(team, 'Giants') ~ 'SF',
                            str_detect(team, 'Rockies') ~ 'COL',
                            str_detect(team, 'Diamondbacks') ~ 'ARI',
                            str_detect(team, 'Guardians') ~ 'CLE',
                            str_detect(team, 'Twins') ~ 'MIN',
                            str_detect(team, 'White Sox') ~ 'CWS',
                            str_detect(team, 'Tigers') ~ 'DET',
                            str_detect(team, 'Royals') ~ 'KC',
                            str_detect(team, 'Cubs') ~ 'CHC',
                            str_detect(team, 'Pirates') ~ 'PIT',
                            str_detect(team, 'Reds') ~ 'CIN',
                            str_detect(team, 'Brewers') ~ 'MIL',
                            str_detect(team, 'Cardinals') ~ 'STL',
                            str_detect(team, 'Red Sox') ~ 'BOS',
                            str_detect(team, 'Yankees') ~ 'NYY',
                            str_detect(team, 'Rays') ~ 'TB',
                            str_detect(team, 'Blue Jays') ~ 'TOR',
                            str_detect(team, 'Orioles') ~ 'BAL',
                            str_detect(team, 'Mets') ~ 'NYM',
                            str_detect(team, 'Nationals') ~ 'WSH',
                            str_detect(team, 'Phillies') ~ 'PHI',
                            str_detect(team, 'Marlins') ~ 'MIA',
                            str_detect(team, 'Braves') ~ 'ATL',
                            TRUE ~ 'None'
    )) %>%
    mutate(implied_share = case_when(money_line < 0 ~ round((money_line/(money_line-100)),2),
                                     TRUE ~ round((100/(money_line + 100)),2))) %>%
    separate(total, into = c('garbage','total'), sep = ' ') %>%
    select(-garbage) %>%
    mutate(total = as.numeric(substr(total,1,3))) %>%
    mutate(implied_total = round((implied_share*total),2))

  sd_total <- sd(lines$implied_total)
  avg_total <- mean(lines$implied_total)

  lines <- lines %>%
    mutate(z_score = round((implied_total - avg_total)/sd_total,2))

}


#lines <- clean_totals(url)



team_ids <- c(108,109,110,111,112,113,114,115,116,117,118,119,120,121,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,158)

rosters_func <- function(team) {
  mlb_rosters(team_id = team, date = Sys.Date(), roster_type = 'active')
}

rosters <- map_dfr(team_ids, rosters_func) %>%
  inner_join(teams, by = c('parent_team_id' = 'team_id')) %>%
  select(player = person_id, player_name = person_full_name, player_team = team_abbreviation) %>%
  mutate(player_name = ifelse(player_name == 'Vladimir Guerrero' & player_team == 'TOR','Vladimir Guerrero Jr.', player_name)) %>%
  mutate(player_name = ifelse(player_name == 'Enrique Hernandez' & player_team == 'BOS','Kiké Hernandez', player_name))

team_stadiums <- mlb_teams(season = 2021, sport_ids = c(1)) %>%
  arrange(team_abbreviation) %>%
  select(team_name, team_abbreviation, venue_name) %>%
  mutate(team_name = ifelse(team_abbreviation == 'ARI', 'Diamondbacks', team_name)) %>%
  mutate(team_name = ifelse(team_abbreviation == 'CLE', 'Guardians', team_name))


park_factors <- fg_park(2021) %>%
  mutate(home_team = ifelse(home_team == 'Indians','Guardians',home_team)) %>%
  left_join(team_stadiums, by = c('home_team' = 'team_name')) %>%
  select(team_abbreviation, venue_name, single, double, triple, hr, so, UIBB, GB, FB, LD, IFFB, FIP, basic_5yr, basic_3yr = `3yr`, basic_1yr = `1yr`) %>%
  mutate(single = single/100, double = double/100, triple = triple/100, hr = hr/100, so = so/100, UIBB = UIBB/100, GB = GB/100, FB = FB/100, LD = LD/100, IFFB = IFFB/100, FIP = FIP/100) %>%
  mutate(team_abbreviation)


#clarify the season for the statcast data
season = 2022
#what date for the app to be run
date_of_game = Sys.Date()


xstats_batters <- statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = 2022,
  abs = 25,
  min_pa = 25,
  min_pitches = 20,
  min_field = "q",
  min_run = 0,
  player_type = "batter",
  fielding_type = "player",
  oaa_position = "",
  oaa_roles = "",
  team = "",
  arsenal_type = "n_",
  run_type = "raw",
  min2b = 0,
  min3b = 0,
  position = "",
  bats = "",
  hand = ""
)

xstats_pitchers <- statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = 2022,
  abs = 25,
  min_pa = 25,
  min_pitches = 20,
  min_field = "q",
  min_run = 0,
  player_type = "pitcher",
  fielding_type = "player",
  oaa_position = "",
  oaa_roles = "",
  team = "",
  arsenal_type = "n_",
  run_type = "raw",
  min2b = 0,
  min3b = 0,
  position = "",
  bats = "",
  hand = ""
)

devtools::install_github(repo = "BillPetti/baseballr")

ev_batters <- statcast_leaderboards(
  leaderboard = "exit_velocity_barrels",
  year = 2022,
  abs = 25,
  min_pa = 25,
  min_pitches = 20,
  min_field = "q",
  min_run = 0,
  player_type = "batter",
  fielding_type = "player",
  oaa_position = "",
  oaa_roles = "",
  team = "",
  arsenal_type = "n_",
  run_type = "raw",
  min2b = 0,
  min3b = 0,
  position = "",
  bats = "",
  hand = ""
)

ev_pitchers <- statcast_leaderboards(
  leaderboard = "exit_velocity_barrels",
  year = 2022,
  abs = 25,
  min_pa = 25,
  min_pitches = 5,
  min_field = "q",
  min_run = 0,
  player_type = "pitcher",
  fielding_type = "player",
  oaa_position = "",
  oaa_roles = "",
  team = "",
  arsenal_type = "n_",
  run_type = "raw",
  min2b = 0,
  min3b = 0,
  position = "",
  bats = "",
  hand = ""
)

#batter_splits vs. different handed pitchers
batter_splits <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/batters_22_splits.csv') %>%
  mutate(mean_ev_index_split = avg_launch_speed/mean(avg_launch_speed, na.rm = T), brl_index_split = brl_pct/mean(brl_pct, na.rm = T), xba_index_split = xba/mean(xba, na.rm = T), woba_index_split = woba/mean(woba, na.rm = T), xwoba_index_split = xwoba/mean(xwoba, na.rm = T), iso_index_split = iso/mean(iso, na.rm = T)) %>%
  select(batter, stand, p_throws, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)
pitcher_splits <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/pitchers_22_splits.csv') %>%
  mutate(mean_ev_index_split = avg_launch_speed/mean(avg_launch_speed, na.rm = T), brl_index_split = brl_pct/mean(brl_pct, na.rm = T), xba_index_split = xba/mean(xba, na.rm = T), woba_index_split = woba/mean(woba, na.rm = T), xwoba_index_split = xwoba/mean(xwoba, na.rm = T), iso_index_split = iso/mean(iso, na.rm = T)) %>%
  select(pitcher, stand, p_throws, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)



updated_batters <- xstats_batters %>%
  left_join(rosters, by = c('player_id' = 'player')) %>%
  select(player = player_id, slg, est_slg, woba, est_woba, ba, est_ba) %>%
  mutate(xslg_index = est_slg/mean(est_slg), xwoba_index = est_woba/mean(est_woba), xba_index = est_ba/mean(est_ba)) %>%
  inner_join(ev_batters, by = c('player' = 'player_id')) %>%
  mutate(player_name = paste0(first_name,' ',last_name), brl_pa_index = brl_pa/mean(brl_pa), brl_pct_index = brl_percent/mean(brl_percent), hard_hit_index = ev95percent/mean(ev95percent), max_ev_index = max_hit_speed/mean(max_hit_speed), mean_ev_index = avg_hit_speed/mean(avg_hit_speed), sweet_spot_index = anglesweetspotpercent/mean(anglesweetspotpercent)) %>%
  mutate(type = 'batter') %>%
  select(player, player_name, type, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pa_index, brl_pct_index, hard_hit_index, max_ev_index, mean_ev_index, sweet_spot_index) %>%
  inner_join(batter_splits, by = c('player' = 'batter')) %>%
  select(player, player_name, type, stand, p_throws, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pa_index, brl_pct_index, hard_hit_index, max_ev_index, mean_ev_index, sweet_spot_index, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)

updated_pitchers <- xstats_pitchers %>%
  left_join(rosters, by = c('player_id' = 'player')) %>%
  select(player = player_id,slg, est_slg, woba, est_woba, ba, est_ba) %>%
  mutate(xslg_index = est_slg/mean(est_slg), xwoba_index = est_woba/mean(est_woba), xba_index = est_ba/mean(est_ba)) %>%
  inner_join(ev_pitchers, by = c('player' = 'player_id')) %>%
  mutate(player_name = paste0(first_name,' ',last_name), brl_pa_index = brl_pa/mean(brl_pa), brl_pct_index = brl_percent/mean(brl_percent), hard_hit_index = ev95percent/mean(ev95percent), max_ev_index = max_hit_speed/mean(max_hit_speed), mean_ev_index = avg_hit_speed/mean(avg_hit_speed), sweet_spot_index = anglesweetspotpercent/mean(anglesweetspotpercent)) %>%
  mutate(type = 'pitcher') %>%
  select(player, player_name, type, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pa_index, brl_pct_index, hard_hit_index, max_ev_index, mean_ev_index, sweet_spot_index) %>%
  inner_join(pitcher_splits, by = c('player' = 'pitcher')) %>%
  select(player, player_name, type, stand, p_throws, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pa_index, brl_pct_index, hard_hit_index, max_ev_index, mean_ev_index, sweet_spot_index, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)

updated_players <- updated_batters %>%
  bind_rows(updated_pitchers) %>%
  inner_join(rosters, by = c('player' = 'player'))

#can we get the game time to join in the data from gambling and not run into a DH issue
#also need to get pitcher handedness so we can get L/R splits
salary <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/fdsalary.csv') %>%
  janitor::clean_names() %>%
  filter(is.na(injury_indicator)) %>%
  mutate(nickname = ifelse(nickname == 'Kike Hernandez' & team == 'BOS','Kiké Hernandez', nickname)) %>%
  inner_join(rosters, by = c('nickname' = 'player_name')) %>%
  select(key_mlbam = player, id, position, nickname, salary, game, opponent, injury_indicator, injury_details, probable_pitcher, batting_order, roster_position, team, opponent) %>%
  inner_join(updated_players, by = c('key_mlbam' = 'player', 'team' = 'player_team')) %>%
  filter(type == 'batter') %>%
  distinct() %>%
  separate(game, into = c('away','home'), sep = '@') %>%
  inner_join(park_factors, by = c('home'='team_abbreviation'))


pitchers <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/fdsalary.csv') %>%
  janitor::clean_names() %>%
  filter(probable_pitcher == 'Yes') %>%
  inner_join(rosters, by = c('nickname' = 'player_name')) %>%
  select(pitcher_id = player, pitcher_name = nickname, pitcher_team = team, pitcher_salary = salary) %>%
  inner_join(updated_players, by = c('pitcher_id' = 'player', 'pitcher_team' = 'player_team')) %>%
  filter(type == 'pitcher') %>%
  mutate(pitcher_xslg_index = xslg_index, pitcher_xwoba_index = xwoba_index, pitcher_xba_index = xba_index, pitcher_barrel_pa_index = brl_pa_index, pitcher_barrel_pct_index = brl_pct_index, pitcher_hard_hit_index = hard_hit_index, pitcher_max_ev_index = max_ev_index, pitcher_mean_ev_index = mean_ev_index, pitcher_sweet_spot_index = sweet_spot_index) %>%
  select(-c(xslg_index, xwoba_index, player_name.y, player_name.x)) %>%
  distinct()

players <- salary %>%
  inner_join(pitchers, by = c('opponent' = 'pitcher_team')) %>%
  distinct()

teams <- players %>%
  distinct(team) %>%
  arrange(team)


batter_index <- updated_batters %>%
  select(-c(slg, est_slg, woba, est_woba, ba, est_ba)) %>%
  mutate_if(is.numeric, round, 2)


pitcher_index <- updated_pitchers %>%
  select(-c(slg, est_slg, woba, est_woba, ba, est_ba)) %>%
  mutate_if(is.numeric, round, 2)

indexes <- batter_index %>%
  rbind(pitcher_index)


matchup <- function(batter_mlb_id, pitcher_mlb_id) {

#    batter_mlb_id = 547989
#    pitcher_mlb_id = 592332
  
  batter <- players %>%
    filter(key_mlbam == batter_mlb_id & (position != 'P' | nickname == 'Shohei Ohtani')) %>%
    select(key_mlbam, team, opponent, batter_name = nickname, position, salary) %>%
    inner_join(updated_batters, by = c('key_mlbam' = 'player')) %>%
    select(-c(slg,est_slg,woba, est_woba, ba, est_ba))
  
  
  pitcher <- pitchers %>%
    filter(pitcher_id == pitcher_mlb_id)
  
  pitcher_stats <- indexes %>%
    filter(player == pitcher$pitcher_id & type == 'pitcher')
  
  batter_stats <- indexes %>%
    filter(player == batter$key_mlbam & type == 'batter')
  
  batter_name <- batter$batter_name
  pitcher_name <- pitcher$pitcher_name

  total_x_slg <- pitcher_stats$xslg_index * batter$xslg_index
  total_x_woba <- pitcher_stats$xwoba_index * batter$xwoba_index
  total_x_ba <- pitcher_stats$xba_index * batter$xba_index
  total_brl_pa_index <- pitcher_stats$brl_pa_index * batter_stats$brl_pa_index
  total_brl_pct_index <- pitcher_stats$brl_pct_index * batter_stats$brl_pct_index
  total_hard_hit_index <- pitcher_stats$hard_hit_index * batter_stats$hard_hit_index
  total_max_ev_index <- pitcher_stats$max_ev_index * batter_stats$max_ev_index
  total_mean_ev_index <- pitcher_stats$mean_ev_index * batter_stats$mean_ev_index
  total_sweet_spot_index <- pitcher_stats$sweet_spot_index * batter_stats$sweet_spot_index
  
  matchup_percentages <- bind_cols(batter) %>%
    mutate_if(is.numeric, round, 2) %>%
    bind_cols(pitcher_name = pitcher_name) %>%
    mutate(Price = dollar(as.numeric(salary))) %>%
    select(Batter = batter_name, team, Position = position, Opponent = opponent, Price, Pitcher = pitcher_name, salary) %>%
    bind_cols(brl_pa = total_brl_pa_index, brl_pct = total_brl_pct_index, hard_hit = total_hard_hit_index, max_ev = total_max_ev_index, mean_ev = total_mean_ev_index, sweet_spot = total_sweet_spot_index, xSlg = total_x_slg, xWOBA = total_x_woba, xBA = total_x_ba) %>%
    distinct()
  
  print(batter$batter_name)
  
  
  return(matchup_percentages)
}

order_matchup <- function(df) {
  matchup(df$key_mlbam, df$pitcher_id)
}

all_matchups <- function() {
  day <- players %>%
    filter(position != 'P' | nickname == 'Shohei Ohtani') %>%
    arrange(batting_order) %>%
    inner_join(indexes, by = c("key_mlbam" = "player")) %>%
    arrange(nickname) %>%
    distinct()


  new_lineups <- day %>%
    split(1:nrow(.))

  whole_day_stats <- map_dfr(new_lineups, order_matchup) %>%
    distinct() 

  return(whole_day_stats)
}

whole_day_stats <- all_matchups()



lineup_stats <- function(team_name){

  
  pitcher_name <- whole_day_stats %>%
    filter(team == team_name) %>%
    select(Pitcher) %>%
    distinct()
  
  opposing_team <- whole_day_stats %>%
    filter(team == team_name) %>%
    select(Opponent) %>%
    distinct()
  
  order <- whole_day_stats %>%
    filter(team == team_name) 
  
  
  new_lineups <- order %>%
    split(1:nrow(.)) %>%
    as.list()
  
  
  all_matchups <- order %>%
    select(Batter, team, Price, Pitcher, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xSlg, xWOBA) %>%
    mutate_if(is.numeric,round,2) %>%
    gt() %>%
    gt_merge_stack(col1 = Batter, Pitcher) %>%
    gt_color_rows(brl_pa:xWOBA, palette = "grDevices::blues9") %>%
    tab_header(title = glue('{team_name} Lineup Projections vs. {opposing_team$Opponent} for {date_of_game}')) %>%
    tab_options(table.width = 12)
  
  return(all_matchups)
}



all_players <- function() {
  
  pitcher_info <- pitchers %>%
    select(1:4)
  
  
  batter_matchups <- whole_day_stats %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(Rank = rank(desc(xSlg + xWOBA + brl_pa + xBA + hard_hit + brl_pct + sweet_spot))) %>%
    arrange(Rank) %>%
    inner_join(pitcher_info, by = c('Pitcher' = 'pitcher_name')) %>%
    mutate(Value = round((salary/1000)/xWOBA,2)) %>%
    select(Batter, Team = team, Position, Price, Pitcher, Opponent, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xBA, xSlg, xWOBA, Value, Rank)
  
  return(batter_matchups)
}



pitcher_stats <- function(game_date) {
  
  
  pitcher_aggs <- whole_day_stats  %>%
    group_by(Pitcher) %>%
    dplyr::summarize(brl_pa = mean(brl_pa), brl_pct = mean(brl_pct), hard_hit = mean(hard_hit), max_ev = mean(max_ev), mean_ev = mean(mean_ev), sweet_spot = mean(sweet_spot), xBA = mean(xBA), xWOBA = mean(xWOBA), xSlg = mean(xSlg)) %>%
    ungroup() %>%
    mutate(Rank = rank(desc(xSlg + xWOBA + brl_pa + xBA + hard_hit))) %>%
    mutate_if(is.numeric, round, 2) %>%
    arrange(xWOBA) %>%
    select(Pitcher, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xSlg, xWOBA, Rank) %>%
    inner_join(pitchers, by = c('Pitcher' = 'pitcher_name')) %>%
    mutate(Price = dollar(as.numeric(pitcher_salary))) %>%
    mutate(Value = round((pitcher_salary/1000)/xWOBA,2)) %>%
    select(Pitcher, Price, Team = pitcher_team, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xSlg, xWOBA, Rank) %>%
    mutate(Rank = round(Rank,0)) %>%
    gt() %>%
    gt_color_rows(brl_pa:Rank, palette = "RColorBrewer::Reds") %>%
    tab_header(title = glue('Pitcher Averages for {date_of_game}'))
  
  return(pitcher_aggs)
}

raw_batters <- batter_index %>%
  select(Player = player_name, player_id = player, brl_pa_index, brl_pct_index,hard_hit_index, max_ev_index,mean_ev_index, sweet_spot_index, xba_index,xslg_index, xwoba_index) %>%
  arrange(desc(brl_pa_index))



positions_list <- c('','C','1B','2B','3B','SS','OF')

positions <- function(position_choice = c('C','1B','2B','3B','SS','OF'), difference = 20) {

  # put a table next to it or something so we have both

  position_players <- whole_day_stats %>%
    filter(grepl(paste(position_choice,collapse="|"), Position)) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(salary_rank = round(rank(desc(salary)),0)) %>%
    mutate(xWOBA_rank = round(rank(desc(xWOBA)),0)) %>%
    mutate(dollar_per_woba = round((salary/1000)/xWOBA,2)) %>%
    mutate(fd_difference = salary_rank - xWOBA_rank) %>%
    mutate(difference_rank = rank(xWOBA_rank - salary_rank)) %>%
    mutate(value_rank = rank(dollar_per_woba)) %>%
    mutate(number = rank(difference_rank))
  
  label_rows <- position_players %>%
    filter(rank(value_rank) <= difference | rank(desc(value_rank)) <= difference)

  rank_salary <- ggplot(position_players, aes(x = xWOBA_rank, y = salary_rank)) + geom_point() + geom_text_repel(data = label_rows, aes(label = glue('{Batter}: {dollar_per_woba}'))) + scale_x_reverse() + scale_y_reverse() + labs(title = glue('{position_choice} Breakdown'), subtitle = glue('Top and Bottom {difference} Values are Labeled'), caption = 'Data taken from baseballr package, Chart made by @matt12oneil')



  return(rank_salary)
}


# positions_reactive <- function(position_choice = c('C','1B','2B','3B','SS','OF'), difference = 20) {
#   
#   # put a table next to it or something so we have both
#   
#   position_players <- reactive(whole_day_stats %>%
#     filter(grepl(paste(position_choice,collapse="|"), Position)) %>%
#     mutate(on_base = 100 - SO - field_out) %>%
#     mutate_if(is.numeric, round, 2) %>%
#     mutate(salary_rank = round(rank(desc(salary)),0)) %>%
#     mutate(xWOBA_rank = round(rank(desc(xWOBA)),0)) %>%
#     mutate(dollar_per_woba = round((salary/1000)/xWOBA,2)) %>%
#     mutate(fd_difference = salary_rank - xWOBA_rank) %>%
#     mutate(difference_rank = rank(xWOBA_rank - salary_rank)) %>%
#     mutate(value_rank = rank(dollar_per_woba)) %>%
#     mutate(number = rank(difference_rank)))
#   
#   label_rows <- reactive(position_players %>%
#     filter(rank(value_rank) <= difference | rank(desc(value_rank)) <= difference))
#   
#   rank_salary <- ggplot(position_players, aes(x = xWOBA_rank, y = salary_rank)) + geom_point() + geom_text_repel(data = label_rows, aes(label = glue('{Batter}: {dollar_per_woba}'))) + scale_x_reverse() + scale_y_reverse() + labs(title = glue('{position_choice} Breakdown'), subtitle = glue('Top and Bottom {difference} Values are Labeled'), caption = 'Data taken from baseballr package, Chart made by @matt12oneil')
#   
#   
#   
#   return(rank_salary)
# }


positions_table <- function(position_choice = c('C','1B','2B','3B','SS','OF'), difference = 25) {
  
  # put a table next to it or something so we have both
  
  position_players <- whole_day_stats %>%
    filter(grepl(paste(position_choice,collapse="|"), Position)) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(salary_rank = round(rank(desc(salary)),0)) %>%
    mutate(xWOBA_rank = round(rank(desc(xWOBA)),0)) %>%
    mutate(dollar_per_woba = round((salary/1000)/xWOBA,2)) %>%
    mutate(fd_difference = salary_rank - xWOBA_rank) %>%
    mutate(difference_rank = rank(xWOBA_rank - salary_rank)) %>%
    mutate(value_rank = rank(dollar_per_woba)) %>%
    filter(value_rank <= difference) %>%
    select(Batter,team,Price, Pitcher, xSlg, xWOBA, Value = dollar_per_woba) %>%
    arrange(Value) %>%
    gt()
  
  
  
  return(position_players)
}

optimizer <- function(salary_left, max_teams = 4) {
  
  salary_left = 10000
  
  proj <- whole_day_stats %>%
    separate_rows(Position,sep = '/') %>%
    mutate(c_1b = str_detect(Position, 'C') | str_detect(Position, '1B')) %>%
    mutate(second = str_detect(Position, '2B')) %>%
    mutate(third = str_detect(Position, '3B')) %>%
    mutate(short = str_detect(Position, 'SS')) %>%
    mutate(of = str_detect(Position, 'OF'))
  
  n_players <- whole_day_stats %>%
    select(Batter) %>%
    n_distinct()
  
  n_teams <- whole_day_stats %>%
    select(team) %>%
    n_distinct()
  
  mat <- matrix(c(proj$salary
                ,rep(1,nrow(proj))
                ,proj$c_1b
                ,proj$second
                ,proj$third
                ,proj$short
                ,proj$of
                ,proj$c_1b
                ,proj$second
                ,proj$third
                ,proj$short
                ,proj$of
                ,t(model.matrix(~ team + 0, proj))
                ,t(model.matrix(~ Batter + 0, proj))
                ), ncol = nrow(proj), byrow = T)
  
  constraint_rhs <- c((35000-salary_left),8,1,1,1,1,3,2,2,2,2,4,rep(2,n_teams),rep(1,n_players))
  
  constraint_direction <- c('<=', '==', '>=','>=','>=','>=','>=', '<=','<=','<=','<=','<=',rep('<=',n_teams),rep('<=',n_players))
  
  output <- lp(direction = 'max', objective.in = proj$xWOBA, const.mat = mat, const.rhs = constraint_rhs, const.dir = constraint_direction, all.bin = T)
  
  
  sol <- which(output$solution == 1)
  
  solution <-proj[sol, ]
  
  return(solution)
}


# maximize xWOBA with respect to constraint Salary




ui = fluidPage(
  setBackgroundColor(color = '#D3D3D3'),
  
  titlePanel(""),
    
  mainPanel(
    tabsetPanel(
      tabPanel("Lineups", selectInput("team1",
                                      choices =  teams$team,
                                      label = 'Select a Team'
      ),
      gt_output(outputId = "team1")),
      tabPanel('Batter Index',
               DT::dataTableOutput("batter_index"
               )), 
      tabPanel('Fanduel Batters',
               DT::dataTableOutput("all_data"
                                   )), 
      tabPanel("C/1B Breakdown", 
               plotOutput(outputId = 'c_1b'),
               br(),
               br(),
               gt_output(outputId = 'c_1b_gt')
      ),
      tabPanel("2B Breakdown", 
               plotOutput(outputId = 'second'),
               br(),
               br(),
               gt_output(outputId = 'second_gt')
      ),
      tabPanel("3B Breakdown", 
               plotOutput(outputId = 'third'),
               br(),
               br(),
               gt_output(outputId = 'third_gt')
      ),
      tabPanel("SS Breakdown", 
               plotOutput(outputId = 'short'),
               br(),
               br(),
               gt_output(outputId = 'ss_gt')
      ),
      tabPanel("OF Breakdown", 
               plotOutput(outputId = 'of'),
               br(),
               br(),
               gt_output(outputId = 'of_gt')
      ),
      tabPanel("Position Players Breakdown", 
               plotOutput(outputId = 'all'),
               br(),
               br(),
               gt_output(outputId = 'all_gt')
      ),
      tabPanel("Pitchers", gt_output(outputId = 'pitchers'))
      # tabPanel("Reactive GG", 
      #          numericInput(inputId = 'number_choice'),
      #          selectInput('Positions','Choose Position(s)',positions, multiple = T),
      #          br(),
      #          br(),
      #          plotOutput(outputId = 'reactive')
      #          )
      )))







server <- function(input, output) {
  output$team1 <- render_gt(
  lineup_stats(input$team1)
  )
  output$batter_index <- DT::renderDataTable(
    raw_batters, options = list(pageLength = 35)
  )
  output$all_data <- DT::renderDataTable(
    all_players(), options = list(pageLength = 35)
  )
  output$pitchers <- render_gt(
    pitcher_stats(date_of_game)
  )
  output$c_1b <- renderPlot(
   positions(position_choice = c('C','1B'), difference = 12)
  )
  output$second <- renderPlot(
    positions(position_choice = '2B', difference = 10)
  )
  output$third <- renderPlot(
    positions(position_choice = '3B', difference = 10)
  )
  output$short <- renderPlot(
    positions(position_choice = 'SS', difference = 10)
  )
  output$of <- renderPlot(
    positions(position_choice = 'OF', difference = 15)
  )
  output$all <- renderPlot(
    positions()
  )
  output$c_1b_gt <- render_gt(
    positions_table(position_choice = c('C','1B'), difference = 25)
  )
  output$second_gt <- render_gt(
    positions_table(position_choice = '2B', difference = 25)
  )
  output$third_gt <- render_gt(
    positions_table(position_choice = '3B', difference = 25)
  )
  output$ss_gt <- render_gt(
    positions_table(position_choice = 'SS', difference = 25)
  )
  output$of_gt <- render_gt(
    positions_table(position_choice = 'OF', difference = 25)
  )
  output$all_gt <- render_gt(
    positions_table()
  )
  # output$reactive <- renderPlot({
  #   positions_reactive(input$Positions, input$number_choice)}
  # )
}
shinyApp(ui = ui, server = server) 




# input$choose_position

