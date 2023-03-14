pacman::p_load(shiny,shiny,tidyverse,dplyr,formattable,gtExtras,rsconnect,baseballr,retrosheet,gt,stringr,janitor,DT,furrr,data.table,readxl,scales,shinyWidgets,lubridate,ggrepel,rvest,XML,httr,jsonlite,lpSolve,tidytable)

devtools::install_github("camdenk/mlbplotR")

library(mlbplotR)

#only take balls put in play
valid_events <- c("single","double","triple","home_run","walk","strikeout","field_out","force_out","sac_fly","fielders_choice","grounded_into_double_play","fielders_choice_out","sac_bunt","field_error","hit_by_pitch","double_play","strikeout_double_play","other_out","triple_play","sac_bunt_double_play")




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

teams <- mlb_teams(season = 2022, sport_ids = c(1)) %>%
  select(team_id, team_full_name, team_abbreviation)


rosters_func <- function(team) {
  mlb_rosters(team_id = team, date = Sys.Date(), roster_type = 'active') %>%
    inner_join(teams, by = c('parent_team_id' = 'team_id')) %>%
    select(player = person_id, player_name = person_full_name, player_team = team_abbreviation) %>%
    mutate(player_name = ifelse(player_name == 'Vladimir Guerrero' & player_team == 'TOR','Vladimir Guerrero Jr.', player_name)) %>%
    mutate(player_name = ifelse(player_name == 'Enrique Hernandez' & player_team == 'BOS','Kiké Hernandez', player_name))
}

#get players and their active teams
rosters <- map_dfr(teams$team_id, rosters_func)

#get the stadiums that each team plays in
team_stadiums <- mlb_teams(season = 2021, sport_ids = c(1)) %>%
  arrange(team_abbreviation) %>%
  select(team_name, team_abbreviation, venue_name) %>%
  mutate(team_name = ifelse(team_abbreviation == 'ARI', 'Diamondbacks', team_name)) %>%
  mutate(team_name = ifelse(team_abbreviation == 'CLE', 'Guardians', team_name))

#get park factors for each stadium from previous year
park_factors <- fg_park(2022) %>%
  left_join(team_stadiums, by = c('home_team' = 'team_name')) %>%
  select(team_abbreviation, venue_name, single, double, triple, hr, so, UIBB, GB, FB, LD, IFFB, FIP, basic_5yr, basic_3yr = `3yr`, basic_1yr = `1yr`) %>%
  mutate(single = single/100, double = double/100, triple = triple/100, hr = hr/100, so = so/100, UIBB = UIBB/100, GB = GB/100, FB = FB/100, LD = LD/100, IFFB = IFFB/100, FIP = FIP/100) %>%
  mutate(team_abbreviation)


#clarify the season for the statcast data
season = 2022
#what date for the app to be run
date_of_game = Sys.Date()



xstats <- function(type, game_season){
  expected <- statcast_leaderboards(
    leaderboard = "expected_statistics",
    year = game_season,
    abs = 25,
    min_pa = 25,
    min_pitches = 20,
    min_field = "q",
    min_run = 0,
    player_type = type,
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
  return(expected)
}
xstats_batters <- xstats("batter", season)
xstats_pitchers <- xstats("pitcher", season)



ev <- function(type, game_season){
  ev_batters <- statcast_leaderboards(
    leaderboard = "exit_velocity_barrels",
    year = game_season,
    abs = 25,
    min_pa = 25,
    min_pitches = 20,
    min_field = "q",
    min_run = 0,
    player_type = type,
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
}
ev_batters <- ev("batter",season)
ev_pitchers <- ev("pitcher",season)


statcast_combined <- function(cat){
  xstats <- xstats(cat, season)
  ev <- ev(cat, season)
  stats <- xstats %>%
    inner_join(ev, by = c('player_id','year','first_name','last_name')) %>%
    select(player_id, first_name, last_name, ba, est_ba, slg, est_slg, woba, est_woba, anglesweetspotpercent, max_hit_speed, avg_hit_speed, fbld_avg_hit_speed = fbld, gb_avg_hit_speed = gb, ev95percent, brl_percent, brl_pa) %>%
    left_join(rosters, by = c('player_id' = 'player')) %>%
    select(player_id, player_name, player_team, ba, est_ba, slg, est_slg, woba, est_woba, anglesweetspotpercent, max_hit_speed, avg_hit_speed, fbld_avg_hit_speed, gb_avg_hit_speed, ev95percent, brl_percent, brl_pa) %>%
    mutate(type = cat)
  return(stats)
}
batters_statcast <- statcast_combined("batter")
pitchers_statcast <- statcast_combined("pitcher")

indexes <-function(dt){
  index <- dt %>%
    mutate(est_ba_index = est_ba/mean(est_ba), est_slg_index = est_slg/mean(est_slg), est_woba_index = est_woba/mean(est_woba), angle_sweet_spot_index = anglesweetspotpercent/mean(anglesweetspotpercent), max_hit_speed_index = max_hit_speed/mean(max_hit_speed), avg_hit_speed_index = avg_hit_speed/mean(avg_hit_speed), hard_hit_index = ev95percent/mean(ev95percent), brl_percent_index = brl_percent/mean(brl_percent), brl_pa_index = brl_pa/mean(brl_pa)) %>%
    select(player_id, player_name, player_team, type, est_ba_index, est_slg_index, est_woba_index, angle_sweet_spot_index, max_hit_speed_index, avg_hit_speed_index, hard_hit_index, brl_percent_index, brl_pa_index) 
  return(index)
}
batter_indexes <- indexes(batters_statcast)
pitcher_indexes <- indexes(pitchers_statcast)
indexes_total <- batter_indexes %>%
  bind_rows(pitcher_indexes)

#batter_splits vs. different handed pitchers
batter_splits <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/batters_22_splits.csv') %>%
  mutate(mean_ev_index_split = avg_launch_speed/mean(avg_launch_speed, na.rm = T), brl_index_split = brl_pct/mean(brl_pct, na.rm = T), xba_index_split = xba/mean(xba, na.rm = T), woba_index_split = woba/mean(woba, na.rm = T), xwoba_index_split = xwoba/mean(xwoba, na.rm = T), iso_index_split = iso/mean(iso, na.rm = T)) %>%
  mutate(type = 'batter') %>%
  select(player_id = batter, stand, p_throws, type, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)
pitcher_splits <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/pitchers_22_splits.csv') %>%
  mutate(mean_ev_index_split = avg_launch_speed/mean(avg_launch_speed, na.rm = T), brl_index_split = brl_pct/mean(brl_pct, na.rm = T), xba_index_split = xba/mean(xba, na.rm = T), woba_index_split = woba/mean(woba, na.rm = T), xwoba_index_split = xwoba/mean(xwoba, na.rm = T), iso_index_split = iso/mean(iso, na.rm = T)) %>%
  mutate(type = 'pitcher') %>%
  select(player_id = pitcher, stand, p_throws, type, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)
splits_total <- batter_splits %>%
  bind_rows(pitcher_splits)

splits <- function(cat){
  splits <- indexes_total %>%
    filter(type == cat) %>%
    inner_join(splits_total, by = c('player_id','type')) %>%
    select(player_id, player_name, player_team, type, stand, p_throws, est_ba_index, est_slg_index, est_woba_index, angle_sweet_spot_index, max_hit_speed_index, avg_hit_speed_index, hard_hit_index, brl_percent_index, brl_pa_index, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)
}
batter_splits <- splits('batter')
pitcher_splits <- splits('pitcher')




batters_statcast <- xstats_batters %>%
  left_join(rosters, by = c('player_id' = 'player')) %>%
  select(player = player_id, slg, est_slg, woba, est_woba, ba, est_ba) %>%
  mutate(xslg_index = est_slg/mean(est_slg), xwoba_index = est_woba/mean(est_woba), xba_index = est_ba/mean(est_ba)) %>%
  inner_join(ev_batters, by = c('player' = 'player_id')) %>%
  mutate(player_name = paste0(first_name,' ',last_name), brl_pa_index = brl_pa/mean(brl_pa), brl_pct_index = brl_percent/mean(brl_percent), hard_hit_index = ev95percent/mean(ev95percent), max_ev_index = max_hit_speed/mean(max_hit_speed), mean_ev_index = avg_hit_speed/mean(avg_hit_speed), sweet_spot_index = anglesweetspotpercent/mean(anglesweetspotpercent)) %>%
  mutate(type = 'batter') %>%
  select(player, type, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pct_index, max_ev_index, mean_ev_index, sweet_spot_index) %>%
  inner_join(batter_splits, by = c('player' = 'player_id','type')) %>%
  select(player, player_name, type, stand, p_throws, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pa_index, brl_pct_index, hard_hit_index, max_ev_index, mean_ev_index, sweet_spot_index, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)

pitchers_statcast <- xstats_pitchers %>%
  left_join(rosters, by = c('player_id' = 'player')) %>%
  select(player = player_id, slg, est_slg, woba, est_woba, ba, est_ba) %>%
  mutate(xslg_index = est_slg/mean(est_slg), xwoba_index = est_woba/mean(est_woba), xba_index = est_ba/mean(est_ba)) %>%
  inner_join(ev_pitchers, by = c('player' = 'player_id')) %>%
  mutate(player_name = paste0(first_name,' ',last_name), brl_pa_index = brl_pa/mean(brl_pa), brl_pct_index = brl_percent/mean(brl_percent), hard_hit_index = ev95percent/mean(ev95percent), max_ev_index = max_hit_speed/mean(max_hit_speed), mean_ev_index = avg_hit_speed/mean(avg_hit_speed), sweet_spot_index = anglesweetspotpercent/mean(anglesweetspotpercent)) %>%
  mutate(type = 'pitcher') %>%
  select(player, type, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pct_index, max_ev_index, mean_ev_index, sweet_spot_index) %>%
  inner_join(pitcher_splits, by = c('player' = 'player_id','type')) %>%
  select(player, player_name, type, stand, p_throws, est_slg, slg, est_woba, woba, est_ba, ba, xslg_index, xwoba_index, xba_index, brl_pa_index, brl_pct_index, hard_hit_index, max_ev_index, mean_ev_index, sweet_spot_index, mean_ev_index_split, brl_index_split, xba_index_split, woba_index_split, xwoba_index_split, iso_index_split)
  
salary <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/fdsalary.csv') %>%
  janitor::clean_names() %>%
  filter(is.na(injury_indicator)) %>%
  mutate(nickname = ifelse(nickname == 'Kike Hernandez' & team == 'BOS','Kiké Hernandez', nickname)) %>%
  inner_join(rosters, by = c('nickname' = 'player_name')) %>%
  select(key_mlbam = player, id, position, nickname, salary, game, opponent, injury_indicator, injury_details, probable_pitcher, batting_order, roster_position, team, opponent) %>%
  inner_join(batters_statcast, by = c('key_mlbam' = 'player')) %>%
  filter(type == 'batter')%>%
  distinct() %>%
  separate(game, into = c('away','home'), sep = '@') %>%
  inner_join(park_factors, by = c('home'='team_abbreviation')) %>%
  select(batter_id = key_mlbam, player_name, team, salary, opponent, p_throws, stand, batter_xslg = xslg_index, batter_xwoba = xwoba_index, batter_xba = xba_index, batter_barrel_pa = brl_pa_index, batter_barrel_pct = brl_pct_index, batter_hard_hit = hard_hit_index, batter_max_ev = max_ev_index, batter_mean_ev = mean_ev_index, batter_sweet_spot = sweet_spot_index, batter_mean_ev_split = mean_ev_index_split, batter_mean_ev_split = mean_ev_index_split, batter_brl_split = brl_index_split, batter_xba_split = xba_index_split, batter_woba_split = woba_index_split, batter_xwoba_split = xwoba_index_split, batter_iso_split = iso_index_split, park_factors = basic_1yr) %>%
  distinct()


pitchers <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/fdsalary.csv') %>%
  janitor::clean_names() %>%
  filter(probable_pitcher == 'Yes') %>%
  inner_join(rosters, by = c('nickname' = 'player_name')) %>%
  select(pitcher_id = player, pitcher_name = nickname, pitcher_team = team, pitcher_salary = salary) %>%
  inner_join(pitchers_statcast, by = c('pitcher_id' = 'player')) %>%
  filter(type == 'pitcher') %>%
  select(pitcher_id, pitcher_name, pitcher_team, pitcher_salary, p_throws, stand, pitcher_xslg = xslg_index, pitcher_xwoba = xwoba_index, pitcher_xba = xba_index, pitcher_barrel_pa = brl_pa_index, pitcher_barrel_pct = brl_pct_index, pitcher_hard_hit = hard_hit_index, pitcher_max_ev = max_ev_index, pitcher_mean_ev = mean_ev_index, pitcher_sweet_spot = sweet_spot_index, pitcher_mean_ev_split = mean_ev_index_split, pitcher_mean_ev_split = mean_ev_index_split, pitcher_brl_split = brl_index_split, pitcher_xba_split = xba_index_split, pitcher_woba_split = woba_index_split, pitcher_xwoba_split = xwoba_index_split, pitcher_iso_split = iso_index_split) %>%
  distinct()

players <- salary %>%
  inner_join(pitchers, by = c('opponent' = 'pitcher_team', "p_throws","stand")) %>%
  distinct() 

#adding in the account for park factors (basic 1 year)

whole_day_stats <- players %>%
  mutate(xslg = batter_xslg * pitcher_xslg * park_factors
         , xwoba = batter_xwoba * pitcher_xwoba * park_factors
         , xba = batter_xba * pitcher_xba * park_factors
         , barrel_pa = batter_barrel_pa * pitcher_barrel_pa * park_factors
         , barrel_pct = batter_barrel_pct * pitcher_barrel_pct * park_factors 
         , hard_hit = batter_hard_hit * pitcher_hard_hit * park_factors
         , max_ev = batter_max_ev * pitcher_max_ev * park_factors
         , mean_ev = batter_max_ev * pitcher_max_ev * park_factors
         , sweet_spot = batter_sweet_spot * pitcher_sweet_spot * park_factors
         , mean_ev_split = batter_mean_ev_split * pitcher_mean_ev_split * park_factors
         , xba_split = batter_xba_split * pitcher_xba_split * park_factors
         , woba_split = batter_woba_split * pitcher_woba_split * park_factors
         , xwoba_split = batter_xwoba_split * pitcher_xwoba_split * park_factors
         , brl_split = batter_brl_split * pitcher_brl_split * park_factors
         , iso_split = batter_iso_split * pitcher_iso_split * park_factors) %>%
  select(batter_id, batter = player_name, batter_team = team, batter_salary = salary, pitcher_id, pitcher = pitcher_name, pitcher_team = opponent, pitcher_salary, p_throws, batter_stand = stand, xslg, xwoba, xba, barrel_pa, barrel_pct, hard_hit, mean_ev, max_ev, sweet_spot, mean_ev_split, xba_split, woba_split, xwoba_split, brl_split, iso_split)

teams <- players %>%
  distinct(team) %>%
  arrange(team)  

lineup_stats_no_split <- function(team_name){
  
  pitcher_name <- whole_day_stats %>%
    filter(batter_team == team_name) %>%
    select(pitcher) %>%
    distinct()
  
  opposing_team <- whole_day_stats %>%
    filter(batter_team == team_name) %>%
    select(pitcher_team) %>%
    distinct()
  
  order <- whole_day_stats %>%
    filter(batter_team == team_name)
  
  
  all_matchups <- order %>%
    select(Batter = batter, Pitcher = pitcher_name, Salary = batter_salary, xslg, xba, xwoba, barrel_pa, barrel_pct, hard_hit, mean_ev, max_ev, sweet_spot, woba) %>%
    mutate_if(is.numeric,round,2) %>%
    gt() %>%
    gt_merge_stack(col1 = Batter, Pitcher) %>%
    gt_color_rows(xslg:woba, palette = "grDevices::blues9") %>%
    tab_header(title = glue("{team_name} Lineup Projections vs. {opposing_team$Opponent} Pitcher {pitcher_name}")) %>%
    tab_options(table.width = 12)
  
  return(all_matchups)
}

lineup_stats_w_split <- function(team_name){
  
  pitcher_name <- whole_day_stats %>%
    filter(batter_team == team_name) %>%
    select(pitcher) %>%
    distinct()
  
  opposing_team <- whole_day_stats %>%
    filter(batter_team == team_name) %>%
    select(pitcher_team) %>%
    distinct()
  
  order <- whole_day_stats %>%
    filter(batter_team == team_name)
  
  
  all_matchups <- order %>%
    select(Batter = batter, Pitcher = pitcher_name, Salary = batter_salary, mean_ev_split, xba_split, woba_split, xwoba_split, brl_split, iso_split) %>%
    mutate_if(is.numeric,round,2) %>%
    gt() %>%
    gt_merge_stack(col1 = Batter, Pitcher) %>%
    gt_color_rows(mean_ev_split:iso_split, palette = "grDevices::blues9") %>%
    tab_header(title = glue("{team_name} Lineup Projections vs. {opposing_team$Opponent} Pitcher {pitcher_name}")) %>%
    tab_options(table.width = 12)
  
  return(all_matchups)
}

pitcher_stats <- function() {
  
  clean_pitchers <- pitchers %>%
    select(pitcher_id, pitcher_name, pitcher_team, pitcher_salary) %>%
    distinct()
    
  pitcher_aggs <- whole_day_stats %>%
    group_by(pitcher, pitcher_id, pitcher_team, batter_team, pitcher_team) %>%
    summarize(barrel_pa = mean(barrel_pa), barrel_pct = mean(barrel_pct), hard_hit = mean(hard_hit), max_ev = mean(max_ev), mean_ev = mean(mean_ev), sweet_spot = mean(sweet_spot), xba = mean(xba), xwoba = mean(xwoba), xslg = mean(xslg)) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(Rank = rank((barrel_pa + barrel_pct + (2*hard_hit) + max_ev + mean_ev + (2*sweet_spot) + xba + (2*xslg) + (2*xwoba)))) %>%
    arrange(Rank) %>%
    select(Pitcher = pitcher, pitcher_id, Team = pitcher_team, Opponent = batter_team, xba, xwoba, xslg, barrel_pa, barrel_pct, hard_hit, max_ev, mean_ev, sweet_spot, Rank) %>%
    inner_join(clean_pitchers, by = c('Pitcher' = 'pitcher_name', "Team" = 'pitcher_team', 'pitcher_id')) %>%
    mutate(Price = dollar(as.numeric(pitcher_salary))) %>%
    mutate(Value = round((pitcher_salary/1000)/xwoba,2)) %>%
    select(Pitcher, Price, Team, Opponent, xba, xwoba, xslg, barrel_pa, barrel_pct, hard_hit, max_ev, mean_ev, sweet_spot, Rank, Value) %>%
    mutate(Rank = round(Rank,0)) %>%
    arrange(Rank) %>%
    gt() %>%
    gt_color_rows(xba:Rank, palette = "RColorBrewer::Reds") %>%
    tab_header(title = glue('Pitcher Averages for {date_of_game}'))
  
  return(pitcher_aggs)
}
pitcher_stats()
