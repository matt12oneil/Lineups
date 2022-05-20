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


#"sac_fly_double_play",

#only take balls put in play
valid_events <- c("single","double","triple","home_run","walk","strikeout","field_out","force_out","sac_fly","fielders_choice","grounded_into_double_play","fielders_choice_out","sac_bunt","field_error","hit_by_pitch","catcher_interf","double_play","strikeout_double_play","other_out","triple_play","sac_bunt_double_play")



teams <- mlb_teams(season = 2022, sport_ids = c(1)) %>%
  select(team_id, team_full_name, team_abbreviation)

history <- read_csv('https://raw.githubusercontent.com/matt12oneil/Lineups/master/fdhistory.csv') %>%
  janitor::clean_names() %>%
  filter(sport == 'mlb' & salary_cap == '$35k' & (!str_detect(link, 'voided') & !str_detect(link, 'endedunmatched')) & date >= '2022-01-01') %>%
  mutate(winnings = as.numeric(winnings), score = as.numeric(score)) %>%
  group_by(date,score) %>%
  summarize(total_entries = sum(entry), winnings = as.double(sum(winnings))) %>%
  arrange(desc(score))

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

url <- 'https://sportsdata.usatoday.com/baseball/mlb/odds'
lines <- clean_totals(url)



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


#function to pull in statcast data for all of season
annual_statcast_query <- function(season) {
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-04-01')),
                    as.Date(paste0(season, '-',month(Sys.Date()),'-',day(Sys.Date()))), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates,
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(scrape_statcast_savant)
  plan(multisession)
  payload <- furrr::future_map(.x = seq_along(date_grid$start_date),
                               ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                                 
                                 payload <- safe_savant(start_date = date_grid$start_date[.x],
                                                        end_date = date_grid$end_date[.x], type = 'pitcher')
                                 
                                 return(payload)
                               })
  
  plan(multisession)
  payload_df <- furrr::future_map(payload, 'result')
  plan(multisession)
  number_rows <- furrr::future_map_dfr(.x = seq_along(payload_df),
                                       ~{number_rows <- tibble::tibble(week = .x,
                                                                       number_rows = length(payload_df[[.x]]$game_date))}) %>%
    data.table() %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  plan(multisession)
  payload_df_reduced_formatted <- furrr::future_map(.x = seq_along(payload_df_reduced),
                                                    ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                             "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                             "fielder_8", "fielder_9")
                                                    
                                                    df <- purrr::pluck(payload_df_reduced, .x) %>%
                                                      dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                                      dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                        ifelse(is.na(x), 999999999, x)
                                                      })
                                                    
                                                    character_columns <- data_base_column_types %>%
                                                      dplyr::filter(class == "character") %>%
                                                      dplyr::pull(variable)
                                                    
                                                    numeric_columns <- data_base_column_types %>%
                                                      dplyr::filter(class == "numeric") %>%
                                                      dplyr::pull(variable)
                                                    
                                                    integer_columns <- data_base_column_types %>%
                                                      dplyr::filter(class == "integer") %>%
                                                      dplyr::pull(variable)
                                                    df <- data.table(df) %>%
                                                      dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                                      dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                                      dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                                    
                                                    return(df)
                                                    })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}

#function to add in new columns to statcast
format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    df <- df
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3,
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1,
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- data.table(df) %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}

#pull in statcast play by play
updated <- annual_statcast_query(season) %>%
  clean_names() %>%
  filter(game_type == 'R' & events %in% valid_events) %>%
  format_append_statcast() %>%
  select(pitch_type, game_date, release_speed, player_name, batter, pitcher, events, stand, zone, p_throws, home_team, away_team, type, type, hit_location, bb_type, balls, strikes, on_3b, on_2b, on_1b, outs_when_up, hit_distance_sc, launch_angle, launch_speed, estimated_ba_using_speedangle, estimated_woba_using_speedangle, pitch_number, if_fielding_alignment, of_fielding_alignment, delta_run_exp, delta_home_win_exp, barrel)

#write.csv(updated, '/Users/mattoneil/Documents/MO/updated.csv')

#bring in expected stats for batters and pitchers

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

# take the average to start getting the index
avg_x_woba <- mean(xstats_batters$est_woba)
avg_x_slg <- mean(xstats_batters$est_slg)
avg_x_ba <- mean(xstats_batters$est_ba)

avg_brl_pa <- mean(ev_batters$brl_pa)
avg_barrel_pct <- mean(ev_batters$brl_percent)
avg_hard_hit_pct <- mean(ev_batters$ev95percent)
avg_max_hit_ev <- mean(ev_batters$max_hit_speed)
avg_mean_ev <- mean(ev_batters$avg_hit_speed)
avg_sweet_spot_pct <- mean(ev_batters$anglesweetspotpercent)


# dfs_2021 <- read_excel(path = '/Users/mattoneil/Documents/MO/Lineups App/MLB-2021-DFS-Dataset.xlsx') %>%
#   distinct(player_id, player_name)

updated_batters <- updated %>%
  distinct(player = batter) %>%
  left_join(rosters, by = c('player' = 'player')) %>%
  inner_join(xstats_batters, by = c('player' = 'player_id')) %>%
  select(player, slg, est_slg, woba, est_woba, ba, est_ba) %>%
  mutate(xslg_index = est_slg/avg_x_slg, xwoba_index = est_woba/avg_x_woba, xba_index = est_ba/avg_x_ba) %>%
  inner_join(ev_batters, by = c('player' = 'player_id')) %>%
  mutate(brl_pa_index = brl_pa/avg_brl_pa, brl_pct_index = brl_percent/avg_barrel_pct, hard_hit_index = ev95percent/avg_hard_hit_pct, max_ev_index = max_hit_speed/avg_max_hit_ev, mean_ev_index = avg_hit_speed/avg_mean_ev, sweet_spot_index = anglesweetspotpercent/avg_sweet_spot_pct) %>%
  mutate(type = 'batter')

updated_pitchers <- updated %>%
  distinct(player = pitcher)  %>%
  left_join(rosters, by = c('player' = 'player')) %>%
  inner_join(xstats_pitchers, by = c('player' = 'player_id')) %>%
  select(player, slg, est_slg, woba, est_woba, ba, est_ba) %>%
  mutate(xslg_index = est_slg/avg_x_slg, xwoba_index = est_woba/avg_x_woba, xba_index = est_ba/avg_x_ba) %>%
  inner_join(ev_pitchers, by = c('player' = 'player_id')) %>%
  mutate(brl_pa_index = brl_pa/avg_brl_pa, brl_pct_index = brl_percent/avg_barrel_pct, hard_hit_index = ev95percent/avg_hard_hit_pct, max_ev_index = max_hit_speed/avg_max_hit_ev, mean_ev_index = avg_hit_speed/avg_mean_ev, sweet_spot_index = anglesweetspotpercent/avg_sweet_spot_pct) %>%
  mutate(type = 'pitcher')

updated_players <- updated_batters %>%
  bind_rows(updated_pitchers) %>%
  inner_join(rosters, by = c('player' = 'player')) %>%
  select(-c(slg, est_slg, woba, est_woba, ba, est_ba, avg_hit_angle, anglesweetspotpercent,max_hit_speed,avg_hit_speed, fbld, gb, max_distance, avg_distance, avg_hr_distance, ev95plus, `ev95per-swing`, ev95percent,barrels,brl_percent,brl_pa))

#all players that played in 2021
players_2021 <- data.table(get_chadwick_lu()) %>%
  filter(is.na(key_mlbam) == F & (mlb_played_last == 2022 | mlb_played_last == 2021)) %>%
  mutate(full_name = paste(name_first, name_last))

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
  select(-c(xslg_index, xwoba_index)) %>%
  distinct()

players <- salary %>%
  inner_join(pitchers, by = c('opponent' = 'pitcher_team')) %>%
  distinct()

teams <- players %>%
  distinct(team) %>%
  arrange(team)

batter_statcast <- updated %>%
  inner_join(players_2021, by =  c("batter" = "key_mlbam")) %>%
  group_by(batter, player_name = paste0(name_first, ' ', name_last), events) %>%
  summarize(total = n()) %>%
  pivot_wider(names_from = events, values_from = total) %>%
  ungroup() %>%
  mutate_all( ~ replace(., is.na(.), 0)) %>%
  group_by(batter, player_name, single, double, triple, home_run) %>%
  summarize(
    SO = sum(strikeout, strikeout_double_play),
    BB = walk,
    field_outs = sum(
      field_out,
      force_out,
      fielders_choice,
      grounded_into_double_play,
      fielders_choice_out,
      field_error,
      double_play,
      other_out,
      triple_play
    ),
    events = sum(
      field_out,
      force_out,
      fielders_choice,
      grounded_into_double_play,
      fielders_choice_out,
      field_error,
      double_play,
      other_out,
      triple_play,
      single,
      double,
      triple,
      home_run,
      strikeout,
      strikeout_double_play
    )
  ) %>%
  ungroup()

batter_totals <- batter_statcast %>%
  summarize(
    single = sum(single),
    double = sum(double),
    triple = sum(triple),
    home_run = sum(home_run),
    SO = sum(SO),
    BB = sum(BB),
    field_outs = sum(field_outs),
    events = sum(events)
  ) %>%
  ungroup()

total_percents <- batter_totals %>%
  summarize(
    SO_total_percent = SO / events,
    BB_total_percent = BB / events,
    single_total_percent = single / events,
    double_total_percent = double / events,
    triple_total_percent = triple / events,
    home_run_total_percent = home_run / events,
    field_outs_total_percent = field_outs / events
  )

batter_index <- batter_statcast %>%
  group_by(player = batter, player_name, events) %>%
  summarize(
    SO_percent = SO / events,
    BB_percent = BB / events,
    single_percent = single / events,
    double_percent = double / events,
    triple_percent = triple / events,
    home_run_percent = home_run / events,
    field_outs_percent = field_outs / events
  ) %>%
  ungroup() %>%
  cbind(total_percents) %>%
  group_by(player,
           player_name,
           Qualifier = events,
           Position = 'Batter') %>%
  summarize(
    SO_index = SO_percent / SO_total_percent,
    BB_index = BB_percent / BB_total_percent,
    single_index = single_percent / single_total_percent,
    double_index = double_percent / double_total_percent,
    triple_index = triple_percent / triple_total_percent,
    home_run_index = home_run_percent / home_run_total_percent,
    field_outs_index = field_outs_percent / field_outs_total_percent
  ) %>%
  ungroup() %>%
  inner_join(updated_batters, by = c('player')) %>%
  select(-c(slg, est_slg, woba, est_woba, ba, est_ba)) %>%
  arrange(desc(home_run_index)) %>%
  mutate_if(is.numeric, round, 2)

pitcher_statcast <- updated %>%
  inner_join(players_2021, by =  c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, player_name = paste0(name_first, ' ', name_last), events) %>%
  summarize(total = n()) %>%
  pivot_wider(names_from = events, values_from = total) %>%
  ungroup() %>%
  mutate_all( ~ replace(., is.na(.), 0)) %>%
  group_by(pitcher, player_name, single, double, triple, home_run) %>%
  summarize(
    SO = sum(strikeout, strikeout_double_play),
    BB = walk,
    field_outs = sum(
      field_out,
      force_out,
      fielders_choice,
      grounded_into_double_play,
      fielders_choice_out,
      field_error,
      double_play,
      other_out,
      triple_play
    ),
    events = sum(
      field_out,
      force_out,
      fielders_choice,
      grounded_into_double_play,
      fielders_choice_out,
      field_error,
      double_play,
      other_out,
      triple_play,
      single,
      double,
      triple,
      home_run,
      strikeout,
      strikeout_double_play
    )
  ) %>%
  ungroup()


pitcher_totals <- pitcher_statcast %>%
  summarize(
    single = sum(single),
    double = sum(double),
    triple = sum(triple),
    home_run = sum(home_run),
    SO = sum(SO),
    BB = sum(BB),
    field_outs = sum(field_outs),
    events = sum(events)
  )



pitcher_index <- pitcher_statcast %>%
  group_by(player = pitcher, player_name, events) %>%
  summarize(
    SO_percent = SO / events,
    BB_percent = BB / events,
    single_percent = single / events,
    double_percent = double / events,
    triple_percent = triple / events,
    home_run_percent = home_run / events,
    field_outs_percent = field_outs / events
  ) %>%
  ungroup() %>%
  cbind(total_percents) %>%
  group_by(player,
           player_name,
           Qualifier = events,
           Position = 'Pitcher') %>%
  summarize(
    SO_index = SO_percent / SO_total_percent,
    BB_index = BB_percent / BB_total_percent,
    single_index = single_percent / single_total_percent,
    double_index = double_percent / double_total_percent,
    triple_index = triple_percent / triple_total_percent,
    home_run_index = home_run_percent / home_run_total_percent,
    field_outs_index = field_outs_percent / field_outs_total_percent
  ) %>%
  ungroup() %>%
  inner_join(updated_pitchers, by = c('player')) %>%
  select(-c(slg, est_slg, woba, est_woba, ba, est_ba)) %>%
  arrange(desc(SO_index)) %>%
  mutate_if(is.numeric, round, 2)

indexes <- batter_index %>%
  rbind(pitcher_index)


matchup <- function(batter_mlb_id, pitcher_mlb_id) {

   # batter_mlb_id = 592450
   # pitcher_mlb_id = 579328
  
  batter <- players %>%
    filter(key_mlbam == batter_mlb_id & (position != 'P' | nickname == 'Shohei Ohtani')) %>%
    select(key_mlbam, team, opponent, batter_name = nickname, position, salary, single, double, triple, hr, so, UIBB) %>%
    inner_join(updated_batters, by = c('key_mlbam' = 'player')) %>%
    select(-c(slg,est_slg,woba, est_woba, ba, est_ba))
  
  
  pitcher <- pitchers %>%
    filter(pitcher_id == pitcher_mlb_id)
  
  pitcher_stats <- indexes %>%
    filter(player == pitcher$pitcher_id & Position == 'Pitcher')
  
  batter_stats <- indexes %>%
    filter(player == batter$key_mlbam & Position == 'Batter')
  
  batter_name <- batter$batter_name
  pitcher_name <- pitcher$pitcher_name
  SO_percent <- total_percents$SO_total_percent * pitcher_stats$SO_index * batter_stats$SO_index * batter$so
  BB_percent <- total_percents$BB_total_percent * pitcher_stats$BB_index * batter_stats$BB_index * batter$UIBB
  single_percent <- total_percents$single_total_percent * pitcher_stats$single_index * batter_stats$single_index * batter$single
  double_percent <- total_percents$double_total_percent * pitcher_stats$double_index * batter_stats$double_index * batter$double
  triple_percent <- total_percents$triple_total_percent * pitcher_stats$triple_index * batter_stats$triple_index * batter$triple
  home_run_percent <- total_percents$home_run_total_percent * pitcher_stats$home_run_index * batter_stats$home_run_index * batter$hr
  field_outs_percent <- total_percents$field_outs_total_percent * pitcher_stats$field_outs_index * batter_stats$field_outs_index
  total_x_slg <- pitcher_stats$xslg_index * batter$xslg_index
  total_x_woba <- pitcher_stats$xwoba_index * batter$xwoba_index
  total_x_ba <- pitcher_stats$xba_index * batter$xba_index
  total_brl_pa_index <- pitcher_stats$brl_pa_index * batter_stats$brl_pa_index
  total_brl_pct_index <- pitcher_stats$brl_pct_index * batter_stats$brl_pct_index
  total_hard_hit_index <- pitcher_stats$hard_hit_index * batter_stats$hard_hit_index
  total_max_ev_index <- pitcher_stats$max_ev_index * batter_stats$max_ev_index
  total_mean_ev_index <- pitcher_stats$mean_ev_index * batter_stats$mean_ev_index
  total_sweet_spot_index <- pitcher_stats$sweet_spot_index * batter_stats$sweet_spot_index
  
  matchup_percentages <- cbind(SO_percent) %>%
    bind_cols(BB_percent = BB_percent,single_percent = single_percent,double_percent = double_percent,triple_percent = triple_percent,home_run_percent= home_run_percent,field_outs_percent = field_outs_percent) %>%
    as.data.frame() %>%
    group_by(SO_percent,BB_percent,single_percent,double_percent,triple_percent,home_run_percent,field_outs_percent) %>%
    summarize(total = sum(SO_percent,BB_percent,single_percent,double_percent,triple_percent,home_run_percent,field_outs_percent)) %>%
    ungroup() %>%
    bind_cols(batter) %>%
    select(-c(single, double, triple, hr,so,UIBB)) %>%
    mutate(SO = (SO_percent/total)*100, BB = (BB_percent/total)*100, single = (single_percent/total)*100, double = (double_percent/total)*100, triple = (triple_percent/total)*100, home_run = (home_run_percent/total)*100, field_out = (field_outs_percent/total)*100) %>%
    mutate(weighted = (3*BB_percent + 3*single_percent + 6*double_percent + 9*triple_percent + 18.7*home_run_percent)) %>%
    mutate(on_base = sum(BB + single + double + triple + home_run)) %>%
    mutate_if(is.numeric, round, 2) %>%
    bind_cols(pitcher_name = pitcher_name) %>%
    mutate(Price = dollar(as.numeric(salary))) %>%
    select(Batter = batter_name, team, Position = position, Opponent = opponent, Price, Pitcher = pitcher_name, SO, BB, single, double, triple, home_run, field_out, on_base ,weighted, salary) %>%
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
    mutate(on_base = (100 - SO - field_out)) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(Rank = rank(rank(desc(xSlg)) + rank(desc(xWOBA)))) %>%
    arrange(Rank) %>%
    inner_join(pitcher_info, by = c('Pitcher' = 'pitcher_name')) %>%
    mutate(Value = round((salary/1000)/xWOBA,2)) %>%
    select(Batter, Team = team, Position, Price, Pitcher, Opponent, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xBA, xSlg, xWOBA, Value, Rank)
  
  return(batter_matchups)
}



pitcher_stats <- function(game_date) {
  
  
  pitcher_aggs <- whole_day_stats  %>%
    mutate(on_base = (100 - SO - field_out)) %>%
    group_by(Pitcher) %>%
    dplyr::summarize(SO = mean(SO), brl_pa = mean(brl_pa), brl_pct = mean(brl_pct), hard_hit = mean(hard_hit), max_ev = mean(max_ev), mean_ev = mean(mean_ev), sweet_spot = mean(sweet_spot), xBA = mean(xBA), xWOBA = mean(xWOBA), xSlg = mean(xSlg)) %>%
    ungroup() %>%
    mutate(Rank = rank(rank(xSlg) + rank(xWOBA) + rank(desc(SO)))) %>%
    mutate_if(is.numeric, round, 2) %>%
    arrange(xWOBA) %>%
    select(Pitcher, SO, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xSlg, xWOBA, Rank) %>%
    inner_join(pitchers, by = c('Pitcher' = 'pitcher_name')) %>%
    mutate(Price = dollar(as.numeric(pitcher_salary))) %>%
    mutate(Value = round((pitcher_salary/1000)/xWOBA,2)) %>%
    select(Pitcher, Price, Team = pitcher_team, SO, brl_pa, brl_pct, hard_hit, max_ev, mean_ev, sweet_spot, xSlg, xWOBA, Rank) %>%
    mutate(Rank = round(Rank,0)) %>%
    gt() %>%
    gt_color_rows(SO:Rank, palette = "RColorBrewer::Reds") %>%
    tab_header(title = glue('Pitcher Averages for {date_of_game}'))
  
  return(pitcher_aggs)
}

raw_batters <- batter_index %>%
  select(Player = player_name, brl_pa_index, brl_pct_index,hard_hit_index, max_ev_index,mean_ev_index, sweet_spot_index, xba_index,xslg_index, xwoba_index) %>%
  arrange(desc(brl_pa_index))



positions_list <- c('','C','1B','2B','3B','SS','OF')

positions <- function(position_choice = c('C','1B','2B','3B','SS','OF'), difference = 20) {

  # put a table next to it or something so we have both

  position_players <- whole_day_stats %>%
    filter(grepl(paste(position_choice,collapse="|"), Position)) %>%
    mutate(on_base = 100 - SO - field_out) %>%
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
    mutate(on_base = 100 - SO - field_out) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(salary_rank = round(rank(desc(salary)),0)) %>%
    mutate(xWOBA_rank = round(rank(desc(xWOBA)),0)) %>%
    mutate(dollar_per_woba = round((salary/1000)/xWOBA,2)) %>%
    mutate(fd_difference = salary_rank - xWOBA_rank) %>%
    mutate(difference_rank = rank(xWOBA_rank - salary_rank)) %>%
    mutate(value_rank = rank(dollar_per_woba)) %>%
    #mutate(number = rank(difference_rank)) %>% 
    filter(value_rank <= difference) %>%
    select(Batter,team,Price, Pitcher, on_base, xSlg, xWOBA, Value = dollar_per_woba) %>%
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