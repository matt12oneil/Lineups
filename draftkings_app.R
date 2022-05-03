#next steps
  #wOBA and xOBA
  #weighting the pitchers more heavily on strikeouts than on base
  #now value pitchers by points
  #get this on GitHub
  #implied totals
  #change pitcher scoring ranks

pacman::p_load_current_gh("billpetti/baseballr")

library(shiny)
library(glue)
library(tidyverse)
library(dplyr)
library(formattable)
library(gtExtras)
library(rsconnect)
#library(baseballr)
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

#only take balls put in play
# valid_events <- c("single","double","triple","home_run","walk","strikeout","field_out","force_out","sac_fly","fielders_choice","grounded_into_double_play","fielders_choice_out","sac_bunt","field_error","hit_by_pitch","catcher_interf","double_play","strikeout_double_play","sac_fly_double_play","other_out","triple_play","sac_bunt_double_play")
teams <- mlb_teams(season = 2022, sport_ids = c(1)) %>%
  select(team_id, team_full_name, team_abbreviation)

rosters <- mlb_rosters(team_id = 108, date = Sys.Date(), roster_type = 'active') %>%
  bind_rows(mlb_rosters(team_id = 109, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 110, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 111, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 112, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 113, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 114, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 115, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 116, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 117, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 118, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 119, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 120, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 121, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 133, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 134, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 135, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 136, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 137, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 138, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 139, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 140, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 141, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 142, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 143, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 144, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 145, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 146, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 147, date = Sys.Date(), roster_type = 'active')) %>%
  bind_rows(mlb_rosters(team_id = 158, date = Sys.Date(), roster_type = 'active')) %>%
  inner_join(teams, by = c('parent_team_id' = 'team_id')) %>%
  select(player = person_id, player_name = person_full_name, team = team_abbreviation)

teams <- mlb_teams(season = 2021, sport_ids = c(1)) %>%
  select(team_name, team_abbreviation, venue_name)

park_factors <- fg_park(2021) %>%
  inner_join(teams, by = c('home_team' = 'team_name')) %>%
  select(team_abbreviation, venue_name, single, double, triple, hr, so, UIBB, GB, FB, LD, IFFB, FIP, basic_5yr, basic_3yr = `3yr`, basic_1yr = `1yr`) %>%
  mutate(single = single/100, double = double/100, triple = triple/100, hr = hr/100, so = so/100, UIBB = UIBB/100, GB = GB/100, FB = FB/100, LD = LD/100, IFFB = IFFB/100, FIP = FIP/100)


#clarify the season for the statcast data
season = 2022
#what date for the app to be run
date_of_game = Sys.Date()


setwd('/Users/mattoneil/Documents/MO/Lineups App/')
updated <- readr::read_csv(file = 'updated.csv')
dfs_2021 <- read_excel(path = 'MLB-2021-DFS-Dataset.xlsx') %>%
  distinct(player_id, player_name)

updated_batters <- updated %>%
  distinct(player = batter) %>%
  inner_join(dfs_2021, by = c('player' = 'player_id'))

updated_pitchers <- updated %>%
  distinct(player = pitcher)  %>%
  inner_join(dfs_2021, by = c('player' = 'player_id'))

updated_players <- updated_batters %>%
  bind_rows(updated_pitchers) %>%
  inner_join(rosters, by = c('player'))

#all players that played in 2021
players_2021 <- data.table(get_chadwick_lu()) %>%
  filter(is.na(key_mlbam) == F & (mlb_played_last == 2022 | mlb_played_last == 2021)) %>%
  mutate(full_name = paste(name_first, name_last))


salary <- read_csv('/Users/mattoneil/Documents/MO/Lineups App/DKSalaries.csv') %>%
  janitor::clean_names() %>%
  separate(game_info, into = c('game','crap'), extra = 'drop', sep = ' ') %>%
  separate(game, into = c('away','home'), extra = 'drop', sep = '@') %>%
  select(-crap) %>%
  mutate(opponent = case_when(as.character(away) == as.character(team_abbrev) ~ as.character(home),
                   as.character(home) == as.character(team_abbrev) ~ as.character(away),
                   TRUE ~ 'neither')) %>%
  inner_join(dfs_2021, by = c('name' = 'player_name'), keep = F) %>%
  select(key_mlbam = player_id, id, position, name, salary, team = team_abbrev, opponent, home) %>%
  inner_join(updated_players, by = c('key_mlbam' = 'player', 'team')) %>%
  distinct() %>%
  select(key_mlbam, position, name, salary, team, opponent, home) %>%
  inner_join(park_factors, by = c('home'='team_abbreviation'))

fd_pitchers <- read_csv('/Users/mattoneil/Documents/MO/Lineups App/fdsalary.csv') %>%
  janitor::clean_names() %>%
  filter(probable_pitcher == 'Yes') %>%
  inner_join(dfs_2021, by = c('nickname' = 'player_name')) %>%
  select(pitcher_id = player_id, pitcher_name = nickname, pitcher_team = team, pitcher_salary = salary) %>%
  inner_join(updated_players, by = c('pitcher_id' = 'player', 'pitcher_team' = 'team')) %>%
  distinct() %>%
  select(pitcher_id, pitcher_name, pitcher_team) 

dk_pitchers <- read_csv('/Users/mattoneil/Documents/MO/Lineups App/DKSalaries.csv') %>%
  janitor::clean_names()

pitchers <- fd_pitchers %>%
  inner_join(dk_pitchers, by = c('pitcher_name' = 'name')) %>%
  select(pitcher_id, pitcher_name, pitcher_salary = salary, pitcher_team)

players <- salary %>%
  inner_join(pitchers, by = c('opponent' = 'pitcher_team')) %>%
  distinct()

teams <- players %>%
  distinct(team) %>%
  arrange(team)

#all games for day based on of the entire game
# day_games <- get_game_pks_mlb(date_of_game, level_ids = c(1)) %>%
#   janitor::clean_names() %>%
#   select(game_pk, game_type, season, game_date, double_header, day_night, teams_away_league_record_pct, teams_home_league_record_pct, teams_away_team_name, teams_home_team_name, venue_name)

# # team_list <- function(game_date) {
#   
#   all_day_lineups <- function(id) {
#     away_lineup <- data.table(get_probables_mlb(id)[1,]) %>%
#       cbind(get_batting_orders(id))
#     home_lineup <- data.table(get_probables_mlb(id)[2,]) %>%
#       cbind(get_batting_orders(id))
#     game_lineup <- away_lineup %>%
#       rbind(home_lineup)
#     colnames(game_lineup) <- c('game_pk','game_date','pitcher_name','pitcher_id','pitcher_team','pitcher_team_id','home_plate_full_name','home_plate_id','batter_id','batter_name','position','batting_order_number','batting_position_num','home_away','batter_team','batter_team_id')
#     
#     game_lineup <- game_lineup %>%
#       filter(batter_team != pitcher_team)
#     return(game_lineup)
#   }
#   
# 
#   
#   
#   day_games <- get_game_pks_mlb(game_date, level_ids = c(1)) %>%
#     janitor::clean_names() %>%
#     select(game_pk, game_type, season, game_date, double_header, day_night, teams_away_league_record_pct, teams_home_league_record_pct, teams_away_team_name, teams_home_team_name, venue_name)
#   
#   plan(multisession)
#   lineups <- data.table(future_map_dfr(day_games[,1], all_day_lineups))
#   
#   day <- lineups %>%
#     filter(!is.na(pitcher_id)) %>%
#     select(team = batter_team) %>%
#     distinct() %>%
#     arrange(team)
#   
#   return(day)
# }



#function to pull in statcast data for all of season
# annual_statcast_query <- function(season) {
#   
#   data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
#   
#   dates <- seq.Date(as.Date(paste0(season, '-03-01')),
#                     as.Date(paste0(season, '-10-01')), by = '4 days')
#   
#   date_grid <- tibble::tibble(start_date = dates, 
#                               end_date = dates + 3)
#   
#   safe_savant <- purrr::safely(scrape_statcast_savant)
#   plan(multisession)
#   payload <- furrr::future_map(.x = seq_along(date_grid$start_date), 
#                         ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
#                           
#                           payload <- safe_savant(start_date = date_grid$start_date[.x], 
#                                                  end_date = date_grid$end_date[.x], type = 'pitcher')
#                           
#                           return(payload)
#                         })
#   
#   plan(multisession)
#   payload_df <- furrr::future_map(payload, 'result')
#   plan(multisession)
#   number_rows <- furrr::future_map_dfr(.x = seq_along(payload_df), 
#                                ~{number_rows <- tibble::tibble(week = .x, 
#                                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
#     data.table() %>%
#     dplyr::filter(number_rows > 0) %>%
#     dplyr::pull(week)
#   
#   payload_df_reduced <- payload_df[number_rows]
#   
#   plan(multisession)
#   payload_df_reduced_formatted <- furrr::future_map(.x = seq_along(payload_df_reduced), 
#                                              ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
#                                                                       "fielder_4", "fielder_5", "fielder_6", "fielder_7",
#                                                                       "fielder_8", "fielder_9")
#                                              
#                                              df <- purrr::pluck(payload_df_reduced, .x) %>%
#                                                dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
#                                                dplyr::mutate_at(.vars = cols_to_transform, function(x) {
#                                                  ifelse(is.na(x), 999999999, x)
#                                                })
#                                              
#                                              character_columns <- data_base_column_types %>%
#                                                dplyr::filter(class == "character") %>%
#                                                dplyr::pull(variable)
#                                              
#                                              numeric_columns <- data_base_column_types %>%
#                                                dplyr::filter(class == "numeric") %>%
#                                                dplyr::pull(variable)
#                                              
#                                              integer_columns <- data_base_column_types %>%
#                                                dplyr::filter(class == "integer") %>%
#                                                dplyr::pull(variable)
#                                              
#                                              df <- data.table(df) %>%
#                                                dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
#                                                dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
#                                                dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
#                                              
#                                              return(df)
#                                              })
#   
#   combined <- payload_df_reduced_formatted %>%
#     dplyr::bind_rows()
#   
#   combined
# }
# 
# #function to add in new columns to statcast
# #format_append_statcast <- function(df) {
#   
#   # function for appending new variables to the data set
#   
#   additional_info <- function(df) {
#     
#     # apply additional coding for custom variables
#     df <- df
#     
#     df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
#                                    ifelse(type == "X" & events == "double", 2,
#                                           ifelse(type == "X" & events == "triple", 3, 
#                                                  ifelse(type == "X" & events == "home_run", 4, NA)))))
#     
#     df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
#                               ifelse(type == "X" & events == "double", 1,
#                                      ifelse(type == "X" & events == "triple", 1, 
#                                             ifelse(type == "X" & events == "home_run", 1, NA)))))
#     
#     df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
#     
#     df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
#     
#     df <- df %>%
#       dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
#     
#     df <- df %>%
#       dplyr::mutate(spray_angle = round(
#         (atan(
#           (hc_x-125.42)/(198.27-hc_y)
#         )*180/pi*.75)
#         ,1)
#       )
#     
#     df <- df %>%
#       dplyr::filter(!is.na(game_year))
#     
#     return(df)
#   }
#   
#   df <- data.table(df) %>%
#     additional_info()
#   
#   df$game_date <- as.character(df$game_date)
#   
#   df <- df %>%
#     dplyr::arrange(game_date)
#   
#   df <- df %>%
#     dplyr::filter(!is.na(game_date))
#   
#   df <- df %>%
#     dplyr::ungroup()
#   
#   df <- df %>%
#     dplyr::select(setdiff(names(.), c("error")))
#   
#   return(df)
# }
# 
# # #run the function to pull in data
# #updated <- annual_statcast_query(season) %>%
#   #clean_names() %>%
#   #filter(game_type == 'R' & events %in% valid_events) %>%
#   #format_append_statcast() %>%
#   #select(pitch_type, game_date, release_speed, player_name, batter, pitcher, events, stand, zone, p_throws, home_team, away_team, type, type, hit_location, bb_type, balls, strikes, on_3b, on_2b, on_1b, outs_when_up, hit_distance_sc, launch_angle, launch_speed, estimated_ba_using_speedangle, estimated_woba_using_speedangle, pitch_number, if_fielding_alignment, of_fielding_alignment, delta_run_exp, delta_home_win_exp, barrel)
# 
# #write.csv(updated, '/Users/mattoneil/Documents/MO/updated.csv')





#get indexes for park factors, will want to add this in
#park_factors <- fg_park(2021) 

#park_indexes <- park_factors %>%
#group_by(home_team) %>%
#summarize(SO_park_index = so/100, BB_park_index = UIBB/100, single_park_index = single/100, double_park_index = double/100, triple_park_index = triple/100, field_outs_park_index = 1)




#bring in aggregate data


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
      sac_fly_double_play,
      triple_play,
      sac_bunt_double_play
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
      sac_fly_double_play,
      triple_play,
      sac_bunt_double_play,
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
      sac_fly_double_play,
      triple_play,
      sac_bunt_double_play
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
      sac_fly_double_play,
      triple_play,
      sac_bunt_double_play,
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
  arrange(desc(SO_index)) %>%
  mutate_if(is.numeric, round, 2)

indexes <- batter_index %>%
  rbind(pitcher_index)





# all_day_lineups <- function(id) {
#   away_lineup <- get_probables_mlb(id)[1,] %>%
#     cbind(get_batting_orders(id))
#   home_lineup <- get_probables_mlb(id)[2,] %>%
#     cbind(get_batting_orders(id))
#   game_lineup <- away_lineup %>%
#     rbind(home_lineup)
#   colnames(game_lineup) <- c('game_pk','game_date','pitcher_name','pitcher_id','pitcher_team','pitcher_team_id','home_plate_full_name','home_plate_id','batter_id','batter_name','position','batting_order_number','batting_position_num','home_away','batter_team','batter_team_id')
#   
#   game_lineup <- game_lineup %>%
#     filter(batter_team != pitcher_team)
#   return(game_lineup)
# }

# lineups <- map_dfr(day_games[,1], all_day_lineups)



matchup <- function(batter_mlb_id, pitcher_mlb_id) {
  
  #batter_mlb_id = 571657
  #pitcher_mlb_id = 657140
  
  batter <- players %>%
    filter(key_mlbam == batter_mlb_id & (position != 'SP' | name == 'Shohei Ohtani')) %>%
    select(key_mlbam, team, opponent, batter_name = name, position, salary, single, double, triple, hr, so, UIBB) 
  
  
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
  
  matchup_percentages <- cbind(SO_percent) %>%
    cbind(BB_percent,single_percent,double_percent,triple_percent,home_run_percent,field_outs_percent) %>%
    as.data.frame() %>%
    group_by(SO_percent,BB_percent,single_percent,double_percent,triple_percent,home_run_percent,field_outs_percent) %>%
    summarize(total = sum(SO_percent,BB_percent,single_percent,double_percent,triple_percent,home_run_percent,field_outs_percent)) %>%
    ungroup() %>%
    cbind(batter) %>%
    select(-c(single, double, triple, hr,so,UIBB)) %>%
    mutate(SO = (SO_percent/total)*100, BB = (BB_percent/total)*100, single = (single_percent/total)*100, double = (double_percent/total)*100, triple = (triple_percent/total)*100, home_run = (home_run_percent/total)*100, field_out = (field_outs_percent/total)*100) %>%
    mutate(weighted = (3*BB_percent + 3*single_percent + 6*double_percent + 9*triple_percent + 18.7*home_run_percent)) %>%
    mutate_if(is.numeric, round, 2) %>%
    cbind(pitcher_name) %>%
    mutate(Price = dollar(as.numeric(salary))) %>%
    select(Batter = batter_name, team, Position = position, Price, Pitcher = pitcher_name, SO, BB, single, double, triple, home_run,field_out,weighted, salary) %>%
    distinct()
  
  print(batter$batter_name)
  
  #get outcome percentages for all batter chart so we can order by
  
  return(matchup_percentages)
}

order_matchup <- function(df) {
  matchup(df$key_mlbam, df$pitcher_id)
}

lineup_stats <- function(team_name){
  
  
  pitcher_name <- players %>%
    filter(team == team_name) %>%
    select(pitcher_name) %>%
    distinct()
  
  opposing_team <- players %>%
    filter(team == team_name) %>%
    select(opponent) %>%
    distinct()
  
  order <- players %>%
    filter(team == team_name & position != 'SP') %>%
    inner_join(batter_index, by = c("key_mlbam" = "player")) %>%
    filter(Qualifier >= 50)
  
  
  new_lineups <- order %>%
    split(1:nrow(.)) %>%
    as.list()
  
  
  all_matchups <- map_dfr(new_lineups, order_matchup) %>%
    mutate(on_base = 100 - SO - field_out) %>%
    mutate_if(is.numeric, round, 2) %>% 
    gt() %>%
    gt_merge_stack(col1 = Batter, Pitcher) %>%
    gt_color_rows(SO:on_base, palette = "grDevices::blues9") %>%
    tab_header(title = glue('{team_name} Lineup Projections vs. {opposing_team$opponent} for {date_of_game}')) %>%
    tab_options(table.width = 12)
  
  return(all_matchups)
}



day <- players %>%
  filter(position != 'SP' | name == 'Shohei Ohtani') %>%
  inner_join(batter_index, by = c("key_mlbam" = "player")) %>%
  filter(Qualifier >= 50) %>%
  arrange(name) %>%
  distinct()


new_lineups <- day %>%
  split(1:nrow(.))

whole_day_stats <- map_dfr(new_lineups, order_matchup) %>%
  distinct() 



all_players <- function(game_date) {
  
  
  batter_matchups <- whole_day_stats %>%
    mutate(on_base = (100 - SO - field_out)) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(Value = rank(salary/weighted)) %>%
    mutate(Rank = rank(desc(on_base) + rank(desc(weighted)))) %>%
    arrange(Rank) %>%
    inner_join(pitchers, by = c('Pitcher' = 'pitcher_name')) %>%
    select(Batter, Team = team,Position, Price, Pitcher, Opponent = pitcher_team, SO, BB, Single = single, Double = double, Triple = triple, `Home Run` = home_run, Weighted =  weighted, `On Base` = on_base, Rank, Value)
  
  return(batter_matchups)
}




pitcher_stats <- function(game_date) {
  
  
  pitcher_aggs <- whole_day_stats  %>%
    mutate(on_base = (100 - SO - field_out)) %>%
    group_by(Pitcher, team) %>%
    dplyr::summarize(SO = mean(SO), BB = mean(BB), single = mean(single), double = mean(double), triple = mean(triple), home_run = mean(home_run), field_out = mean(field_out), on_base = mean(on_base), weighted = mean(weighted)) %>%
    arrange(on_base) %>%
    ungroup() %>%
    mutate(Total = (rank(desc(SO)) + rank(on_base) + rank(weighted))/3) %>%
    mutate_if(is.numeric, round, 2) %>%
    arrange(Total) %>%
    select(Pitcher, SO, on_base, weighted, Total, team) %>%
    inner_join(pitchers, by = c('Pitcher' = 'pitcher_name')) %>%
    mutate(Price = dollar(as.numeric(pitcher_salary)))%>%
    select(Pitcher, Price, Team = pitcher_team, Opponent = team, SO, `On Base` = on_base, Weighted = weighted, Total) %>%
    gt() %>%
    gt_color_rows(SO:Total, palette = "RColorBrewer::Reds") %>%
    tab_header(title = glue('Pitcher Averages for {date_of_game}'))
  
  return(pitcher_aggs)
}



positions_list <- c('','C','1B','2B','3B','SS','OF')

positions <- function(position_choice) {
  
  
  position_matchups <- whole_day_stats %>%
    filter(grepl(position_choice, Position)) %>%
    mutate(on_base = 100 - SO - field_out) %>%
    mutate_if(is.numeric, round, 2)%>%
    arrange(rank(desc(on_base) + rank(desc(weighted))/2)) %>%
    gt() %>%
    gt_merge_stack(col1 = Batter, Pitcher) %>%
    gt_color_rows(SO:on_base, palette = "grDevices::blues9") %>%
    tab_header(title = glue('{position_choice} Projections for {date_of_game}')) %>%
    tab_options(table.width = 12)
  
  
  
  return(position_matchups)
}




#View(teams_lu_table)

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
      tabPanel('DraftKings Batters',
               DT::dataTableOutput("all_data"
               )), 
      tabPanel("Position Breakdown", selectInput("choose_position",
                                                 choices = positions_list,
                                                 label = 'Select a Position'),
               gt_output(outputId = 'position')),
      tabPanel("Pitchers", gt_output(outputId = 'pitchers'))
    )
    
    
  )
)




server <- function(input, output) {
  output$team1 <- render_gt(
    lineup_stats(input$team1)
  )
  output$all_data <- DT::renderDataTable(
    all_players(date_of_game), options = list(pageLength = 25)
  )
  output$position <- render_gt(
    positions(input$choose_position)
  )
  output$pitchers <- render_gt(
    pitcher_stats(date_of_game)
  )
  
}
shinyApp(ui = ui, server = server) 


