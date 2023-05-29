library(baseballr)
library(tidyverse)
library(tidytable)
library(magrittr)
library(furrr)



days <- mlb_schedule(season = 2022, level_ids = '1') %>%
  filter(series_description == 'Regular Season' & date <= Sys.Date()) %>%
  distinct(date)
  #filter(date <= '2022-05-31')
#filter(date <= '2022-04-15')
colnames(days) <- 'day'



all_events <- function(time){
  print(time)
  statcast_search(
    start_date = time
    , end_date = time
    , player_type = 'batter'
  ) %>%
    filter(events != '') %>%
    select(pitch_name, release_speed, player_name, batter, pitcher, events, description, zone, stand, p_throws, hit_location, bb_type, on_3b, on_2b, on_1b, hit_distance_sc, launch_speed, launch_angle, estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, woba_denom, babip_value, iso_value) %>%
    mutate(date = time)
}


events <- future_map_dfr(
  .x = days$day,
  .f = all_events,
  .id = NULL,
  .options = furrr_options(),
  .env_globals = parent.frame(),
  .progress = FALSE
)


b_splits_by_day <- function(time){
  batters <- events %>%
    filter(date <= time) %>%
    baseballr::code_barrel() %>%
    mutate(as_of_date = time) %>%
    tidytable::group_by(batter, player_name, stand, p_throws, as_of_date) %>%
    tidytable::summarize(events = n(), avg_launch_speed = mean(launch_speed, na.rm = T), avg_launch_angle = mean(launch_angle, na.rm = T), xba = mean(estimated_ba_using_speedangle, na.rm = T), woba = mean(woba_value, na.rm = T), xwoba = mean(estimated_woba_using_speedangle, na.rm = T), iso = mean(iso_value,na.rm = T), brl_pct = mean(barrel, na.rm = T)) %>%
    drop_na.()
  
  return(batters)
}

batter_splits_by_day <- future_map_dfr(
  .x = days$day,
  .f = b_splits_by_day,
  .id = NULL,
  .options = furrr_options(),
  .env_globals = parent.frame(),
  .progress = FALSE
)

p_splits_by_day <- function(time){
  pitchers <- events %>%
    filter(date <= time) %>%
    baseballr::code_barrel() %>%
    mutate(as_of_date = time) %>%
    tidytable::group_by(pitcher, p_throws, stand, as_of_date) %>%
    tidytable::summarize(events = n(), avg_launch_speed = mean(launch_speed, na.rm = T), avg_launch_angle = mean(launch_angle, na.rm = T), xba = mean(estimated_ba_using_speedangle, na.rm = T), woba = mean(woba_value, na.rm = T), xwoba = mean(estimated_woba_using_speedangle, na.rm = T), iso = mean(iso_value,na.rm = T), brl_pct = mean(barrel, na.rm = T)) %>%
    drop_na.()
}

pitcher_splits_by_day <- future_map_dfr(
  .x = days$day,
  .f = p_splits_by_day,
  .id = NULL,
  .options = furrr_options(),
  .env_globals = parent.frame(),
  .progress = FALSE
)

b_totals_by_day <- function(time){
  
}

batter_totals_by_day

p_totals_by_day <- function(time){
  
}

batter_totals_by_day

events %>%
  dplyr::group_by(batter, player_name) %>%
  statline_from_statcast(base = 'pa') %>%
  ungroup()

#readr::write_csv(batters,"splits_data/batter_splits.csv")
#readr::write_csv(pitchers,"splits_data/pitcher_splits.csv")