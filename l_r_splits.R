pacman::p_load(tidyverse,dplyr,formattable,gtExtras,rsconnect,baseballr,retrosheet,gt,stringr,janitor,DT,furrr,data.table,readxl,scales,shinyWidgets,lubridate,ggrepel,rvest,XML,httr,jsonlite,lpSolve,tidytable, glue,piggyback)

devtools::install_github("BillPetti/baseballr", ref = "development_branch")

tictoc::tic()



days <- mlb_schedule(season = 2022, level_ids = '1') %>%
  filter(series_description == 'Regular Season' & date <= Sys.Date()) %>%
  distinct(date)
colnames(days) <- 'day'

all_events <- function(time){
  print(time)
  statcast_search(
    start_date = time
    , end_date = time
    , player_type = 'batter'
  ) %>%
    filter(events != '') %>%
    select(pitch_name, release_speed, player_name, batter, pitcher, events, description, zone, stand, p_throws, hit_location, bb_type, on_3b, on_2b, on_1b, hit_distance_sc, launch_speed, launch_angle, estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, woba_denom, babip_value, iso_value)
}




events <- future_map_dfr(
  .x = days$day,
  .f = all_events,
  .id = NULL,
  .options = furrr_options(),
  .env_globals = parent.frame(),
  .progress = FALSE
)


tictoc::toc()

batters <- events %>%
  code_barrel() %>%
  group_by(batter, player_name, stand, p_throws) %>%
  summarize(events = n(), avg_launch_speed = mean(launch_speed, na.rm = T), avg_launch_angle = mean(launch_angle, na.rm = T), xba = mean(estimated_ba_using_speedangle, na.rm = T), woba = mean(woba_value, na.rm = T), xwoba = mean(estimated_woba_using_speedangle, na.rm = T), iso = mean(iso_value,na.rm = T), brl_pct = mean(barrel, na.rm = T))

pitchers <- events %>%
  code_barrel() %>%
  group_by(pitcher, p_throws, stand) %>%
  summarize(events = n(), avg_launch_speed = mean(launch_speed, na.rm = T), avg_launch_angle = mean(launch_angle, na.rm = T), xba = mean(estimated_ba_using_speedangle, na.rm = T), woba = mean(woba_value, na.rm = T), xwoba = mean(estimated_woba_using_speedangle, na.rm = T), iso = mean(iso_value,na.rm = T), brl_pct = mean(barrel, na.rm = T))

write.csv(batters, "/Users/mattoneil/Documents/MO/Lineups/batters_22_splits.csv", row.names=FALSE)
write.csv(pitchers, "/Users/mattoneil/Documents/MO/Lineups/pitchers_22_splits.csv", row.names=FALSE)



pb_upload(file = "batters_22_splits.csv", 
          repo = "matt12oneil/Lineups",
          name = "batters_22_splits.csv",
          tag = "updated_splits",
          .token = gh::gh_token())

pb_upload(file = "pitchers_22_splits.csv", 
          repo = "matt12oneil/Lineups",
          name = "pitchers_22_splits.csv",
          tag = "updated_splits",
          .token = gh::gh_token())




