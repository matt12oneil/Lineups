leaders_fg <- fg_batter_leaders(x = 2023, y = 2023) %>%
  janitor::clean_names() %>%
  mutate(fpts = 3*x1b+6*x2b+9*x3b+12*hr+3*(bb+ibb)+3.2*r+3.5*rbi+6*sb) %>%
  mutate(fppg = fpts/g) %>%
  mutate(pitches_pa = pitches/pa) %>%
  select(-c(ab,h,x1b,x2b,x3b,hr,r,rbi,bb,ibb,so,hbp,sf,sh,gdp,sb,cs,avg,gb,fb,ld,iffb,pitches,balls,strikes,ifh,bu,buh,buh_pct,fld,p_li,ph_li,ph,clutch,kn_pct,k_nv,xx_pct,po_pct,w_kn,w_fb,w_sl,w_ct,w_cb,w_ch,w_sf,w_kn_c,f_bv,s_lv,c_tv,c_bv,c_hv,s_fv,bs_r, v_fa_sc,v_ft_sc,v_fc_sc,v_fs_sc,v_fo_sc,v_si_sc,v_sl_sc,v_cu_sc,v_kc_sc,v_ep_sc,v_ch_sc,v_sc_sc,v_kn_sc,fa_x_sc,ft_x_sc,fc_x_sc,fs_x_sc,fo_x_sc,si_x_sc,sl_x_sc,cu_x_sc,kc_x_sc,ep_x_sc,ch_x_sc,sc_x_sc,kn_x_sc,fa_z_sc,ft_z_sc,fc_z_sc,fs_z_sc,fo_z_sc,si_z_sc,sl_z_sc,cu_z_sc,kc_z_sc,ep_z_sc,ch_z_sc,sc_z_sc,kn_z_sc,w_fa_sc,w_ft_sc,w_fc_sc,w_fo_sc,w_si_sc,w_sl_sc,w_cu_sc,w_kc_sc,w_ep_sc,w_ch_sc,w_sc_sc,w_kn_sc,w_ft_c_sc,pace,def,w_sb,ubr,off,lg,w_gdp,pace_pi,fs_x_pi,kn_x_pi,sb_x_pi,si_x_pi,sl_x_pi,xx_x_pi,ch_z_pi,cs_z_pi,cu_z_pi,fa_z_pi,fc_z_pi,fs_z_pi,kn_z_pi,sb_z_pi,si_z_pi,xx_z_pi,w_ch_pi,w_cs_pi,w_cu_pi,w_fa_pi,w_fc_pi,w_fs_pi,w_kn_pi,w_sb_pi,w_si_pi,w_sl_pi,w_xx_pi,w_ch_c_pi,w_cs_c_pi,w_cu_c_pi,w_cu_c_pi,w_fa_c_pi,w_fc_c_pi,w_fs_c_pi,w_kn_c_pi,w_sb_c_pi,w_si_c_pi,w_sl_c_pi,w_xx_c_pi,pace_pi, number, season,obp,slg,ops,babip)) %>%
  mutate(playerid = as.character(playerid))

ev_batters <- statcast_leaderboards(
  leaderboard = "exit_velocity_barrels",
  year = 2023,
  abs = 5,
  min_pa = 'q',
  min_pitches = 5,
  min_field = "q",
  min_run = 0,
  player_type = 'batter',
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
) %>%
  janitor::clean_names() %>%
  select(player_id,avg_hit_angle,anglesweetspotpercent,max_hit_speed,avg_hit_speed,ev95plus,ev95percent,brl_percent,brl_pa) %>%
  mutate(player_id = as.character(player_id))

xstats_batters <- statcast_leaderboards(
  leaderboard = "expected_statistics",
  year = 2023,
  abs = 5,
  min_pa = 'q',
  min_pitches = 5,
  min_field = "q",
  min_run = 0,
  player_type = 'batter',
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
) %>%
  janitor::clean_names() %>%
  select(player_id,est_ba,est_slg) %>%
  mutate(player_id = as.character(player_id))


players <- chadwick_player_lu() %>%
  filter(!is.na(key_mlbam) & !is.na(key_fangraphs) & mlb_played_last == year(Sys.Date())) %>%
  select(key_mlbam, key_fangraphs) %>%
  mutate(key_mlbam = as.character(key_mlbam), key_fangraphs = as.character(key_fangraphs))


leaders <- leaders_fg %>%
  inner_join(players, by = c('playerid' = 'key_fangraphs')) %>%
  inner_join(xstats_batters, by = c('key_mlbam' = 'player_id')) %>%
  inner_join(ev_batters, by = c('key_mlbam' = 'player_id')) %>%
  arrange(desc(fppg))
