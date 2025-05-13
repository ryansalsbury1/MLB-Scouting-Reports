#load packages
library("baseballr")
library("tidyverse")

#get all teams
mlb_teams <- mlb_teams(season = 2023, sport_ids = c(1))

#get standings by date
standings <- data.frame()
dates <- as.list(seq(as.Date("2023-03-30"), Sys.Date(), by="days"))
for (d in dates) {
  
  al_standings <- mlb_standings(season = 2023, date=d, league_id = 103) %>% select(team_records_team_name, league_id, division_id, team_records_wins, team_records_losses, team_records_winning_percentage, team_records_runs_scored, team_records_runs_allowed, team_records_run_differential, team_records_sport_rank, team_records_division_rank, team_records_league_rank, team_records_division_games_back, team_records_wild_card_games_back, last_updated)
  nl_standings <- mlb_standings(season = 2023, date=d,  league_id = 104) %>% select(team_records_team_name, league_id, division_id, team_records_wins, team_records_losses, team_records_winning_percentage, team_records_runs_scored, team_records_runs_allowed, team_records_run_differential, team_records_sport_rank, team_records_division_rank, team_records_league_rank, team_records_division_games_back, team_records_wild_card_games_back, last_updated)
  lg_standings <- rbind(al_standings, nl_standings)
  
  colnames(lg_standings) <- c("team", "league_id", "division_id", "wins", "losses", "win_pct", "rs", "ra", "run_diff", "mlb_rank", "div_rank", "lg_rank", "div_games_back", "wc_games_back", "last_updated")
  
  
  lg_standings$date <- d
  
  standings <- rbind(standings, lg_standings)
}

#get division names for each team
standings <- left_join(standings, mlb_teams[c("team_full_name", "division_name")], by = c("team" = "team_full_name"))


#replace blank games back with a 0
standings$div_games_back <- gsub("-", 0, standings$div_games_back)
standings$wc_games_back <- gsub("-", 0, standings$wc_games_back)

#pythagorean win pct
standings$pyth_win_pct <- round((standings$rs**2) / ((standings$rs**2) + (standings$ra**2)),3)

#expected wins & losses
standings <- standings %>% mutate(xwins = round(pyth_win_pct * (wins + losses),0), xlosses = round((1-pyth_win_pct) * (wins + losses),0))

#per game run and runs against
standings$rs_g <- standings$rs / (standings$wins+standings$losses)
standings$ra_g <- standings$ra / (standings$wins+standings$losses)

#combine win/loss columns
standings$win_loss <- paste(standings$wins,"-", standings$losses)
standings$xwin_loss <- paste(standings$xwins,"-", standings$xlosses)


#get logos and team colors
library(mlbplotR)
team_logos <- mlbplotR::load_mlb_teams()

standings <- left_join(standings, team_logos[c("team_name", "team_abbr", "team_logo_espn", "team_scoreboard_logo_espn", "team_cap_logo_on_light", "team_cap_logo_on_dark", "team_color", "team_color2")], by = c("team" = "team_name"))

write.csv(standings, "standings.csv", row.names = FALSE)


##player batting##
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,23,37,38,40,36,102,314,107,209,210,211,308,206,207,208,61,58,315,316,317&season=2023&month=0&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&startdate=2023-03-30&enddate=", Sys.Date(), "&sort=19,d&page=1_5000") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#get top 12 players per team
data <- data.table::setDT(data)[order(team,-pa), .SD[1:10], by=team] %>% filter(pa > 0)


#percent ranks
data <- data %>% mutate(avg_rk = round(percent_rank(avg) * 100,0))
data <- data %>% mutate(obp_rk = round(percent_rank(obp) * 100,0))
data <- data %>% mutate(slg_rk = round(percent_rank(slg) * 100,0))
data <- data %>% mutate(iso_rk = round(percent_rank(iso) * 100,0))
data <- data %>% mutate(bbk_rk = round(percent_rank(bb_k) * 100,0))
data <- data %>% mutate(o_swing_rk = round(percent_rank(-o_swing_pct) * 100,0))
data <- data %>% mutate(csw_rk = round(percent_rank(-csw_pct) * 100,0))
data <- data %>% mutate(contact_rk = round(percent_rank(contact_pct) * 100,0))
data <- data %>% mutate(soft_rk = round(percent_rank(-soft_pct) * 100,0))
data <- data %>% mutate(med_rk = round(percent_rank(med_pct) * 100,0))
data <- data %>% mutate(hard_rk = round(percent_rank(hard_pct) * 100,0))
data <- data %>% mutate(barrel_rk = round(percent_rank(barrel_pct) * 100,0))
data <- data %>% mutate(pull_rk = round(percent_rank(pull_pct) * 100,0))
data <- data %>% mutate(cent_rk = round(percent_rank(cent_pct) * 100,0))
data <- data %>% mutate(oppo_rk = round(percent_rank(oppo_pct) * 100,0))
data <- data %>% mutate(wrc_rk = round(percent_rank(wrc_plus) * 100,0))
data <- data %>% mutate(war_rk = round(percent_rank(war) * 100,0))
data <- data %>% mutate(xba_rk = round(percent_rank(xba) * 100,0))
data <- data %>% mutate(xslg_rk = round(percent_rank(xslg) * 100,0))
data <- data %>% mutate(xwoba_rk = round(percent_rank(xwoba) * 100,0))

player_batting <- data
write.csv(data, "player_batting.csv", row.names = FALSE)



####player batting vs LHP###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,23,36,37,38,40,54&season=2023&month=13&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&page=1_500") %>% 
  xml2::read_html()

#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#only include player in plyers_batting
data <- data[data$name %in% player_batting$name, ]

#get top 10 players per team
data <- data.table::setDT(data)[order(team,-pa), .SD[1:10], by=team] %>% filter(pa > 0)


#percentile ranks
data <- data %>% mutate(avg_rk = round(percent_rank(avg) * 100,0))
data <- data %>% mutate(obp_rk = round(percent_rank(obp) * 100,0))
data <- data %>% mutate(slg_rk = round(percent_rank(slg) * 100,0))
data <- data %>% mutate(iso_rk = round(percent_rank(iso) * 100,0))
data <- data %>% mutate(bbk_rk = round(percent_rank(bb_k) * 100,0))
data <- data %>% mutate(wrc_rk = round(percent_rank(wrc_plus) * 100,0))

#add split column
data <- add_column(data, split = "vs LHP", .after = "name")

player_batting_lhp <- data

####player batting vs RHP###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,23,36,37,38,40,54&season=2023&month=14&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&startdate=&enddate=&page=1_500") %>% 
  xml2::read_html()

#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}

#remove # column (1st column)
data <- data[-c(1)]


#only include player in plyers_batting
data <- data[data$name %in% player_batting$name, ]

#get top 12 players per team
data <- data.table::setDT(data)[order(team,-pa), .SD[1:10], by=team] %>% filter(pa > 0)


#percentile ranks
data <- data %>% mutate(avg_rk = round(percent_rank(avg) * 100,0))
data <- data %>% mutate(obp_rk = round(percent_rank(obp) * 100,0))
data <- data %>% mutate(slg_rk = round(percent_rank(slg) * 100,0))
data <- data %>% mutate(iso_rk = round(percent_rank(iso) * 100,0))
data <- data %>% mutate(bbk_rk = round(percent_rank(bb_k) * 100,0))
data <- data %>% mutate(wrc_rk = round(percent_rank(wrc_plus) * 100,0))

#add split column
data <- add_column(data, split = "vs RHP", .after = "name")

player_batting_rhp <- data

#combine lhp and rhp data
player_batting_splits <- rbind(player_batting_lhp, player_batting_rhp)

#only include players who have faced both lhp and rhp
player_batting_splits <- player_batting_splits %>%
  group_by(name) %>%
  dplyr::filter(n() == 2) %>%
  ungroup()

write.csv(player_batting_splits, "player_batting_splits.csv", row.names = FALSE)

####team batting###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,23,37,38,40,34,35,36,102,314,107,209,210,211,308,206,207,208,61&season=2023&month=0&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31&page=1_5000") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]


#league ranks
data <- data %>% mutate(avg_rk = dense_rank(desc(avg)))
data <- data %>% mutate(obp_rk = dense_rank(desc(obp)))
data <- data %>% mutate(slg_rk = dense_rank(desc(slg)))
data <- data %>% mutate(iso_rk = dense_rank(desc(iso)))
data <- data %>% mutate(bb_rk = dense_rank(desc(bb_pct)))
data <- data %>% mutate(k_rk = dense_rank(desc(-k_pct)))
data <- data %>% mutate(bbk_rk = dense_rank(desc(bb_k)))
data <- data %>% mutate(o_swing_rk = dense_rank(desc(-o_swing_pct)))
data <- data %>% mutate(csw_rk = dense_rank(desc(-csw_pct)))
data <- data %>% mutate(contact_rk = dense_rank(desc(contact_pct)))
data <- data %>% mutate(soft_rk = dense_rank(desc(-soft_pct)))
data <- data %>% mutate(med_rk = dense_rank(desc(med_pct)))
data <- data %>% mutate(hard_rk = dense_rank(desc(hard_pct)))
data <- data %>% mutate(barrel_rk = dense_rank(desc(barrel_pct)))
data <- data %>% mutate(pull_rk = dense_rank(desc(pull_pct)))
data <- data %>% mutate(cent_rk = dense_rank(desc(cent_pct)))
data <- data %>% mutate(oppo_rk = dense_rank(desc(oppo_pct)))
data <- data %>% mutate(wrc_rk = dense_rank(desc(wrc_plus)))

#create variable to use in radar join below
team_batting <- data

write.csv(team_batting, "team_batting.csv", row.names = FALSE)

####team batting vs LHP###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,23,36,37,38,40,54&season=2023&month=13&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0") %>% 
  xml2::read_html()

#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#league ranks
data <- data %>% mutate(avg_rk = dense_rank(desc(avg)))
data <- data %>% mutate(obp_rk = dense_rank(desc(obp)))
data <- data %>% mutate(slg_rk = dense_rank(desc(slg)))
data <- data %>% mutate(iso_rk = dense_rank(desc(iso)))
data <- data %>% mutate(bbk_rk = dense_rank(desc(bb_k)))
data <- data %>% mutate(wrc_rk = dense_rank(desc(wrc_plus)))

#add split column
data <- add_column(data, split = "vs LHP", .after = "team")

team_batting_lhp <- data

####team batting vs RHP###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,23,36,37,38,40,54&season=2023&month=14&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0") %>% 
  xml2::read_html()

#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}

#remove # column (1st column)
data <- data[-c(1)]

#league ranks
data <- data %>% mutate(avg_rk = dense_rank(desc(avg)))
data <- data %>% mutate(obp_rk = dense_rank(desc(obp)))
data <- data %>% mutate(slg_rk = dense_rank(desc(slg)))
data <- data %>% mutate(iso_rk = dense_rank(desc(iso)))
data <- data %>% mutate(bbk_rk = dense_rank(desc(bb_k)))
data <- data %>% mutate(wrc_rk = dense_rank(desc(wrc_plus)))

#add split column
data <- add_column(data, split = "vs RHP", .after = "team")

team_batting_rhp <- data

#combine lhp and rhp data
team_batting_splits <- rbind(team_batting_lhp, team_batting_rhp)

write.csv(team_batting_splits, "team_batting_splits.csv", row.names = FALSE)

####player starting pitching###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=0&type=c,13,14,8,41,42,43,36,37,38,40,48,49,51,111,112,105,331,221,222,223,325,117,118,119,122,59&season=2023&month=0&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31&page=1_5000") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#get top 7 players per team based on total batters faced
data <- data.table::setDT(data)[order(team,-tbf), .SD[1:7], by=team] %>% filter(tbf > 0)

#remove tbf
data <- data %>% select(-c("tbf"))

#league ranks
data <- data %>% mutate(avg_rk = round(percent_rank(-as.numeric(avg)) * 100,0))
data <- data %>% mutate(whip_rk = round(percent_rank(-whip) * 100,0))
data <- data %>% mutate(babip_rk = round(percent_rank(-babip) * 100,0))
data <- data %>% mutate(k9_rk = round(percent_rank(k_9) * 100,0))
data <- data %>% mutate(bb9_rk = round(percent_rank(-bb_9) * 100,0))
data <- data %>% mutate(kbb_rk = round(percent_rank(k_bb) * 100,0))
data <- data %>% mutate(gb_rk = round(percent_rank(gb_pct) * 100,0))
data <- data %>% mutate(fb_rk = round(percent_rank(-fb_pct) * 100,0))
data <- data %>% mutate(hrfb_rk = round(percent_rank(-hr_fb) * 100,0))
data <- data %>% mutate(hr9_rk = round(percent_rank(-hr_9) * 100,0))
data <- data %>% mutate(zone_rk = round(percent_rank(zone_pct) * 100,0))
data <- data %>% mutate(fstrike_rk = round(percent_rank(f_strike_pct) * 100,0))
data <- data %>% mutate(o_swing_rk = round(percent_rank(o_swing_pct) * 100,0))
data <- data %>% mutate(csw_rk = round(percent_rank(csw_pct) * 100,0))
data <- data %>% mutate(soft_rk = round(percent_rank(soft_pct) * 100,0))
data <- data %>% mutate(med_rk = round(percent_rank(-med_pct) * 100,0))
data <- data %>% mutate(hard_rk = round(percent_rank(-hard_pct) * 100,0))
data <- data %>% mutate(barrel_rk = round(percent_rank(-barrel_pct) * 100,0))
data <- data %>% mutate(era_rk = round(percent_rank(-era_) * 100,0))
data <- data %>% mutate(fip_rk = round(percent_rank(-fip_) * 100,0))
data <- data %>% mutate(xfip_rk = round(percent_rank(-xfip_) * 100,0))
data <- data %>% mutate(siera_rk = round(percent_rank(-siera) * 100,0))
data <- data %>% mutate(war_rk = round(percent_rank(war) * 100,0))

player_starting_pitching <- data
write.csv(data, "player_starting_pitching.csv", row.names = FALSE)


####player relief pitching###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c,13,14,8,41,42,43,36,37,38,40,48,49,51,111,112,105,331,221,222,223,325,117,118,119,122,59&season=2023&month=0&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31&page=1_5000") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#get top 7 players per team based on total batters faced
data <- data.table::setDT(data)[order(team,-tbf), .SD[1:7], by=team] %>% filter(tbf > 0)

#remove tbf
data <- data %>% select(-c("tbf"))

#league ranks
data <- data %>% mutate(avg_rk = round(percent_rank(-as.numeric(avg)) * 100,0))
data <- data %>% mutate(whip_rk = round(percent_rank(-whip) * 100,0))
data <- data %>% mutate(babip_rk = round(percent_rank(-babip) * 100,0))
data <- data %>% mutate(k9_rk = round(percent_rank(k_9) * 100,0))
data <- data %>% mutate(bb9_rk = round(percent_rank(-bb_9) * 100,0))
data <- data %>% mutate(kbb_rk = round(percent_rank(k_bb) * 100,0))
data <- data %>% mutate(gb_rk = round(percent_rank(gb_pct) * 100,0))
data <- data %>% mutate(fb_rk = round(percent_rank(-fb_pct) * 100,0))
data <- data %>% mutate(hrfb_rk = round(percent_rank(-hr_fb) * 100,0))
data <- data %>% mutate(hr9_rk = round(percent_rank(-hr_9) * 100,0))
data <- data %>% mutate(zone_rk = round(percent_rank(zone_pct) * 100,0))
data <- data %>% mutate(fstrike_rk = round(percent_rank(f_strike_pct) * 100,0))
data <- data %>% mutate(o_swing_rk = round(percent_rank(o_swing_pct) * 100,0))
data <- data %>% mutate(csw_rk = round(percent_rank(csw_pct) * 100,0))
data <- data %>% mutate(soft_rk = round(percent_rank(soft_pct) * 100,0))
data <- data %>% mutate(med_rk = round(percent_rank(-med_pct) * 100,0))
data <- data %>% mutate(hard_rk = round(percent_rank(-hard_pct) * 100,0))
data <- data %>% mutate(barrel_rk = round(percent_rank(-barrel_pct) * 100,0))
data <- data %>% mutate(era_rk = round(percent_rank(-era_) * 100,0))
data <- data %>% mutate(fip_rk = round(percent_rank(-fip_) * 100,0))
data <- data %>% mutate(xfip_rk = round(percent_rank(-xfip_) * 100,0))
data <- data %>% mutate(siera_rk = round(percent_rank(-siera) * 100,0))
data <- data %>% mutate(war_rk = round(percent_rank(war) * 100,0))

player_relief_pitching <- data
write.csv(data, "player_relief_pitching.csv", row.names = FALSE)


####team pitching###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,41,42,43,36,37,38,40,48,49,51,111,112,105,331,221,222,223,325,117,118,119,122&season=2023&month=0&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]


#league ranks
data <- data %>% mutate(avg_rk = dense_rank(desc(-as.numeric(avg))))
data <- data %>% mutate(whip_rk = dense_rank(desc(-whip)))
data <- data %>% mutate(babip_rk = dense_rank(desc(-babip)))
data <- data %>% mutate(k9_rk = dense_rank(desc(k_9)))
data <- data %>% mutate(bb9_rk = dense_rank(desc(-bb_9)))
data <- data %>% mutate(kbb_rk = dense_rank(desc(k_bb)))
data <- data %>% mutate(gb_rk = dense_rank(desc(gb_pct)))
data <- data %>% mutate(fb_rk = dense_rank(desc(-fb_pct)))
data <- data %>% mutate(hrfb_rk = dense_rank(desc(-hr_fb)))
data <- data %>% mutate(hr9_rk = dense_rank(desc(-hr_9)))
data <- data %>% mutate(zone_rk = dense_rank(desc(zone_pct)))
data <- data %>% mutate(fstrike_rk = dense_rank(desc(f_strike_pct)))
data <- data %>% mutate(o_swing_rk = dense_rank(desc(o_swing_pct)))
data <- data %>% mutate(csw_rk = dense_rank(desc(csw_pct)))
data <- data %>% mutate(soft_rk = dense_rank(desc(soft_pct)))
data <- data %>% mutate(med_rk = dense_rank(desc(-med_pct)))
data <- data %>% mutate(hard_rk = dense_rank(desc(-hard_pct)))
data <- data %>% mutate(barrel_rk = dense_rank(desc(-barrel_pct)))
data <- data %>% mutate(era_rk = dense_rank(desc(-era_)))
data <- data %>% mutate(fip_rk = dense_rank(desc(-fip_)))
data <- data %>% mutate(xfip_rk = dense_rank(desc(-xfip_)))
data <- data %>% mutate(siera_rk = dense_rank(desc(-siera)))


write.csv(data, "team_pitching.csv", row.names = FALSE)


####team pitching splits###

#vs lhh
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,30,48,49,27,47,34,43&season=2023&month=13&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=&enddate=") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]


#league ranks
data <- data %>% mutate(avg_rk = dense_rank(desc(-as.numeric(avg))))
data <- data %>% mutate(obp_rk = dense_rank(desc(-as.numeric(obp))))
data <- data %>% mutate(slg_rk = dense_rank(desc(-as.numeric(slg))))
data <- data %>% mutate(kbb_rk = dense_rank(desc(k_bb)))
data <- data %>% mutate(woba_rk = dense_rank(desc(-woba)))
data <- data %>% mutate(fip_rk = dense_rank(desc(-fip)))
data <- data %>% mutate(xfip_rk = dense_rank(desc(-xfip)))

#add split column
data <- add_column(data, split = "vs LHH", .after = "team")

team_pitching_lhh <- data

#vs rhh
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,30,48,49,27,47,34,43&season=2023&month=14&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#league ranks
data <- data %>% mutate(avg_rk = dense_rank(desc(-as.numeric(avg))))
data <- data %>% mutate(obp_rk = dense_rank(desc(-as.numeric(obp))))
data <- data %>% mutate(slg_rk = dense_rank(desc(-as.numeric(slg))))
data <- data %>% mutate(kbb_rk = dense_rank(desc(k_bb)))
data <- data %>% mutate(woba_rk = dense_rank(desc(-woba)))
data <- data %>% mutate(fip_rk = dense_rank(desc(-fip)))
data <- data %>% mutate(xfip_rk = dense_rank(desc(-xfip)))

#add split column
data <- add_column(data, split = "vs RHH", .after = "team")

team_pitching_rhh <- data

#combine lhp and rhp data
team_pitching_splits <- rbind(team_pitching_lhh, team_pitching_rhh)

write.csv(team_pitching_splits, "team_pitching_splits.csv", row.names = FALSE)


####player pitching splits###

#vs lhh
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,6,4,30,48,49,27,47,34,43&season=2023&month=13&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&startdate=&enddate=&page=1_500") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#only include player in plyers_pitching
data <- data[(data$name %in% player_starting_pitching$name) | data$name %in% player_relief_pitching$name, ]

#top 10 per team
data <- data.table::setDT(data)[order(team,-tbf), .SD[1:10], by=team] %>% filter(tbf > 0)

#remove tbf
data <- data %>% select(-c("tbf"))


#percentile ranks
data <- data %>% mutate(avg_rk = round(percent_rank(-as.numeric(avg)) * 100,0))
data <- data %>% mutate(obp_rk = round(percent_rank(-as.numeric(obp)) * 100,0))
data <- data %>% mutate(slg_rk = round(percent_rank(-as.numeric(slg)) * 100,0))
data <- data %>% mutate(kbb_rk = round(percent_rank(as.numeric(k_bb)) * 100,0))
data <- data %>% mutate(woba_rk = round(percent_rank(-as.numeric(woba)) * 100,0))
data <- data %>% mutate(fip_rk = round(percent_rank(-as.numeric(fip)) * 100,0))
data <- data %>% mutate(xfip_rk = round(percent_rank(-as.numeric(xfip)) * 100,0))

#add split column
data <- add_column(data, split = "vs LHH", .after = "name")

player_pitching_lhh <- data

#vs rhh
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=c,6,4,30,48,49,27,47,34,43&season=2023&month=14&season1=2023&ind=0&team=0&rost=1&age=0&filter=&players=0&page=1_500") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
data <- data[-c(1)]

#only include player in plyers_pitching
data <- data[(data$name %in% player_starting_pitching$name) | data$name %in% player_relief_pitching$name, ]

#top 10 per team
data <- data.table::setDT(data)[order(team,-tbf), .SD[1:10], by=team] %>% filter(tbf > 0)

#remove tbf
data <- data %>% select(-c("tbf"))

#percentile ranks
data <- data %>% mutate(avg_rk = round(percent_rank(-as.numeric(avg)) * 100,0))
data <- data %>% mutate(obp_rk = round(percent_rank(-as.numeric(obp)) * 100,0))
data <- data %>% mutate(slg_rk = round(percent_rank(-as.numeric(slg)) * 100,0))
data <- data %>% mutate(kbb_rk = round(percent_rank(as.numeric(k_bb)) * 100,0))
data <- data %>% mutate(woba_rk = round(percent_rank(-as.numeric(woba)) * 100,0))
data <- data %>% mutate(fip_rk = round(percent_rank(-as.numeric(fip)) * 100,0))
data <- data %>% mutate(xfip_rk = round(percent_rank(-as.numeric(xfip)) * 100,0))

#add split column
data <- add_column(data, split = "vs RHH", .after = "name")

player_pitching_rhh <- data

#combine lhp and rhp data
player_pitching_splits <- rbind(player_pitching_lhh, player_pitching_rhh)

#only include players who have faced both lhh and rhh
player_pitching_splits <- player_pitching_splits %>%
  group_by(name) %>%
  dplyr::filter(n() == 2) %>%
  ungroup()

write.csv(player_pitching_splits, "player_pitching_splits.csv", row.names = FALSE)









####for radar plot
#team batting last 10 games
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,6,61&season=2023&month=1000&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=", Sys.Date() - 11, "&enddate=2023-12-31&page=1_5000") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_batting_last10 <- data[-c(1)]

####team starting pitching###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=0&type=c,41,122&season=2023&month=0&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_starting_pitching <- data[-c(1)]

####team starting pitching last 10###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=0&type=c,41,122&season=2023&month=1000&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=", Sys.Date() - 11, "&enddate=2023-12-31") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_starting_pitching_last10 <- data[-c(1)]


####team relief pitching###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c,41,122&season=2023&month=0&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_relief_pitching <- data[-c(1)]


####team relief pitching last 10###
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=0&type=c,41,122&season=2023&month=1000&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=", Sys.Date() - 11, "&enddate=2023-12-31") %>% 
  xml2::read_html()


#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_relief_pitching_last10 <- data[-c(1)]


#team fielding
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,4,199&season=2023&month=0&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=2023-01-01&enddate=2023-12-31") %>% 
  xml2::read_html()

#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_fielding <- data[-c(1)]

#team fielding last 10
payload <- paste0("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=c,4,199&season=2023&month=1000&season1=2023&ind=0&team=0,ts&rost=0&age=0&filter=&players=0&startdate=", Sys.Date() - 11, "&enddate=2023-12-31") %>% 
  xml2::read_html()

#get raw data
data <- (payload %>% rvest::html_elements("table"))[[7]] %>% rvest::html_table(fill=TRUE)

#clean up data
#remove 1st and 3rd rows
data <- data[-c(1,3),]
#make 1st row column headers
names(data) <- data[1,]
#remove first row (headers)
data <- data[-1,]
#update column names
c <- as.matrix(names(data))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("+", "_plus", c, fixed = TRUE)
names(data) <- tolower(c)
#replace all % with ""
data <- as.data.frame(sapply(data, function(x) (gsub("%", "", x))))
#replace blank with na
is.na(data) <- data==""
#convert numerical columns to numbers
for(i in c(4:ncol(data))) {
  suppressWarnings(
    data[,i] <- as.numeric(as.character(data[,i]))
  )
}


#remove # column (1st column)
team_fielding_last10 <- data[-c(1)]

#combine data

#select columns
team_batting <- team_batting %>% select(team, wrc_plus)
team_batting_last10 <- team_batting_last10 %>% select(team, wrc_plus)
team_starting_pitching <- team_starting_pitching %>% select(team, siera)
team_starting_pitching_last10 <- team_starting_pitching_last10 %>% select(team, siera)
team_relief_pitching <- team_relief_pitching %>% select(team, siera)
team_relief_pitching_last10 <- team_relief_pitching_last10 %>% select(team, siera)
team_fielding <- team_fielding %>% select(team, def)
team_fielding_last10 <- team_fielding_last10 %>% select(team, def)

#rename columns
colnames(team_batting) <- c("team", "Hitting")
colnames(team_batting_last10) <- c("team", "Hitting")
colnames(team_starting_pitching) <- c("team", "Starting Pitching")
colnames(team_starting_pitching_last10) <- c("team", "Starting Pitching")
colnames(team_relief_pitching) <- c("team", "Bullpen")
colnames(team_relief_pitching_last10) <- c("team", "Bullpen")
colnames(team_fielding) <- c("team", "Defense")
colnames(team_fielding_last10) <- c("team", "Defense")



#add stat type
team_batting$stat <- "Season"
team_batting_last10$stat <- "Last 10"
team_starting_pitching$stat <- "Season"
team_starting_pitching_last10$stat <- "Last 10"
team_relief_pitching$stat <- "Season"
team_relief_pitching_last10$stat <- "Last 10"
team_fielding$stat <- "Season"
team_fielding_last10$stat <- "Last 10"

#rescale data to be between 0 and 100
library(scales)
team_batting$Hitting <- rescale(team_batting$Hitting, to = c(0,100), from = c(min(team_batting$Hitting),max(team_batting$Hitting)))
team_batting_last10$Hitting <- rescale(team_batting_last10$Hitting, to = c(0,100), from = c(min(team_batting_last10$Hitting),max(team_batting_last10$Hitting)))
team_starting_pitching$`Starting Pitching` <- rescale(team_starting_pitching$`Starting Pitching`, to = c(100,0), from = c(min(team_starting_pitching$`Starting Pitching`),max(team_starting_pitching$`Starting Pitching`)))
team_starting_pitching_last10$`Starting Pitching` <- rescale(team_starting_pitching_last10$`Starting Pitching`, to = c(100,0), from = c(min(team_starting_pitching_last10$`Starting Pitching`),max(team_starting_pitching_last10$`Starting Pitching`)))
team_relief_pitching$Bullpen <- rescale(team_relief_pitching$Bullpen, to = c(100,0), from = c(min(team_relief_pitching$Bullpen),max(team_relief_pitching$Bullpen)))
team_relief_pitching_last10$Bullpen <- rescale(team_relief_pitching_last10$Bullpen, to = c(100,0), from = c(min(team_relief_pitching_last10$Bullpen),max(team_relief_pitching_last10$Bullpen)))
team_fielding$Defense <- rescale(team_fielding$Defense, to = c(0,100), from = c(min(team_fielding$Defense),max(team_fielding$Defense)))
team_fielding_last10$Defense <- rescale(team_fielding_last10$Defense, to = c(0,100), from = c(min(team_fielding_last10$Defense),max(team_fielding_last10$Defense)))



#append each type of table
batting <- rbind(team_batting, team_batting_last10)
starting_pitching <- rbind(team_starting_pitching, team_starting_pitching_last10)
relief_pitching <- rbind(team_relief_pitching, team_relief_pitching_last10)
fielding <- rbind(team_fielding, team_fielding_last10)

radar_plot_data <- left_join(batting, starting_pitching, by = c("team", "stat")) %>%  left_join(., relief_pitching, by = c("team", "stat")) %>% left_join(., fielding, by = c("team", "stat")) %>% select (team, stat, Hitting, "Starting Pitching", Bullpen, Defense)

write.csv(radar_plot_data, "radar_plot_data.csv", row.names = FALSE)




