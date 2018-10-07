rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# Combine the data
#---------------------------------------------------------------------------------
load(paste0(dsource, "espn_stats_games.Rda"))
load(paste0(dsource, "espn_info_players.Rda"))

# Start with master list of players
key <- c("PLAYER", "ESPN.ID", "TEAM", "YEAR", "WEEK", "OPPONENT")
master <- rbind(
  pas[key],
  rsh[key],
  rec[key],
  krt[key],
  prt[key],
  kck[key],
  fum[key],
  int[key],
  dfs[key]
  ) %>%
  unique
print(nrow(master))

# Add player information
master <- master %>%
  merge(players,by = c("PLAYER","ESPN.ID"),all.x=T)

# Add passing
master <- master %>%
  merge(pas[c(key, "C/ATT", "YDS", "TD", "INT")],by=key,all.x=T) %>%
  mutate(
    YDS = as.numeric(YDS),
    TD = as.numeric(TD),
    INT = as.numeric(INT)) %>%
  within({
    `C/ATT`[is.na(`C/ATT`)] <- "0/0"
  }) %>%
  rename(YDS.PAS = YDS, TD.PAS = TD, INT.PAS = INT)

# Add rushing
master <- master %>%
  merge(rsh[c(key, "CAR", "YDS", "TD", "LONG")], by = key, all.x = T) %>%
  mutate(
    CAR = as.numeric(CAR),
    YDS = as.numeric(YDS),
    TD = as.numeric(TD),
    LONG = as.numeric(LONG)) %>%
  rename(YDS.RSH = YDS, TD.RSH = TD, LONG.RSH = LONG)

# Add receiving
master <- master %>%
  merge(rec[c(key, "REC", "YDS", "TD", "LONG")],by=key,all.x=T) %>%
  mutate(
    REC = as.numeric(REC),
    YDS = as.numeric(YDS),
    TD = as.numeric(TD),
    LONG = as.numeric(LONG)) %>%
  rename(YDS.REC = YDS, TD.REC = TD, LONG.REC = LONG)

# Add kick/punt returns
master <- master %>%
  merge(krt[c(key, "NO", "YDS", "TD", "LONG")],by=key,all.x=T) %>%
  mutate(
    NO = as.numeric(NO),
    YDS = as.numeric(YDS),
    TD = as.numeric(TD),
    LONG = as.numeric(LONG)) %>%
  rename(NO.KRT = NO, YDS.KRT = YDS, TD.KRT = TD, LONG.KRT = LONG) %>%
  merge(prt[c(key, "NO", "YDS", "TD", "LONG")],by=key,all.x=T) %>%
  mutate(
    NO = as.numeric(NO),
    YDS = as.numeric(YDS),
    TD = as.numeric(TD),
    LONG = as.numeric(LONG)) %>%
  rename(NO.PRT = NO, YDS.PRT = YDS, TD.PRT = TD, LONG.PRT = LONG)

# Add kicking
master <- master %>%
  merge(kck[c(key, "FG", "XP", "LONG")],by=key,all.x=T) %>%
  mutate(LONG = as.numeric(LONG)) %>%
  within({
    FG[is.na(FG)] <- "0/0"
    XP[is.na(XP)] <- "0/0"
  }) %>%
  rename(LONG.KCK = LONG)

# Add turnovers
master <- master %>%
  merge(fum[c(key, "FUM", "LOST", "RECV")],by=key,all.x=T) %>%
  mutate(
    FUM = as.numeric(FUM),
    LOST = as.numeric(LOST),
    RECV = as.numeric(RECV)
    ) %>%
  rename(FUM.TOT = FUM, FUM.LOS = LOST) %>%
  merge(int[c(key, "INT", "YDS", "TD")],by=key,all.x=T) %>%
  mutate(
    INT = as.numeric(INT),
    YDS = as.numeric(YDS),
    TD = as.numeric(TD)
  ) %>%
  rename(INT.DFS = INT, YDS.INT = YDS, TD.INT = TD)

# Add defense
master <- master %>%
  merge(dfs[c(key, "TOT", "SACKS", "TD")],by=key,all.x=T) %>%
  mutate(
    TOT = as.numeric(TOT),
    SACKS = as.numeric(SACKS),
    TD = as.numeric(TD)) %>%
  within({
    TD[!is.na(TD.INT) & TD.INT!=0 & is.na(TD)] <-
      TD.INT[!is.na(TD.INT) & TD.INT!=0 & is.na(TD)]
  }) %>%
  rename(TACK = TOT, TD.DFS = TD)

#---------------------------------------------------------------------------------
# Calculate PPR
#---------------------------------------------------------------------------------
recodes <- c(key,"C/ATT","FG","XP",paste("DRAFT",c("YR","RD","PK"),sep="."))
recodes <- names(master)[!names(master) %in% recodes]
for (i in recodes) {
  master[i][is.na(master[i])] <- 0
}

# Kicking functions
kick.made<-function(x) {
  sapply(x,function(y) as.numeric(strsplit(y,"/")[[1]][1]))
  }
kick.miss<-function(x) {
  sapply(x,function(y) {
    as.numeric(strsplit(y,"/")[[1]][2]) - as.numeric(strsplit(y,"/")[[1]][1])
  })
  }

# Add PPR value
master <- within(master,{
  PTS <- 
    ((1/25)*plyr::round_any(YDS.PAS,25,f=floor) + 4*TD.PAS - 2*INT.PAS) +
    ((1/10)*plyr::round_any(YDS.REC,10,f=floor) + 6*TD.REC + REC) + 
    ((1/10)*plyr::round_any(YDS.RSH,10,f=floor) + 6*TD.RSH - 2*FUM.LOS) +
    (3*kick.made(FG) - kick.miss(FG) + kick.made(XP)) +
    (SACKS + 6*TD.DFS + 2*INT.DFS + 2*RECV) 
})

#---------------------------------------------------------------------------------
# Save
#---------------------------------------------------------------------------------
saveRDS(master, paste0(dshiny, "nfl_stats.Rds"))
