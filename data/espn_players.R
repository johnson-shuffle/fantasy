rm(list = ls())
source("~/../GoogleDrive/Research/Fantasy/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# Players
#---------------------------------------------------------------------------------
load(paste0(dsource, "espn_stats_games.Rda"))

# Players
players <- indiv[c("PLAYER", "ESPN.ID", "ESPN.LINK")] %>% unique

#---------------------------------------------------------------------------------
# Player information
#---------------------------------------------------------------------------------

# Additional information
players$POSITION <- NA
players$BIRTHDAY <- NA
players$COLLEGE <- NA
players$DRAFT.YR <- NA
players$DRAFT.RD <- NA
players$DRAFT.PK <- NA

# Loop through players
for (i in 1:nrow(players)) {
  
  foo <- get.player(players$ESPN.ID[i])
  players$POSITION[i] <- foo$POSITION
  players$BIRTHDAY[i] <- foo$BIRTHDAY
  players$COLLEGE[i] <- foo$COLLEGE
  players$DRAFT.YR[i] <- foo$DRAFT.YR
  players$DRAFT.RD[i] <- foo$DRAFT.RD
  players$DRAFT.PK[i] <- foo$DRAFT.PK
  print(paste0(i," of ", nrow(players)))
  
}

# Save
save(players, file = paste0(dsource, "espn_info_players.Rda"))
