#---------------------------------------------------------------------------------
# Preample
#---------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# Game info
#---------------------------------------------------------------------------------
#url<-"http://scores.espn.go.com/nfl/scoreboard?"
#weeks <- c(1:17)
#years <- c(2006:2014)

#game.info <- NULL
#for (y in years) {
#  for (w in weeks) {
#    info <- get.game(w,y)
#    game.info <- rbind(game.info,info)
#    Sys.sleep(3)
#    print(paste("Week",w,"in",y,"done"))
#  }
#}

# Postponed game: 2014 - Wk 12
#game.info <- subset(game.info,GAME.ID=="400554331")

# Save
#save(game.info, file = paste0(dsource, "espn_info_games.Rda"))

#---------------------------------------------------------------------------------
# Game Stats
#---------------------------------------------------------------------------------
load(file = paste0(dsource, "espn_info_games.Rda"))

team <- NULL # Team stats
indiv <- NULL # Individual stats

for (i in 1:nrow(missing)) {
  
  # Pause between iterations
  Sys.sleep(2)
  
  # Get game stats
  check <- game.info$GAME.LINK[i] %>%
    html_session %>%
    html_table(fill = TRUE, header = FALSE)
  
  if (length(check) < 21) {
    next
  } else {
    foo <- get.stats(game.info[i,])
    foo.names <- names(foo)
  }
  
  # Bind
  team <- plyr::rbind.fill(team,foo$Team)
  indiv <- plyr::rbind.fill(indiv,foo$Individual)
  
  # Clean up
  rm(foo, foo.names)
  
  # Save
  save(list = c("team", "indiv"), file = paste0(dsource,"espn_stats_games.Rda"))
  
  # Status
  print(paste0(i, " / ", nrow(game.info)))
  
}

#---------------------------------------------------------------------------------
# Save
#---------------------------------------------------------------------------------
save(list = c("team", "indiv"), file = paste0(dsource, "espn_stats_games.Rda"))
