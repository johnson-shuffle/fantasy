rm(list=ls())
source("~/GoogleDrive/Research/Fantasy/scripts/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# 2015 team rosters
#---------------------------------------------------------------------------------
teams <- read.csv(paste0(dsource,"team_codes.csv"))

url<-"http://espn.go.com/nfl/team/roster/_/name/"

rosters <- NULL
for (t in teams$TEAM) {
  #Sys.sleep
  page <- paste0(url,t)
  ses <- html_session(page)
  position <- ses %>%
    html_table %>%
    extract2(1) %>%
    subset(!X1 %in% c("Offense","Defense","Special Teams","NO")) %>%
    extract("X3") %>%
    setNames("POSITION")
  players <- ses %>%
    html_nodes("a[href^='http://espn.go.com/nfl/player/']") %>%
    plyr::ldply(function(x) trim(xml_attrs(x)[["href"]])) %>%
    cbind(position) %>%
    mutate(
      PLAYER.FULL = gsub("-"," ",sapply(V1,function(x) strsplit(x,"\\/")[[1]][9])),
      ESPN.ID = sapply(V1,function(x) strsplit(x,"\\/")[[1]][8]),
      ESPN.LINK = V1,
      TEAM = t,
      V1 = NULL
    )
  rosters <- rbind(rosters, players)
  cat("*")
}

# Save
save(rosters,file=paste(dsource,"nfl_rosters.Rda",sep=""))
