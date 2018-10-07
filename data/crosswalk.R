#---------------------------------------------------------------------------------
# Preample
#---------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# Load
#---------------------------------------------------------------------------------
load(paste0(ddata, "espn_info_players.Rda"))
load(paste0(ddata, "projections.Rda"))
load(paste0(ddata, "rotoguru_FanDuel.Rda"))

#---------------------------------------------------------------------------------
# Crosswalk 1 - Fan Duel
#---------------------------------------------------------------------------------
fd.data$name <- paste(
  sapply(fd.data$name, function(x) strsplit(x, ",")[[1]][2]),
  sapply(fd.data$name, function(x) strsplit(x, ",")[[1]][1]),
  sep = " ") %>%
  gsub("NA ", "", .) %>%
  gsub("Defense", "", .) %>%
  trim

miss.1 <- fd.data$name[!fd.data$name %in% players$PLAYER] %>% 
  unique %>%
  print

fd.data <- within(fd.data, {
  PLAYER <- name
  PLAYER[name == "E.J Manuel"] <- "EJ Manuel"
  PLAYER[name == "Robert Griffin III"] <- "Robert Griffin"
  PLAYER[name == "Michael Vick"] <- "Mike Vick"
  PLAYER[name == "Anthony Dixon"] <- "Boobie Dixon"
  PLAYER[name == "Dan Herron"] <- "Daniel Herron"
  PLAYER[name == "LaMichael James"] <- "LaMike James"
  PLAYER[name == "Steve Johnson"] <- "Stevie Johnson"
  PLAYER[name == "Tim Wright"] <- "Timothy Wright"
  PLAYER[name == "Ben Watson"] <- "Benjamin Watson"
  PLAYER[name == "Philly Brown"] <- "Corey Brown"
  PLAYER[name == "Josh Bellamy"] <- "Joshua Bellamy"
  PLAYER[name == "Gator Hoskins"] <- "Harold Hoskins"
  PLAYER[name == "Marqueis Gray"] <- "MarQueis Gray"
  PLAYER[name == "Glenn Winston"] <- "Glenn Winston"
  PLAYER[name == "Cecil Shorts"] <- "Cecil Shorts III"
  PLAYER[name == "Mickey Shuler"] <- "Mickey Charles Shuler"
  PLAYER[name == "George Atkinson"] <- "George Atkinson III"
  PLAYER[name == "Duke Johnson"] <- "Duke Johnson Jr."
  PLAYER[name == "T.J. Jones"] <- "TJ Jones"
  name[name == "Arizona"] <- "Arizona Cardinals"
  name[name == "Atlanta"] <- "Atlanta Falcons"
  name[name == "Baltimore"] <- "Baltimore Ravens"
  name[name == "Buffalo"] <- "Buffalo Bills"
  name[name == "Carolina"] <- "Carolina Panthers"
  name[name == "Chicago"] <- "Chicago Bears"
  name[name == "Cincinnati"] <- "Cincinnati Bengals"
  name[name == "Cleveland"] <- "Cleveland Browns"
  name[name == "Dallas"] <- "Dallas Cowboys"
  name[name == "Denver"] <- "Denver Broncos"
  name[name == "Detroit"] <- "Detroit Lions"
  name[name == "Green Bay"] <- "Green Bay Packers"
  name[name == "Houston"] <- "Houston Texans"
  name[name == "Indianapolis"] <- "Indianapolis Colts"
  name[name == "Jacksonville"] <- "Jacksonville Jaguars"
  name[name == "Kansas City"] <- "Kansas City Chiefs"
  name[name == "Miami"] <- "Miami Dolphins"
  name[name == "Minnesota"] <- "Minnesota Vikings"
  name[name == "New England"] <- "New England Patriots"
  name[name == "New Orleans"] <- "New Orleans Saints"
  name[name == "New York G"] <- "New York Giants"
  name[name == "New York J"] <- "New York Jets"
  name[name == "Oakland"] <- "Oakland Raiders"
  name[name == "Philadelphia"] <- "Philadelphia Eagles"
  name[name == "Pittsburgh"] <- "Pittsburgh Steelers"
  name[name == "San Diego"] <- "San Diego Chargers"
  name[name == "San Francisco"] <- "San Francisco 49ers"
  name[name == "Seattle"] <- "Seattle Seahawks"
  name[name == "St. Louis"] <- "St Louis Rams"
  name[name == "Tampa Bay"] <- "Tampa Bay Buccaneers"
  name[name == "Tennessee"] <- "Tennessee Titans"
  name[name == "Washington"] <- "Washington Redskins"
})

#---------------------------------------------------------------------------------
# Crosswalk 2 - Projections
#---------------------------------------------------------------------------------

# Use full name - NOT VERY GOOD
miss.2 <- proj$PLAYER[!proj$PLAYER %in% players$PLAYER] %>% unique

# Use first initial - BETTER
proj$PLAYER.2 <- paste(
  sapply(proj$PLAYER, function(x) substr(strsplit(x, " ")[[1]][1], 1, 1)),
  sapply(proj$PLAYER, function(x) strsplit(x, " ")[[1]][2]),
  sep = ". "
  )
 
players$PLAYER.2 <- paste(
  sapply(players$PLAYER, function(x) substr(strsplit(x, " ")[[1]][1], 1, 1)),
  sapply(players$PLAYER, function(x) strsplit(x, " ")[[1]][2]),
  sep = ". "
  )

miss.2 <- proj$PLAYER.2[!proj$PLAYER.2 %in% players$PLAYER.2] %>% 
  unique %>% 
  print

# No big issues - mostly players with all 0 projections
lapply(miss.2, function(x) subset(proj, PLAYER.2 == x))

#---------------------------------------------------------------------------------
# Merge
#---------------------------------------------------------------------------------
crosswalk <- players %>%
  merge(fd.data[c("gid", "pos", "name", "PLAYER")] %>% unique, 
        by.x = c("PLAYER", "POSITION"), 
        by.y = c("PLAYER", "pos"),
        all.x = T, all.y = F)

# Add defenses
dfs <- subset(fd.data, pos == "Def")[c("gid", "name")] %>%
  unique

crosswalk <- plyr::rbind.fill(crosswalk, dfs)

# Duplicates
crosswalk$ESPN.ID[duplicated(crosswalk$ESPN.ID)]

# Fix duplicates
crosswalk <- crosswalk %>% 
  mutate(
    gid = as.numeric(gid),
    ESPN.ID = as.numeric(ESPN.ID)
    ) %>%
  subset(!gid %in% 4592) %>%
  within({
    gid[gid == 5204] <- 5170
    gid[gid == 5295] <- 2580
    }) %>%
  subset(!gid %in% 4592) %>%
  subset(!gid %in% 4367 | ESPN.ID != 12699) %>%
  subset(!gid %in% 4461 | ESPN.ID != 10482) %>%
  unique

#---------------------------------------------------------------------------------
# Save
#---------------------------------------------------------------------------------
save(crosswalk, file = paste0(ddata, "crosswalk.Rda"))
