#----------------------------------------------%>>%---------------------------------
# Preample
#-------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

library(lpSolve)
library(Rglpk)
library(doParallel)
library(doSNOW)

#-------------------------------------------------------------------------------
# Customize which players get dropped from consideration
#-------------------------------------------------------------------------------
week <- 16

alert.drops <- c("Q", "D", "IR", "O", "NA")

manual.drops <- c("")

projection.drops <- 0

#-------------------------------------------------------------------------------
# Function to extract info from table (tr) nodes
#-------------------------------------------------------------------------------
extract.info <- function(x) {
  for (j in 1:6) {
    if (j != 5) {
      v <- xml_children(x)[j] %>>% xml_text
      assign(paste0("v", j), v)
    } else {
      t <- xml_children(x)[j][[1]] %>>%
        xml_contents %>>% 
        xml_name %>>% 
        match("b", .)
      team <- xml_children(x)[j][[1]] %>>% 
        xml_contents %>>% 
        xml_text %>>% 
        extract(t)
      opp <- xml_children(x)[j][[1]] %>>% 
        xml_contents %>>% 
        xml_text %>>% 
        extract(c(1, 2)[!c(1, 2) %in% t]) %>>% 
        gsub("@", "", .)
      if (t == 2) {home = 1} else {home = 0}
    }
  }
  data.frame(
    pos = v1,
    name = v2,
    fd.points = v3,
    games = v4,
    team = team,
    opp = opp,
    home = home,
    fd.salary = v6
  ) 
}

#-------------------------------------------------------------------------------
# FanDuel data for next nfl games
#-------------------------------------------------------------------------------
dat1 <- html_session("https://www.fanduel.com/nextnflgame") %>>%
  html_nodes("tr") %>>%
  extract(3:length(.)) %>>%
  plyr::ldply(extract.info) %>>%
  mutate(
    pos = replace(pos, pos == "D", "Def"),
    pos = replace(pos, pos == "K", "PK"),
    team = replace(team, team == "JAC", "JAX"),
    team = replace(team, team == "WAS", "WSH"),
    opp = replace(opp, opp == "JAC", "JAX"),
    opp = replace(opp, opp == "WAS", "WSH"),
    fd.salary = gsub("\\$", "", fd.salary),
    fd.salary = gsub(",", "", fd.salary),
    alerts = rep("", nrow(.))
  )

# Extract alerts
for (i in 1 : nrow(dat1)) {
  x <- substr(dat1$name[i], nchar(dat1$name[i]), nchar(dat1$name[i]))
  if (x == "I" | x == ".") {next}
  while (x == toupper(x)) {
    dat1$name[i] <- substr(dat1$name[i], 1, nchar(dat1$name[i]) - 1)
    dat1$alerts[i] <- paste0(x, dat1$alerts[i]) %>>%
      gsub("\\.", "", .)
    x <- substr(dat1$name[i], nchar(dat1$name[i]), nchar(dat1$name[i]))
  }
  rm(i, x)
}

#-------------------------------------------------------------------------------
# Run through the crosswalk
#-------------------------------------------------------------------------------
load(paste0(ddata, "crosswalk.Rda"))

# Change FB's to RB's
crosswalk$POSITION <- crosswalk$POSITION %>>% gsub("FB", "RB", .)

# Not in the crosswalk
union(
  dat1$name[!dat1$name %in% crosswalk$name],
  dat1$name[!dat1$name %in% crosswalk$PLAYER]
  ) %>>%
  extract(! . %in% dat1$name[dat1$games == 0]) %>>%
  extract(! . %in% dat1$name[dat1$pos == "Def"]) %>>%
  extract(! . %in% crosswalk$PLAYER) %>>%
  unique %>>%
  sort

# Add player id's to match crosswalk
dat1 <- within(dat1, {
  ESPN.ID <- NA
  ESPN.ID[name == "Akeem Hunt"] <- 2576408
  ESPN.ID[name == "Andrew Turzilli"] <- 2513916
  ESPN.ID[name == "AJ McCarron"] <- 16810
  ESPN.ID[name == "Anthony Dixon"] <- 13212
  ESPN.ID[name == "Benny Cunningham"] <- 16055
  ESPN.ID[name == "Blake Annen"] <-16964
  ESPN.ID[name == "Brian Parker"] <- 2508328
  ESPN.ID[name == "Cecil Shorts"] <- 14184
  ESPN.ID[name == "Christopher Ivory"] <- 13587
  ESPN.ID[name == "Corey (Philly) Brown"] <- 17399
  ESPN.ID[name == "Dan Herron"] <- 15041
  ESPN.ID[name == "Darius Jennings"] <- 2577808
  ESPN.ID[name == "David Cobb"] <- 2576303
  ESPN.ID[name == "Duke Johnson"] <- 2969962
  ESPN.ID[name == "E.J. Bibbs"] <- 3039924
  ESPN.ID[name == "E.J. Manuel"] <- 15403
  ESPN.ID[name == "Foswhitt Whittaker"] <- 15755
  ESPN.ID[name == "Jamarcus Nelson"] <- 2515759
  ESPN.ID[name == "Javontee Herndon"] <- 17005
  ESPN.ID[name == "Jay Ajayi"] <- 2573300
  ESPN.ID[name == "Jeremy Butler"] <- 17122
  ESPN.ID[name == "Jesse James"] <- 2979590
  ESPN.ID[name == "Joey Iosefa"] <- 2517262
  ESPN.ID[name == "Josh Bellamy"] <- 15555
  ESPN.ID[name == "Kellen Moore"] <- 14882
  ESPN.ID[name == "Kevin Smith"] <- 17257
  ESPN.ID[name == "Kyle Miller"] <- 14289
  ESPN.ID[name == "LaMichael James"] <- 14890
  ESPN.ID[name == "Michael Vick"] <- 2549 
  ESPN.ID[name == "Nick O'Leary"] <- 2576804 
  ESPN.ID[name == "Odell Beckham Jr"] <- 16733
  ESPN.ID[name == "Ryan Hewitt"] <- 17169
  ESPN.ID[name == "Sean Mannion"] <- 2517017
  ESPN.ID[name == "Seantavius Jones"] <- 17191
  ESPN.ID[name == "Steve Johnson"] <- 11458
  ESPN.ID[name == "Ted Ginn Jr"] <- 10453
  ESPN.ID[name == "Tim Wright"] <- 16285
  ESPN.ID[name == "Tyrell Williams"] <- 2587819
})

# Merge with crosswalk
dat2 <- dat1 %>>%
  merge(crosswalk[c("PLAYER", "ESPN.ID", "POSITION")],
        by.x = c("name", "pos"),
        by.y = c("PLAYER", "POSITION"),
        all.x = T, all.y = F) %>>%
  within({
    ESPN.ID.x[is.na(ESPN.ID.x)] <- ESPN.ID.y[is.na(ESPN.ID.x)]
    }) %>>%
  merge(crosswalk[c("name", "ESPN.ID", "POSITION")],
        by.x = c("name", "pos"),
        by.y = c("name", "POSITION"),
        all.x = T, all.y = F) %>>%
  within({
    ESPN.ID.x[is.na(ESPN.ID.x)] <- ESPN.ID[is.na(ESPN.ID.x)]
  }) %>>%
  mutate(
    ESPN.ID = ESPN.ID.x
    ) %>>%
  select(- ESPN.ID.x, -ESPN.ID.y) %>>%
  subset(!ESPN.ID %in% c(3776, 14608, 10482, 16020)) %>>% 
  unique

# Final check
subset(dat2, is.na(ESPN.ID) & pos != "Def" & games > 0 & fd.points > 0)

#-------------------------------------------------------------------------------
# Add projections
#-------------------------------------------------------------------------------
load(paste0(ddata, "projections_fp.Rda"))

# Subset to desired year and week
proj <- subset(proj, YEAR == 2015 & WEEK == week) %>>%
  group_by(PLAYER) %>>%
  subset(PLAYER != "Ryan Griffin" | REC != 0) %>>%
  mutate(
    PROJ = mean(as.numeric(PROJ))
    ) %>>%
  unique

# Average projection by position (weird problem w/ makes this necessary)
proj.avg <- dat2 %>>%
  merge(proj, 
        by.x = c("name", "team"),
        by.y = c("PLAYER", "TEAM"),
        all.x = T, all.y = F) %>>%
  group_by(pos) %>>%
  summarise(
    PROJ.AVG = mean(as.numeric(PROJ), na.rm = T)
    ) 

# Merge together
dat3 <- dat2 %>>%
  merge(proj, 
        by.x = c("name", "team"),
        by.y = c("PLAYER", "TEAM"),
        all.x = T, all.y = F) %>>%
  merge(proj.avg, by = "pos", sort = F) %>>%
  within({
    PROJ[is.na(PROJ)] <- PROJ.AVG[is.na(PROJ)]
  }) %>>%
  arrange(name)
  
#-------------------------------------------------------------------------------
# ESPN stats with FanDuel scoring
#-------------------------------------------------------------------------------
load(paste0(ddata, "espn_stats_games.Rda"))

# Merge stats with schedules (adds home indicator)
load(paste0(ddata, "nfl_schedules.Rda"))
indiv <- schedule %>>%
  mutate(
    HOME = as.numeric(!grepl("@", OPPONENT)),
    OPPONENT = NULL
  ) %>>%
  merge(indiv, by = c("TEAM", "YEAR", "WEEK"), all.x = F, all.y =T)

# Kicking function
kicking <- function(x) {
  s <- strsplit(x, "/")
  made <- sapply(s, function(y) {
    as.numeric(s[[1]][1])
    }) %>>% 
    unname
  miss <- sapply(s, function(y) {
    as.numeric(s[[1]][2]) - as.numeric(s[[1]][1])
    }) %>>% 
    unname
  list(made = made, miss = miss)
}

# Individual points
ind.pts <- within(indiv, {
  OFF.PTS <- 
    ((1/25)*PAS.YDS + 4*PAS.TD - PAS.INT) + 
    ((1/10)*REC.YDS + 6*REC.TD + 0.5*REC) + 
    ((1/10)*RSH.YDS + 6*RSH.TD - 2*LOST) +
    (6*KRT.TD + 6*PRT.TD) +
    (3*kicking(FG)$made - kicking(FG)$miss + kicking(XP)$made)
  })

# Defensive points - Team fantasy points
def.ind <- indiv %>>%
  mutate(
    DEF.PTS = SACKS + 2*RECV + 6*DFS.TD + 2*DFS.INT
    ) %>>%
  group_by(TEAM, YEAR, WEEK) %>>%
  summarise(
    DEF.PTS = sum(DEF.PTS)
    )

# Defensive points - Points allowed (then merge with team fantasy points)
def.pts <- indiv %>>%
  mutate(
    SCORE = 6*(PAS.TD + RSH.TD + KRT.TD + PRT.TD) + KCK.PTS
    ) %>>%
  group_by(OPPONENT, YEAR, WEEK, HOME) %>>%
  summarise(
    DEF.PTS = sum(SCORE)
    ) %>>%
  within({
    DEF.PTS.2 <- NA
    DEF.PTS.2[DEF.PTS == 0] <- 10
    DEF.PTS.2[DEF.PTS >= 1 & DEF.PTS <= 6] <- 7
    DEF.PTS.2[DEF.PTS >= 7 & DEF.PTS <= 13] <- 4
    DEF.PTS.2[DEF.PTS >= 14 & DEF.PTS <= 20] <- 1
    DEF.PTS.2[DEF.PTS >= 21 & DEF.PTS <= 27] <- 0
    DEF.PTS.2[DEF.PTS >= 28 & DEF.PTS <= 34] <- -1
    DEF.PTS.2[DEF.PTS >= 35] <- -4
  }) %>>%
  merge(def.ind, 
        by.x = c("OPPONENT", "YEAR", "WEEK"),
        by.y = c("TEAM", "YEAR", "WEEK"),
        all.x = T, all.y = F) %>>%
  mutate(
    DEF.PTS = DEF.PTS.y + DEF.PTS.2,
    DEF.PTS.x = NULL,
    DEF.PTS.y = NULL,
    DEF.PTS.2 = NULL
    ) %>>%
  within({
    name <- NA
    name[OPPONENT == "ARI"] <- "Arizona Cardinals"
    name[OPPONENT == "ATL"] <- "Atlanta Falcons"
    name[OPPONENT == "BAL"] <- "Baltimore Ravens"
    name[OPPONENT == "BUF"] <- "Buffalo Bills"
    name[OPPONENT == "CAR"] <- "Carolina Panthers"
    name[OPPONENT == "CHI"] <- "Chicago Bears"
    name[OPPONENT == "CIN"] <- "Cincinnati Bengals"
    name[OPPONENT == "CLE"] <- "Cleveland Browns"
    name[OPPONENT == "DAL"] <- "Dallas Cowboys"
    name[OPPONENT == "DEN"] <- "Denver Broncos"
    name[OPPONENT == "DET"] <- "Detroit Lions"
    name[OPPONENT == "GB"] <- "Green Bay Packers"
    name[OPPONENT == "HOU"] <- "Houston Texans"
    name[OPPONENT == "IND"] <- "Indianapolis Colts"
    name[OPPONENT == "JAX"] <- "Jacksonville Jaguars"
    name[OPPONENT == "KC"] <- "Kansas City Chiefs"
    name[OPPONENT == "MIA"] <- "Miami Dolphins"
    name[OPPONENT == "MIN"] <- "Minnesota Vikings"
    name[OPPONENT == "NE"] <- "New England Patriots"
    name[OPPONENT == "NO"] <- "New Orleans Saints"
    name[OPPONENT == "NYG"] <- "New York Giants"
    name[OPPONENT == "NYJ"] <- "New York Jets"
    name[OPPONENT == "OAK"] <- "Oakland Raiders"
    name[OPPONENT == "PHI"] <- "Philadelphia Eagles"
    name[OPPONENT == "PIT"] <- "Pittsburgh Steelers"
    name[OPPONENT == "SD"] <- "San Diego Chargers"
    name[OPPONENT == "SF"] <- "San Francisco 49ers"
    name[OPPONENT == "SEA"] <- "Seattle Seahawks"
    name[OPPONENT == "STL"] <- "St Louis Rams"
    name[OPPONENT == "TB"] <- "Tampa Bay Buccaneers"
    name[OPPONENT == "TEN"] <- "Tennessee Titans"
    name[OPPONENT == "WSH"] <- "Washington Redskins"
  })

# Position points - Average by week
pos.pts <- ind.pts %>>%
  merge(crosswalk[c("ESPN.ID", "POSITION")], 
        by = "ESPN.ID",
        all.x = T, all.y = F) %>>%
  group_by(POSITION, YEAR, WEEK) %>>%
  summarise(
    POS.PTS = mean(OFF.PTS)
    )

#-------------------------------------------------------------------------------
# Final adjustments
#-------------------------------------------------------------------------------

# Drops - Questionable, Doubtful, Injured Reserve, Out, Not Active
dat <- dat3 %>>%  subset(!alerts %in% alert.drops)

# Drops - Additional players that are unavailable
dat <- dat %>>% subset(name != manual.drops)

# Drops - Projections greater than lower bound (helps remove non-starters)
dat <- dat %>>% subset(PROJ > projection.drops | is.na(PROJ))

#-------------------------------------------------------------------------------
# Sampling points, sampling function, measure of skew
#-------------------------------------------------------------------------------
sample.pts <- lapply(1:nrow(dat), function(x) {
    s <- subset(ind.pts, ESPN.ID == dat$ESPN.ID[x] & YEAR == 2015)$OFF.PTS
    if (!is.na(dat$ESPN.ID[x])) {
      out <- s
      }
    if (length(s) == 0) {
      out <- subset(pos.pts, POSITION == dat$pos[x] & YEAR == 2015)$POS.PTS
      }
    if (dat$pos[x] == "Def") {
      out <- subset(def.pts, name == dat$name[x] & YEAR == 2015)$DEF.PTS
    }
    out
})

sample.fun <- function(id.num, seed.num) {
  set.seed(seed.num)
  sample(sample.pts[[id.num]], 1)
}

skew <- lapply(sample.pts, function(x) median(x) - mean(x)) %>>% unlist 
skew <- (skew - min(skew)) / (max(skew) - min(skew)) + 1

#-------------------------------------------------------------------------------
# Optimize function
#-------------------------------------------------------------------------------
optimal.team <- function(dat) {
  
  NP <- dat %>>% nrow
  NT <- dat$team %>>% unique %>>% length
    
  qb <- grepl("QB", dat$pos) %>>% as.numeric
  rb <- grepl("RB", dat$pos) %>>% as.numeric
  wr <- grepl("WR", dat$pos) %>>% as.numeric
  te <- grepl("TE", dat$pos) %>>% as.numeric
  pk <- grepl("PK", dat$pos) %>>% as.numeric
  def <- grepl("Def", dat$pos) %>>% as.numeric
  cost <- dat$fd.salary %>>% as.numeric
  team <- NULL
  for (i in 1:NT) {
    team <- rbind(team, grepl(unique(dat$team)[i], dat$team) %>>% as.numeric)
  }
  
  f.obj <- dat$obj %>>% as.numeric
  f.con <- rbind(
    qb, rb, wr, te, pk, def, cost, team, diag(NP)
    )
  f.rhs <- c(
    1, 2, 3, 1, 1, 1, 60000, rep(4, NT), rep(0, NP)
    )
  f.dir <- c(
    "==", "==", "==", "==", "==", "==", "<=", rep("<=", NT), rep(">=", NP)
    ) 
  
  att1 <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = T, num.bin.solns = 1)
  
  att2 <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, types = "B", max = T)
  
  list(
    sol1 = att1,
    sol2 = att2,
    id1 = dat$id[grep(1, att1$solution)] %>>% paste(collapse = "-"),
    id2 = dat$id[grep(1, att2$solution)] %>>% paste(collapse = "-")
  )
  
}

#-------------------------------------------------------------------------------
# Bootstrap - Setup for parallel processing on Windows machine
#-------------------------------------------------------------------------------
iter <- 5E5

c1 <- makeCluster(4, outfile = paste0(dfantasy, "parallel_out.txt"))
registerDoSNOW(c1)

par.packages <- c("magrittr", "dplyr", "lpSolve", "Rglpk")

teams <- foreach (i = 1:iter, .packages = par.packages) %dopar% {
  
  temp <- dat %>>%
    mutate(
      fd.points = fd.points %>>% as.numeric,
      fd.salary = fd.salary %>>% as.numeric,
      id.num = 1:n(),
      points = sapply(id.num, function(x) sample.fun(x, i)),
      obj = (1/3)*fd.points + (1/3)*points + (1/3)*PROJ
    ) %>>%
    within({
      obj[pos == "Def"] <- 
        (1/2)*fd.points[pos == "Def"] + (1/2)*points[pos == "Def"]
    })
  
  temp.teams <- optimal.team(temp)
  
  list(temp.teams$id1, temp.teams$id2, temp$obj)

}

stopCluster(c1)

#-------------------------------------------------------------------------------
# Frequencies
#-------------------------------------------------------------------------------

# Team frequencies
team.freq <- teams %>>%
  lapply(function(x) x[1:2]) %>>%
  unlist %>>%
  plyr::count(.) %>>% 
  arrange(desc(freq)) %>>%
  setNames(c("team", "freq")) %>>%
  mutate(
    team.code = 1:n()
  )

# Player frequencies
dat$id.num <- 1:nrow(dat)
player.freq <- teams %>>%
  lapply(function(x) x[1:2]) %>>%
  unlist %>>%
  sapply(function(x) strsplit(x, "-")) %>>%
  unlist %>>%
  plyr::count(.) %>>%
  merge(dat[c("id.num", "name", "pos")], by.x = "x", by.y = "id.num") %>>%
  arrange(pos, desc(freq))

#-------------------------------------------------------------------------------
# Save
#-------------------------------------------------------------------------------
save(list = c("dat", "teams", "iter", "team.freq", "player.freq"), 
     file = paste0(dfantasy, "week_", week, "_sims.Rda"))
