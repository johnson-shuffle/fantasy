#---------------------------------------------------------------------------------
# Preample
#---------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

library(caret)
library(randomForest)
library(zoo)

#---------------------------------------------------------------------------------
# Prep data
#---------------------------------------------------------------------------------
load(file = paste0(dsource, "espn_stats_games.Rda"))
load(file = paste0(dsource, "espn_info_games.Rda"))
load(file = paste0(dsource, "espn_info_players.Rda"))
load(file = paste0(dsource, "nfl_schedules.Rda"))

# Kicker data
dat.kickers <- indiv %>%
  select(YEAR, WEEK, TEAM, ESPN.ID, PTS) %>%
  merge(players, by = "ESPN.ID") %>%
  merge(select(schedule, YEAR, WEEK, TEAM, OPPONENT),
        by = c("YEAR", "WEEK", "TEAM")) %>%
  mutate(
    OPPONENT.2 = gsub("@", "", OPPONENT)
  ) %>%
  subset(POSITION == "PK") %>%
  ungroup

# Team data
dat.teams <- team %>%
  merge(game.info[c("GAME.ID", "YEAR", "WEEK")], by = "GAME.ID") %>%
  rename(TEAM = Team) %>%
  merge(schedule,  
        by = c("YEAR", "WEEK", "TEAM"),
        all = T) %>%
  mutate(
    OPPONENT.2 = gsub("@", "", OPPONENT)
    ) %>%
  merge(., 
        select(., - TEAM, - DAY, - GAME.ID, - OPPONENT), 
        by.x = c("YEAR", "WEEK", "TEAM"), 
        by.y = c("YEAR", "WEEK", "OPPONENT.2"),
        all.x = T, all.y = F) %>%
  mutate(
    OFF.TOT = `Total Yards.x`,
    OFF.TOT = replace(OFF.TOT, is.na(OFF.TOT), 0),
    DEF.TOT = `Total Yards.y`,
    DEF.TOT = replace(DEF.TOT, is.na(DEF.TOT), 0),
    WEEK = as.numeric(WEEK)
    ) %>%
  arrange(TEAM, YEAR, WEEK) %>%
  group_by(TEAM, YEAR) %>%
  mutate(
    GAME = 1,
    GAME = replace(GAME, OPPONENT == "bye", 0),
    OFF.TOT.m = cumsum(OFF.TOT) / cumsum(GAME),
    DEF.TOT.m = cumsum(DEF.TOT) / cumsum(GAME)
    ) %>%
  ungroup

# Score data
dat.scores <- indiv %>%
  mutate(
    FGA = sapply(FG, function(y) strsplit(y, "/")[[1]][2]),
    TDS = PAS.TD + RSH.TD
    ) %>%
  group_by(YEAR, WEEK, TEAM, OPPONENT) %>%
  summarise(
    FGA = sum(as.numeric(FGA)),
    TDS = sum(TDS)
    ) %>%
  merge(schedule, 
        by = c("YEAR", "WEEK", "TEAM"),
        all = T) %>%
  arrange(TEAM, YEAR, WEEK) %>%
  group_by(TEAM, YEAR) %>%
  mutate(
    GAME = 1,
    GAME = replace(GAME, OPPONENT.y == "bye", 0),
    FGA = replace(FGA, OPPONENT.y == "bye", 0),
    TDS = replace(TDS, OPPONENT.y == "bye", 0),
    OFF.FGA.m = cumsum(FGA) / cumsum(GAME),
    OFF.TDS.m = cumsum(TDS) / cumsum(GAME)
    ) %>%
  ungroup %>%
  arrange(OPPONENT.x, YEAR, WEEK) %>%
  group_by(OPPONENT.x, YEAR) %>%
  mutate(
    DEF.FGA.m = cumsum(FGA) / cumsum(GAME),
    DEF.TDS.m = cumsum(TDS) / cumsum(GAME)
  ) %>%
  ungroup

#---------------------------------------------------------------------------------
# Merge data
#---------------------------------------------------------------------------------
dat <- dat.kickers %>%
  mutate(
    WEEK = WEEK - 1
    ) %>%
  subset(WEEK != 0) %>%
  merge(select(dat.teams, YEAR, WEEK, TEAM, OFF.TOT.m),
        by = c("YEAR", "WEEK", "TEAM"),
        all.x = T, all.y = F) %>%
  merge(select(dat.teams, YEAR, WEEK, OPPONENT.2, DEF.TOT.m),
        by = c("YEAR", "WEEK", "OPPONENT.2"),
        all.x = T, all.y = F) %>%
  merge(select(dat.scores, YEAR, WEEK, TEAM, OFF.FGA.m, OFF.TDS.m),
        by = c("YEAR", "WEEK", "TEAM")) %>%
  merge(select(dat.scores, YEAR, WEEK, OPPONENT.x, DEF.FGA.m, DEF.TDS.m),
        by.x = c("YEAR", "WEEK", "OPPONENT.2"),
        by.y = c("YEAR", "WEEK", "OPPONENT.x")) %>%
  mutate(
    WEEK = WEEK + 1
  )

#---------------------------------------------------------------------------------
# Models
#---------------------------------------------------------------------------------
set.seed(1)
train.index <- createDataPartition(dat$PTS, p = 3/4, list = FALSE)
train.dat <- dat[train.index,]
test.dat <- dat[-train.index,] %>% subset(!is.na(OFF.FGA.m))

# Variables
vars <- c(
  paste("OFF", c("TOT.m", "FGA.m", "TDS.m"), sep = "."),
  paste("DEF", c("TOT.m", "FGA.m", "TDS.m"), sep = ".")
  )

# Model One
rf.model <- train(
  as.formula(paste0("PTS ~", paste(vars, collapse = "+"))),
  data = train.dat,
  method = "rf",
  trControl = trainControl(method = "cv", number = 20),
  importance = T
  )

# Predict One
test.dat$PROJ.PTS.1 <- predict(rf.model, newdata = test.dat)

# Summarize
(test.dat$PTS - test.dat$PROJ.PTS.1) %>% hist

# Model Two
gbm.model <- train(
  as.formula(paste0("PTS ~", paste(vars, collapse = "+"))),
  data = train.dat,
  distribution = "gaussian",
  method = "gbm",
  trControl = trainControl(method = "repeatedcv", number = 20, repeats = 20),
  verbose = F
  )

# Predict Two
test.dat$PROJ.PTS.2 <- predict(gbm.model, newdata = test.dat)

# Summarize
(test.dat$PTS - test.dat$PROJ.PTS.2) %>% hist

# Model Three
svm.model <- train(
  as.formula(paste0("PTS ~", paste(vars, collapse = "+"))),
  data = train.dat,
  method = "svmRadial",
  trControl = trainControl(number = 20),
  scaled = F
  )

# Predict Three
test.dat$PROJ.PTS.3 <- predict(gbm.model, newdata = test.dat)

# Summarize
(test.dat$PTS - test.dat$PROJ.PTS.3) %>% geom_histogram


#---------------------------------------------------------------------------------
# Plot
#---------------------------------------------------------------------------------

# Plot data
dat.plot <- test.dat[c("PTS", "PROJ.PTS.1", "PROJ.PTS.2", "PROJ.PTS.3")] %>%
  melt(id = "PTS") %>%
  mutate(VAL = PTS - value)

# Plot
p <- ggplot(data = dat.plot) +
  geom_density(aes(x = VAL, colour = factor(variable)))
print(p)
