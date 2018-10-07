#---------------------------------------------------------------------------------
# Preample
#---------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# ESPN stats - FanDuel scoring
#---------------------------------------------------------------------------------
load(paste0(ddata, "espn_stats_games.Rda"))

# Merge stats with schedules (adds home indicator)
load(paste0(ddata, "nfl_schedules.Rda"))
indiv <- schedule %>%
  mutate(
    HOME = as.numeric(!grepl("@", OPPONENT)),
    OPPONENT = NULL
  ) %>%
  merge(indiv, by = c("TEAM", "YEAR", "WEEK"), all.x = F, all.y =T)

# Kicking function
kicking <- function(x) {
  s <- strsplit(x, "/")
  made <- sapply(s, function(y) {
    as.numeric(s[[1]][1])
  }) %>% 
    unname
  miss <- sapply(s, function(y) {
    as.numeric(s[[1]][2]) - as.numeric(s[[1]][1])
  }) %>% 
    unname
  list(made = made, miss = miss)
}

# Individual points
indiv <- within(indiv, {
  OFF.PTS <- 
    ((1/25)*PAS.YDS + 4*PAS.TD - PAS.INT) + 
    ((1/10)*REC.YDS + 6*REC.TD + 0.5*REC) + 
    ((1/10)*RSH.YDS + 6*RSH.TD - 2*LOST) +
    (6*KRT.TD + 6*PRT.TD) +
    (3*kicking(FG)$made - kicking(FG)$miss + kicking(XP)$made)
})

#---------------------------------------------------------------------------------
# Presenting... Beast Mode
#---------------------------------------------------------------------------------
beast.mode <- subset(indiv, PLAYER == "Marshawn Lynch")

bm <- ggplot(data = beast.mode) +
  geom_bar(
    aes(x = OFF.PTS, fill = factor(HOME, levels = c(1,0))), 
    colour = "black",
    position = "dodge") +
  scale_fill_manual(
    name = "Home Game?",
    labels = c("Y", "N"),
    values = c("grey50", "grey75")) +
  theme_classic() +
  labs(
    x = "FanDuel Score",
    y = "Frequency") +
  theme(
    legend.position = c(.85, .85),
    axis.title.x = element_text(vjust = 0),
    axis.title.y = element_text(vjust = 1)
    )
bm
ggsave(paste0(dtex, "figures/beast_mode.pdf"), bm, height = 5, width = 10)

#---------------------------------------------------------------------------------
# Simulation results
#---------------------------------------------------------------------------------
load(paste0(dfantasy, "week_9_sims.Rda"))

#---------------------------------------------------------------------------------
# Teams
#---------------------------------------------------------------------------------

# Team frequencies
team.freq <- teams %>%
  unlist %>%
  plyr::count(.) %>% 
  arrange(desc(freq)) %>%
  setNames(c("team", "freq")) %>%
  mutate(
    team.code = toupper(letters[1:nrow(.)])
    )

# Team frequency plot
tf <- ggplot(data = team.freq) +
  geom_bar(
    aes(x = factor(team.code, levels = rev(team.code)), y = freq / 2E5),
    stat = "identity",
    fill = "grey75",
    colour = "black") +
  coord_flip() +
  theme_classic() +
  labs(
    x = "Team",
    y = "Frequency") +
  theme(
    axis.title.x = element_text(vjust = 0),
    axis.title.y = element_text(vjust = 1)
    )
tf
ggsave(paste0(dtex, "figures/team_freq_intro.pdf"), tf, height = 5, width = 10) 

# Function to extract teams
get.team <- function(sim) {
  s <- strsplit(sim, "-") %>% unlist
  dat %>% 
    mutate(id.num = 1:n()) %>% 
    subset(id.num %in% s)
}

# Look at various rosters
get.team(team.freq$team[4])

#---------------------------------------------------------------------------------
# Players
#---------------------------------------------------------------------------------

# Player frequencies
dat$id.num <- 1:nrow(dat)
player.freq <- teams %>%
  unlist %>%
  sapply(function(x) strsplit(x, "-")) %>%
  unlist %>%
  plyr::count(.) %>%
  merge(dat[c("id.num", "name")], by.x = "x", by.y = "id.num") %>%
  arrange(desc(freq))
 
# Player frequency plot
pf <- ggplot(data = player.freq) +
  geom_bar(
    aes(x = factor(name, levels = rev(name)), y = freq / 2E5),
    stat = "identity",
    fill = "grey75",
    colour = "black") +
  coord_flip() +
  theme_classic() +
  labs(
    x = "Player",
    y = "Frequency") +
  theme(
    axis.title.x = element_text(vjust = 0),
    axis.title.y = element_text(vjust = 1))
pf
ggsave(paste0(dtex, "figures/player_freq_intro.pdf"), pf, height = 5, width = 10) 
