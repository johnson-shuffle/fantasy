#---------------------------------------------------------------------------------
# Preample
#---------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

#---------------------------------------------------------------------------------
# ESPN stats - FanDuel scoring
#---------------------------------------------------------------------------------
load(paste0(ddata, "espn_stats_games.Rda"))

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
# Simulation results
#---------------------------------------------------------------------------------
week <- 12

load(paste0(dfantasy, "week_", week, "_sims.Rda"))

#---------------------------------------------------------------------------------
# Teams
#---------------------------------------------------------------------------------

# Team frequencies
team.freq <- teams %>%
  lapply(function(x) x[1:2]) %>%
  unlist %>%
  plyr::count(.) %>% 
  arrange(desc(freq)) %>%
  setNames(c("team", "freq")) %>%
  mutate(
    team.code = 1:n()
    )

# Team frequency plot
tf <- ggplot(data = team.freq) +
  geom_bar(
    aes(x = factor(team.code, levels = rev(team.code)), y = freq / (2*iter)),
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
ggsave(paste0(dtex, "figures/team_freq_", week, ".pdf"), 
       tf, 
       height = 10, width = 10) 

# Function to extract teams
get.team <- function(sim) {
  s <- strsplit(sim, "-") %>% unlist
  dat %>% 
    mutate(id.num = 1:n()) %>% 
    subset(id.num %in% s)
}

# Look at various rosters
for (i in 1:10) {
  get.team(team.freq$team[i])[c("pos", "name", "fd.salary", "team", "home")] %>%
    arrange(pos) %>%
    print
}

# Check salary cap constraint
salary <- function(x) get.team(team.freq$team[x])$fd.salary %>% as.numeric %>% sum
60000 - lapply(1:10, salary) %>% unlist 

#---------------------------------------------------------------------------------
# Players
#---------------------------------------------------------------------------------

# Player frequencies
dat$id.num <- 1:nrow(dat)
player.freq <- teams %>%
  lapply(function(x) x[1:2]) %>%
  unlist %>%
  sapply(function(x) strsplit(x, "-")) %>%
  unlist %>%
  plyr::count(.) %>%
  merge(dat[c("id.num", "name", "pos")], by.x = "x", by.y = "id.num") %>%
  arrange(desc(freq))
 
# Player frequency plot
pf <- ggplot(data = player.freq %>% subset(freq / (2*iter) >= 0.05)) +
  geom_bar(
    aes(x = factor(name, levels = rev(name)), y = freq / (2*iter), fill = pos),
    stat = "identity",
    colour = "black") +
  scale_fill_manual(
    name = "Position",
    values = paste0("grey", seq(20, 100, length.out = 6))) +
  coord_flip() +
  theme_classic() +
  labs(
    x = "Player",
    y = "Frequency") +
  theme(
    axis.title.x = element_text(vjust = 0),
    axis.title.y = element_text(vjust = 1),
    legend.position = c(0.85, 0.25))
pf
ggsave(paste0(dtex, "figures/player_freq_", week, ".pdf"), 
       pf, 
       height = 10, width = 10) 
