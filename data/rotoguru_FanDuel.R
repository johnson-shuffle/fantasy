# ------------------------------------------------------------------------------
# Preample
# ------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

# ------------------------------------------------------------------------------
# Fan Duel data from rotoguru.com
# ------------------------------------------------------------------------------
url1 <- "http://rotoguru1.com/cgi-bin/fyday.pl?week="
url2 <- "&year="
url3 <- "&game=fd&scsv=1"

fd.data <- NULL

for (y in 2011:2015) {
  for (w in 1:17) {

    url <- paste0(url1, w, url2, y, url3)
    
    dat <- html_session(url) %>>% 
      html_nodes("pre") %>>% 
      xml_text() %>>%
      strsplit("\n") %>>%
      extract2(1) %>>%
      data.frame() %>>%
      setNames("data") %>>%
      mutate(
        week = sapply(data, function(x) strsplit(x, ";")[[1]][1]),
        year = sapply(data, function(x) strsplit(x, ";")[[1]][2]),
        gid = sapply(data, function(x) strsplit(x, ";")[[1]][3]),
        name = sapply(data, function(x) strsplit(x, ";")[[1]][4]),
        pos = sapply(data, function(x) strsplit(x, ";")[[1]][5]),
        team = sapply(data, function(x) strsplit(x, ";")[[1]][6]),
        home = sapply(data, function(x) strsplit(x, ";")[[1]][7]),
        home = gsub("a", 0, home),
        home = gsub("h", 1, home),
        opp = sapply(data, function(x) strsplit(x, ";")[[1]][8]),
        fd.points = sapply(data, function(x) strsplit(x, ";")[[1]][9]),
        fd.salary = sapply(data, function(x) strsplit(x, ";")[[1]][10]),
        data = NULL
      ) %>>%
      subset(gid != "GID")
    
    fd.data <- rbind(fd.data, dat)
    rm(dat)
    cat("*")
    
  } # End week loop
} # End year loop

save(fd.data, file = paste0(d$data, "rotoguru_FanDuel.Rda"))

# ------------------------------------------------------------------------------
# Link to espn
# ------------------------------------------------------------------------------
load(paste0(d$data, "espn_info_players.Rda"))
load(paste0(d$data, "guru_fan_duel.Rda"))

fd.data$PLAYER <- paste(
  sapply(fd.data$name, function(x) strsplit(x, ",")[[1]][2]),
  sapply(fd.data$name, function(x) strsplit(x, ",")[[1]][1]),
  sep = " ") %>>% 
  str_trim()

fd.data$PLAYER[!fd.data$PLAYER %in% players$PLAYER] %>>% 
  unique() %>>%
  subset(!substr(., 1 ,2) == "NA")
