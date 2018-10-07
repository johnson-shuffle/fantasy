#---------------------------------------------------------------------------------
# Function to get game information for a given week and year
#---------------------------------------------------------------------------------
get.game <- function(week, year) {
  
  url <- "http://scores.espn.go.com/nfl/scoreboard?" %>%
    paste0(.,"year=",year,"&seasontype=2&week=",week)
  
  setwd(d$fantasy)
  writeLines(sprintf("
    var page = require('webpage').create();
    page.open('%s', function () {
      console.log(page.content); // page source
      phantom.exit();
    });", url), con = "scrape.js")
  system("phantomjs scrape.js > scrape.html")
  
  out <- read_html("scrape.html") %>% 
    html_nodes("script") %>% 
    extract(grep("window.espn.scoreboardData",lapply(.,xml_text))) %>% 
    xml_text %>% 
    strsplit(":") %>% 
    extract2(1) %>%
    extract(grep("boxscore\\?gameId",.)) %>%
    strsplit(",") %>% 
    unlist %>%
    extract(grep("boxscore\\?gameId",.)) %>%
    gsub("//","http://",.) %>%
    gsub('\\\"',"",.) %>%
    plyr::ldply(.) %>%
    mutate(
      YEAR = year,
      WEEK = week,
      GAME.LINK = V1,
      GAME.ID = sapply(V1,function(x) strsplit(x,"=")[[1]][2]),
      V1 = NULL
    ) %>%
    unique
  
  return(out)
  
}

#---------------------------------------------------------------------------------
# Function to get game stats given game information
#---------------------------------------------------------------------------------
get.stats <- function(info) {
  
  # Custom cbind.fill function
  cbind.fill <- function(x, y){
    while (nrow(x) != nrow(y)) {
      i <- 1
      while (x$PLAYER[i] == y$PLAYER[i]) {i <- i+1}
      old.y <- y
      y <- old.y[1:(i - 1), ]
      y[i, ] <- c(x$PLAYER[i], NA)
      y[(i + 1):(nrow(old.y) + 1), ] <- old.y[i:nrow(old.y), ]
    }
    out <- cbind(x, y$ESPN.LINK) %>%
      setNames(c("PLAYER", "CATEGORY", "ESPN.LINK"))
    return(out)
  }
  
  # Create an html session
  ses <- info$GAME.LINK %>% html_session
  
  # List of players references
  player.refs <- html_nodes(ses,"a[href^='http://espn.go.com/nfl/player/']")
  
  # List of tables
  tbls <- html_table(ses, fill = TRUE, header = FALSE)
  
  # Teams
  team.1 <- tbls[[1]][2,1]
  team.2 <- tbls[[1]][3,1]
  
  # Table categories
  tbl.cats <- c(
    rep("Passing", 2),
    rep("Rushing", 2),
    rep("Receiving", 2),
    rep("Fumbles", 2),
    rep("Defensive", 2),
    rep("Interceptions", 2),
    rep("Kick Returns", 2),
    rep("Punt Returns", 2),
    rep("Kicking", 2),
    rep("Punting", 2)
    )
  
  # Table names
  tbl.names <- list(
    c("C/ATT", "PAS.YDS", "PAS.AVG", "PAS.TD", "PAS.INT", "SACKED", "QBR", "RAT"),
    c("C/ATT", "PAS.YDS", "PAS.AVG", "PAS.TD", "PAS.INT", "SACKED", "QBR", "RAT"),
    c("CAR", "RSH.YDS", "RSH.AVG", "RSH.TD", "RSH.LONG"),
    c("CAR", "RSH.YDS", "RSH.AVG", "RSH.TD", "RSH.LONG"),
    c("REC", "REC.YDS", "REC.AVG", "REC.TD", "REC.LONG", "TGTS"),
    c("REC", "REC.YDS", "REC.AVG", "REC.TD", "REC.LONG", "TGTS"),
    c("FUM", "LOST", "RECV"),
    c("FUM", "LOST", "RECV"),
    c("TOT", "SOLO", "SACKS", "TFL", "PD", "QB.HITS", "DFS.TD"),
    c("TOT", "SOLO", "SACKS", "TFL", "PD", "QB.HITS", "DFS.TD"),
    c("DFS.INT", "INT.YDS", "INT.TD"),
    c("DFS.INT", "INT.YDS", "INT.TD"),
    c("KRT.NO", "KRT.YDS", "KRT.AVG", "KRT.LONG", "KRT.TD"),
    c("KRT.NO", "KRT.YDS", "KRT.AVG", "KRT.LONG", "KRT.TD"),
    c("PRT.NO", "PRT.YDS", "PRT.AVG", "PRT.LONG", "PRT.TD"),
    c("PRT.NO", "PRT.YDS", "PRT.AVG", "PRT.LONG", "PRT.TD"),
    c("FG", "KCK.PCT", "KCK.LONG", "XP", "KCK.PTS"),
    c("FG", "KCK.PCT", "KCK.LONG", "XP", "KCK.PTS"),
    c("PNT.NO", "PNT.YDS", "PNT.AVG", "PNT.TB", "PNT.IN20", "PNT.LONG"),
    c("PNT.NO", "PNT.YDS", "PNT.AVG", "PNT.TB", "PNT.IN20", "PNT.LONG")
    )
  
  # Team stats
  Team <- paste0("http://espn.go.com/nfl/matchup?gameId=", info$GAME.ID) %>%
    html_session %>%
    html_table %>%
    extract2(2) %>%
    setNames(c("stat", team.1, team.2)) %>%
    melt(id = "stat") %>% 
    unique %>%
    dcast(variable ~ stat) %>%
    rename(Team = variable) %>%
    within(GAME.ID <- info$GAME.ID)
  
  # List of players from each of the tables
  player.cats <- NULL
  for (j in 2:21) {
    df <- tbls[[j]] %>%
      mutate(tbl.cat = tbl.cats[[j - 1]]) %>%
      extract(c(1, ncol(.))) %>%
      setNames(c("PLAYER", "CATEGORY")) %>%
      subset(PLAYER != "TEAM")    
    player.cats <- rbind(player.cats, df)
    rm(df)
  }
  player.cats <- player.cats %>%
    subset(substr(PLAYER, 1, 3) != "No ") %>%
    subset(PLAYER != "")
  
  # Collect links for each individual player's website
  player.links <- cbind(
    plyr::ldply(player.refs, function(x) trim(xml_text(x))),
    plyr::ldply(player.refs, function(x) trim(xml_attrs(x)[["href"]]))
    ) %>%
    setNames(c("PLAYER", "ESPN.LINK"))

  # Final list of players for each of the tables
  players <- cbind.fill(player.cats, player.links) %>%
    subset(PLAYER != "")

  # Collect stats from each of the tables
  tbls.loop <- tbls
  for (k in c(2:(length(tbls) - 2))[2:(length(tbls) - 2) %% 2 == 0]) {
    
    # Even-numbered table
    if (ncol(tbls.loop[[k]]) <= 1) {
      tbls.loop[[k]] <- NA %>% data.frame %>% setNames("PLAYER")
    } else {
      tbls.loop[[k]] <- tbls.loop[[k]][1:(length(tbl.names[[k - 1]]) + 1)]
      names(tbls.loop[[k]]) <- c("PLAYER", tbl.names[[k - 1]])
      tbls.loop[[k]] <- tbls.loop[[k]] %>%
        mutate(
          TEAM = team.1,
          OPPONENT = team.2,
          WEEK = info$WEEK,
          YEAR = info$YEAR,
          GAME.ID = info$GAME.ID
        ) 
    }
    
    # Odd-numbered table
    if (ncol(tbls.loop[[k + 1]]) <= 1) {
      tbls.loop[[k + 1]] <- NA %>% data.frame
    } else {
      tbls.loop[[k + 1]] <- tbls.loop[[k + 1]][1:(length(tbl.names[[k - 1]]) + 1)]
      names(tbls.loop[[k + 1]]) <- c("PLAYER", tbl.names[[k - 1]])
      tbls.loop[[k + 1]] <- tbls.loop[[k + 1]] %>%
        mutate(
          TEAM = team.2,
          OPPONENT = team.1,
          WEEK = info$WEEK,
          YEAR = info$YEAR,
          GAME.ID = info$GAME.ID
        ) 
    }
    
    # Bind together and clean
    tbl <- plyr::rbind.fill(tbls.loop[[k]],tbls.loop[[k + 1]]) %>%
      subset(PLAYER %in% players$PLAYER) 
    
    # Add additional information: game ID, matchup ID, player link/ID
    if (nrow(tbl) != 0) {
      tbl <- tbl %>%
        within({
          ESPN.LINK <- 
            subset(players,CATEGORY == tbl.cats[[k - 1]])$ESPN.LINK
          ESPN.ID <- 
            sapply(ESPN.LINK,function(x) strsplit(x,"\\/")[[1]][8])
        })
      assign(tbl.cats[[k]],tbl)
    }
    
    rm(tbl)
    
  } # End of loop through tables
  
  # Identifiers
  key <- c(
    "PLAYER", 
    "ESPN.ID", 
    "ESPN.LINK", 
    "TEAM",
    "OPPONENT", 
    "YEAR", 
    "WEEK", 
    "GAME.ID")
  
  # Names of tables
  tbl.list <- c(
    "Passing",
    "Rushing",
    "Receiving",
    "Kicking",
    "Defensive",
    "Kick Returns",
    "Punt Returns",
    "Fumbles",
    "Interceptions")
  tbl.list <- tbl.list[tbl.list %in% ls()]
  
  # Combine individual stats
  Individual <- Passing
  for (i in 2:length(tbl.list)) {
    Individual <- merge(Individual, get(tbl.list[i]), by = key, all = T) %>% 
      unique
  }
  
  # Output
  out <- c(
    "Team",
    "Individual",
    "Passing",
    "Rushing",
    "Receiving",
    "Kicking",
    "Defensive",
    "Kick Returns",
    "Punt Returns",
    "Fumbles",
    "Interceptions")
  out <- out[out %in% ls()]
  mget(out)
  
}

#---------------------------------------------------------------------------------
# Function to get player info given player id from game stats and rosters
#---------------------------------------------------------------------------------
get.player <- function(ESPN.ID) {
  
  page <- paste0("http://espn.go.com/nfl/player/stats/_/id/", ESPN.ID)
  ses <- html_session(page)
  pos <- html_nodes(ses, "ul[class^='general-info']")
  info <- xml_children(html_nodes(ses, "ul[class^='player-metadata']"))
  
  # Check and add information
  POSITION <- NA
  BIRTHDAY <- NA
  COLLEGE <- NA
  DRAFT.YR <- NA
  DRAFT.RD <- NA
  DRAFT.PK <- NA
  
  if (!is.na(xml_text(xml_children(pos[[1]]))[1])) {
    POSITION <- gsub("^#[0-9]*", "", xml_text(xml_children(pos[[1]])[1])) %>%
      trim
    if (POSITION == "Center") {
      POSITION <- "C"
    } else if (POSITION == "Cornerback") {
      POSITION <- "CB"
    } else if (POSITION == "Defensive Back") {
      POSITION <- "DB"
    } else if (POSITION == "Defensive End") {
      POSITION <- "DE"
    } else if (POSITION == "Defensive Lineman") {
      POSITION <- "DL"
    } else if (POSITION == "Defensive Tackle") {
      POSITION <- "DT"
    } else if (POSITION == "Free Safety") {
      POSITION <- "FS"
    } else if (POSITION == "Fullback") {
      POSITION <- "FB"
    } else if (POSITION == "Guard") {
      POSITION <- "G"
    } else if (POSITION == "Linebacker") {
      POSITION <- "LB"
    } else if (POSITION == "Long Snapper") {
      POSITION <- "LS"
    } else if (POSITION == "Nose Tackle") {
      POSITION <- "NT"
    } else if (POSITION == "Offensive Guard") {
      POSITION <- "G"
    } else if (POSITION == "Offensive Lineman") {
      POSITION <- "OL"
    } else if (POSITION == "Offensive Tackle") {
      POSITION <- "OT"
    } else if (POSITION == "Place kicker") {
      POSITION <- "PK"
    } else if (POSITION == "Punter") {
      POSITION <- "P"
    } else if (POSITION == "Quarterback") {
      POSTION <- "QB"
    } else if (POSITION == "Running Back") {
      POSITION <- "RB"
    } else if (POSITION == "Safety") {
      POSITION <- "S"
    } else if (POSITION == "Strong Safety") {
      POSITION <- "SS"
    } else if (POSITION == "Tight End") {
      POSITION <- "TE"
    } else if (POSITION == "Wide Receiver") {
      POSITION <- "WR"
    } 
  }
  
  if (grepl("Born", paste(xml_children(info), collapse = " "))) {
    val <- grep("Born", xml_children(info))
    BIRTHDAY <- gsub("Born", "", xml_text(info)[val]) %>%
      trim
    BIRTHDAY <- paste(
      strsplit(BIRTHDAY, " ")[[1]][1],
      strsplit(BIRTHDAY, " ")[[1]][2],
      strsplit(BIRTHDAY, " ")[[1]][3],
      sep = " ")
  }
  
  if (grepl("College", paste(xml_children(info), collapse = " "))) {
    val <- grep("College", xml_children(info))
    COLLEGE <- gsub("College","", xml_text(info)[val]) %>% 
      trim
  }
  
  if (grepl("Draft", paste(xml_children(info), collapse = " "))) {
    val <- grep("Draft",xml_children(info))
    DRAFT.YR <- gsub(":","",strsplit(xml_text(info)[val]," ")[[1]][3]) %>%
      trim %>%
      as.numeric
    DRAFT.RD <- gsub(":", "", strsplit(xml_text(info)[val], " ")[[1]][4]) %>%
      gsub("st", "", .) %>%
      gsub("nd", "", .) %>%
      gsub("rd", "", .) %>%
      gsub("th", "", .) %>%
      trim %>%
      as.numeric
    DRAFT.PK <- gsub(":", "", strsplit(xml_text(info)[val], " ")[[1]][6]) %>%
      gsub("st", "", .) %>%
      gsub("nd", "", .) %>%
      gsub("rd", "", .) %>%
      gsub("th", "", .) %>%
      trim %>%
      as.numeric
  }
  
  out <- list(
    POSITION = POSITION,
    BIRTHDAY = BIRTHDAY,
    COLLEGE = COLLEGE,
    DRAFT.YR = DRAFT.YR,
    DRAFT.RD = DRAFT.RD,
    DRAFT.PK = DRAFT.PK
    )
  
}

#---------------------------------------------------------------------------------
# Function to get fantasy predictions from fftoday.com and fantasypros.com
#---------------------------------------------------------------------------------
get.projection <- function(week, year) {
  
  # Codes
  codes <- c("qb", "rb", "wr", "te", "k")
  
  tbl.names <- list(
    c("ATT","CMP","PAS.YDS","PAS.TDS","INT","CAR","RSH.YDS","RSH.TDS","LOST","PTS"),
    c("CAR","RSH.YDS","RSH.TDS","REC","REC.YDS","REC.TDS","LOST","PTS"),
    c("CAR","RSH.YDS","RSH.TDS","REC","REC.YDS","REC.TDS","LOST","PTS"),
    c("REC","REC.YDS","REC.TDS","LOST","PTS"),
    c("FGM","FGA","XPM","PTS")
  )
  
  # Loop through positions
  for (i in 1:5) {
    
    # Get table data
    url <- "http://www.fantasypros.com/nfl/projections/"
    trs <- paste0(url, codes[i], ".php?week=", week) %>%
      html_session %>%
      html_nodes("tr[class^='mpb-available']")
    
    # Pre-allocate
    n <- length(trs)
    k <- length(tbl.names[[i]])
    dat <- matrix(rep(NA, n*(3 + k)), nrow = n) %>%
      data.frame %>%
      setNames(c("PLAYER", "FP.ID", "TEAM", tbl.names[[i]]))
    
    # Loop through positions
    for (p in 1:n) {
      l <- trs[[p]] %>%
        xml_children %>% 
        extract(1) %>%
        xml_children %>%
        length %>%
        min(., 3)
      
      dat$PLAYER[p] <- trs[[p]] %>%
        xml_children %>% 
        extract(1) %>%
        xml_children %>%
        extract(l) %>% 
        xml_attrs %>%
        extract2(1) %>%
        extract("fp-player-name")
      
      dat$FP.ID[p] <- trs[[p]] %>%
        xml_children %>% 
        extract(1) %>%
        xml_children %>%
        extract(l) %>% 
        xml_attrs %>%
        extract2(1) %>%
        extract("class") %>%
        strsplit("-") %>%
        extract2(1) %>%
        extract(5)
      
      if (l == 3) {
        dat$TEAM[p] <- trs[[p]] %>%
          xml_children %>% 
          extract(1) %>%
          xml_children %>%
          extract(2) %>%
          xml_text
      }
      
      for (v in 1:k) {
        dat[p, v + 3] <- trs[[p]] %>%
          xml_children %>%
          extract(v + 1) %>%
          xml_text %>%
          as.numeric
      }
      
    } # End loop through players  
    
    assign(codes[i], dat)
    
  } # End loop through positions
  
  proj <- plyr::rbind.fill(qb, rb, wr, te, k) %>%
    mutate(
      YEAR = year,
      WEEK = week
      ) %>%
    select(-PTS)
  
  rm(qb, rb, wr, te, k)
  
  proj$TEAM <- gsub("JAC", "JAX", proj$TEAM)
  proj$TEAM <- gsub("WAS", "WSH", proj$TEAM)
  
  proj[is.na(proj)] <- 0
  
  proj[c(2,4:ncol(proj))] <- plyr::colwise(as.numeric)(proj[c(2,4:ncol(proj))])
  
  proj <- proj %>%
    within({
      PROJ <- 
        ((1/25)*PAS.YDS + 4*PAS.TDS - 2*INT) +
        ((1/10)*REC.YDS + 6*REC.TDS + REC) +
        ((1/10)*RSH.YDS + 6*RSH.TDS - 2*LOST) +
        3*FGM - (FGA - FGM) + XPM
    })
  
}

#---------------------------------------------------------------------------------
# Wrapper function to update everything
#---------------------------------------------------------------------------------
update.stats <- function(week, year) {
  
  # Load files to be updated
  load(file = paste0(d$data, "espn_info_games.Rda"))
  load(file = paste0(d$data, "espn_stats_games.Rda"))
  load(file = paste0(d$data, "espn_info_players.Rda"))
  load(paste0(d$data, "projections_fp.Rda"))
  
  # Update game info
  game.info <- get.game(week, year) %>% 
    rbind(game.info, .) %>% 
    unique
  
  save(game.info, file = paste0(d$data, "espn_info_games.Rda"))
  
  # Update game stats
  for (i in subset(game.info, WEEK == week & YEAR == year)$GAME.ID) {
    
    pos <- grep(i, game.info$GAME.ID)
    
    check <- game.info$GAME.LINK[pos] %>%
      html_session %>%
      html_table(fill = TRUE, header = FALSE)
    
    if (length(check) < 21) {
      break
    } else {
      foo <- get.stats(game.info[pos, ])
      foo.names <- names(foo)
    }
    
    team <- plyr::rbind.fill(team, foo$Team) %>% 
      unique
    indiv <- plyr::rbind.fill(indiv, foo$Individual) %>% 
      unique
    
    cat("*")
    
  }
  
  converts <- c(
    "PLAYER", 
    "ESPN.ID", 
    "ESPN.LINK", 
    "TEAM",
    "OPPONENT", 
    "YEAR", 
    "WEEK", 
    "GAME.ID",
    names(players),
    "C/ATT",
    "FG",
    "XP") 
  
  converts <- names(indiv)[!names(indiv) %in% converts]
  for (i in converts) {
    indiv[,i] <- as.numeric(indiv[,i])
    indiv[,i][is.na(indiv[,i])] <- 0
  }
  
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
  
  indiv <- within(indiv,{
    `C/ATT`[is.na(`C/ATT`)] <- "0/0"
    FG[is.na(FG)] <- "0/0"
    XP[is.na(XP)] <- "0/0"
    PTS <- 
      ((1/25)*PAS.YDS + 4*PAS.TD - 2*PAS.INT) +
      ((1/10)*REC.YDS + 6*REC.TD + REC) + 
      ((1/10)*RSH.YDS + 6*RSH.TD - 2*LOST) +
      (3*kicking(FG)$made - kicking(FG)$miss + kicking(XP)$made) +
      (SACKS + 6*DFS.TD + 2*DFS.INT + 2*RECV) 
    })
  
  save(list = c("team", "indiv"), file = paste0(d$data, "espn_stats_games.Rda"))
  
  # Update player info
  new.players <- indiv %>%
    select(PLAYER, ESPN.ID, ESPN.LINK) %>%
    unique %>%
    subset(!ESPN.ID %in% players$ESPN.ID)
  
  if (nrow(new.players) > 0) {
    
    new.players$POSITION <- NA
    new.players$BIRTHDAY <- NA
    new.players$COLLEGE <- NA
    new.players$DRAFT.YR <- NA
    new.players$DRAFT.RD <- NA
    new.players$DRAFT.PK <- NA
    
    for (i in 1:nrow(new.players)) {
      foo <- get.player(new.players$ESPN.ID[i])
      new.players$POSITION[i] <- foo$POSITION
      new.players$BIRTHDAY[i] <- foo$BIRTHDAY
      new.players$COLLEGE[i] <- foo$COLLEGE
      new.players$DRAFT.YR[i] <- foo$DRAFT.YR
      new.players$DRAFT.RD[i] <- foo$DRAFT.RD
      new.players$DRAFT.PK[i] <- foo$DRAFT.PK
      cat("+")  
    }
    
  }
  
  players <- rbind(players, new.players) %>% 
    unique
  
  save(players, file = paste0(d$data, "espn_info_players.Rda"))
  
  # Update projections
  proj <- get.projection(week + 1, year) %>% 
    rbind(proj, .) %>% 
    unique
  
  save(proj, file = paste0(d$data, "projections_fp.Rda"))
  
}
