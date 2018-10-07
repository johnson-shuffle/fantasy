# ------------------------------------------------------------------------------
# Preample
# ------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

# ------------------------------------------------------------------------------
# Projections
# ------------------------------------------------------------------------------

# Positions
codes <- c(10, 20, 30, 40, 80)

# Table names
tbl.names <- list(
    c("CMP", "ATT","PAS.YDS","PAS.TDS","INT","CAR","RSH.YDS","RSH.TDS","PROJ"),
    c("CAR","RSH.YDS","RSH.TDS","REC","REC.YDS","REC.TDS","PROJ"),
    c("REC","REC.YDS","REC.TDS","PROJ"),
    c("REC","REC.YDS","REC.TDS","PROJ"),
    c("FGMADE","FGMISS","XPMADE","XPMISS","PROJ")
  )

# Loop
proj <- NULL

years <- c(2006:2014)
weeks <- c(1:17)

for (y in years) {
  for (w in weeks) {
    for (i in 1:5) {
      
      url1 <- "http://www.fftoday.com/rankings/playerwkproj.php?"
      url2 <- "&LeagueID=1"
      
      tbl <- url1 %>%
        paste0("Season=", y, "&GameWeek=", w, "&PosID=", codes[i], url2) %>%
        html_session() %>%
        html_table(fill = T, header = T) %>%
        extract2(11) %>%
        extract(-1)
      
      names(tbl) <- c("PLAYER", "TEAM", "OPP", tbl.names[[i]])

      tbl <- subset(tbl, TEAM != "Team") %>%
        mutate(
          YEAR = y,
          WEEK = w
        )
      
      proj <- plyr::rbind.fill(proj, tbl) ; rm(tbl)
    
    } # End loop through positions

  cat("*")

  } # End loop through weeks
} # End loop throuh years

proj[is.na(proj)] <- 0

proj[c(4:ncol(proj))] <- plyr::colwise(as.numeric)(proj[c(4:ncol(proj))])

proj <- proj %>%
  within({
    PROJ <- 
      ((1/25)*PAS.YDS + 4*PAS.TDS - 2*INT) +
      ((1/10)*REC.YDS + 6*REC.TDS + REC) +
      ((1/10)*RSH.YDS + 6*RSH.TDS) +
      3*FGMADE - FGMISS + XPMADE
    })

# Save
save(proj, file = paste0(d$data, "projections_fft.Rda"))
