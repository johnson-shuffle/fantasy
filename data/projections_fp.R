# --------------------------------------------------------------------------------
# Preample
# --------------------------------------------------------------------------------
rm(list = ls())
source("~/GoogleDrive/Research/Fantasy/fantasy.Rprofile")

# --------------------------------------------------------------------------------
# Projections
# --------------------------------------------------------------------------------
years <- 2015
weeks <- c(1:12)

proj <- NULL
for (w in weeks) {
  
  p <- get.projection(w, years) %>%
    mutate(
      YEAR = years,
      WEEK = w
      )
  
  proj <- rbind(proj, p)

  Sys.sleep(1)
  cat("*")

}

# Save
save(proj, file = paste0(d$data, "projections_fp.Rda"))
