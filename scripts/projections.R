#---------------------------------------------------------------------------------
# Preample
#---------------------------------------------------------------------------------
rm(list = ls())
if (file.exists("scripts/setup.R")) {
  source("scripts/setup.R")
} else if (file.exists("../scripts/setup.R")) {
  source("../scripts/setup.R")
}

#---------------------------------------------------------------------------------
# Projections
#---------------------------------------------------------------------------------
years <- c(2015)

proj <- NULL
for (y in years) {
  if (y < 2015) {
    max.week <- 17
  } else {
    max.week <- 12
  }
  for (w in 1:max.week) {
    p <- get.projection(w, y)
    proj <- rbind(proj, p)
    Sys.sleep(1)
    cat("*")
  }
}

# Save
save(proj, file = paste0(ddata, "projections.Rda"))
