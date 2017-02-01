library(readr)
library(tibble)

# Collect folder information and move into the raw data folder
WD <- getwd()
devtools::wd(".", "data-raw")

# Convert csv files to tibbles in package
tz_keywords <- read_csv("AllKeywords.csv")
devtools::use_data(tz_keywords, overwrite = TRUE)

tz_metadata <- read_csv("AllMetadata.csv")
devtools::use_data(tz_metadata, overwrite = TRUE)

tz_location <- read_csv("AllLocations.csv")
devtools::use_data(tz_location, overwrite = TRUE)

devtools::document()
devtools::install(local=FALSE)

# Go back to where you were
setwd(WD)
