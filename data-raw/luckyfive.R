
# Read csv data set
luckyfive  <- utils::read.table("luckyfive.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
# Convert to zoo
luckyfive <- zoo::zoo(luckyfive[, -1L],
                 as.POSIXct(luckyfive[, 1L], tz = "UTC", origin = "1970-01-01"))
# Add to package data folder
usethis::use_data(luckyfive, version = 2)

