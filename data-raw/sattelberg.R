
# Read csv data set
sattelberg  <- utils::read.table("sattelberg.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
# Convert to zoo
sattelberg <- zoo::zoo(sattelberg[, -1L],
                 as.POSIXct(sattelberg[, 1L], tz = "UTC", origin = "1970-01-01"))
# Add to package data folder
usethis::use_data(sattelberg, version = 2)

