
# Read csv data set
ellboegen  <- utils::read.table("ellboegen.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
# Convert to zoo
ellboegen <- zoo::zoo(ellboegen[, -1L],
                 as.POSIXct(ellboegen[, 1L], tz = "UTC", origin = "1970-01-01"))
# Add to package data folder
usethis::use_data(ellboegen, version = 2)

