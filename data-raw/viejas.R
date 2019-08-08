
# Read csv data set
viejas  <- utils::read.table("viejas.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
# Convert to zoo
viejas <- zoo::zoo(viejas[, -1L],
                 as.POSIXct(viejas[, 1L], tz = "UTC", origin = "1970-01-01"))
# Add to package data folder
usethis::use_data(viejas, version = 2)

