
sattelberg <- utils::read.table("sattelberg.csv", sep = ";", 
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
usethis::use_data(sattelberg)

