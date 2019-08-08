
luckyfive  <- utils::read.table("luckyfive.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
usethis::use_data(luckyfive)

