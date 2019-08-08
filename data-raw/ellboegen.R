
ellboegen  <- utils::read.table("ellboegen.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
usethis::use_data(ellboegen)

