
viejas  <- utils::read.table("viejas.csv",  sep = ";",
                                na.strings = "NA",
                                header = TRUE, strip.white = TRUE)
usethis::use_data(viejas)

