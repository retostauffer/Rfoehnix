# -------------------------------------------------------------------
# - NAME:        create_data.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2019-09-11
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2019-09-11, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2019-09-11 18:42 on marvin
# -------------------------------------------------------------------


# Creates some small csv files in "data" used for the
# "how to > import data" vignette.
library("foehnix")
ell <- demodata("ellboegen")[1:100]
ell[3, c("dd", "ff")] <- NA
sat <- demodata("sattelberg")[5:95]

# -------------------------------------------------------------------
# Demo A
# -------------------------------------------------------------------
# zoo to data.frame
zoo2df <- function(x) {
    x <- cbind(data.frame(date_time = strftime(index(x), "%Y-%m-%d %H:%M:%S")), as.data.frame(x))
    rownames(x) <- NULL
    return(x)
}

# Default csv file
demo_ell <- zoo2df(ell)
demo_sat <- zoo2df(sat)
library("gdata")
write.csv(demo_ell, "data/ellboegen_A.csv",  row.names = FALSE)
write.csv(demo_sat, "data/sattelberg_A.csv", row.names = FALSE)

library("zoo")
read.zoo("data/ellboegen_A.csv", format = "%Y-%m-%d %H:%M:%S",
         tz = "UTC", sep = ",", header = TRUE)


# -------------------------------------------------------------------
# Demo B
# -------------------------------------------------------------------
# zoo to data.frame
zoo2df <- function(x) {
    x <- cbind(as.data.frame(x), data.frame(date_time = strftime(index(x), "%Y%m%d%H%M%S")))
    rownames(x) <- NULL
    return(x)
}

# Default csv file
demo_ell <- zoo2df(ell)
demo_sat <- zoo2df(sat)
library("gdata")
write.fwf(demo_ell, "data/ellboegen_B.csv", na = "missing")
write.fwf(demo_sat, "data/sattelberg_B.csv", na = "missing")

##library("zoo")
##read.zoo("data/ellboegen_B.csv", format = "%Y%m%d%H%M", tz = "UTC", #FUN = as.POSIXct,
##         header = TRUE, na.strings = "missing")


##### -------------------------------------------------------------------
##### Demo C
##### -------------------------------------------------------------------
##### zoo to data.frame
####zoo2df <- function(x) {
####    x <- cbind(data.frame(timestamp = as.numeric(index(x))), as.data.frame(x))
####    rownames(x) <- NULL
####    return(x)
####}
####
##### Default csv file
####demo_ell <- zoo2df(ell)
####demo_sat <- zoo2df(sat)
####write.csv(demo_ell, "data/ellboegen_C.csv")
####write.csv(demo_sat, "data/sattelberg_C.csv")



