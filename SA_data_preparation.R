##########
##########
# SA_data_preparation.R
# first: 20150806
# from csv to data.frame/tbl

##########
##########
### EXPLANATION
# !!! action to be added
# ??? action with doubt

##########
##########
### Data Source
# ASF
# Escota
# BO
### Remarkable points
# csv
#   sep = "," | ";"
#   S.O.
#     Entr
#     TimeEntr
#     voieSortie

##########
##########
### Escota
### from csv to data.frame to tbl
input <- read.table("BDD_ESCOTA.csv", sep = ",", header = TRUE)
input <- tbl_df(input)

names(input) <- c("pays", "ste", "ID", "badge", "sEntr", "cEntr", "voieEntr", "DateEntr", "hEntr", "sSor", "cSor", "voieSor", "hSor", "DateSor")

input <- input %>%
  mutate(
    Entr = 25000000 + sEntr * 1000 + cEntr,
    Sor = 25000000 + sSor * 1000 + cSor,
    Y = substr(DateSor, 1, 4), M = substr(DateSor, 5, 6), D = substr(DateSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", D)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    # HH = as.numeric(substr(hEntr, 1, 2)), MM = as.numeric(substr(hEntr, 3, 4)),
    HH = floor(hEntr / 10000), MM = floor((hEntr / 10000 - HH) * 100),
    TimeEntr = HH + MM / 60,
    # HH = as.numeric(substr(hEntr, 1, 2)), MM = as.numeric(substr(hEntr, 3, 4)),
    HH = floor(hEntr / 10000), MM = floor((hEntr / 10000 - HH) * 100),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, cEntr, Entr, Sor, voieSor, Date, DOW, WOY, TimeEntr, TimeSor) 

input[input$cEntr == 0, ]$Entr <- 0 
# ??? S.O. TimeEntr = TimeSor - 0.5
input[is.na(input$TimeEntr), ]$TimeEntr <- input[is.na(input$TimeEntr), ]$TimeSor - .5 

# Final
input <- input %>% select(ID, Entr, Sor, voieSor, Date, DOW, WOY, TimeEntr, TimeSor)

# get ID.ref
ID.ref <- read.table("ID.ref.csv", sep = ",", header = TRUE)
names(ID.ref)[c(1,3,6)] <- c("Nom", "NOM", "ID")
ID.ref <- ID.ref %>% select(Nom, ID)

# join Input & ID.ref
input.escota <-  inner_join(input, ID.ref)

##########
##########
### BO
# only in ASF
# S.O. always with a virtual Entr but no TimeEntr
input <- read.table("BDD_BO.csv", sep = ";", header = TRUE)
input <- tbl_df(input)

names(input) <- c("ID", "cEntr","cSor", "nEntr","nSor","DEntr", "DSor")

input <- input %>% 
  mutate(
    ID = substr(ID, 6, 12), # get the number of employee
    Entr = 25004000 + cEntr,
    Sor = 25004000 + cSor,
    Y = substr(DSor, 1, 4), M = substr(DSor, 5, 6),Day = substr(DSor, 7, 8),
    Date = as.Date(paste0(Y, "-", M, "-", Day)),
    DOW = as.POSIXlt(Date)$wday,
    WOY = as.numeric(format(Date+3, "%U")),
    HH = as.numeric(substr(DEntr, 9, 10)), MM = as.numeric(substr(DEntr, 11, 12)),
    TimeEntr = HH + MM / 60,
    HH = as.numeric(substr(DSor, 9, 10)), MM = as.numeric(substr(DSor, 11, 12)),
    TimeSor = HH + MM / 60
  ) %>%
  select(ID, cEntr, Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor)

# ??? S.O. TimeEntr = TimeSor - 0.5
input[is.na(input$TimeEntr), ]$TimeEntr <- input[is.na(input$TimeEntr), ]$TimeSor - .5 
# !!! add voieSor, Nom
input$voieSor <- 0
input$Nom <- "PM"

input.BO <- input %>% select(ID, Entr, Sor, voieSor, Date, DOW, WOY, TimeEntr, TimeSor, Nom)


##########
##########
### PM
PM.escota <- input.escota %>% filter(Nom == "PM")
PM.ASF <- input.BO

PM.escota$Societe <- "ESCOTA"
PM.ASF$Societe <- "ASF"

PM <- rbind(PM.escota, PM.ASF)