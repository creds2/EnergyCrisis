library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

elec_all <- read.csv("data/electric/Postcode_level_all_meters_electricity_2020.csv")
elec_stand1 <- read.csv("data/electric/Postcode_Level_Standard_Electricity_2020_A_to_K.csv")
elec_stand2 <- read.csv("data/electric/Postcode_Level_Standard_Electricity_2020_K_to_Z.csv")
elec_eco7 <- read.csv("data/electric/Postcode_Level_Economy_7_Electricity_2020.csv")

names(elec_all) <- c("POSTCODE","elec_meters","elec_kwh_total","elec_kwh_mean","elec_kwh_median")
names(elec_stand1) <- c("POSTCODE","elec_meters","elec_kwh_total","elec_kwh_mean","elec_kwh_median")
names(elec_stand2) <- c("POSTCODE","elec_meters","elec_kwh_total","elec_kwh_mean","elec_kwh_median")
names(elec_eco7) <- c("POSTCODE","elec_meters","elec_kwh_total","elec_kwh_mean","elec_kwh_median")

elec_all$elec_kwh_total <- round(elec_all$elec_kwh_total)
elec_all$elec_kwh_mean <- round(elec_all$elec_kwh_mean)
elec_all$elec_kwh_median <- round(elec_all$elec_kwh_median)

elec_stand1$elec_kwh_total <- round(elec_stand1$elec_kwh_total)
elec_stand1$elec_kwh_mean <- round(elec_stand1$elec_kwh_mean)
elec_stand1$elec_kwh_median <- round(elec_stand1$elec_kwh_median)

elec_stand2$elec_kwh_total <- round(elec_stand2$elec_kwh_total)
elec_stand2$elec_kwh_mean <- round(elec_stand2$elec_kwh_mean)
elec_stand2$elec_kwh_median <- round(elec_stand2$elec_kwh_median)

elec_eco7$elec_kwh_total <- round(elec_eco7$elec_kwh_total)
elec_eco7$elec_kwh_mean <- round(elec_eco7$elec_kwh_mean)
elec_eco7$elec_kwh_median <- round(elec_eco7$elec_kwh_median)

summary(elec_stand1$POSTCODE %in% elec_all$POSTCODE)
summary(elec_stand1$POSTCODE %in% elec_stand2$POSTCODE)

elec_stand1 <- elec_stand1[!elec_stand1$POSTCODE %in% elec_all$POSTCODE,]
elec_stand2 <- elec_stand2[!elec_stand2$POSTCODE %in% elec_all$POSTCODE,]
elec_stand <- rbind(elec_stand1, elec_stand2)
elec_eco7 <- elec_eco7[!elec_eco7$POSTCODE %in% elec_all$POSTCODE,]

elec_all$type <- "all"
elec_stand$type <- "standard"
elec_eco7$type <- "eco7"

summary(elec_stand$POSTCODE %in% elec_eco7$POSTCODE)

elec <- rbind(elec_all, elec_stand, elec_eco7)
summary(duplicated(elec$POSTCODE))

foo <- unique(elec$POSTCODE[duplicated(elec$POSTCODE)])
foo <- elec[elec$POSTCODE %in% foo,]

elec <- elec[nchar(elec$POSTCODE) >= 6, ]
summary(duplicated(elec$POSTCODE))

saveRDS(elec,"data/electric/electric_merged.Rds")
