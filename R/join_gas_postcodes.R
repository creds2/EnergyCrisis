library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

gas <- read.csv("data/gas/Postcode_level_gas_2020.csv")
names(gas) <- c("POSTCODE","gas_meters","gas_kwh_total","gas_kwh_mean","gas_kwh_median")

gas$gas_kwh_total <- round(gas$gas_kwh_total)
gas$gas_kwh_mean <- round(gas$gas_kwh_mean)
gas$gas_kwh_median <- round(gas$gas_kwh_median)

elec <- read.csv("data/electric/Postcode_level_all_meters_electricity_2020.csv")
names(elec) <- c("POSTCODE","elec_meters","elec_kwh_total","elec_kwh_mean","elec_kwh_median")

elec$elec_kwh_total <- round(elec$elec_kwh_total)
elec$elec_kwh_mean <- round(elec$elec_kwh_mean)
elec$elec_kwh_median <- round(elec$elec_kwh_median)

postcodes_all <- readRDS("data/postcode_areas_all.Rds")
postcodes_all <- st_as_sf(postcodes_all)

summary(elec$POSTCODE %in% gas$POSTCODE)
summary(gas$POSTCODE %in% elec$POSTCODE)

gaselec <- full_join(gas, elec, by = "POSTCODE")

gaselec$nchar <- nchar(gaselec$POSTCODE)
gaselec <- gaselec[gaselec$POSTCODE != "Unallocated",]

table(gaselec$nchar)
table(nchar(postcodes_all$POSTCODE))

gaselec$coverage <- "None"
gaselec$coverage[!is.na(gaselec$gas_meters) & !is.na(gaselec$elec_meters)] <- "both"
gaselec$coverage[!is.na(gaselec$gas_meters) & is.na(gaselec$elec_meters)] <- "gas"
gaselec$coverage[is.na(gaselec$gas_meters) & !is.na(gaselec$elec_meters)] <- "elec"

table(gaselec$coverage)

gaselec_notpc <- gaselec[!gaselec$nchar %in% 6:8,]
gaselec_pc <- gaselec[gaselec$nchar %in% 6:8,]

table(gaselec_notpc$coverage)
table(gaselec_pc$coverage)


summary(gaselec_pc$POSTCODE %in% postcodes_all$POSTCODE)
summary(postcodes_all$POSTCODE %in% gaselec_pc$POSTCODE)
# 17685 invlaid postcodes?
foo = gaselec_pc[!gaselec_pc$POSTCODE %in% postcodes_all$POSTCODE,]
bar = st_drop_geometry(postcodes_all[!postcodes_all$POSTCODE %in% gaselec_pc$POSTCODE,])
bar$geometry <- NULL

# outcode <- strsplit(postcodes_all$POSTCODE," ")
# outcode <- sapply(outcode, `[[`, 1)
# 
# postcodes_all$outcode <- outcode
# summary(gas_notpc$POSTCODE %in% postcodes_all$outcode)

# outcodes <- postcodes_all[postcodes_all$outcode %in% gas_notpc$POSTCODE,]
# outcodes$POSTCODE <- NULL
# outcodes <- outcodes %>%
#   group_by(outcode) %>%
#   summarise(do_union = FALSE)
# 
# outcodes = st_buffer(outcodes,0)
# 
# saveRDS(outcodes, "data/outcode_areas.Rds")

outcodes <- readRDS("data/outcode_areas.Rds")
outcodes <- outcodes[outcodes$outcode %in% gaselec_notpc$POSTCODE,]

outcodes_dis <- list()

outcode <- strsplit(gaselec_pc$POSTCODE," ")
outcode <- sapply(outcode, `[[`, 1)
gaselec_pc$outcode <- outcode

for(i in 1:nrow(outcodes)){
  oc <- outcodes[i,]
  if(i %% 100 == 0){
    message(i," ",oc$outcode)
  }
  
  pc <- gaselec_pc[gaselec_pc$outcode == oc$outcode,]
  pc <- postcodes_all[postcodes_all$POSTCODE %in% pc$POSTCODE,]
  
  #qtm(oc, fill = NULL) + qtm(pc)
  pc <- st_combine(pc$geometry)
  
  oc_dis <- st_difference(oc, pc)
  outcodes_dis[[i]] <- oc_dis
}

outcodes_dis <- dplyr::bind_rows(outcodes_dis)
names(outcodes_dis) <- names(postcodes_all)
postcodes_all <- rbind(postcodes_all, outcodes_dis)


gaselec_pc <- left_join(gaselec_pc, postcodes_all, by = "POSTCODE")
gaselec_notpc <- left_join(gaselec_notpc, postcodes_all, by = "POSTCODE")
gaselec_pc$outcode <- NULL

gaselec_all <- rbind(gaselec_pc, gaselec_notpc)

gaselec_all <- gaselec_all[!st_is_empty(gaselec_all$geometry),]
gaselec_all <- st_as_sf(gaselec_all)

# qtm(gaselec_all[nrow(gaselec_all),])
# foo <- gaselec_all[substr(gaselec_all$POSTCODE,1,3) == "YO8",]
# bar <- foo[foo$POSTCODE == "YO8",]
# foo <- foo[foo$POSTCODE != "YO8",]
# qtm(foo, fill = "red") + qtm(bar, fill = "blue")
# 
# qtm(gas_pc[1:1000,], fill = "gas_kwh_median")

gaselec_all <- gaselec_all[,c("POSTCODE",
                         "gas_meters","gas_kwh_total","gas_kwh_mean","gas_kwh_median",
                         "elec_meters","elec_kwh_total","elec_kwh_mean","elec_kwh_median",
                         "geometry")]
gaselec_all[2:9] <- lapply(st_drop_geometry(gaselec_all[2:9]), as.integer)
gaselec_all <- st_transform(gaselec_all, 4326)
summary(st_is_valid(gas_pc))

st_precision(gaselec_all) <- 1000000
st_write(gaselec_all, "data/tiles/gas_electric_postcodes.geojson", delete_dsn = TRUE)

quantile(gaselec_all$elec_kwh_median, probs = seq(0,1,0.1), na.rm = TRUE)
quantile(gaselec_all$gas_kwh_median, probs = seq(0,1,0.1), na.rm = TRUE)


