
library(dplyr)
library(sf)
library(lubridate)

certs <- readRDS("data/epc_all_clean.Rds")

certs <- certs[,c("BUILDING_REFERENCE_NUMBER","ADDRESS1","CURRENT_ENERGY_RATING","CURRENT_ENERGY_EFFICIENCY","POTENTIAL_ENERGY_EFFICIENCY","UPRN",
                  "INSPECTION_DATE","BUILT_FORM","PROPERTY_TYPE","TENURE","CONSTRUCTION_AGE_BAND",
                  "TOTAL_FLOOR_AREA","MAIN_FUEL","MAINHEAT_DESCRIPTION",
                  "GLAZED_TYPE",
                  "FLOOR_DESCRIPTION","FLOOR_ENERGY_EFF",
                  "HOTWATER_DESCRIPTION","HOT_WATER_ENERGY_EFF",
                  "WINDOWS_DESCRIPTION","WINDOWS_ENERGY_EFF",
                  "WALLS_DESCRIPTION","WALLS_ENERGY_EFF",
                  "ROOF_DESCRIPTION","ROOF_ENERGY_EFF",
                  "MAINHEAT_ENERGY_EFF",
                  "MAINHEATCONT_DESCRIPTION","MAINHEATC_ENERGY_EFF",
                  "LIGHTING_ENERGY_EFF","PHOTO_SUPPLY","SOLAR_WATER_HEATING_FLAG")]

# Get most recent EPC

#foo <- certs[grepl("Kite House",certs$ADDRESS1),]

certs <- certs[!is.na(certs$UPRN),]
certs <- certs[order(certs$INSPECTION_DATE, decreasing = TRUE),]
#certs <- certs[!duplicated(certs$BUILDING_REFERENCE_NUMBER),] 
certs <- certs[!duplicated(certs$UPRN),] 

#22627924 - all
# summary(duplicated(certs$BUILDING_REFERENCE_NUMBER))
# Mode    FALSE     TRUE 
# logical 18260045  4367879

# summary(duplicated(certs$UPRN))
# Mode    FALSE     TRUE 
# logical 16905672  5722252 


uprn <- readr::read_csv("D:/OneDrive - University of Leeds/Data/OS/Open UPRN/osopenuprn_202207_csv/osopenuprn_202206.csv")
uprn <- uprn[,c("UPRN","LATITUDE","LONGITUDE")]
#uprn <- uprn[uprn$UPRN %in% certs$UPRN,]
uprn <- st_as_sf(uprn, coords = c("LONGITUDE","LATITUDE"), crs = 4326)


certs$buidling_type <- paste0(certs$BUILT_FORM," ",certs$PROPERTY_TYPE)
certs$fuel <- gsub(" \\(not community\\)","",certs$MAIN_FUEL)
certs$fuel <- gsub(" \\(unknown\\)","",certs$fuel)
certs$fuel <- gsub(" \\(community\\)","",certs$fuel)

certs$BUILT_FORM <- NULL
certs$PROPERTY_TYPE <- NULL
certs$MAIN_FUEL <- NULL
certs$BUILDING_REFERENCE_NUMBER <- NULL

certs$FLOOR_ENERGY_EFF[is.na(certs$FLOOR_ENERGY_EFF)] <- "NO DATA!"

certs$WINDOWS_ENERGY_EFF <- as.character(certs$WINDOWS_ENERGY_EFF)
certs$WINDOWS_ENERGY_EFF[is.na(certs$WINDOWS_ENERGY_EFF)] <- "NO DATA!"
certs$WINDOWS_ENERGY_EFF <- as.factor(certs$WINDOWS_ENERGY_EFF)

certs$WALLS_ENERGY_EFF <- as.character(certs$WALLS_ENERGY_EFF)
certs$WALLS_ENERGY_EFF[is.na(certs$WALLS_ENERGY_EFF)] <- "NO DATA!"
certs$WALLS_ENERGY_EFF <- as.factor(certs$WALLS_ENERGY_EFF)

certs$ROOF_ENERGY_EFF <- as.character(certs$ROOF_ENERGY_EFF)
certs$ROOF_ENERGY_EFF[is.na(certs$ROOF_ENERGY_EFF)] <- "NO DATA!"
certs$ROOF_ENERGY_EFF <- as.factor(certs$ROOF_ENERGY_EFF)

certs$MAINHEAT_ENERGY_EFF <- as.character(certs$MAINHEAT_ENERGY_EFF)
certs$MAINHEAT_ENERGY_EFF[is.na(certs$MAINHEAT_ENERGY_EFF)] <- "NO DATA!"
certs$MAINHEAT_ENERGY_EFF <- as.factor(certs$MAINHEAT_ENERGY_EFF)

certs$MAINHEATC_ENERGY_EFF <- as.character(certs$MAINHEATC_ENERGY_EFF)
certs$MAINHEATC_ENERGY_EFF[is.na(certs$MAINHEATC_ENERGY_EFF)] <- "NO DATA!"
certs$MAINHEATC_ENERGY_EFF <- as.factor(certs$MAINHEATC_ENERGY_EFF)

certs$PHOTO_SUPPLY[is.na(certs$PHOTO_SUPPLY)] <- 0

# Flag Roofs and Floors
certs$FLOOR_ENERGY_EFF <- as.character(certs$FLOOR_ENERGY_EFF)
certs$FLOOR_ENERGY_EFF <- ifelse((certs$FLOOR_ENERGY_EFF %in% c("NO DATA!","N/A")) & 
                                   (certs$FLOOR_DESCRIPTION %in% c("(another dwelling below)", "(other premises below)")),
                                 "dwelling below",certs$FLOOR_ENERGY_EFF)

certs$ROOF_ENERGY_EFF <- as.character(certs$ROOF_ENERGY_EFF)
certs$ROOF_ENERGY_EFF <- ifelse((certs$ROOF_ENERGY_EFF %in% c("NO DATA!","N/A")) & 
                                  (certs$ROOF_DESCRIPTION %in% c("(another dwelling above)", "(other premises above)")),
                                "dwelling above",certs$ROOF_ENERGY_EFF)

table(certs$TENURE)
certs$TENURE[certs$TENURE == "Not defined - use in the case of a new dwelling for which the intended tenure in not known. It is no"] <- "new build - no resident"
certs$TENURE[certs$TENURE == "rental (private)"] <- "Rented (private)"
certs$TENURE[certs$TENURE == "rental (social)"] <- "Rented (social)"
certs$TENURE[certs$TENURE == "owner-occupied"] <- "Owner-occupied"
certs$TENURE[certs$TENURE == "NO DATA!"] <- "unknown"

table(certs$CONSTRUCTION_AGE_BAND)
certs$CONSTRUCTION_AGE_BAND <- gsub("England and Wales: ","",certs$CONSTRUCTION_AGE_BAND)
certs$CONSTRUCTION_AGE_BAND[certs$CONSTRUCTION_AGE_BAND == "NO DATA!"] <- "INVALID!"

certs$TOTAL_FLOOR_AREA <- round(certs$TOTAL_FLOOR_AREA)

# certs$GLAZED_TYPE[certs$GLAZED_TYPE == "NO DATA!"] <- "INVALID!"
# certs$GLAZED_TYPE[certs$GLAZED_TYPE == "NO DATA!"] <- "not defined"
# certs$GLAZED_TYPE[certs$GLAZED_TYPE == "double glazing, unknown install date"] <- "double, known data"
# certs$GLAZED_TYPE[certs$GLAZED_TYPE == "triple glazing"] <- "triple, known data"
certs$GLAZED_TYPE <- NULL
certs$PHOTO_SUPPLY <- NULL
certs$SOLAR_WATER_HEATING_FLAG <- NULL

certs$building_type <- gsub("NO DATA! ","",certs$buidling_type)
certs$building_type <- gsub("NA","",certs$building_type)
certs$buidling_type <- NULL

certs$FLOOR_DESCRIPTION <- gsub("¦","2",certs$FLOOR_DESCRIPTION)
certs$WALLS_DESCRIPTION <- gsub("¦","2",certs$WALLS_DESCRIPTION)
certs$ROOF_DESCRIPTION <- gsub("¦","2",certs$ROOF_DESCRIPTION)



certs$INSPECTION_YEAR <- year(ymd(certs$INSPECTION_DATE))
certs$INSPECTION_DATE <- NULL


certs$FLOOR_ENERGY_EFF <- ifelse(certs$FLOOR_ENERGY_EFF == "NO DATA!",certs$FLOOR_DESCRIPTION,certs$FLOOR_ENERGY_EFF)

certs$FLOOR_DESCRIPTION <- NULL

certs$UPRN <- as.numeric(certs$UPRN)

certs <- left_join(certs, uprn, by = "UPRN")
certs$UPRN <- NULL
certs <- st_as_sf(certs)

#summary(st_is_empty(certs))

certs <- certs[!st_is_empty(certs),]
#certs$PROPERTY_TYPE.1 <- NULL
#certs$MAINHEAT_DESCRIPTION.1 <- NULL

# SHort names save space
names(certs)
names(certs) <- c("addr","cur_rate","cur_ee","per_ee",
                  "tenure","age","area",
                  "heat_d","floor_ee","water_d","water_ee",
                  "wind_d","wind_ee","wall_d","wall_ee",
                  "roof_d","roof_ee","heat_ee",
                  "con_d","con_ee","light_ee","fuel",
                  "building_type","year","geometry"  )

# write_geojson <- function(x, path){
#   headder <- paste0('{\n"type":"FeatureCollection",\n"name": "',deparse(substitute(x)),'",')
#   if(sf::st_is_longlat(x)){
#     headder <- paste0(headder,'"crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },\n')
#   }
#   headder <- paste0(headder,'"features":[')
#   footer <- ']\n}'
#   commas <- c(rep(',', nrow(x) - 1),'')
#   x <- c(headder, 
#          paste0(geojsonsf::sf_geojson(x, atomise = TRUE),commas),
#          footer)
#   data.table::fwrite(list(x), path, quote  = FALSE)
# }
# 
# 
# headder <- paste0('{\n"type":"FeatureCollection",\n"name": "',deparse(substitute(certs)),'",')
# if(sf::st_is_longlat(certs)){
#   headder <- paste0(headder,'"crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },\n')
# }
# headder <- paste0(headder,'"features":[')
# footer <- ']\n}'
# commas <- c(rep(',', 100000 - 1),'')
# x <- list(c(headder,
#             stringi::stri_c(geojsonsf::sf_geojson(certs, atomise = TRUE), commas, sep = ""),
#             footer))
# data.table::fwrite(x, path, quote  = FALSE)
# 
# bench::mark(x1 = c(headder,
#                    stringi::stri_c(geojsonsf::sf_geojson(certs[1:100000,], atomise = TRUE), commas, sep = ""),
#                    footer)
#             )
# 
# system.time(x <- geojsonsf::sf_geojson(certs[1:10000,]))
# 
# certs2 <- certs[,c("addr","cur_rate","cur_ee","per_ee",
#                    "tenure","age","area",
#                    "floor_ee","water_ee","wind_ee","wall_ee",
#                    "roof_ee","heat_ee","con_ee","light_ee","fuel",
#                    "building_type","year","geometry"  )]
# 
# y <- certs2[1:100000,]
# 
# system.time(x <- geojsonsf::sf_geojson(y, atomise = TRUE))
write_sf(certs, "data/tiles/epc2.geojson", delete_dsn = TRUE)

#certs <- geojsonsf::geojson_sf("data/tiles/epc.geojson")

# Wiggle potion of overlapping points
certs$dup <- duplicated(certs$geometry)

certs_dup <- certs[certs$dup,]
certs <- certs[!certs$dup,]

wiggle <- function(x){
  x[[1]] <- x[[1]] + round(runif(1,-0.00005,0.00005),6)
  x[[2]] <- x[[2]] + round(runif(1,-0.00005,0.00005),6)
  x
}
# x
# wiggle(x)
# 
# y <- st_as_sfc(list(x, wiggle(x)))
# st_crs(y) <- 4326
# qtm(y)

certs_dup2 <- lapply(certs_dup$geometry, wiggle)
certs_dup2 <- st_as_sfc(certs_dup2)
st_crs(certs_dup2) <- 4326

certs_dup$geometry <- certs_dup2
certs_all <- rbind(certs, certs_dup)
certs_all$dup <- NULL
names(certs_all)

write_sf(certs_all, "C:/tiles/epc/epc_wiggle2.geojson", delete_dsn = TRUE)

certs_sample <- certs_all[sample(1:nrow(certs_all),nrow(certs_all)/10),]

write_sf(certs_sample, "C:/tiles/epc/epc_wiggle_sample2.geojson", delete_dsn = TRUE)

#write_geojson(certs, "data/tiles/epc.geojson")

library(tmap)
tmap_mode("view")
qtm(certs[1:1000,])
