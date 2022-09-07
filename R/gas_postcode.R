library(sf)



dir.create("tmppc")
unzip("D:/OneDrive - University of Leeds/Data/Postcodes/Postcode Polygons/Postcodes_20200826.zip",
      exdir = "tmppc")

zips <- list.files("tmppc/codepoint-poly_4637385/", recursive = TRUE, pattern = ".zip", full.names = TRUE)

postcodes <- list()
for(i in 1:length(zips)){
  message(i)
  dir.create("tmppc2")
  unzip(zips[i], exdir = "tmppc2")
  fl <- list.files("tmppc2", full.names = TRUE, pattern = ".shp")
  if(length(fl) != 1){
    stop("Muliple files")
  }
  pc <- read_sf(fl)
  pc <- pc[,c("POSTCODE","geometry")]
  postcodes[[i]] <- pc
  rm(pc, fl)
  unlink("tmppc2", recursive = TRUE)
  
}

postcodes_all <- data.table::rbindlist(postcodes)
saveRDS(postcodes_all, "data/postcode_areas_all.Rds")
unlink("tmppc", recursive = TRUE)
