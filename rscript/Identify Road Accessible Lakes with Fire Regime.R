library(sf)
library(tidyverse)

proj_crs <- 4617


# Load all spatial data---
shapefile.dir <- "C:/Users/amelia_cox/Documents/Shapefiles"
canada <- st_read(file.path(shapefile.dir, "Canada_shp", "lpr_000b21a_e.shp")) %>% st_transform(proj_crs)
nwt <- canada %>% filter(PRNAME=="Northwest Territories / Territoires du Nord-Ouest")

roads <- st_read(file.path(shapefile.dir,"National Road Network_NT", "NRN_NT_14_0_SHAPE_en", "NRN_NT_14_0_ROADSEG.shp"))%>% 
  st_transform(proj_crs)
#Create Accessible Roads Buffer----
roads_buffer <- roads %>% filter(!RTNUMBER1 %in% c(8,10,"Unknown")) %>% st_union %>% st_buffer(1000)

# ggplot()+
#   geom_sf(data=roads, aes(color= RTNUMBER1))+ 
#   geom_sf(data=roads_buffer, fill="yellow")

#Identify all lakes within 1km of the roads----


#Use Lakemorpho to estimate lake characteristics----
#Lakemorpho only works for lakes greater than XXX in area
library(lakemorpho)
library(raster)

#Add fire attributes ----
roadaccessible <- st_read(file.path(shapefile.dir, "National Hydro Network", "Lake with road access Attributes_1km.shp"))

fire <- st_read(file.path(shapefile.dir, "National Fire Database", "NFDB_poly_20210707.shp"))
