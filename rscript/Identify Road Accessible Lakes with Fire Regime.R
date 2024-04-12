library(sf)
library(tidyverse)

proj_crs <- 4617


# Load all spatial data---
root <- getwd()
shapefile.dir <- "C:/Users/amelia_cox/Documents/Shapefiles"
canada <- st_read(file.path(shapefile.dir, "Canada_shp", "lpr_000b21a_e.shp")) %>% st_transform(proj_crs)
nwt <- canada %>% filter(PRNAME=="Northwest Territories / Territoires du Nord-Ouest")

roads <- st_read(file.path(shapefile.dir,"National Road Network_NT", "NRN_NT_14_0_SHAPE_en", "NRN_NT_14_0_ROADSEG.shp"))%>% 
  st_transform(proj_crs)
#Create Accessible Roads Buffer----
roads2 <- roads %>% 
  filter(!RTNUMBER1 %in% c(8,10,"Unknown")| grepl("Kakisa", roads$RTENAME1EN))
roads_buffer <- roads2 %>% 
  st_union %>% 
  st_buffer(1000)

# ggplot()+
#   geom_sf(data=roads, aes(color= RTNUMBER1))+ 
#   geom_sf(data=roads_buffer, fill="yellow")

#Identify all lakes within 1km of the roads----
workunitlist <- list.files(file.path(shapefile.dir, "National Hydro Network"))
workunitlist <- workunitlist[grep("Watershed", workunitlist)]

for (i in 1:length(workunitlist)){
  waterbodyfile <- list.files(file.path(shapefile.dir, "National Hydro Network", workunitlist[i]))
  waterbody <- st_read(file.path(shapefile.dir, "National Hydro Network",workunitlist[i], waterbodyfile[grep("WATERBODY_2.shp", waterbodyfile)]))
  
  int <- st_intersects(waterbody, roads_buffer)
  waterbody$Access<- sapply(1:length(int), function(x){length(int[[x]])>0})
  
  if(i==1){
    MasterAccess<- waterbody %>% filter(Access==T)
  } else{
    MasterAccess<- rbind(MasterAccess, waterbody %>% filter(Access==T))
    
  } 
}
MasterAccess <- MasterAccess %>% 
  filter(TYPE_TEXT=="Lake")%>% 
  st_zm()
st_write(MasterAccess, file.path(root, "data", "Road Accessible Lakes_1km.shp"))
#Use Lakemorpho to estimate lake characteristics----
#Lakemorpho only works for lakes greater than XXX in area
library(lakemorpho)
library(raster)
elev <- raster(file.path(shapefile.dir, "Canadian Digital Elevation Model, 1945-2011 - Cloud Optimized.tif"))

MasterAccess <- st_transform(MasterAccess, proj4string(elev))
MasterAccess$Area <- as.numeric(st_area(MasterAccess))

MasterAccess <- MasterAccess %>% mutate(Depth=NA, 
                                        MajorAxis=NA, 
                                        MinorAxis=NA, 
                                        AxisRatio=NA, 
                                        Shoreline=NA, 
                                        Volume=NA)
lapply(1:nrow(MasterAccess), function(i){
  #if(MasterAccess$Area[i]>)
  mor <- lakeMorphoClass(MasterAccess[i,], elev)
  MasterAccess$Depth[i] <<- lakeMaxDepth(mor)
  MasterAccess$MajorAxis[i] <<- lakeMajorAxisLength(mor)
  MasterAccess$MinorAxis[i] <<- lakeMinorAxisLength(mor)
  MasterAccess$AxisRatio[i] <<- lakeMinorMajorRatio(mor)
  MasterAccess$Shoreline[i] <<- lakeShorelineLength(mor)
  MasterAccess$Volume[i] <<- lakeVolume(mor)
  
  
})
MasterAccess <- st_transform(MasterAccess, proj_crs)

st_write(MasterAccess, file.path(root, "data", "Road Accessible Lakes_1km.shp"))

# Calculate Distance to the Road----
roadaccessible <- st_read(file.path(root, "data", "Road Accessible Lakes_1km.shp"))%>% st_transform(4617)
roadaccessible$DistancetoRoad <-  as.numeric(st_distance(roadaccessible ,roads %>% filter(!RTNUMBER1 %in% c(8,10,"Unknown")| grepl("Kakisa", roads$RTENAME1EN)) %>% st_union))
roadaccessible1k <- roadaccessible %>% filter(DistancetoRoad<1000)%>% st_zm()

rm(roadaccessible)
#Add fire attributes  ----

fire <- st_read(file.path(shapefile.dir, "ENR Fire History", "ENR_FMD_FireHistory.shp")) %>% st_zm() %>% st_transform(proj_crs)
sf_use_s2(FALSE)
fire2000 <- fire %>% filter(YEAR >2000)
listfire <- st_intersects(roadaccessible1k, fire %>% filter(fireyear >2000))

roadaccessible1k$MostRecentBurn <-sapply(1:length(listfire), function(x){
  t<- listfire[[x]]
  years <- fire2000$YEAR[t]
  
  if(length(t)>0){
    return(years[order(years, decreasing = T)][1])
  } else {
    return(NA)
  }
  
})

roadaccessible1k$PreviousBurn <-sapply(1:length(listfire), function(x){
  t<- listfire[[x]]
  years <- fire2000$YEAR[t]
  if(length(t)>1){
    return(years[order(years, decreasing = T)][2])
  } else {
    return(NA)
  }
  
})


roadaccessible1k %>%st_drop_geometry()%>% group_by(MostRecentBurn, PreviousBurn) %>% 
  summarise(N= n()) %>% 
  arrange(desc(MostRecentBurn))

roadaccessible1k %>% mutate(FireHistCat= NA, 
                            
                              TimeSinceBurnCat= NA)


ggplot()+
  geom_sf(data=nwt, fill="palegreen", alpha=0.3)+
  geom_sf(data=roads2)+
  geom_sf(data=roads_buffer, fill="yellow", color=NA)+
  coord_sf(xlim=c(-110, -125), ylim=c(60, 63.5))
  
