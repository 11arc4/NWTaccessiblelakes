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
rm(int, waterbody)

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
<<<<<<< HEAD
#Skip a couple weird shaped lakes (677, 5377, 6087)
#for(i in c(1:676, 678:nrow(MasterAccess))){
  for(i in c(6088:nrow(MasterAccess))){
    if(MasterAccess$Area[i] <= 8100){
    mor<- lakeMorphoClass(MasterAccess[i,]  , elev)
    # #MasterAccess$Depth[i] <<- lakeMaxDepth(mor)
    # MasterAccess$MajorAxis[i] <- lakeMajorAxisLength(mor)
    # MasterAccess$MinorAxis[i] <- lakeMinorAxisLength(mor)
    # MasterAccess$AxisRatio[i] <- lakeMinorMajorRatio(mor)
     MasterAccess$Shoreline[i] <- lakeShorelineLength(mor)
    # #MasterAccess$Volume[i] <<- lakeVolume(mor)
  } else {
    #Can only get depth and volume estimates for larger lakes
    mor <- lakeSurroundTopo( MasterAccess[i,] %>% as_Spatial() , elev)
    MasterAccess$Depth[i] <- lakeMaxDepth(mor)
    MasterAccess$MajorAxis[i] <- lakeMajorAxisLength(mor)
    MasterAccess$MinorAxis[i] <- lakeMinorAxisLength(mor)
    MasterAccess$AxisRatio[i] <- lakeMinorMajorRatio(mor)
    MasterAccess$Shoreline[i] <- lakeShorelineLength(mor)
    MasterAccess$Volume[i] <- lakeVolume(mor)
  }
}

  
MasterAccess <- st_transform(MasterAccess, proj_crs)

st_write(MasterAccess, file.path(root, "data", "Road Accessible Lakes_1km_add Kakisa.shp"))

# Calculate Distance to the Road----
MasterAccess$DistancetoRoad <-  as.numeric(st_distance(MasterAccess ,
                                                       roads %>% filter(!RTNUMBER1 %in% c(8,10,"Unknown")| grepl("Kakisa", roads$RTENAME1EN)) %>% st_union))
roadaccessible1k <- MasterAccess %>% filter(DistancetoRoad<1000)%>% st_zm()

rm(roadaccessible)
#Add fire attributes  ----
roadaccessible <- st_read(file.path(root, "data", "Road Accessible Lakes_1km.shp"))%>% st_transform(4617)

fire <- st_read(file.path(shapefile.dir, "ENR Fire History", "ENR_FMD_FireHistory.shp")) %>% st_zm() %>% st_transform(proj_crs)
sf_use_s2(FALSE)
fire2000 <- fire%>% filter(fireyear >2000)
listfire <- st_intersects(roadaccessible1k, fire2000 )

roadaccessible1k$MostRecentBurn <-sapply(1:length(listfire), function(x){
  t<- listfire[[x]]
  years <- fire2000$fireyear[t]

  if(length(t)>0){
    return(years[order(years, decreasing = T)][1])
  } else {
    return(NA)
  }
  
})

roadaccessible1k$PreviousBurn <-sapply(1:length(listfire), function(x){
  t<- listfire[[x]]
  years <- fire2000$fireyear[t]
  if(length(t)>1){
    return(years[order(years, decreasing = T)][2])
  } else {
    return(NA)
  }
  
})


roadaccessible1k %>%st_drop_geometry()%>% group_by(MostRecentBurn, PreviousBurn) %>% 
  summarise(N= n()) %>% 
  arrange(desc(MostRecentBurn))

roadaccessible1k <- roadaccessible1k %>% 
  mutate(FireHistCat= ifelse (is.na(PreviousBurn) & is.na(MostRecentBurn), "No Fire", 
                                                 ifelse(is.na(PreviousBurn)& MostRecentBurn==2023, "2023 fire", 
                                                        ifelse(MostRecentBurn %in% c(2012:2015), "~10 years ago", 
                                                               ifelse(MostRecentBurn==2023 & PreviousBurn %in% c(2012:2015), "2023 and ~10 years ago", NA))))
)

ggplot()+
  geom_sf(data=nwt, fill="white")+
  geom_sf(data=fire %>% 
            filter(fireyear %in% c(2012:2015, 2023)) , 
          aes(fill=factor(fireyear, levels= c(2023, 2012:2015))), 
          alpha=0.3)+
  geom_sf(data=roads2)+
  #geom_sf(data=roads_buffer, fill="yellow", color=NA)+
  geom_sf(data=roadaccessible1k, aes(fill=FireHistCat), color=NA)+
  coord_sf(xlim=c(-116, -118), ylim=c(60, 61.5)) #Dehcho + some south slave
  #coord_sf(xlim=c(-110, -125), ylim=c(60, 63.5)) #entire Map
ggsave(file.path(root, "figures", "Kakisa accessible lakes Map.png"), units="in",  height=8)


ggplot(data=roadaccessible1k %>%st_drop_geometry() %>% filter(!is.na(FireHistCat)), aes(x= DistancetoRoad))+
  geom_histogram()+
  facet_wrap(~FireHistCat)+
  theme_classic()+
  labs(x= "Distance to nearest road (m)")
ggsave(file.path(root, "figures", "Distance to road sample options.png"), units="in", width=8, height=8)
