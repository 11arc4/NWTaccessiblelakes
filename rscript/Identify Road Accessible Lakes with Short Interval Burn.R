library(sf)
library(tidyverse)
library(lwgeom)

proj_crs <- 4617


# Load all spatial data---
root <- getwd()
shapefile.dir <- "C:/Users/amelia_cox/Documents/Shapefiles"
canada <- st_read(file.path(shapefile.dir, "Canada_shp", "lpr_000b21a_e.shp")) %>% st_transform(proj_crs)
nwt <- canada %>% filter(PRNAME=="Northwest Territories / Territoires du Nord-Ouest")
lakes <- st_read(file.path(shapefile.dir, "Lakes", "lhy_000c16a_e.shp")) %>% st_transform(proj_crs)

roads <- st_read(file.path(shapefile.dir,"National Road Network_NT", "NRN_NT_14_0_SHAPE_en", "NRN_NT_14_0_ROADSEG.shp"))%>% 
  st_transform(proj_crs)
#Create Accessible Roads Buffer----
roads2 <- roads %>% 
  filter(!RTNUMBER1 %in% c(8,10,"Unknown")| grepl("Kakisa", RTENAME1EN))

fire <- st_read(file.path(shapefile.dir, "ENR Fire History", "ENR_FMD_FireHistory.shp")) %>% 
  st_zm() %>% 
  st_transform(proj_crs)
library(viridis)



# 
# roads_buffer_500 <- roads2 %>% 
#   st_union %>% 
#   st_buffer(500)
roads_buffer_700 <- roads2 %>%
  filter(RTNUMBER1 %in% c(1,2,3,5)| grepl("Kakisa", RTENAME1EN))%>%
  filter(PAVSTATUS=="Paved")%>%
  st_union %>%
  st_buffer(700)
sf_use_s2(FALSE)

fire1990 <- fire %>% filter(fireyear>=1990) 

MasterAccess<- st_read(file.path(root, "data", "Road Accessible Lakes_2km.shp"))


listfire <- st_intersects(MasterAccess , fire1990)

MasterAccess$MostRecentBurn <-sapply(1:length(listfire), function(x){
  t<- listfire[[x]]
  years <- fire1990$fireyear[t]
  
  if(length(t)>0){
      return(years[order(years, decreasing = T)][1])
  } else {
    return(NA)
  }
  
})

MasterAccess$PreviousBurn <-sapply(1:length(listfire), function(x){
  t<- listfire[[x]]
  years <- fire1990$fireyear[t]
  if(length(t)>1){
    return(years[order(years, decreasing = T)][2])
  } else {
    return(NA)
  }
  
})
MasterAccess$Area <- as.numeric(st_area(MasterAccess))
SouthAccess <- st_intersection(MasterAccess, roads_buffer_700)


SouthAccess$DistancetoRoad <-  as.numeric(st_distance(SouthAccess ,
                                                      roads %>% filter(RTNUMBER1 %in% c(1,2,3,5,6)| grepl("Kakisa", RTENAME1EN))%>%st_union()))

st_write(file.path(root, "data", "Road Accessible Lakes_South Slave and Dehcho.shp"))

bounds <- st_bbox(c(xmin=-110,xmax=-120, ymin=60, ymax=61.5), crs= st_crs(proj_crs)) %>% st_as_sfc()

SouthAccess <- st_read(file.path(root, "data", "Road Accessible Lakes_South Slave and Dehcho.shp"))
SouthAccess2 <- st_intersection(SouthAccess, roads_buffer_700)%>% st_intersection(bounds)
SouthAccess2 <- SouthAccess2 %>% rename("DistancetoRoad"="DstnctR", "MostRecentBurn"="MstRcnB", "PreviousBurn"="PrvsBrn")


Nsum <- SouthAccess2 %>%  st_drop_geometry() %>% 
  filter(DistancetoRoad<300)%>%
  group_by(MostRecentBurn, PreviousBurn) %>% 
  summarise(N= n())
View(Nsum)


SouthAccess %>% st_drop_geometry() %>% 
  filter( MostRecentBurn==2023 , !is.na(PreviousBurn))%>%
  group_by(MostRecentBurn, PreviousBurn) %>% 
  summarise(N= n())





BurnOptions <- SouthAccess2 %>% 
  mutate(BurnHist = paste(MostRecentBurn, PreviousBurn, sep=" ")) %>% 
  filter(BurnHist %in% c("NA NA", paste(2023, 2012:2016, sep=" "),paste( 2012:2015,"NA", sep=" "), "2023 NA" )) 

#What's the top 70% quantile area for the 500m lakes
area75<- quantile(SouthAccess2$Area, 0.75)
area70<- quantile(SouthAccess2$Area, 0.75)


BurnOptions_500 <- BurnOptions %>% 
  filter(DistancetoRoad <500)%>%
  filter(Area>area70)


ggplot(BurnOptions %>% st_drop_geometry())+ 
  geom_histogram(aes(x=Area))+ 
  facet_wrap(~BurnHist, scales="free")

ggplot(BurnOptions %>% st_drop_geometry(), aes(x=DistancetoRoad, y=Area))+ 
  geom_point()+
  facet_wrap(~BurnHist, scales="free")



ggplot()+
  geom_sf(data=nwt, fill="white")+
  geom_sf(data=fire %>% filter(fireyear>1990) ,
          alpha=0.3, color=NA, aes(fill=fireyear), show.legend = T)+
  geom_sf(data=bounds, color="black", fill=NA)+
  
  geom_sf(data=roads2)+
  geom_sf(data=SouthAccess2 %>% filter(DistancetoRoad<500 & Area>area70), fill="lightblue", color="lightblue")+
  geom_sf(data=BurnOptions_500, fill="blue", color="blue")+
  coord_sf(xlim=c(-110, -121), ylim=c(60, 61.5))+ 
  scale_fill_viridis()#Dehcho + some south slave
ggsave(file.path(root, "figures", "Fire History2.png"), units="in", width=20, height=12)
#Maybe add eco
st_write(BurnOptions_500, file.path(root, "data", "South Slave and Dehcho pond options.shp"))
st_write(BurnOptions_500 %>% select(Description = BurnHist), "South Slave and Dehcho pond options.kml", driver = "kml", delete_dsn = TRUE)


ggplot()+
  geom_sf(data=nwt, fill="white")+
  geom_sf(data=fire %>% filter(fireyear>1990) ,
          alpha=0.3, color=NA, aes(fill=fireyear), show.legend = T)+
  geom_sf(data=lakes, color="lightblue", fill="lightblue", alpha=0.6)+
  
  geom_sf(data=roads2)+
  geom_sf(data=SouthAccess2 %>% filter(DistancetoRoad<500 & Area>area70), fill="lightblue", color="lightblue")+
  geom_sf(data=BurnOptions_500, fill="blue", color="blue")+
  coord_sf(xlim=c(-111.8, -115), ylim=c(60, 61.2))+ #Fort Smith
  scale_fill_viridis()+
  labs(fill="Year", color="Year")
  
ggsave(file.path(root, "figures", "Sample Sites_Fort Smith.png"), units="in", width=12, height=12)



ggplot()+
  geom_sf(data=nwt, fill="white")+
  geom_sf(data=fire %>% filter(fireyear>1990) ,
          alpha=0.3, color=NA, aes(fill=fireyear), show.legend = T)+
  geom_sf(data=lakes, color="lightblue", fill="lightblue", alpha=0.6)+
  geom_sf(data=roads2)+
  coord_sf(xlim=c(-110, -121), ylim=c(60, 61.5))+ 
  labs(fill="Year", color="Year")+
  scale_fill_viridis()#Dehcho + some south slave
ggsave(file.path(root, "figures", "Fire History_no lakes.png"), units="in", width=20, height=12)

ggplot()+
  geom_sf(data=nwt, fill="white")+
  geom_sf(data=fire %>% filter(fireyear>1990) ,
          alpha=0.3, color=NA, aes(fill=fireyear), show.legend = T)+
  geom_sf(data=lakes, color="lightblue", fill="lightblue", alpha=0.6)+
  geom_sf(data=roads2)+
  coord_sf(xlim=c(-112, -121), ylim=c(60, 63.2))+ 
  labs(fill="Year", color="Year")+
  scale_fill_viridis()#Dehcho + some south slave
ggsave(file.path(root, "figures", "Fire History NWT.png"), units="in", width=12, height=12)
