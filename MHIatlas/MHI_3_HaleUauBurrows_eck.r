### This script combines 'Ua'u burrow data from the 4 study groups on Haleakala
## burrow data is standardized across agencies/areas and summary/visualization is done to better understand data input, inconsistencies, etc.
## writen by: E Kelsey, March 30, 2020
## last updated: 

## set wd
setwd("~/WERC-SC/MHIatlas")

## load libraries
library(dplyr)
library(data.table)
library(lubridate)
library(mosaic)
library(leaflet)
library(sp)
library(raster)
library(ggplot2)
library(crosstalk)

## read in data files
# Haleakala National Park
read.csv('~/WERC-SC/MHIatlas/HALE_uau_Master_2019.csv', stringsAsFactors = FALSE) %>% 
  mutate(Study = rep("hale", length(FID))) -> hale 

# MNSRP Kahikinui
read.csv('~/WERC-SC/MHIatlas/MNSRP_Kahikinui_burrows.csv', stringsAsFactors = FALSE) %>% 
  mutate(Study = rep("kahikinui", length(FID))) -> kahikinui

# Auwahi Wind Farm conservation area
read.csv('~/WERC-SC/MHIatlas/Auwahi_2018_HAPE_burrows.csv', stringsAsFactors = FALSE) %>% 
  mutate(Study = rep("auwahi", length(FID))) -> auwahi

# DKIST aka Alpine Wildlife Sanctuary
read.csv('~/WERC-SC/MHIatlas/DKIST_2018_All_Burrows.csv', stringsAsFactors = FALSE) %>% 
  mutate(Study = rep("dkist", length(FID))) -> dkist

### check for overlap btwn HALE and DKIST
dkist_burrow <- dkist %>% 
  mutate(burrow = Burrow) %>% 
  dplyr::select(burrow, xlong, ylat)

AWS_burrow <- hale %>% 
  mutate(burrow = BURROW_NO) %>% 
  filter(COLONY == "SC" | COLONY == "WS") %>% 
  dplyr::select(burrow, xlong, ylat) %>% 
  full_join(dkist_burrow)

summary(duplicated(AWS_burrow))
summary(duplicated(AWS_burrow, by = "burrow"))
summary(duplicated(AWS_burrow, by = c("xlong", "ylat")))
# duplicated <- AWS_burrow %>% 
#   duplicated(by = "burrow") %>% 
#   as.data.table() %>% 
#   bind_cols(AWS_burrow) %>%
#   filter(. == TRUE)
  
### combine all datasets
auwahi_sm <- auwahi %>% 
  filter(Feature == "Petrel Burrow") %>% 
  transmute(Study, BurrowID = UniqueID)

dkist_sm <- dkist %>% 
  transmute(Study, BurrowID = Burrow)

hale_sm <- hale %>% 
  transmute(Study, BurrowID = BURROW_NO)

kahi_sm <- kahikinui %>% 
  transmute(Study, BurrowID = Notes)

allStudies <- kahi_sm %>% 
  bind_rows(auwahi_sm, dkist_sm, hale_sm)

# summary(duplicated(allStudies, by = c("xlong", "ylat")))
# duplicated <- allStudies %>%
#   duplicated(by = c("xlong", "ylat")) %>%
#   as.data.table() %>%
#   bind_cols(allStudies) %>%
#   filter(. == TRUE)



pal <- colorFactor(c("navy", "red", "black", "white"), domain = c("hale", "dkist", "kahikinui", "auwahi"))#set colors for values from the fiel dyou are going to map to color
leaflet(allStudies) %>%
  # addTiles() %>%
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(lng = allStudies$xlong,
                   lat = allStudies$ylat,
                   popup = paste(allStudies$BurrowID, allStudies$Study, sep="; "), #if you want to use multiple fields you have to paste them together
                   radius=6,
                   color=~pal(Study),#tell it a field that is a factor, for which you mapped the values and colors above
                   stroke = FALSE, 
                   fillOpacity = 0.5) %>%
  addScaleBar()

bscols(
  filter_select("Magnitude Level", "Magnitude Level", quakes_sd, ~mag.level)
)
bscols(map)

#make it a spatial points object
allStudies_sp <- allStudies
coordinates(allStudies_sp) <- ~xlong+ylat #define the coordinate fields
proj4string(allStudies_sp) <- CRS("+proj=longlat +datum=WGS84") #define the projection, in this case Geographic WGS84

leaflet(allStudies_sp) %>%
  # addTiles() %>%
  addProviderTiles(providers$OpenTopoMap) %>%
  addCircleMarkers(popup = allStudies_sp$BurrowID,
                   radius=6,
                   color=~pal(Study),
                   stroke = FALSE, 
                   fillOpacity = 0.5) %>%
  # addPolylines(data=v_lines) %>%
  addScaleBar()

#### habitat rasters
elevation <- raster("~/WERC-SC/MHIatlas/hi_dem")
slope <- raster("~/WERC-SC/MHIatlas/hi_dem_sl")
# evt <- raster("~/WERC-SC/MHIatlas/Hawaii_BaseLayers/Landfire/EVT/HI_130EVT/hi_130evt")
# evc <- raster("~/WERC-SC/MHIatlas/Hawaii_BaseLayers/Landfire/EVC/HI_130EVC/hi_130evc")

# get burrows into same projection as habitat layers
allStudies_sp <- spTransform(allStudies_sp, crs(elevation))
extent <- 1.3*extent(allStudies_sp) # 1.3 TIMES DATA EXTENT
elevation_sm <- crop(elevation, extent)
# evc_sm <- crop(evc, extent)
# plot(evc_sm)
plot(allStudies_sp, add=TRUE)

#extract values 
allStudies_sp$elev <- raster::extract(elevation, allStudies_sp)
allStudies_sp$slope <- raster::extract(slope, allStudies_sp)
# UaU_sp$evt <- raster::extract(evt, UaU_sp)
# UaU_sp$evc <- raster::extract(evc, UaU_sp)
# evt_df <- as.data.frame(evt)

ggplot(as.data.frame(allStudies_sp), aes(x=slope)) + geom_histogram()

allStudies <- as.data.frame(allStudies_sp) %>% 
  



### create standardized ID format across files
### explore dates of burrow marking
### occupancy rate



