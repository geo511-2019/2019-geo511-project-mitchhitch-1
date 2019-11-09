
library(sf)
library(sp)
library(profvis)
library(dplyr)
library(tidyverse)
library(dplyr)
library(parallel)

####user input data (demo below) would like to add in soils and climate data but for now KISS
shapeobj1<-  st_read("~/Desktop/UBfall2019/Erie-Tax-Parcels-Centroid-Points-SHP/Erie_2018_Tax_Parcel_Centroid_Points_SHP.shp")
pointcoords<- read.csv("Environmental_Remediation_Sites.csv")


####convert data as necessary

  ############This is a workign first step
point_trn<-st_as_sf(pointcoords,coords=c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%
  st_transform(st_crs(shapeobj1))






####join data to intersecting objects

  ###results in obj of same size as original shapeobj1
#shapeobj2 <- shapeobj1[!is.na(st_contains(pointcoords,shapeobj1)), ] 

  ###This looks like the way forward. This would be followed by additional data sets
shapeobj2<-st_join(shapeobj1,point_trn,join=st_nearest_feature,left=FALSE)### st_nearest_points fails due to too much mem std::bad_alloc


plot(shapeobj2)

####User selects columns of interest and fixed value columns (done in shiny app)
  ####text should be converted to factors here
  ##each column should be listed with a drop down menu for use, ignore, or fixed
    ##if fixed then set the fixed value from a list of possible values or factors
      ##this information is then used to filter (should probably have a submit button to avoid continuous run)

names(shapeobj2)

######DEMO all this here#####
head(point_trn)
shapeobj_trim<-select(shapeobj2,ACRES,Program.Facility.Name,Project.Name,Contaminants,Program.Type,DEC.Region,Waste.Name,OU,Site.Class)
names(shapeobj_trim)
plot(shapeobj_trim)

####user chooses number of sites desired "n"
  ###should be simple enough
n=5

####stepwise comparison of similarity (distance for values ) for all possible combinations of "n" sites
  ###this should be a simple /s algo generating a first list generating a score, generating a sequential list
    ###generating and comparing the two score, and keeping the better score and repeat with next sequential list





####generated site list displayed in leaflet map and table 
  ### downloadable option could be nice too.



####profit /s




  

