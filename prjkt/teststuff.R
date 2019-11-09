
library(sf)
library(sp)
library(profvis)
library(dplyr)
library(tidyverse)
library(rgeos)
library(dplyr)

####user input data (demo below) would like to add in soils and climate data but for now KISS
shapeobj1<-  st_read("~/Desktop/Erie-Tax-Parcels-Centroid-Points-SHP/Erie_2018_Tax_Parcel_Centroid_Points_SHP.shp")
pointcoords<- read.csv("Environmental_Remediation_Sites.csv")


####convert data as necessary

    ###supposidly i can convert the data points to a st via st_point but I need to c(long,lat) which fails
      ### to work because st_point is picky and the documentation is vauge


  ############This is a workign first step
point_trn<-st_as_sf(pointcoords,coords=c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%
  st_transform(st_crs(shapeobj1))






####join data to intersecting objects

  ###results in obj of same size as original shapeobj1
#shapeobj2 <- shapeobj1[!is.na(st_contains(pointcoords,shapeobj1)), ] 

  ###This looks like the way forward. This would be followed by additional data sets
shapeobj2<-st_join(shapeobj1,point_trn,join=st_nearest_points,maxdist=100,left=FALSE)###fails due to too much mem std::bad_alloc


plot(shapeobj2)

####User selects columns of interest and fixed value columns (done in shiny app)
  ####text should be converted to factors here
  ##each column should be listed with a drop down menu for use, ignore, or fixed
    ##if fixed then set the fixed value from a list of possible values or factors
      ##this information is then used to filter (should probably have a submit button to avoid continuous run)



######DEMO all this here#####





####user chooses number of sites desired "n"
  ###should be simple enough


####stepwise comparison of similarity (distance for values ) for all possible combinations of "n" sites
  ###this should be a simple /s algo generating a first list generating a score, generating a sequential list
    ###generating and comparing the two score, and keeping the better score and repeat with next sequential list


####generated site list displayed in leaflet map and table 
  ### downloadable option could be nice too.



####profit /s




  

