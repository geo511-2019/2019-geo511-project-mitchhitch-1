
library(sf)
library(sp)
library(profvis)
library(dplyr)
library(tidyverse)
library(dplyr)
library(doParallel)
library(nngeo)#st_nn


####user input data (demo below) would like to add in soils and climate data but for now KISS
shapeobj1<-  st_read("~/Desktop/UBfall2019/Erie-Tax-Parcels-Centroid-Points-SHP/Erie_2018_Tax_Parcel_Centroid_Points_SHP.shp")
pointcoords<- read.csv("Environmental_Remediation_Sites.csv")


####convert data as necessary

  ############This is a workign first step
point_trn<-st_as_sf(pointcoords,coords=c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%
  st_transform(st_crs(shapeobj1)) %>% 
  st_crop(st_bbox(shapeobj1))





####join data to intersecting objects


  ###This looks like the way forward. This would be followed by additional data sets in a for loop

shapeobj2<-st_join(point_trn,shapeobj1,join=st_nn,k=1,maxdist=500)


plot(shapeobj2)

####User selects columns of interest and fixed value columns (done in shiny app)
      ###user also needs to select site name field
  ####text should be converted to factors here
  ##each column should be listed with a drop down menu for use, ignore, or fixed
    ##if fixed then set the fixed value from a list of possible values or factors
      ##this information is then used to filter (should probably have a submit button to avoid continuous run)

names(shapeobj2)

######DEMO all this here#####

shapeobj_trim<-select(shapeobj2,ACRES,Program.Facility.Name,Project.Name,Contaminants,Program.Type,DEC.Region,Control.Code,OU,Site.Class) %>% 
  filter(ACRES>0)


################!!!!!!!!!!!!!!NEED TO FIND WAY TO UNION IDENTICAL GEOMETRY POINTS!!!!!!!!!!!!#####






####user chooses number of sites desired "n"
  ###should be simple enough
number=5

####stepwise comparison of similarity (distance for values ) for all possible combinations of "n" sites
  ###this should be a simple /s algo generating a first list generating a score, generating a sequential list
    ###generating and comparing the two score, and keeping the better score and repeat with next sequential list

#####score diff between two rows
  ###input rows need already to be filtered for desired test columns
score.calc<-function(rowprime,rowtest){
  
  colscore<-vector(length=length(rowprime))
  
  for(i in 1:length(rowprime)){
    colscore[i]<- if(is.factor(rowprime[i])||is.character(rowprime[i])){
      if(rowprime[i]==rowtest[i]){
        colscore[i]<-0
      }
      else
        colscore[i]<-1
    }
    else
      colscore[i]<-abs(as.numeric(rowprime[i])-as.numeric(rowtest[i]))
  }
  return(sum(colscore))
}
# ####This tests the funciton should return 6
# fake<-data.frame(name=c("this","that"),value=c(10,5))
# score.calc(fake[1,],fake[2,])
# 
# score.calc(shapeobj_test[1,],shapeobj_test[2,])
# 
# fake<-shapeobj_test
# for(i in 1:nrow(shapeobj_test)){
# fake[i,6]<-score.calc(shapeobj_test[1,],shapeobj_test[i,])
# }
# 
# shapeobj_test[1,]

#cols to testselect(as.data.frame(sf), -geometry)
shapeobj_test<-shapeobj_trim %>%
  as.data.frame() %>% 
  select(-geometry) %>% 
  select(Program.Facility.Name,DEC.Region,Control.Code,Contaminants,Site.Class,ACRES)###user input except name

str(shapeobj_test)
##########run through each
registerDoParallel(cores = 6)


####This works but takes forevver
possible_n<- foreach(r=1:nrow(shapeobj_test))%dopar%{
  
  for(i in 1:nrow(shapeobj_test)){
    x[i,ncol(shapeobj_test)+1]<-score.calc(shapeobj_test[r,],shapeobj_test[i,])
  }
  x<-head(arrange(x,x[,6]),n=number)
  print(paste(r/nrow(x)*100,"%"))
  x
}



###establish values of smallest (best) value for comparisions in score accross lists
minimum<-foreach(i=1:length(possible_n),.combine=c)%dopar%{
  possible_n[[i]][5,6]
} %>% 
  min()

####generate a list of best combinations
best<-foreach(i=1:length(possible_n))%dopar%{
  if(possible_n[[i]][5,6]==minimum){
    return(possible_n[[i]])
  }
} %>% 
  compact()
##############!!!!!!!!!!!!!best NEEDS TO BE REJOINED TO ORIGINAL GEOMETRIES FIRST RUN LOST KEY- 
###########################-TO JOIN BY (SITE NAME OR SOMETHING) SEE ABOVE!!!!


####generated site list displayed in leaflet map and table 
  ### downloadable option could be nice too.



####profit /s




  

