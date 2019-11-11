
library(sf)
library(sp)
library(profvis)
library(dplyr)
library(tidyverse)
library(dplyr)
library(doParallel)
library(nngeo)#st_nn


####user input data (demo below) would like to add in soils and climate data but for now KISS
shapeobj1<-  st_read("data/Erie-Tax-Parcels-Centroid-Points-SHP/Erie_2018_Tax_Parcel_Centroid_Points_SHP.shp")
pointcoords<- read.csv("data/Environmental_Remediation_Sites.csv")


####convert data as necessary

  ############This is a workign first step
point_trn<-st_as_sf(pointcoords,coords=c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%
  st_transform(st_crs(shapeobj1)) %>% 
  st_crop(st_bbox(shapeobj1))  






####join data to intersecting objects


  ###This looks like the way forward. This would be followed by additional data sets in a for loop
      ####aggregate unions identical points collapsing certain factors into vectors of factor level
      ####should retain shapeobj1 for joining at the end and regaining factor levels
shapeobj2<-st_join(point_trn,shapeobj1,join=st_nn,k=1,maxdist=500) %>% 
  aggregate( by=list(.$Program.Facility.Name),FUN=unique,do_union=TRUE,join = st_intersects)






####User selects columns of interest and fixed value columns (done in shiny app)
      ###user also needs to select site name field
  ####text should be converted to factors here
  ##each column should be listed with a drop down menu for use, ignore, or fixed
    ##if fixed then set the fixed value from a list of possible values or factors
      ##this information is then used to filter (should probably have a submit button to avoid continuous run)

unique_name<-"Program.Facility.Name"#user input
######DEMO all this here#####

shapeobj_trim<-select(shapeobj2,ACRES,Program.Facility.Name,Project.Name,Contaminants,Program.Type,DEC.Region,Control.Code,OU,Site.Class) %>% 
  filter(ACRES>0)






####user chooses number of sites desired "n"
  ###should be simple enough
number=5

####stepwise comparison of similarity (distance for values ) for all possible combinations of "n" sites
  ###this should be a simple /s algo generating a first list generating a score, generating a sequential list
    ###generating and comparing the two score, and keeping the better score and repeat with next sequential list

#####score diff between two rows
  ###input rows need already to be filtered for desired test columns
    ####factors lost there levels somehow but the unique factor number remained in a vector see aggregate above

###individual vlaues work but when a row is submitted it returns incorrect eval
score.calc<-function(rowprime,rowtest){

  colscore<-vector(length=length(rowprime))
  for(i in 1:ncol(rowprime)){
    prime<-rowprime[i]
    if(is.list(prime)){
      prime<-unlist(prime)
    }
    test<-rowtest[i]
    if(is.list(test)){
      test<-unlist(test)
    }
    if(length(prime)>1||length(test)>1){
      colscore[i]<-as.double(length(c(
        setdiff(prime,test),
        setdiff(test,prime)))
      )

    }
    else if(is.double(prime)||is.numeric(prime)){
        colscore[i]<-as.double(abs((prime-test)/(prime+test)))

    }
      else if(is.factor(prime)||is.character(prime)){
        if(prime==test){
          colscore[i]<-0

        }
        else{
          colscore[i]<-1

        }
      }
      else 
        colscore[i]<-NA
        }

  return(sum(colscore))
}






####This tests the funciton should return 7
#score.calc(shapeobj_test[29,],shapeobj_test[66,])




#cols to testselect(as.data.frame(sf), -geometry)
shapeobj_test<-shapeobj_trim %>%
  as.data.frame() %>% 
  select(-geometry) %>% 
  select(Program.Facility.Name,DEC.Region,Control.Code,Contaminants,Site.Class,ACRES)###user input except name

##########run through each
registerDoParallel(cores = 6)

####This works but takes forevver (Note that there should be no zero scores because every site has unique names this is ok becuase it is uniform)
possible_n<- foreach(r=1:nrow(shapeobj_test))%dopar%{
  x<-shapeobj_test
  for(i in 1:nrow(shapeobj_test)){
  
    x[i,ncol(shapeobj_test)+1]<-score.calc(shapeobj_test[r,],shapeobj_test[i,])
    
  }
  
  x<-head(arrange(x,x[,ncol(x)]),n=number)
  x
}


sum(possible_n[[1]][,ncol(possible_n[[1]])])
###establish values of smallest (best) value for comparisons in score across lists
###uses difference between of most different values to generate list. this could be improved
minimum<-foreach(i=1:length(possible_n),.combine=c)%dopar%{
  sum(possible_n[[i]][,ncol(possible_n[[i]])])
  } %>% 
  min()
minimum


possible_n[[1]][5,ncol(possible_n[[1]])]
####generate a list of best combinations
best<-foreach(i=1:length(possible_n))%dopar%{
  if(sum(possible_n[[i]][,ncol(possible_n[[i]])])==minimum){
    return(possible_n[[i]])
  }
} %>% 
  compact()
##############        
out1[j,]<-shapeobj2[j,]
        
      }
      
    }
  }
  out1<-compact(out1)
  return(out1)
}%>% 
  compact()



  ###the resulting combinations
####generated site list displayed in leaflet map and table 
  ### downloadable option could be nice too.

out1<-shapeobj2
for(j in 1:nrow(shapeobj2)){
  
  for(k in 1:number){
    
    if(!(lapply(select(as.data.frame(shapeobj2[j,unique_name]),-geometry),as.character)%in%
         lapply(best[[1]][k,unique_name],as.character))){
      
      for(l in 1:ncol(shapeobj2[j,])){
        out1[j,l]<-NA
      }
    }
    else{
      
      out1[j,]<-shapeobj2[j,]
      
    }
    
  }
} %>% 
out1<-compact(out1)
return(out1)


####profit /s
