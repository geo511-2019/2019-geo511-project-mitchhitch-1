

###Function to unify csv's to sf objects with same crs as File1 
#####(note that File 1 must be a shape file)
#####This should be run AFTER columns have been trimmed
TransCoord_csv<-function(File1,FileX,FileX_Lon="Longitude",FileX_Lat="Latitude",FileX_crs=4326){
  
  FileX<-st_as_sf(FileX,coords=c(FileX_Lon,FileX_Lat)) %>% 
    st_set_crs(FileX_crs) %>% 
    st_transform(st_crs(File1)) %>% 
    st_crop(st_bbox(File1))
  FileX
  
}


### Join all files into 1 sf ONLY run afer TransCoord_csv or st_transform for shape files.
All_F<-function(File1,...){
  OtherF<-list(...)
  countF<-length(OtherF)
  MainF<-File1
  for(i in 1:countF){
    MainF=st_join(OtherF[[1]],MainF,join=st_nn,k=1,maxdist=500) %>% 
      aggregate( by=list(.[,1]),FUN=unique,do_union=TRUE,join = st_intersects)
    
  }

}


###This ranks the similarit of rows
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


###Select best sets of sites
Out_best<-function(possible_n){
  #Determine min possible value for sets
minimum<-foreach(i=1:length(possible_n),.combine=c)%dopar%{
  sum(possible_n[[i]]$Score)
} %>% 
  min()
  #Select sets with min possible value
best<-foreach(i=1:length(possible_n))%dopar%{
  if(sum(possible_n[[i]]$Score)==minimum){
    return(possible_n[[i]])
  }
} %>% 
  compact()

best

}
