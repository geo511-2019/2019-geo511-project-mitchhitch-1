##render is reactive



library(shiny)
library(leaflet)
library(dplyr)
library(tools)
library(sf)
library(sp)
library(tidyverse)
library(doParallel)
library(nngeo)#st_nn
library(stringr)



ui <- fluidPage(
    #Make into tabs
    navbarPage(
        "Best Rep",
        tabPanel("Step 1",
                 #Upload file1
                 h2('Upload your files'),
                 textOutput("at least one must be a .shp file (.csv with headers and .shp only"),
                 fileInput('file1', 'This must be a shp'),
                 #upload file2
                 fileInput('file2', 'Choose second file'),
                 #select number of reps desired
                 numericInput("n_reps","How many replicates would you like?",value=30,min=2,max=300)
                 ),
        
        tabPanel("Step 2",
                 
                 #Select which columns you care about backend in server
                 uiOutput("SelectCols1"),
                 uiOutput("SelectCols2")
                 
                 
                 ),
        tabPanel("Step 3",
                 #Fixed variables within the columns
                 
                 #Run Button
                 actionButton("RUN", "Click and walk away")
                 ),
        tabPanel("Results",
                 #leaflet
                 #Table
                 leafletOutput("bestmap"),

                 
                 )
    )
    
    
    
    
)

        

server <- function(input, output,session) {
    options(shiny.maxRequestSize=30000*1024^2)
    
    #################funcz
    #####This should be run AFTER columns have been trimmed
    TransCoord_csv<-function(File1,FileX,FileX_Lon="Longitude",FileX_Lat="Latitude",FileX_crs=4326){
        
        FileX<-st_as_sf(FileX,coords=c(FileX_Lon,FileX_Lat)) %>% 
            st_set_crs(FileX_crs) %>% 
            st_transform(st_crs(File1)) %>% 
            st_crop(st_bbox(File1))
        FileX
        
    }
    
    
    ### Join all files into 1 sf ONLY run afer TransCoord_csv or st_transform for shape files.
    All_F<-function(File1,File2){
        # OtherF<-list(...)
        # countF<-length(OtherF)
        MainF<-File1
        # for(i in 1:countF){
        #     MainF=st_join(OtherF[[1]],MainF,join=st_nn,k=1,maxdist=500) %>% 
        #         aggregate( by=list(.[,1]),FUN=unique,do_union=TRUE,join = st_intersects)
        # }
        File2<-File2
        MainF=st_join(File2,MainF,join=st_nn,k=1,maxdist=500) %>% 
            aggregate( by=list(.$Program.Facility.Name),FUN=unique,do_union=TRUE,join = st_intersects)
        MainF
        
    }
    
    ###runs the itterative ranking
    Run.score<-function(shapeobj_trim,number){
        number=number
        foreach(r=1:nrow(shapeobj_trim))%dopar%{
            x<-shapeobj_trim %>% 
                select(-geometry)
            x_ret<-shapeobj_trim
            for(i in 1:nrow(x)){
                
                x_ret[i,"Score"]<-score.calc(x[r,],x[i,])
                
            }
            
            x_ret<-head(x_ret[order(x_ret$Score),],n=number)
            x_ret
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
    ##########################3
    ###File1
    data1<- reactive({
        filein <- input$file1
        if (is.null(filein)) { 
            return() 
        } 
        else if (file_ext(filein$datapath)=="csv"){
            data = read.csv(file=filein$datapath)
            return(data)
        }
        else{
            # if (str_trunc(file_ext(filein),3,"right",ellipsis="")=="zip"){
            #     shpDF<-unzip(filein)
            # }
            # else{
                shpDF <- filein
            # }
            
            prevWD <- getwd()
            dir.create("unzipped")
            tempdir <- "unzipped"
            unzip(shpDF$datapath, exdir = tempdir)
            setwd(tempdir)
            setwd(list.dirs(recursive = FALSE))
            files<-
            # for (i in 1:list.files()){
            #     file.rename(list.files()[i], shpDF$name[i])
            # }
            shpName <- list.files()[grep(list.files(),pattern="*.shp")[1]]
            shpFile <- st_read(shpName)
            return(shpFile)
        } 
        
    })
    
    
    ###File2
    data2 <- reactive({
        filein <- input$file2
        if (is.null(filein)) { 
            return() 
        } 
        else if (file_ext(filein$datapath)=="csv"){
            data = read.csv(file=filein$datapath)
            return(data)
        }
        else if (!is.null(filein)){
            # if (str_trunc(file_ext(filein),3,"right",ellipsis="")=="zip"){
            #     shpDF<-unzip(filein)
            # }
            # else{
                shpDF <- filein
            # }
                
            prevWD <- getwd()
            dir.create("unzipped")
            tempdir <- "unzipped"
            unzip(shpDF$datapath, exdir = tempdir)
            setwd(tempdir)
            setwd(list.dirs(recursive = FALSE))
            files<-
                # for (i in 1:list.files()){
                #     file.rename(list.files()[i], shpDF$name[i])
                # }
            shpName <- list.files()[grep(list.files(),pattern="*.shp")[1]]
            shpFile <- st_read(shpName)
            return(shpFile)
        } 
        else {
            return()
        }

    })
 
    ##This is just a demo table for File1
    output$table_display <- renderTable({
        f <- data1()
        f <- select(f, c(input$columns1)) #subsetting takes place here
        head(f)
    })
    

    ###This allows for selecting desired columns
    ##File1
    output$SelectCols1 <- renderUI({
        selectInput("columns1", "Select Columns", choices= names(data1()),multiple = TRUE)
    })
    ##File2
    output$SelectCols2 <- renderUI({
        selectInput("columns2", "Select Columns", choices= names(data2()),multiple = TRUE)
    })
    
    
    
    RealRun<-eventReactive(input$RUN,{ 
        withProgress(message = 'Running Comparison', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        req(input$columns1)
        req(input$columns2)
        number<-input$n_reps
        incProgress(1/n, detail = paste("Doing part", 1))
        registerDoParallel(cores = 6)
        incProgress(1/n, detail = paste("Doing part", 2))
        
        incProgress(1/n, detail = paste("Doing part", 3))
        
        data1_trim<-select(data1(), input$columns1)
        incProgress(1/n, detail = paste("Doing part", 4))
        
        data2_trim<-select(data2(), input$columns2)
        incProgress(1/n, detail = paste("Doing part", 5))
        
        data2_tran<-TransCoord_csv(data1_trim,data2_trim)
        incProgress(1/n, detail = paste("Doing part", 6))
        
        data_ALL<-All_F(data1_trim,data2_tran)
        incProgress(1/n, detail = paste("Doing part", 7))
        
        Ranked_list<-Run.score(data_ALL,number)
        incProgress(1/n, detail = paste("Doing part", 8))
        
        best<-Out_best(Ranked_list)
        incProgress(1/n, detail = paste("Doing part", 9))
        
        first<-st_transform(best[[1]],'+proj=longlat +datum=WGS84')
        incProgress(1/n, detail = paste("Doing part", 10))
        
        first
        })
    })
    
    output$bestmap <- renderLeaflet({
        
        leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(data = st_geometry(RealRun()), popup=first$Program.Facility.Name)
        
    })
    
    
    #observe( on button press run the script from the values in place or set defaults.)


    
}


shinyApp(ui = ui, server = server)