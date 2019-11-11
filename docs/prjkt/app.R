#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 
# 
library(shiny)
library(datasets)
library(miniUI)

ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel( 
            ####user uploads points of interest####
            fileInput("file1", "Upload point data (csv only)",
                      # limit the file extention to the following:
                      accept=c("text/csv", 
                               "text/comma-separated-values,text/plain", 
                               ".csv")),
        
            ###Option to use default(erie county parcels)
            selectInput("shapselect", label = "Use default(only Erie Count, NY) shape file?",
                        choices = c(TRUE,FALSE), selected = FALSE),
            
         #CHANGE:This part should be in the reactive server#############
            # if(shapeselect=FALSE)
            #     fileInput("file2", "Choose optional shape File",accept=".shp"),
            
            h5("Upload file for data analysis"),
            
            # no choices before a file uploaded
            uiOutput("columns"),
            
            # inputs to generate pivot table (demo in a single tab here, but in real app, this will be on a different tab)
            selectInput("vars", "vars to use:", choices = NULL, multiple = TRUE, selected = NULL),
            selectInput("fromvars", "select from vars:", choices = NULL, multiple = TRUE, selected = NULL)
        ),
        mainPanel( 
            verbatimTextOutput("print")
        ))
)

server <- function(input, output, session) {
    options(shiny.maxRequestSize=30*1024^2)
    dt <- reactiveValues()
    
    # upload file
    observeEvent(input$file, {
        inFile <- input$file
        req(inFile)
        # upload the file to dataset
        dt$data = read.csv(inFile$datapath, header = TRUE)
    })
    
    # create columns groupcheckbox ui
    output$columns <- renderUI({
        
        # get the col names of f and assign them to a list
        cols = mapply(list, names(dt$data))
        
        # render column group checkbox ui after loading the data
        checkboxGroupInput("columns", "Select columns to display", choices = cols, selected = cols)
    })

    ######## dependent on columns selection ########
    observeEvent(input$columns, {
        dt$cols <- input$columns
        if (is.null(dt$cols)) dt$cols <- ""
        # update input$vars for pivottable tab
        print("observerEvent")
        print(dt$cols)
        
        updateSelectInput(session, "vars", "vars to use:", choices = dt$cols, selected = NULL)
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$vars, {
        if( is.null(input$vars)) { 
            updateSelectInput(session, "fromvars", "select from vars:", choices = "", selected = NULL)  
        }
        else {
            updateSelectInput(session, "fromvars", "select from vars:", choices = input$vars, selected = NULL)
        }
    }, ignoreNULL = FALSE)
    
    output$print <- renderPrint(
        list(
            paste("input$columns:", paste(input$columns,collapse=",")),
            paste("input$vars:",paste(input$vars,collapse=",")), 
            paste("input$fromvars:", paste(input$fromvars,collapse=","))
        )
    )
}
shinyApp(ui = ui, server = server)





