##render is reactive



library(shiny)


ui <- fluidPage(
    #Make into tabs
    navbarPage(
        "Best Rep",
        tabPanel("Step 1",
                 #Upload file
                 #select number of reps desired
                 numericInput("n_reps","How many replicates would you like?",value=30,min=2,max=300)
                 ),
        tabPanel("Step 2"
                 #Select which columns you care about
                 
                 ),
        tabPanel("Step 3"
                 #Fixed variables within the columns
                 
                 #Run Button
                 
                 ),
        tabPanel("Results",
                 #leaflet
                 #Table
                 textOutput("test")
                 
                 )
    )
    
    
    
)

        

server <- function(input, output) {
    
    #observe( on button press run the script from the values in place or set defaults.)
    
    output$plot<-leaflet()
    output$test<- renderText({
        paste("You selected:",input$n_reps)
    })

    
}


shinyApp(ui = ui, server = server)