## Only run examples in interactive R sessions
if (interactive()) {
    
    ui <- fluidPage(
        p("The checkbox group controls the select input"),
        checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                           c("Item A", "Item B", "Item C")),
        
        selectInput("inSelect", "Select input",
                    c("Item A", "Item B", "Item C")),
        
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        textOutput("text")
    )
    
    server <- function(input, output, session) {
        observe({
            userfile <- data.frame(input$file1)
             
            if(is.null(userfile))              
                #return(read_csv(userfile)),
                userfile<- (data.frame(Doubles=double(),Ints=integer()))
            
            
            
            
            x <- input$inCheckboxGroup
            
            # Can use character(0) to remove all choices
            if (is.null(x))
                x <- character(0)
            
            # Can also set the label and select items
            updateSelectInput(session, "inSelect",
                              label = paste("Select input label", length(userfile)),
                              choices = names(userfile),
                              selected = tail(userfile, 1)
            )
            
            output$text <- renderPrint({
                userfile
            })
        })
    }
    
    shinyApp(ui, server)
}