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
    titlePanel("Old Faithful Geyser Data"),
    mainPanel(
        actionButton("updater", "Update message"),
        textOutput("textbox")
    )
)


glycoPipe <- function() {
    start_time <- Sys.time()
    list(
        today      = weekdays(start_time, abbreviate = FALSE),
        start_time = start_time
    )
}


server <- function(input, output) {
    result <- glycoPipe()
    start_time_text <- strftime(result$start_time, "%H:%M:%S")
    
    output$textbox <- renderText({
        input$updater
        elapsed <- Sys.time() - result$start_time
        sprintf(
            "Started at %s on %s, which was %.0f seconds ago",
            start_time_text,
            result$today,
            elapsed
        )
    })
}


shinyApp(ui = ui, server = server)