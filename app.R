#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

ui <- fluidPage(
  
  fluidRow(
    
    column(5,
           fileInput("user_file", "Choose CSV File",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),
           downloadButton("download_data", "Download"),
           tags$hr(),
           tableOutput("contents")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetOutput <- reactive({
    
    inFile <- input$user_file
    
    if (is.null(inFile))
      return(NULL)
    
    outgoing.data <- read.csv(inFile$datapath, header = T) %>%
      select(VanID, Date, Time, Name, Phone, Cell.Phone, Email, Role, Status, Signup.Date) %>%
      tidyr::separate(Time, into = c("start", "end"), sep = " - ") %>%
      # parse strings to hours
      mutate(start = parse_date_time(start, '%H:%M %p') - hours(3),
             start = format(start, '%I:%M %p'),
             end = parse_date_time(end, '%H:%M %p') - hours(3),
             end = format(end, '%I:%M %p'),
             Shift = paste(start, end, sep = " - "))  %>%
      dplyr::select(VanID, Date, Shift, Name, Phone, Cell.Phone,
                    Email, Role, Status, Signup.Date) 
    
  })
  
  output$contents <- renderTable({
    datasetOutput() 
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("participants_asof_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetOutput(), file, row.names = F)
    }
  )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
