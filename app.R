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
    
    column(11,
           fileInput("user_file", "Choose CSV File",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),
           downloadButton("download_contacts", "Download Contacts + Shifts"),
           downloadButton("download_schedule", "Download Shift Schedule"),
           tags$hr(),
           tableOutput("contents")
    )
  )
)



server <- function(input, output) {
  
  # generate "long" list -- every shfit and every participant is one row
  datasetOutputLong <- reactive({
    
    inFile <- input$user_file
    
    if (is.null(inFile))
      return(NULL)
    
    outgoing.data <- read.csv(inFile$datapath, header = T) %>%
      dplyr::select(VanID, Date, Time, Name, Phone, Cell.Phone, Email, Role, Status, Signup.Date) %>%
      tidyr::separate(Time, into = c("start", "end"), sep = " - ") %>%
      # parse strings to hours then make time zone adjustment
      dplyr::mutate(start = parse_date_time(start, '%H:%M %p') - hours(3),
                     start = format(start, '%I:%M %p'),
                     end = parse_date_time(end, '%H:%M %p') - hours(3),
                     end = format(end, '%I:%M %p'),
                     Shift = paste(start, end, sep = " - "))
    
  })
  
  # make "wide" list -- visual of scheduled shifts across tope
  # participants on left side, and X for where each participant is signed up
  datasetOutputWide <- reactive({
    
    if (is.null(datasetOutputLong()))
      return(NULL)
    
    datasetOutputLong() %>%
      dplyr::group_by(Date, Shift) %>%
      # get a 24 hour version of the time to correctly sort
      dplyr::mutate(shift_count = n()) %>%
      dplyr::mutate(time_24hr = format(parse_date_time(start, '%H:%M %p'), "%H:%M")) %>%
      dplyr::arrange(Date, time_24hr) %>%
      dplyr::mutate(Date = format(as.Date(Date, tryFormats = "%m/%d/%Y"), "%b %d"),
                    date_shift = paste(Date, Shift, sep = "\n"),
                    date_shift = gsub(" AM", "", date_shift),
                    date_shift = gsub(" PM", "", date_shift),
                    date_shift = paste0(date_shift, " (", shift_count, ")"),
                    participating = "X") %>%
      dplyr::ungroup() %>%
      dplyr::select(date_shift, Name, participating) %>%
      tidyr::pivot_wider(names_from = date_shift, values_from = participating) %>%
      dplyr::mutate_all(~replace(., is.na(.), " ")) 
    
  })
  
  output$contents <- renderTable({
    
    if (is.null(input$user_file))
      return(NULL)
    
    datasetOutputWide()
  })
  
  output$download_contacts <- downloadHandler(
    filename = function() {
      paste0("participants_asof_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetOutputLong() %>%
                  dplyr::select(VanID, Date, Shift, Name, Phone, Cell.Phone,
                                Email, Role, Status, Signup.Date)
                , file, row.names = F)
    }
  )
  
  output$download_schedule <- downloadHandler(
    filename = function() {
      paste0("shifts_asof_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetOutputWide()
                , file, row.names = F)
    }
  )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
