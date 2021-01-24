#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(readr)
library(maps)

sites<-read_csv("./Data/Sites.csv")


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("UK meteo data"),
    tabsetPanel(
        tabPanel("Plot",
                 #columns
                 fluidRow(
                     column(3,
                            wellPanel(
                                selectizeInput("stations",
                                               "Select Stations", 
                                               sites[,2], #site station
                                               options = list(maxItems = 5)
                                ),
                                radioButtons("weather",
                                             "Select Weather variable",
                                             c("Wind speed"="wind_speed",
                                               "Air temperature"="air_temperature",
                                               "Humidity"="rltv_hum",
                                               "Visibility"="visibility")
                                ),
                                radioButtons("aggregation",
                                             "Select Aggregation",
                                             c("Raw hourly data",
                                               "Daily averages",
                                               "Monthly averages",
                                               "Daily maxima",
                                               "Daily minima")
                                ),
                                uiOutput("timeaxis")
                                
                            )
                            
                            
                     ),
                     column(6,
                            plotOutput("Graph")
                            
                     ),
                     column(3,
                            plotOutput("map")
                            
                     )
                 ),
                 
                 fluidRow(
                     column(3,
                            wellPanel(
                                downloadButton("downloadTable", "Download Table"),
                                downloadButton("downloadReport","Download Report")
                            )
                     ),
                     column(9,
                            dataTableOutput("summary_table")
                     ),
                     column(3,
                     )
                 )
        ),
        tabPanel("Hutton",
                 fluidRow(
                     column(3,
                            wellPanel(
                                selectizeInput("hutton_station","Select station",sites[,2]),
                                uiOutput("pickMonth")
                            )
                     ),
                     column(6,
                            dataTableOutput("hutton_table")
                     ),
                     column(3)
                 ))
        
    )
))