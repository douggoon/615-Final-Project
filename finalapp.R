
pacman::p_load("leaflet")
library(shiny)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
ticketsdata <- read.csv("new.csv", stringsAsFactors = FALSE)
df_count <- read.csv("df_count.csv")

ui <- navbarPage("My Application",
                 tabPanel("Introduction",
                          titlePanel("UPS Parking Violations in Boston"),  

                            mainPanel(
                              p("UPS is the largest logistics and shipping company in the United States. There are many costs of running such a large company and one of those costs is paying for 
                                parking violations. Drivers are instructed to avoid illegally parking as much as possible but most of the time in a major city like Boston, there just isn't a legal
                                way to park the vehicle even if it's for a few minutes to drop off packages. In order to stay on schedule, drivers are forced to illegally park. In this project, 
                                we will investigate how serious of an issue parking violations in Boston are for the company using data from over the course of 4 years (2015-2018). The data is from
                                the 4 package centers I am responsible for that deliver into Boston.")
                      
                              ))
                          ,
                 tabPanel("Plot",
                          titlePanel("Number of Violations per Month"),  
                          sidebarLayout(
                            sidebarPanel(
                              p("This bar plot shows the number of violations per month over the last 4 years. November and December have the highest number of violations because it is 
                                the peak/holiday season. Volume of packages increases, which means there are more drivers on the road and for longer work days. Also, temporary drivers are hired,
                                so there are more inexperienced drivers on the road. The last factor also is the inclement weather can make it hard for drivers to find places to pull over to 
                                drop off packages. There is also a spike in the summer months of June and July, which might indicate the volume increases in the city.")
                            ),
                          mainPanel(
                            plotOutput("plot")
                          )
                 ))
                 ,
                 tabPanel("Map",
                          titlePanel("Location of UPS Violations in Boston"),  
                          sidebarLayout(
                            sidebarPanel(
                              p("The areas of red circles are proportional to the frequencies of violations at each location.")
                            ),
                          mainPanel( 
                            leafletOutput("tmap", width="900", height="600")
                          )
                 )
                 
))

server<-function(input, output, session) {
  output$plot <- renderPlot({
    df_count$Month <- factor(df_count$Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
    ggplot(df_count, aes(x = factor(Month), y = freq, fill = factor(Year))) + 
      geom_col(stat = "identity") +
      labs(title = "Number of Violations", x = "Month", y = "Count") + scale_fill_discrete(name = "Year") +
      theme()
  })
  
  output$tmap <- renderLeaflet({
    pointsdt <- ticketsdata
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng = pointsdt$lon, 
                       lat = pointsdt$lat, 
                       label= as.character(pointsdt$location),
                       radius = sqrt(pointsdt$freq),
                       color = "red") %>%
      setView(lng = -71.0809757, lat = 42.3502648, zoom = 13)
  })
  
  
}
shinyApp(ui = ui, server = server)