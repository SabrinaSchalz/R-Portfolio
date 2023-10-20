
#setwd("C:/Users/slyth/Documents/R/Shiny/Corvids Hinckley")
#deployApp()

library(tidyverse)
library(mapview)
library(shiny)
library(leaflet)
library(rsconnect)

#map

coord<-read_csv("hinckley_corvids.csv")

species <- function(name) {
species_name<-coord %>%
filter(Species == name)
return(species_name)
}


#app


ui <- fluidPage(
  titlePanel("Hinckley Corvids"),

  sidebarLayout(
    sidebarPanel(
p("Carrion crows, magpies and jackdaws spotted in Hinckley since May 2023."),
p("Winter = Dec - Feb, Spring = Mar - May, Summer = Jun - Aug, Autumn = Sep - Nov)"),
p("Last Update: 16th October 2023"),
width = 7),

selectInput("Species", 
                  label = "Species",
                  choices = list("Carrion crow", 
                                 "Magpie",
					   "Jackdaw"),
                  selected = "Carrion crow")

  ),

   fluidRow(),

    mainPanel(
      leafletOutput("crow_map")
    )
)


server <- function(input, output) {


output$crow_map <- renderLeaflet({

data<-switch(input$Species, 
                   "Carrion crow" = species("Carrion crow"),
			 "Magpie" = species("Magpie"),
                   "Jackdaw" = species("Jackdaw"))

crow_map<-mapview(data, xcol = "Longitude", ycol = "Latitude", zcol = "Count", cex = "Count", crs = 4269, grid = FALSE)

    crow_map@map
  })

}

shinyApp(ui = ui, server = server)
