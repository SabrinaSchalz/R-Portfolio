
#setwd("C:/Users/slyth/Documents/R/Shiny/Corvids Hinckley")
#deployApp()

library(tidyverse)
library(mapview)
library(shiny)
library(leaflet)
library(rsconnect)

#map function

coord<-read_csv("hinckley_corvids.csv")

species <- function(name) {
species_name<-coord %>%
filter(Species == name)
return(species_name)
}

all <- function(names) {
all_name<-coord %>%
filter(All == names)
return(all_name)
}


#manunal maps of crows
#crows<-coord%>%
#filter(Species == "Carrion crow")

#crows_ID<-crows%>%
#filter(ID != "None")

#crows_perched<-crows%>%
#filter(Location_Level != "Air")

#crows_air<-crows%>%
#filter(Location_Level == "Air")

#crows_two<-crows%>%
#filter(Count == 2)

#crows_season<-mapview(crows, xcol = "Longitude", ycol = "Latitude", zcol = "Season", cex = "Count", crs = 4269, grid = FALSE)
#crows_id<-mapview(crows_ID, xcol = "Longitude", ycol = "Latitude", zcol = "ID", cex = "ID", crs = 4269, grid = FALSE)
#crows_location<-mapview(crows, xcol = "Longitude", ycol = "Latitude", zcol = "Location_Level", cex = "Count", crs = 4269, grid = FALSE)
#crows_perch<-mapview(crows_perched, xcol = "Longitude", ycol = "Latitude", zcol = "Location_Level", cex = "Count", crs = 4269, grid = FALSE)
#crows_flight<-mapview(crows_air, xcol = "Longitude", ycol = "Latitude", zcol = "Count", cex = "Count", crs = 4269, grid = FALSE)
#crows_pair<-mapview(crows_two, xcol = "Longitude", ycol = "Latitude", zcol = "Season", cex = "Count", crs = 4269, grid = FALSE)




#app


ui <- fluidPage(
  titlePanel("Hinckley Corvids"),

  sidebarLayout(
    sidebarPanel(
p("Carrion crows, magpies and jackdaws spotted in Hinckley since May 2023."),
p("Last Update: 29th March 2024"),
width = 7),

selectInput("Species", 
                  label = "Species",
                  choices = list("All",
					"Carrion crow", 
                              "Magpie",
					"Jackdaw",
					"Raven",
					"Rook",
					"Jay"),
                  selected = "All")

  ),

   fluidRow(),

    mainPanel(
      leafletOutput("crow_map")
    )
)


server <- function(input, output) {


output$crow_map <- renderLeaflet({

data<-switch(input$Species, 
			 "All" = all("All"),
                   "Carrion crow" = species("Carrion crow"),
			 "Magpie" = species("Magpie"),
			 "Raven" = species("Raven"),
			 "Rook" = species("Rook"),
			 "Jay" = species("Jay"),
                   "Jackdaw" = species("Jackdaw"))

crow_map<-mapview(data, xcol = "Longitude", ycol = "Latitude", zcol = "Count", cex = "Count", crs = 4269, grid = FALSE)

    crow_map@map
  })

}

shinyApp(ui = ui, server = server)
