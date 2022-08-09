
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(leaflet)
library(rgdal)
library(janitor)
library(devtools)
library(formattable)
library(tidyr)
library(reshape2)
library(data.table)
library(DT)
library(expss)
library(rsconnect)

#setwd("//mlpp-server02/MyDocRedirect/annek/My Documents/R")
#getwd()

#https://towardsdatascience.com/create-interactive-map-applications-in-r-and-r-shiny-for-exploring-geospatial-data-96b0f9692f0f
# Inspo: https://nationalequityatlas.org/dashboard-neighborhood-affordability-for-renters


# Input data: shapefiles and csvs
detroit <- readOGR("~/R/OZ_app", "City_of_Detroit_Boundary")
tracts <- readOGR("~/R/OZ_app", "Clean_DetroitTracts")
tracts$cat<- "OZ"
tracts$cat[tracts$Cat_map==0] <-"Eligible"
tracts$cat[tracts$Cat_map==4] <-"Contiguous"
tracts$cat[is.na(tracts$Cat_map)] <- "Not eligible"
pal <- colorFactor(
  palette = c('#f05821', '#28546b', '#808080','#b9d645'),
  domain = tracts$cat
)
df_table <- readRDS(file="master_table.Rda")



ui <- fluidPage(
  fluidRow(
    column(5,
           leafletOutput("map")
    ),
    column(7,
           titlePanel("Detroit Opportunity Zones"),
           uiOutput('text'),
           tableOutput('table'),
           br(),
           p("Source: MLPP analysis of American Community Survey
             (2011-2015)")
    )
  )
)

server <- function(input, output, session) {
  clean_table <- function(table1, table2){
    table3 <- melt(table1, id.vars=c("GEOID"), 
               variable.name="Stats")
    VF_display <- merge(table3, table2, by="Stats")
    VF_display$Indicator <- c("Median family income",
                              "Median home value",
                              "Population",
                              "Poverty rate")
    VF_display <- subset(VF_display, select = -c(Stats, GEOID))
    VF_display <- VF_display[, c(5, 1, 2, 3, 4)]
    colnames(VF_display) <- c("Indicator", 
                              "Selected", "OZ", 
                              "Eligible", 
                              "All tracts")
    VF_display
  }
  selection <- head(df_table,1)
  summarytable <- readRDS(file="summarystats.rda")

  output$map <- renderLeaflet({
    leaflet()%>% 
      addPolygons(data=detroit, layerId=~label, 
                  weight = 1, smoothFactor=.5, 
                  opacity=1, 
                  color="#444444"
      )%>%
      addPolygons(data=tracts,layerId=~GEOID,
                  weight=1, smoothFactor=0.5, opacity=1,
                  fillOpacity=0.5,
                  color = ~pal(cat),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      addLegend(position="bottomright", 
                pal=pal, values=tracts$cat,
                title="OZ status", opacity=1
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  }) 
  
  observeEvent(input$map_shape_click, {
    click = input$map_shape_click
    sub = tracts[tracts$GEOID==input$map_shape_click$id, c("INTPTLAT", "INTPTLON", "GEOID")]
    lat = sub$INTPTLAT
    lon = sub$INTPTLON
    name = sub$GEOID
    status = sub$cat
    select_new <- filter(df_table, GEOID==click$id)
    output$table <- renderTable({
      clean_table(select_new, summarytable)
    })
    if(is.null(click))
      return
    else
      output$text <- renderUI(
        HTML(
          as.character(
            div(
              style="color:#f05821",
                              paste0(name, ": ", 
                              select_new$cat, " tract")
              )
            )
          )
      )
  })
}

shinyApp(ui, server)

