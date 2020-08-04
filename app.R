library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(readr)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(data.table)
library(sp)
library(sf)
library(rgdal)
library(KernSmooth)
library(raster)
library(shinyBS)
library(RCurl)

incidents <- read_csv("https://raw.githubusercontent.com/ScottStetkiewicz/Cumberland_Fire/master/incidents.csv")
geo_unique<-incidents %>% filter(!(is.na(lat)))

ui<-fluidPage(theme = shinytheme("cerulean"),
              sidebarLayout(
                  sidebarPanel(width=2,
                               selectInput("tiles", "Provider Tiles",
                                           c("Esri World Imagery" = providers$Esri.WorldImagery,
                                             "Stamen Toner" = providers$Stamen.Toner,
                                             "NatGeoWorldMap" = providers$Esri.NatGeoWorldMap,
                                             "CartoDB Positron" = providers$CartoDB.Positron),
                                           selected = providers$Stamen.Toner,
                                           multiple = FALSE
                                           ),
                               prettySwitch("add_points","Plot Data Points", fill = TRUE, status = "primary",FALSE),
                               sliderInput("range", "Number of Incidents", min(geo_unique$N), max(geo_unique$N),
                                           value = range(geo_unique$N), step = 1
                               ),
                               selectInput("colors", "Color Scheme",
                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                               ),
                               prettySwitch("legend","Show legend", fill = TRUE, status = "warning",FALSE),
                               prettySwitch("kde","KDE Heatmap", fill = TRUE, status = "success",FALSE),
                               bsTooltip("kde", "This will take a few seconds to reactively compile...", options = list(container = "body")),
                               conditionalPanel(condition="input.kde == true",
                                                   sliderInput("kde_range", "KDE Raster Values", min(1), max(650),
                                                               value = 20, step = 10
                                                   )
                                              )
                  ),
                  mainPanel(width=10,
                      tags$style(type = "text/css", "#map {height: calc(100vh - 53px) !important;}"),
                      leafletOutput("map"),
                      tags$style(type = "text/css", ".container-fluid {padding-left:0px; padding-right:0px;}"),
                      tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
                      tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}")
                  )
              )
)

server <- function(input, output, session) {

    # url<-readOGR('https://raw.githubusercontent.com/ScottStetkiewicz/Cumberland_Fire/blob/master/ri/Municipalities__1997_.shp',layer = 'Municipalities__1997_.shp')
    # ri <- readOGR('~/Desktop/R/CFD/ri/Municipalities__1997_.shp')
    # fname <- getURL('http://raw.githubusercontent.com/ScottStetkiewicz/Cumberland_Fire/master/ri/Municipalities__1997_.shp')
    # ri<-readOGR(fname)
    
    # fname <- sf::st_read('http://raw.githubusercontent.com/ScottStetkiewicz/Cumberland_Fire/master/ri/Municipalities__1997_.shp')
    # fname <- sf::st_read('~/Desktop/R/CFD/ri/Municipalities__1997_.shp')
    # ri <- readOGR(dsn='https://github.com/ScottStetkiewicz/Cumberland_Fire/tree/master/ri', layer = 'Municipalities__1997_.shp')
    # ri <- readOGR(dsn='https://raw.githubusercontent.com/ScottStetkiewicz/Cumberland_Fire/master/ri/Municipalities__1997_.shp', layer = 'Municipalities__1997_')
    
    ri <- read_csv("https://raw.githubusercontent.com/ScottStetkiewicz/Cumberland_Fire/master/ri/ri.csv")
    cumb = subset(ri, NAME=="CUMBERLAND")

    filteredData <- reactive({
        geo_unique[geo_unique$N >= input$range[1] & geo_unique$N <= input$range[2],]
    })

    colorpal <- reactive({
        colorNumeric(input$colors, geo_unique$N)
    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(input$tiles) %>%
            fitBounds(-71.81074,42.03144,-71.00805,41.77592)
    })

    observe({

        proxy<-leafletProxy("map", data = filteredData(), session)

        if (input$add_points) {

        pal <- colorpal()

        labs <- lapply(seq(nrow(filteredData())), function(i) {
            paste0( '<p>', "<b>Address: </b>",filteredData()[i, "LOCATION"], '<p></p>',
                    "<b>Number of Incidents: </b>",filteredData()[i, "N"], '<p></p>'
            )
        })

        proxy %>%
            clearShapes() %>%
            addPolygons(data = cumb,
                        group = "Cumberland",
                        fillOpacity = 0.05,
                        weight = 5) %>%
            addLayersControl(overlayGroups = "Cumberland") %>%
            addCircles(group = "points",
                radius = ~ifelse(N<=5,35,sqrt(N)*50),
                             weight = 2,
                             color = "black",
                             fillColor = ~pal(N),
                             fillOpacity = .65,
                             label = lapply(labs, htmltools::HTML),
                       labelOptions = labelOptions(
                           style=list(
                               'font-family'= 'serif',
                               'background'='rgba(243, 241, 239, 1)',
                               'border-color' = 'rgba(46, 49, 49, 1)',
                               'border-radius' = '2px',
                               'border-style' = 'solid',
                               'border-width' = '2px'))
            )
        } else {
            proxy %>%
                clearGroup("points")
        }
    })

    observe({
        proxy <- leafletProxy("map", data = geo_unique)
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal,
                                values = ~N
            )
        }
    })

    observe({
        
        gu<-data.table(geo_unique)
        g2<-gu %>% filter(between(lat,41.8,42.2))
        kde <- bkde2D(g2[ , list(long, lat)],
                      bandwidth=c(.0045, .0068), gridsize = c(1000,1000))
        KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
        KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < input$kde_range)] <- NA
        palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, reverse = TRUE, na.color = "transparent")
        
        
        if (input$kde){
            leafletProxy("map", session) %>%
                clearImages() %>%
                clearControls() %>%
                addRasterImage(KernelDensityRaster,
                               colors = palRaster,
                               opacity = .6) %>%
                addLegend(pal = palRaster,
                          values = KernelDensityRaster@data@values,
                          title = "Kernel Density of Points",
                          opacity=1)
        } else {
            leafletProxy("map", session) %>%
                clearShapes() %>%
                clearImages()
        }
    })
}

shinyApp(ui, server)