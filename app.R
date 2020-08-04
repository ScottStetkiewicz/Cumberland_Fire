# if (interactive()) {
#   library(shiny)
#   library(ECharts2Shiny)
#   library(readr)
#   
#   master_df <- read_csv("~/Desktop/Johnny Python/terp_radar.csv")
#   can<-master_df %>% 
#     select(3,5:6) %>% 
#     group_by(Strain) %>% 
#     spread(Terpene, Amount)
#     
#   can2<-can[1:20,] 
#   can3<-can2[,2:10] 
#   row.names(can3) <- can2$Strain
#   can3<-as.data.frame(can3)
#   can3[is.na(can3)] <- 0
#   can4<-data.table::transpose(can3)
#   colnames(can4) <- rownames(can3)
#   rownames(can4) <- colnames(can3)
#   
#   # Server function -------------------------------------------
#   server <- function(input, output) {
#     renderRadarChart(div_id = "test",
#                      data = can4)
#   }
#   
#   # UI layout -------------------------------------------------
#   ui <- fluidPage(
#     # We MUST load the ECharts javascript library in advance
#     loadEChartsLibrary(),
#     
#     tags$div(id="test", style="width:50%;height:400px;"),
#     deliverChart(div_id = "test")
#   )
#   
#   # Run the application --------------------------------------
#   shinyApp(ui = ui, server = server)
# }
# # }

library("leaflet")
library("data.table")
library("sp")
library("rgdal")
# library("maptools")
library("KernSmooth")
library("raster")

# inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
# infile <- "mvthefts.csv"

## LOAD DATA
## Also, clean up variable names, and convert dates
# if(!file.exists(infile)){
#   download.file(url = inurl, destfile = infile)
# }
# dat <- data.table::fread(infile)
# setnames(dat, tolower(colnames(dat)))
# setnames(dat, gsub(" ", "_", colnames(dat)))
# dat <- dat[!is.na(longitude)]
# dat[ , date := as.IDate(date, "%m/%d/%Y")]

## Create kernel density output
# kde <- bkde2D(dat[ , list(longitude, latitude)],
#               bandwidth=c(.0045, .0068), gridsize = c(1000,1000))
# gu<-data.table(geo_unique)
# g2<-gu %>% filter(between(lat,41.8,42.2))
# kde <- bkde2D(g2[ , list(long, lat)],
#               bandwidth=c(.0045, .0068), gridsize = c(1000,1000))
# # Create Raster from Kernel Density output
# KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
# 
# #set low density cells as NA so we can make them transparent with the colorNumeric function
# KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 50)] <- NA
# 
# #create pal function for coloring the raster
# palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, reverse = TRUE, na.color = "transparent")
# 
# 
# ri <- readOGR('~/Desktop/R/CFD/ri/Municipalities__1997_.shp')
# cumb = subset(ri, NAME=="CUMBERLAND")
# 
# ## Leaflet map with raster
# leaflet() %>% addTiles() %>%
#   addPolygons(data=cumb, opacity = .1, fillOpacity = 0.05,weight = 5) %>%
#   addRasterImage(KernelDensityRaster, 
#                  colors = palRaster, 
#                  opacity = .6) %>%
#   addLegend(pal = palRaster, 
#             values = KernelDensityRaster@data@values, 
#             title = "Kernel Density of Points")

library(mapview) # for the data
library(leaflet)
library(sf)      # for data wrangling
library(dplyr)   # for data wrangling

# create some borders
borders = franconia %>%
  group_by(district) %>%
  summarise() %>%
  st_cast("MULTILINESTRING")

# create a map with individual layer ordering using addMapPane
leaflet() %>%
  addTiles() %>%
  addMapPane("polygons", zIndex = 410) %>%
  addMapPane("borders", zIndex = 420) %>%
  addPolygons(data = franconia, group = "franconia", weight = 2,
              options = pathOptions(pane = "polygons")) %>%
  addPolylines(data = borders, group = "district", color = "black",
               opacity = 1, options = pathOptions(pane = "borders")) %>%
  addLayersControl(overlayGroups = c("franconia", "district"))