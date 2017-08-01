devtools::install_github("rstudio/shiny")
require(devtools)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(maptools)
library(RMySQL)
library(geojsonio)
library(jsonlite)
library(rgdal)
library(RJSONIO)
library(dplyr)
library(sf)
library(sp)
library(KernSmooth)
library(RColorBrewer)
library(rCharts)


# Connect to TR database
lapply(dbListConnections(MySQL()), dbDisconnect)
termite_db = dbConnect(
  MySQL(),
  user='termite',
  password='T*&^tUYjhgeweis78gvu786%',
  dbname='termite',
  host='termiterdsdylannew.chumy75aae7w.ap-southeast-2.rds.amazonaws.com')

dbListTables(termite_db)
dbListFields(termite_db, 'termiteReport')

# Fetch property data
rs = dbSendQuery(termite_db, "SELECT * FROM termiteReport")
termite_data = fetch(rs, n=-1)

# Fetch shire data based on state prefix
GetShireData <- function(prefix)
{
  location <- paste('ShapeFiles/',tolower(prefix),"lgapolygonshp/",toupper(prefix),"_LGA_POLYGON_shp.shp",sep = "")
  layer <- paste(toupper(prefix),"_LGA_POLYGON_shp",sep = "")
  
  return(readOGR(location,layer=layer,GDAL1_integer64_policy = TRUE))
}

Shires <- new.env()
Shire_Metadata <- new.env()
Shire_names <-c("VIC","WA","TAS","NSW","QLD","SA","NT","ALL")

for (shire in Shire_names) {
  Shires[[shire]] <- GetShireData(shire)
}

# Filter down dataset
termites_found = filter(termite_data, termiteFound == 1)
termites_notfound = filter(termite_data, termiteFound == 0)

# Declare sf data frame and perform Spatial union between points (assumed radius)
RadialUnion <- function()
{
  
  sf_df <- st_as_sf(termites_found, coords=c("longitude","latitude"))
  sf_circles <- st_buffer(sf_df, dist = 0.01)
  sf_combined <- st_union(sf_circles)
  sp <- as(sf_combined, 'Spatial')
  return(sf_combined)
}

# Check if points fall within a shire and start aggregating
AggregatePoints <- function()
{
  coordinates(termites_found) <- ~ longitude + latitude
  proj4string(termites_found) <- proj4string(Shires[["ALL"]])
  proj4string(termites_found) <- CRS("+proj=longlat")
  termites_found <- spTransform(termites_found, proj4string(Shires[["ALL"]]))
  overlap <- (over(termites_found, Shires[["ALL"]]))
  overlap <- sapply(overlap, table)
  return(overlap)
}

MergeDataFrames <- function(shire_prefix,shire_col, df_aggregate)
{
  colnames(df_aggregate) <- c(shire_col, "TermiteCount")
  merged <- merge(Shires[[shire_prefix]], df_aggregate, by = shire_col)
  return(merged)
}

# Aggregate all data points for each shire in Australia

overlap <- AggregatePoints()
print("hello")

VIC_aggregate <- as.data.frame(overlap$VIC_LGA__2)
NSW_aggregate <- as.data.frame(overlap$NSW_LGA__2)
WA_aggregate  <- as.data.frame(overlap$WA_LGA_s_2)
TAS_aggregate <- as.data.frame(overlap$TAS_LGA__2)
QLD_aggregate <- as.data.frame(overlap$QLD_LGA__2)
NT_aggregate  <- as.data.frame(overlap$NT_LGA_s_2)
SA_aggregate  <- as.data.frame(overlap$SA_LGA_s_2)

VIC_merged    <- MergeDataFrames("VIC","VIC_LGA__2",VIC_aggregate)
NSW_merged    <- MergeDataFrames("NSW","NSW_LGA__2",NSW_aggregate)
WA_merged     <- MergeDataFrames("WA","WA_LGA_s_2",WA_aggregate)
TAS_merged    <- MergeDataFrames("TAS","TAS_LGA__2",TAS_aggregate)
QLD_merged    <- MergeDataFrames("QLD","QLD_LGA__2",QLD_aggregate)
NT_merged     <- MergeDataFrames("NT","NT_LGA_s_2",NT_aggregate)
SA_merged     <- MergeDataFrames("SA","SA_LGA_s_2",SA_aggregate)

shire_string <- "<strong>Shire: </strong>"
infestation_string <- "</br><strong> Termite Infestation Reported: </strong>" 

VIC_popover <- paste0(shire_string,VIC_merged$VIC_LGA__2, infestation_string, VIC_merged$TermiteCount)
NSW_popover <- paste0(shire_string,NSW_merged$NSW_LGA__2, infestation_string, NSW_merged$TermiteCount)
WA_popover  <- paste0(shire_string,WA_merged$WA_LGA_s_2, infestation_string, WA_merged$TermiteCount)
TAS_popover <- paste0(shire_string,TAS_merged$TAS_LGA__2, infestation_string, TAS_merged$TermiteCount)
QLD_popover <- paste0(shire_string,QLD_merged$QLD_LGA__2, infestation_string, QLD_merged$TermiteCount)
NT_popover  <- paste0(shire_string,NT_merged$NT_LGA_s_2, infestation_string, NT_merged$TermiteCount)
SA_popover  <- paste0(shire_string,SA_merged$SA_LGA_s_2, infestation_string, SA_merged$TermiteCount)

# Create union based merges for data points
union_polygons <- RadialUnion()

#Create house icon for individual points
house_icon <- makeIcon("Icons/House.png", iconWidth = 12, iconHeight = 12)

