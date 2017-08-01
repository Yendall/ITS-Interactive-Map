
shinyServer(function(input, output){
  
  output$map <- renderLeaflet({
    
    union_polygons %>%
    leaflet() %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 0.7, color = "#b20000", group = "Infestation Radius") %>%
    addProviderTiles("OpenStreetMap.DE", options = providerTileOptions(noWrap = TRUE), group = "OpenStreetMap.DE") %>%
    setView(lng=144.96322, lat=-37.814, zoom=10) %>%
    addPolygons(data=VIC_merged, popup = VIC_popover, weight=1,col = 'black',fillOpacity = 0.0,
                highlightOptions = highlightOptions(color = "red", weight = 5,bringToFront = TRUE)) %>%
    addPolygons(data=NSW_merged, popup = NSW_popover, weight=1,col = 'black',fillOpacity = 0.0,
                highlightOptions = highlightOptions(color = "red", weight = 5,bringToFront = TRUE)) %>%
    addPolygons(data=SA_merged, popup = SA_popover, weight=1,col = 'black',fillOpacity = 0.0,
                highlightOptions = highlightOptions(color = "red", weight = 5,bringToFront = TRUE)) %>%
    addPolygons(data=WA_merged, popup = WA_popover, weight=1,col = 'black',fillOpacity = 0.0,
                highlightOptions = highlightOptions(color = "red", weight = 5,bringToFront = TRUE)) %>%
    addPolygons(data=QLD_merged, popup = QLD_popover, weight=1,col = 'black',fillOpacity = 0.0,
                highlightOptions = highlightOptions(color = "red", weight = 5,bringToFront = TRUE)) %>%
    addPolygons(data=NT_merged, popup = NT_popover, weight=1,col = 'black',fillOpacity = 0.0,
                highlightOptions = highlightOptions(color = "red", weight = 5,bringToFront = TRUE)) %>%
    addMarkers(lng=termites_found$longitude, lat= termites_found$latitude, icon=house_icon, popup= termites_found$address, group="Housing Information") %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap.DE"),
      overlayGroups = c("Housing Information", "Infestation Radius"),
      options = layersControlOptions(collapsed = FALSE))
  })
})