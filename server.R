library(acs)
library(tigris)
library(stringr)
library(leaflet)

#---------Given state, end year, table code, table row #, and generate map.
# endyear is a number (e.g. 2015),
# tcode is the code of the desired table as a string (e.g. "B17001"),
# rcode is the desired sub-table.
# Set ratio to TRUE if you want the proportion of the population/households rather 
# than the total.
# Developed by Katie Clary 03.31.2017
# Based on code from this website: http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way
#tracts <- geo.make(state = sc.num, county = "*", tract = "*")

# must install key to use census data.
# only need to do once
# api.key.install(key="591bda1a6151f1f125d11f35a2d5d8878a0df43b") 

factorName_n <- c("Affluence", 
                  "Singletons\nin Tract", 
                  "Seniors\nin Tract", 
                  "African\nAmericans\nin Tract", 
                  "Noncitizens\nin Tract")

function(input, output, session){
  FA_Results <- readRDS("FA_5Factors.rds")
  rownames(FA_Results$scores) <- str_pad(rownames(FA_Results$scores), 11, "left", pad = 0)
  
  
  output$map <- renderLeaflet({ 
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  sc.char <- reactive({input$sc.num})
  code <- reactive({as.numeric(input$code)})
  
  title <- reactive({factorName_n[code()]})
  
  tract.shape <- reactive({
    readRDS(paste0("tracts/tractSPDF_", sc.char(),".rds"))#tracts(state = as.numeric(sc.char), county = NULL) 
  })
  
  df <- reactive({
    data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == sc.char()), code()]
    #names(data) <- str_pad(names(data), 11, "left", pad="0")
    
    df <- data.frame(names(data), 
                     data, 
                     stringsAsFactors = FALSE)
    
    rownames(df) <- 1:nrow(df)
    names(df) <- c("GEOID", "var")
    df
  })
  
  merged <- reactive({
    geo_join(tract.shape(), df(), "GEOID", "GEOID")
  })
  
  popup <- reactive({
    m <- merged()
    paste0("GEOID: ", m$GEOID, "<br>", title(), " ", round(m$var,2))
  })
  
  pal <- reactive({
    m <- merged()
    colorNumeric(palette = "Spectral",
                 domain = round(m$var, 2),
                 reverse = T)
  })
  
  observe({
    # sc.char <- input$sc.num
    # code <- as.numeric(input$code)
    # 
    # tract.shape <- readRDS(paste0("tracts/tractSPDF_", sc.char,".rds"))#, cb = TRUE)#tracts(state = as.numeric(sc.char), county = NULL) 
    # 
    # data <- FA_Results$scores[which(substr(rownames(FA_Results$scores), 1,2) == sc.char), code]
    # #names(data) <- str_pad(names(data), 11, "left", pad="0")
    # 
    # df <- data.frame(names(data), 
    #                  data, 
    #                  stringsAsFactors = FALSE)
    # 
    # rownames(df) <- 1:nrow(df)
    # names(df) <- c("GEOID", "var")
    # 
    # merged <- geo_join(tract.shape, df, "GEOID", "GEOID")
    #
    # title <- factorName_n[code]
    
    # popup <- paste0("GEOID: ", merged$GEOID, "<br>", title, " ", round(merged$var,2))
    # #pal <- colorNumeric(palette = "Spectral", domain = c(-4.4,4.4), reverse = T)
    # pal <- colorNumeric(palette = "Spectral",
    #                     domain = round(merged$var, 2),
    #                     reverse = T)
    
    p <- pal()
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = merged(),
                  fillColor = ~p(var),
                  color = "#b2aeae",
                  fillOpacity = input$opac,
                  weight = 1,
                  popup = popup())
    # %>%
    #   clearControls() %>%
    #     addLegend(pal = pal,
    #               values = round(merged$var, 2),
    #               position = "bottomright",
    #               title = title)
  })
  
  observeEvent(input$code,{
    m <- merged()
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(pal = pal(),
                values = round(m$var, 2),
                position = "bottomright",
                title = title())
  })
  
}