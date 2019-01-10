library(leaflet)
library(stringr)

# Choices for drop-downs
stateName <- read.csv("state_code.csv")
stateCodes <- str_pad(as.character(stateName[,2]), 2, "left", pad = "0")

var1 <- as.character(stateCodes)
names(var1) <- stateName[,1]
var2 <- c("African Americans in Tract" = 4, 
          "Seniors in Tract" = 3, 
          "Singletons in Tract" = 2, 
          "Affluence" = 1, 
          "Noncitizens in Tract" = 5)

fluidPage("Factors", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",

                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Factor Explorer"),
                                      
                                      selectInput("sc.num", "State", var1, selected = "40"),
                                      selectInput("code", "Factor", var2, selected = 1),
                                      sliderInput("opac", "Opacity", value = 0.5, min = 0, max = 1)
                        ),
                        
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Dimensions of Neighborhood Tracts and Their Associations with Mental Health'), ' by Katherine Forthman et al. (Laureate Institute for Brain Research).'
                        )
                        
                    )
           ),
           
           
           conditionalPanel("false", icon("crosshair"))
)
