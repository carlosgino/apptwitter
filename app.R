library(leaflet)
library(twitteR)

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(4, textInput("searchkw", label = "search:", value = "#movie")),
      column(4, textInput("lat", label = "lat:", value = 40.75)),
      column(4, textInput("long", label = "long:", value = -74)),
      column(8, leafletOutput("myMap")),
      column(12, tableOutput('table'))
    )
  ),
 
  server = function(input, output) {
    
    # Autenticación OAuth
    consumer_key <- readLines("tokens.txt")[1]
    consumer_secret <- readLines("tokens.txt")[2]
    access_token <- readLines("tokens.txt")[3]
    access_secret <- readLines("tokens.txt")[4]
    options(httr_oauth_cache = TRUE) # enable using a local file to cache OAuth access credentials between R sessions
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
    # Enviar consulta a Twitter
    dataInput <- reactive({  
      tweets <- twListToDF(searchTwitter(input$searchkw, n = 100, 
                                         geocode = paste0(input$lat, ",", input$long, ",1000km"))) 
      tweets$created <- as.character(tweets$created)
      tweets <- tweets[!is.na(tweets[, "longitude"]), ]
    })
    
    # Crear un mapa leaflet reactivo
    mapTweets <- reactive({
      map = leaflet() %>% addTiles() %>%
        addMarkers(dataInput()$longitude, dataInput()$latitude, popup = dataInput()$screenName) %>%
        setView(input$long, input$lat, zoom = 11)
    })
    output$myMap = renderLeaflet(mapTweets())
    
    # Crear una tabla reactiva 
    output$table <- renderTable(
      dataInput()[, c("text", "screenName", "longitude", "latitude", "created")]
    )
  }
) 
  
  
  
  
  