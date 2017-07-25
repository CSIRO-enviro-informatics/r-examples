library(shiny)
library(leaflet)
library(RColorBrewer)
library(httr)
library(knlibr)
library(geojsonio)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                textInput("featSearch", "Search for Geo-feature from KN"),
                selectInput("resultList", "Result", list("No results yet"=0),selectize=FALSE, size=5)
  )
)

server <- function(input, output, session) {
  searchResult <- NULL
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    #  quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })

  observe({
    x <- input$featSearch
    searchResult <<- kn_feat_search(input$featSearch, 0, 5)

    if(!is.null(searchResult)) {
      labels <- searchResult[,2]

      #mylist <- list()
      #for(i in 1:length(labels)){
      #  curr <- labels[[i]]
      #  mylist[[]] <- curr
      #}
      myList <- c(1:length(labels))
      names(myList) <- labels
      #names(labels) <- myList

      # Can also set the label and select items
      updateSelectInput(session, "resultList",
                        choices = myList,
                        selected = tail(x, 1)
      )
    }
    else {
      updateSelectInput(session, "resultList",
                        choices = c("No results"),
                        selected = tail(x, 1)
      )
    }
  })

  observeEvent(input$resultList, {
    selected <- input$resultList
    x <- strtoi(selected)
    print(x)
    #get pid
    pid <- searchResult[x]
    print(pid)

    withProgress(message = 'Fetching geo-feature', value = 0, {
      setProgress(value = 0.3, detail = "Querying KN")

      sp <- kn_get_geojson(pid)

      setProgress(value = 0.5, detail = "Rendering spatial object...")

      if(!is.null(sp)) {
        print("received geojson object and rendering in leaflet")
        proxy <- leafletProxy("map",session) %>%
          clearShapes() %>%
          addPolygons(data=sp, color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = "#444444", highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
        setProgress(value = 1, detail = "Done.")


      }
    })

  })


  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addTiles()    %>%
      fitBounds(112.467, -55.050, 168.000, -9.133)
  })



}

shinyApp(ui, server)
