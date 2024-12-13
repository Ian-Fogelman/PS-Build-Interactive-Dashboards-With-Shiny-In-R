# Load libraries
library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)

# Define UI
ui <- fluidPage(
    titlePanel("Multi-tab Dashboard with mtcars"),
    sidebarLayout(
        sidebarPanel(
            h3("Car Dataset Visualizations"),
            selectInput("xvar", "Choose X Variable", choices = names(mtcars)),
            selectInput("yvar", "Choose Y Variable", choices = names(mtcars)),
            sliderInput("cylFilter", "Filter by Cylinders", 
                        min = min(mtcars$cyl), max = max(mtcars$cyl), 
                        value = range(mtcars$cyl), step = 1)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("GGPlot", plotOutput("ggplotTab")),
                tabPanel("Plotly", plotlyOutput("plotlyTab")),
                tabPanel("Leaflet Map", leafletOutput("leafletTab"))
            )
        )
    )
)

# Define Server
server <- function(input, output, session) {
    
    # Reactive filtered data
    filteredData <- reactive({
        mtcars[mtcars$cyl >= input$cylFilter[1] & mtcars$cyl <= input$cylFilter[2], ]
    })
    
    # GGPlot Tab
    output$ggplotTab <- renderPlot({
        ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar)) +
            geom_point(color = "blue", size = 3) +
            theme_minimal() +
            labs(title = "Scatterplot with GGPlot2",
                 x = input$xvar, y = input$yvar)
    })
    
    # Plotly Tab
    output$plotlyTab <- renderPlotly({
        p <- ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar)) +
            geom_point(color = "red", size = 3) +
            theme_light() +
            labs(title = "Interactive Scatterplot with Plotly",
                 x = input$xvar, y = input$yvar)
        ggplotly(p)
    })
    
    # Leaflet Map Tab
    output$leafletTab <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addMarkers(lng = -74.006, lat = 40.7128, 
                       popup = "Placeholder for mtcars (New York)") %>%
            setView(lng = -74.006, lat = 40.7128, zoom = 4)
    })
}

# Run App
shinyApp(ui, server)