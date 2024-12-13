# Load libraries
library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Multi-tab Dashboard with mtcars"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Car Dataset Visualizations", tabName = "visualizations", icon = icon("car")),
	    menuItem("Example Additional Sidebar Item", tabName = "AdditionalText", icon = icon("calendar")),
      selectInput("xvar", "Choose X Variable", choices = names(mtcars)),
      selectInput("yvar", "Choose Y Variable", choices = names(mtcars)),
      sliderInput("cylFilter", "Filter by Cylinders", 
                  min = min(mtcars$cyl), max = max(mtcars$cyl), 
                  value = range(mtcars$cyl), step = 1)
    )
	
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "visualizations",
              tabsetPanel(
                tabPanel("GGPlot", plotOutput("ggplotTab")),
                tabPanel("Plotly", plotlyOutput("plotlyTab")),
                tabPanel("Leaflet Map", leafletOutput("leafletTab"))
              )
      )
	  ,tabItem(tabName ="AdditionalText", 
	  		tabsetPanel(
	  			tabPanel("Text Only",
                         # Adding text content instead of plot
                         tags$h1("Welcome to the Text Only Tab"),
                         tags$p("This tab contains only text and no visualizations. You can use it for instructions, information, or other content.")
                )))
	  
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
