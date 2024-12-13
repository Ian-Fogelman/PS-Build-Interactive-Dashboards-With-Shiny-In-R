# Load required libraries
library(shiny)
library(bslib)
library(ggplot2)

# Define UI
ui <- fluidPage(

  theme = bs_theme(version = 5), # Initialize with Bootstrap 5
  titlePanel("Dynamic Theme Selector"),

  sidebarLayout(
    sidebarPanel(
      h4("Theme Selector"),
      selectInput(
        "theme", 
        label = "Select a Theme:", 
        choices = c(
          "Default" = "default",
          "Minty" = "minty",
          "Cyborg" = "cyborg",
          "Flatly" = "flatly",
          "Solar" = "solar",
	  "lumen" = "lumen"
        ), 
        selected = "default"
      )
    ),

    mainPanel(
      h2("Main Content Area"),
      p("Change the theme using the dropdown menu in the sidebar."),
      p("The current theme will be applied dynamically without refreshing the app."),
      
      # Add a plot output for the example graph
      plotOutput("mtcarsPlot")
    )
  )
  
)

# Define Server
server <- function(input, output, session) {
  observe({
    selected_theme <- switch(
      input$theme,
      "default" = bs_theme(version = 5), # Default theme
      "minty" = bs_theme(version = 5, bootswatch = "minty"),
      "cyborg" = bs_theme(version = 5, bootswatch = "cyborg"),
      "flatly" = bs_theme(version = 5, bootswatch = "flatly"),
      "solar" = bs_theme(version = 5, bootswatch = "solar"),
	  "lumen" = bs_theme(version = 5, bootswatch = "lumen")
    )
    session$setCurrentTheme(selected_theme)
  })

  # Render the mtcars plot
  output$mtcarsPlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point(size = 3, color = "blue") +
      labs(
        title = "Miles per Gallon vs. Weight",
        x = "Weight (1000 lbs)",
        y = "Miles per Gallon"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)