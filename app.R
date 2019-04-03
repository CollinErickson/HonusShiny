#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

year <- "2019"
month <- "04"
day <- "03"
away_code <- "det"
home_code <- "nya"
dbh <- "1"


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Honus"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        HTML(
          get_all_game_boxes_for_sidebar(msl)
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         # plotOutput("distPlot")
        HTML(
          get_highlights(year, month, day, away_code, home_code, dbh, NA)
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      bins <- seq(min(x), max(x), length.out = 30 + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

