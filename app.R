#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(shinyjs)
shinyjs::useShinyjs()
logjs <- cat

year <- "2019"
month <- "04"
day <- "03"
selected_code <- "WSH"
# away_code <- "det"
# home_code <- "nya"
# dbh <- "1"

master_scoreboard_url <- sprintf("http://gd2.mlb.com/components/game/mlb/year_%s/month_%s/day_%s/master_scoreboard.xml",
                                 year, month, day)
require(XML)
ms <- XML::xmlParse(master_scoreboard_url)
msl <- XML::xmlToList(ms)
num_games <- length(msl) - 1
home_abbrevs <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["home_name_abbrev"]}))
away_abbrevs <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["away_name_abbrev"]}))
game_nbrs    <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["game_nbr"]}))
game_pks    <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["game_pk"]}))
cat("game_pks are", game_pks, "\n")
selected_game <- which(home_abbrevs == selected_code | away_abbrevs == selected_code)
if (length(selected_game) == 0) {stop("selected_game is none")}
if (length(selected_game) > 1) {
  logjs("more than one game possible")
  selected_game <- selected_game[1]
}
away_code <- away_abbrevs[selected_game]
home_code <- home_abbrevs[selected_game]
game_nbr  <- game_nbrs[selected_game]
game_pk   <- game_pks[selected_game]
selected_game_xml <- msl[[selected_game]]

all_team_abbrev <- c("ANA", "ARI", "ATL", "BAL", "BOS", "CHA", "CHN", "CIN", "CLE", "COL",
                     "DET", "HOU", "KCA", "LAN", "MIA", "MIL", "MIN", "NYA", "NYN", "OAK",
                     "PHI", "PIT", "SDN", "SEA", "SFN", "SLN", "TBA", "TEX", "TOR", "WAS")
all_team_abbrev <- c("ANA", "ARI", "ATL", "BAL", "BOS", "CHA", "CHN", "CIN", "CLE", "COL",
                     "DET", "HOU", "KCA", "LAN", "MIA", "MIL", "MIN", "NYY", "NYN", "OAK",
                     "PHI", "PIT", "SDN", "SEA", "SFN", "SLN", "TBA", "TEX", "TOR", "WSH")

num_highlights <- NA


# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Honus"),
  
  # top row
  fluidRow(
    column(1, h1("Honus")),
    column(2,
           # HTML("Team:"),
           selectInput("selectedteam",
                       label="Team",
                       choices=all_team_abbrev,
                       selected=selected_code)),
    column(1,
           dateInput("selecteddate",
                     label="Date")),
    column(1,
           actionButton("back1day", label="<")),
    column(1,
           actionButton("gototoday", label="|")),
    column(1,
           actionButton("forward1day", label=">")),
    column(5,
           HTML(
             paste0(
               "<div>", 
               selected_game_xml$.attrs["away_team_name"], "(",
               selected_game_xml$.attrs["away_win"], "-",
               selected_game_xml$.attrs["away_loss"], ")", " vs ",
               selected_game_xml$.attrs["home_team_name"], "(",
               selected_game_xml$.attrs["home_win"], "-",
               selected_game_xml$.attrs["home_loss"], ")",
               "</div>"))
    )
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      HTML(
        get_all_game_boxes_for_sidebar(msl)
      ),
      width=2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("distPlot")
      # HTML(
      #   get_highlights(year, month, day, away_code, home_code, game_nbr, game_pk)
      # )
      fluidRow(
        column(6,
          uiOutput("highlights_html")
        ),
        column(6,
          uiOutput("highlight_embedded")
#           HTML('<video id="videoplayer" controls  onclick="this.paused ? this.play() : this.pause();">
# 					<source src="https://cuts.diamond.mlb.com/FORGE/2019/2019-04/03/17fd926c-c985a511-52b8d70e-csvm-diamondx64-asset_1280x720_59_4000K.mp4" type="video/mp4">
#                Your browser does not support the video tag.
#                </video>')
          # tags$video(id="video2", type = "video/mp4",src = "https://cuts.diamond.mlb.com/FORGE/2019/2019-04/03/17fd926c-c985a511-52b8d70e-csvm-diamondx64-asset_1280x720_59_4000K.mp4",
          #            controls = "controls")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Try to use inputs
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['text']])) {
      cat("input from url is\n", query[['text']], '\n\n')
      updateTextInput(session, "text", value = query[['text']])
    }
  })
  
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   # bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   bins <- seq(min(x), max(x), length.out = 30 + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
  
  output$highlights_html <- renderUI({
    get_highlights_out <- 
      get_highlights(year, month, day, away_code, home_code, game_nbr, game_pk, return_number = T)
    num_highlights <<- get_highlights_out$number
    cat("num highlights is !!!", get_highlights_out$number, "\n")
    
    # Set up so click on highlight title will load video
    for (i_outer in 1:num_highlights) {
      local({
        i <- i_outer # Need this and local or else it always thinks you clicked on last box, see https://github.com/daattali/shinyjs/issues/167
        cat("setting onclick for highlight", i, "\n")
        shinyjs::onclick(id=paste0("headlinetr", i),
                         # expr=set_highlights_for_game_number(i)
                         expr={cat("clicked on game", i, "\n");output$highlight_embedded <- renderUI({
#                            HTML('<video id="videoplayer" controls  onclick="this.paused ? this.play() : this.pause();">
# 					<source src="https://cuts.diamond.mlb.com/FORGE/2019/2019-04/03/17fd926c-c985a511-52b8d70e-csvm-diamondx64-asset_1280x720_59_4000K.mp4" type="video/mp4">
#                Your browser does not support the video tag.
#                </video>')
                           HTML("I")
                         })}
        )
      })
    }
    
    
    HTML(
      get_highlights_out$outstring
    )
  })
  cat("num highlights is $$$$", num_highlights, "\n")
  
  # Set up so click on game from left side will reload highlights
  for (i_outer in 1:num_games) {
    local({
      i <- i_outer # Need this and local or else it always thinks you clicked on last box, see https://github.com/daattali/shinyjs/issues/167
      cat("setting onclick for game", i, "\n")
      shinyjs::onclick(id=paste0("datescoreboardgamenumber", i),
                       # expr=set_highlights_for_game_number(i)
                       expr={cat("clicked on game", i, "\n");output$highlights_html <- renderUI({
                         HTML(
                           get_highlights(year, month, day, away_abbrevs[i], home_abbrevs[i], game_nbrs[i], game_pks[i])
                         )
                       })}
      )
    })
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

