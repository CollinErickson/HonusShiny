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
catn <- function(...) {cat(..., "\n")}

# initial_load_completed <- F
datepicker_loaded <- 0

all_team_abbrev <- c("ANA", "ARI", "ATL", "BAL", "BOS", "CHA", "CHN", "CIN", "CLE", "COL",
                     "DET", "HOU", "KCA", "LAN", "MIA", "MIL", "MIN", "NYA", "NYN", "OAK",
                     "PHI", "PIT", "SDN", "SEA", "SFN", "SLN", "TBA", "TEX", "TOR", "WAS")
all_team_abbrev <- c("ANA", "ARI", "ATL", "BAL", "BOS", "CHA", "CHN", "CIN", "CLE", "COL",
                     "DET", "HOU", "KCA", "LAN", "MIA", "MIL", "MIN", "NYY", "NYN", "OAK",
                     "PHI", "PIT", "SDN", "SEA", "SFN", "SLN", "TBA", "TEX", "TOR", "WSH")

num_highlights <- NA


# jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # shinyjs::extendShinyjs(text = jsCode),
  # selectInput("col", "Colour:",
  #             c("white", "yellow", "red", "blue", "purple")),
  # shinyjs::extendShinyjs(text='shinyjs.opo = function(xxx) {console.log("new width 200");}'),
  shinyjs::extendShinyjs(text='shinyjs.opo = function(xxx) {console.log("new width 200");document.getElementById("videoplayer").width = xxx;}'),
  shinyjs::extendShinyjs(text='shinyjs.goToDatePicked = function(redirectlink) {
	                       /*var day = $( "#datepicker" ).datepicker( "getDate" ).getDate()  ;
                         day = day.toString();
                         if (day.length == 1) { day = "0" + day ;}
                         var month =   $( "#datepicker" ).datepicker( "getDate" ).getMonth() + 1  ;
                         month = month.toString();
                         if (month.length == 1) { month = "0" + month ;}
                         var year =   $( "#datepicker" ).datepicker( "getDate" ).getFullYear()   ;
                         year = year.toString();*/
                         //console.log("goToDatePicked: ", day,month,year, team);
                         
                         /*selectteam
                         var e = document.getElementById("selectteam");
                         var team = e.options[e.selectedIndex].value;
                         */
                         
                         //window.location.href = "/?date=" + year + month + day + "&team=" + team ;
                         window.location.href = redirectlink[0] ;
//console.log("link to go to is ", "/?date=" + year + month + day + "&team=" + team );
//console.log("link to go to is ", redirectlink);
                         
                         }'),
  # Application title
  titlePanel("Honus"),
  
  # top row
  fluidRow(
    column(1, h1("Honus")),
    column(2,
           # HTML("Team:"),
           # selectInput("selectteam",
           #             label="Team",
           #             choices=all_team_abbrev,
           #             selected=selected_code)
           uiOutput("select_team_input")
    ),
    column(1,
           dateInput("datepicker",
                     label="Date")),
    column(1,
           actionButton("back1day", label="<")),
    column(1,
           actionButton("gototoday", label="|")),
    column(1,
           actionButton("forward1day", label=">")),
    column(3,
           # HTML(
           #   paste0(
           #     "<div>", 
           #     selected_game_xml$.attrs["away_team_name"], "(",
           #     selected_game_xml$.attrs["away_win"], "-",
           #     selected_game_xml$.attrs["away_loss"], ")", " vs ",
           #     selected_game_xml$.attrs["home_team_name"], "(",
           #     selected_game_xml$.attrs["home_win"], "-",
           #     selected_game_xml$.attrs["home_loss"], ")",
           #     "</div>"))
           uiOutput("header_team_info")
    ),
    column(2,
           sliderInput("video_size_slider", "Video size", 100, 2000, value=1000, step=10)#,
           # tableOutput("values")
    )
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # HTML(
      #   get_all_game_boxes_for_sidebar(msl)
      # )
      uiOutput("sidebar_boxes")
      ,
      width=2
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("distPlot")
      # HTML(
      #   get_highlights(year, month, day, away_code, home_code, game_nbr, game_pk)
      # )
      fluidRow(
        # column(6,
        #        uiOutput("highlights_html")
        # ),
        # column(6,
        #        uiOutput("highlight_embedded")
        #        #           HTML('<video id="videoplayer" controls  onclick="this.paused ? this.play() : this.pause();">
        #        # 					<source src="https://cuts.diamond.mlb.com/FORGE/2019/2019-04/03/17fd926c-c985a511-52b8d70e-csvm-diamondx64-asset_1280x720_59_4000K.mp4" type="video/mp4">
        #        #                Your browser does not support the video tag.
        #        #                </video>')
        #        # tags$video(id="video2", type = "video/mp4",src = "https://cuts.diamond.mlb.com/FORGE/2019/2019-04/03/17fd926c-c985a511-52b8d70e-csvm-diamondx64-asset_1280x720_59_4000K.mp4",
        #        #            controls = "controls")
        # )
        tags$table(#style="table-layout:fixed;",
          tags$tr(
            tags$td(uiOutput("highlights_html")) , #style="width:900px;table-layout:fixed;overflow:hidden;word-wrap:break-word;min-width=900px;"),
            tags$td(uiOutput("highlight_embedded"))
          )
        )
      )
    )
  ),
  HTML('<div align="right">Suggestions, comments, questions? Email <a href="mailto:Honus1064@gmail.com" style="color:inherit;">Honus1064@gmail.com</a></div>')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  selected_code <- NULL
  # Get inputs for selected_team, date, etc from URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['team']])) {
      cat("input from url is\n", query[['team']], '\n\n')
      shinyjs::logjs(paste("input from url is\n", query[['team']], '\n\n'))
      # updateTextInput(session, "team", value = query[['team']])
      selected_code <<- toupper(query[['team']])
    } else {
      shinyjs::logjs(paste0("NO input from url is\n", names(query)))
      selected_code <<- "NYY"
    }
    if (!is.null(query[['date']]) && nchar(query[['date']])==8) {
      date <- query[['date']]
      year  <- substr(date, 1, 4)
      month <- substr(date, 5, 6)
      day   <- substr(date, 7, 8)
    } else {
      year  <- format(Sys.time(), "%Y")
      month <- format(Sys.time(), "%m")
      day   <- format(Sys.time(), "%d")
      day <- "02"
    }
    
    catn(year, month, day)
    updateDateInput(session=session, inputId="datepicker", value=paste0(year, "-", month, "-", day))
    
    cat("selected code is", selected_code, "\n")
    
    # Now get everything set up
    master_scoreboard_url <- sprintf("http://gd2.mlb.com/components/game/mlb/year_%s/month_%s/day_%s/master_scoreboard.xml",
                                     year, month, day)
    # require(XML)
    ms <- XML::xmlParse(master_scoreboard_url)
    msl <- XML::xmlToList(ms)
    num_games <- length(msl) - 1
    home_abbrevs <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["home_name_abbrev"]}))
    away_abbrevs <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["away_name_abbrev"]}))
    game_nbrs    <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["game_nbr"]}))
    game_pks    <- unname(sapply(1:(length(msl)-1), function(i) {msl[[i]]$.attrs["game_pk"]}))
    cat("game_pks are", game_pks, "\n")
    cat(sprintf("selected_code is %s\n", selected_code))
    selected_game <- which(home_abbrevs == selected_code | away_abbrevs == selected_code)
    if (length(selected_game) == 0) {
      # stop("selected_game is none")
      selected_game <- 1
    }
    if (length(selected_game) > 1) {
      logjs("more than one game possible")
      selected_game <- selected_game[1]
    }
    away_code <- away_abbrevs[selected_game]
    home_code <- home_abbrevs[selected_game]
    game_nbr  <- game_nbrs[selected_game]
    game_pk   <- game_pks[selected_game]
    selected_game_xml <- msl[[selected_game]]
    
    
    # Now render stuff using inputs
    # Render selected team input
    output$select_team_input <- renderUI( {
      selectInput("selectteam",
                  label="Team",
                  choices=all_team_abbrev,
                  selected=selected_code)
    })
    # Render team names in header
    output$header_team_info <- renderUI({
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
    })
    # Render sidebar_boxes
    output$sidebar_boxes <- renderUI({
      HTML(
        get_all_game_boxes_for_sidebar(msl)
      )
    })
    # Render highlights
    output$highlights_html <- renderUI({
      get_highlights_out <- 
        get_highlights(year, month, day, away_code, home_code, game_nbr, game_pk, return_number = T)
      num_highlights <<- get_highlights_out$number
      cat("num highlights is !!!", get_highlights_out$number, "\n")
      
      #     # Set up so click on highlight title will load video
      #     for (i_outer in 1:num_highlights) {
      #       local({
      #         i <- i_outer # Need this and local or else it always thinks you clicked on last box, see https://github.com/daattali/shinyjs/issues/167
      #         # cat("setting onclick for highlight", i, "\n")
      #         shinyjs::onclick(id=paste0("headlinetr", i),
      #                          # expr=set_highlights_for_game_number(i)
      #                          expr={cat("clicked on game", i, "\n");output$highlight_embedded <- renderUI({
      # #                            HTML('<video id="videoplayer" controls  onclick="this.paused ? this.play() : this.pause();">
      # # 					<source src="https://cuts.diamond.mlb.com/FORGE/2019/2019-04/03/17fd926c-c985a511-52b8d70e-csvm-diamondx64-asset_1280x720_59_4000K.mp4" type="video/mp4">
      # #                Your browser does not support the video tag.
      # #                </video>')
      #                            HTML("I")
      #                          })}
      #         )
      #       })
      #     }
      
      
      HTML(
        get_highlights_out$outstring
      )
    })
    cat("num highlights is $$$$", num_highlights, "\n")
    
    # Set up so click on game from left side will reload highlights
    for (i_outer in 1:num_games) {
      local({
        i <- i_outer # Need this and local or else it always thinks you clicked on last box, see https://github.com/daattali/shinyjs/issues/167
        # cat("setting onclick for game", i, "\n")
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
    
    initial_load_completed <<- T
  })
  
  # shinyjs::extendShinyjs(text='shinyjs.opo = function(xxx) {console.log("new width 200");document.getElementById("videoplayer").width = 250;}')
  # shinyjs::extendShinyjs(text='shinyjs.opo = function(xxx) {console.log("new width 200");}')
  # shinyjs::js$opo(1);
  # Reactive slider
  observeEvent(input$video_size_slider, {
    cat("runs when init slider!!!")
    shinyjs::js$opo(input$video_size_slider)
  })
  observeEvent(input$datepicker, {
    cat("Updated datepicker!!!\n")
    if (datepicker_loaded >= 1) {
      # cat("initial load completed!!!\n")
      catn(selected_code)
      catn("dateInput is ", as.character(input$datepicker))
      # catn("day is ", class(day), "month is ", month, "year is ", year, "selected_code is ", selected_code)
      shinyjs::js$goToDatePicked(paste0("/?date=", gsub("-", "", as.character(input$datepicker)), "&team=", selected_code)) # "/?date=" + year + month + day + "&team=" + team
      # shinyjs::js$goToDatePicked(paste0("/?date=", as.character(input$datepicker), "&team=", selected_code)) # "/?date=" + year + month + day + "&team=" + team 
      datepicker_loaded <<- 0
    } else {
      datepicker_loaded <<- datepicker_loaded + 1
    }
  }, ignoreInit = T)
  # output$values <- renderTable({slider_size_reactive()})
  # observeEvent(input$col, {
  #   shinyjs::js$pageCol(input$col)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

