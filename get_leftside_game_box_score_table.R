year <- "2019"
month <- "04"
day <- "02"
get_all_game_boxes_for_sidebar <- function(msl) {
  outstring <- ""
  echo <- function(...) {
    for (i in list(...)) {
      # cat(i)
      outstring <<- paste0(outstring, i)
    }
  }
  
  
  #  create table of scores, each row is a game
  echo("<table id='scorestable'>")
  # $iii = 0; #  keep track of which game each is
  #  loop over each game, last one is .attrs so don't count that
  for (iii in 1:(length(msl) - 1)) {
    a <- msl[[iii]]
    #  add new row for each game
    echo('<tr class="scorestablegame" id="' , "datescoreboardgamenumber" , iii , 
         '" onclick="' , createGameOnclickURLForJS(substr(a$.attrs["away_code"], 0, 3),
                                                   year, month, day, as.integer(a$.attrs["game_nbr"])) ,
         '" style="outline: thin solid"><td class="scorestablegametd">')
    echo("<table><tr><td>",a$.attrs["away_team_name"],
         "</td></tr><tr><td>",a$.attrs["home_team_name"],
         "</td></tr></table>\n")
    #  go through each possibility for game status, should have two tds to fill
    if ((a$status["status"]=="Final") || (a$status["status"] == "Game Over") ||
                                         (a$status["status"] == "Completed Early")) {
      echo("</td><td>")
      winning_pitcher_line='-';losing_pitcher_line='-';
      winning_pitcher_line = paste0("W:" , a$winning_pitcher["last"] , "(" , a$winning_pitcher["wins"] , "-" , a$winning_pitcher["losses"] , ")")
      # echo $a -> winning_pitcher ->attributes() -> last . "(" . $a -> winning_pitcher ->attributes() -> wins . "-" , $a -> winning_pitcher ->attributes() -> losses , ")";;
      losing_pitcher_line  = paste0("L:" , a$losing_pitcher["last"] , "(" , a$losing_pitcher["wins"] , "-" , a$losing_pitcher["losses"] , ")")
      away_gray = FALSE; home_gray = FALSE;
      if( as.integer(a$linescore$r["away"]) >  as.integer(a$linescore$r["home"])) {
        away_pitcher_line = winning_pitcher_line
        home_pitcher_line = losing_pitcher_line;
        away_gray = a$winning_pitcher["last"]=="Gray"
        home_gray = a$losing_pitcher["last"]=="Gray"
      } else {
        home_pitcher_line = winning_pitcher_line
        away_pitcher_line = losing_pitcher_line;
        home_gray = a$winning_pitcher["last"]=="Gray"
        away_gray = a$losing_pitcher["last"]=="Gray"
      }
      # /*echo "<table><tr><td>", a$linescore$r["away"], " ", $away_pitcher_line,"</td></tr>";
      # echo "<tr><td>",a$linescore$r["home"], " ", $home_pitcher_line, "</td></tr></table>\n";
      # echo "</td><td>";
      # echo "F";*/
      echo( "<table><tr><td>", a$linescore$r["away"],"</td>")
      if (a$status["inning"] != "9") {
        echo("<td rowspan='2'>" , #/*$a -> status -> attributes() -> ind . */ 
               "/" , a$status["inning"], "</td>")
      } else {
        echo("<td rowspan='2' style='text-align:center;'>" , #/*$a -> status -> attributes() -> ind .*/
               "</td>")
      }
      echo( "</tr>")
      echo( "<tr><td>",a$linescore$r["home"], "</td>")
      echo( "</tr></table>\n")
      echo( "</td><td>")
      echo( "<table><tr><td")
      if (away_gray) {echo(" style='color:gray;'")}
      echo(">",  away_pitcher_line,"</td></tr>")
      echo( "<tr><td")
      if (home_gray) {echo (" style='color:gray;'")}
      echo(">", home_pitcher_line, "</td></tr></table>\n")
      
    } else if ((a$status["status"]=="In Progress") || (a$status["status"]=="Review")
               || (a$status["status"]=="Manager Challenge") || (a$status["status"]=="Delayed")) {
      echo ("</td><td>")
      echo ("<table><tr><td>")
      echo ("<table><tr><td>",a$linescore$r["away"],
            "</td></tr><tr><td>",a$linescore$r["home"],
            "</td></tr></table>\n")
      echo ("</td><td>")
      echo ("</td><td>")
      # if ($a -> attributes() -> is_no_hiter){echo 'NH';}
      if (a$status["top_inning"]=="Y"){echo ("&#x25B2;")} else {echo ("&#x25BC;")}
      echo (a$status["inning"]) 
      outs = a$status["outs"] 
      if(outs == '0') { #  display number of outs with dots or pipe
        #  do nothing
      } else if (outs == '1'){
        echo ('<b>&#0149;</b>')
      } else if (outs == '2'){
        echo ('<b>:</b>')
      } else if (outs=='3'){
        echo ('&#10073;')
      } else {
        echo (outs)
      };
      # baseRunnerStatus = $a -> runners_on_base -> attributes() -> status;
      logjs("Fix baseRunnerStatus here !!!")
      # echo "<div style='padding-left:0px'>";
      # echo "<img src='/transparent.png' alt='Baserunners' height='12' width='2' />";
      # echo "<img src='/Baserunners" . $baseRunnerStatus . ".png' alt='Baserunners' height='12' width='12' />";
      # echo "</div>";
      if(a$status["status"]=="Delayed"){
        echo(' Delayed<br />',a$status["reason"])
      }
      #  next four lines get link to MLB.tv, FGOD no longer points to media center
      mlbtvlink = a$links["mlbtv"]
      # mlbtvlink_number = explode("'", (string)$mlbtvlink)[1];
      mlbtvlink_number <- strsplit(a$links["mlbtv"],"-")$mlbtv[2]
      mlbtvlink_url = paste0("http:# m.mlb.com/tv/e" , mlbtvlink_number)
      if (a$game_media$media["free"] == "YES") {# == "ALL") { #  link to MLB.tv, says FGOD or is TV symbol
        # echo "<br><a href='http:# mlb.mlb.com/mediacenter/' target='_blank' style='text-decoration: none'>FGOD</a>";
        echo ("<br><a href='" , mlbtvlink_url , "' target='_blank'  style='text-decoration: none;color:inherit'>FGOD</a>")
      } else {
        echo ("<br><a href='" , mlbtvlink_url , "' target='_blank'  style='text-decoration: none;color:inherit'>&#x1F4FA;</a>")
      }
      echo (" <img src='/Baserunners" , baseRunnerStatus ,
            ".png' alt='Baserunners' height='12' width='12' />")
      echo ("</td></tr></table><td>")
      echo ("<table><tr><td>P:", a$pitcher["last"] , "</td></tr>") # $a -> pitcher -> attributes() -> last
      echo ("<tr><td>B:" , a$batter["last"]  , "</td>") # $a -> batter -> attributes() -> last
      echo ("</tr></table>")
      echo ("</td>")
      
    } else if ((a$status["status"]=="Preview") || (a$status["status"]=="Pre-Game")
               || (a$status["status"]=="Warmup")) {
      # echo "</td><td>";
      # echo "<table><tr><td>",a$away_probable_pitcher["last_name"],"</td></tr><tr><td>",a$home_probable_pitcher["last_name"],"</td></tr></table>\n";
      # echo "</td><td>";
      # echo $a->attributes()->time," ET";
      echo("</td><td>")
      echo (a$.attrs["time"]," ET") # $a->attributes()->time
      if(a$game_media$media["free"] == "YES") {
        echo("<br><a href='http:# mlb.mlb.com/mediacenter/' target='_blank' style='color:inherit;text-decoration:none'>FGOD</a>")
      }
      echo ("</td><td>")
      echo ("<table><tr><td")
      if (a$away_probable_pitcher["last_name"]=="Gray") {
        echo (" style='color:gray;'")
      }  #  Gray easter egg, it's Gray Day!
      echo (">",a$away_probable_pitcher["last_name"],"(",a$away_probable_pitcher["wins"],"-",a$away_probable_pitcher["losses"],")</td></tr>")
      echo ("<tr><td"); if (a$away_probable_pitcher["last_name"]=="Gray") {echo (" style='color:gray;'");}
      echo (">",a$home_probable_pitcher["last_name"],"(",a$home_probable_pitcher["wins"],"-",a$home_probable_pitcher["losses"],")</td></tr></table>\n")
    } else if (a$status["status"]=="Delayed Start") {
      echo ("</td><td>")
      echo ("Delayed<br>Start")
      if(a$game_media$media["free"] == "YES") {
        echo ("<br><a href='http:# mlb.mlb.com/mediacenter/' target='_blank' style='color:inherit;text-decoration:none'>FGOD</a>");
      }
      echo ("</td><td>")
      echo ("<table><tr><td");  
      if (a$away_probable_pitcher["last_name"]=="Gray") {echo (" style='color:gray;'");};  #  Gray easter egg, it's Gray Day!
      echo(">",a$away_probable_pitcher["last_name"],"(",
           a$away_probable_pitcher["wins"],"-",a$away_probable_pitcher["losses"],")</td></tr>")
      echo ("<tr><td");
      if (a$home_probable_pitcher["last_name"]=="Gray") {echo (" style='color:gray;'");};
      echo (">",a$home_probable_pitcher["last_name"],"(",a$away_probable_pitcher["wins"],
            "-",a$away_probable_pitcher["losses"],")</td></tr></table>\n");
    } else if (a$status["status"]=="Postponed") {
      echo ("</td><td>");
      # echo "<table><tr><td>",a$linescore$r["away"],"</td></tr><tr><td>",a$linescore$r["home"],"</td></tr></table>";
      echo ("PP");
      echo ("</td><td>");
      # echo "PP";
      echo (a$status["reason"]);
    } else if (a$status["status"]=="Cancelled") { #  e.g. 9/25/16 RIP Jose Fernandez
      echo ("</td><td>")
      echo ("Cancelled")
      echo ("</td><td>")
      echo (a$status["reason"])
    } else if (a$status["status"]=="notDelayed") { #  MOVED THIS INTO In-Progress SINCE I WANT SCORE STILL AND INNING
      echo ("</td><td>");
      # echo "<table><tr><td>",a$linescore$r["away"],"</td></tr><tr><td>",a$linescore$r["home"],"</td></tr></table>";
      echo ("Delayed");
      echo ("</td><td>");
      # echo "PP";
      echo (a$status["reason"]);
    } else {
      echo (a$status["status"]) ;
    }
    echo ("</td></tr>\n")
  }
  echo ("</table>");
  outstring
}
createGameOnclickURLForJS <- function(away, year, month, day, gamenbr){("NEEDTOIMPLEMENTTHIS")}
get_all_game_boxes_for_sidebar(msl)
