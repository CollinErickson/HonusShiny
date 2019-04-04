get_highlights <- function(year, month, day, away_code, home_code, dbh, game_pk, return_number=FALSE) {
  # bburl <- "https://statsapi.mlb.com/api/v1/game/565898/content?language=en"
  bburl <- paste0("https://statsapi.mlb.com/api/v1/game/", game_pk,"/content?language=en")
  cat("going to pull from", bburl, "\n")
  require(jsonlite)
  p1 <- fromJSON(bburl)
  headlines <- p1$highlights$highlights$items$headline
  
  outstring <- ""
  echo <- function(...) {
    for (i in list(...)) {
      # cat(i)
      outstring <<- paste0(outstring, i)
    }
  }
  
  echo('<table><tr><td style="vertical-align:top;"><table style="min-width:250px;" id="headlinestable">')
  # This section prints the headlines for the highlights
  if (length(headlines)>0) { # if there is at least one, loop over them
    urls <- sapply(p1$highlights$highlights$items$playbacks, function(x) x$url[1])
    for (iii in 1:length(headlines)) {
      headline = headlines[iii];
      #$url = $urls[iii]; now using different qualities/sizes
      # Get the urls for various video qualities
      url4000K = urls[iii];
      url2500K = gsub('4000K','1200K',url4000K);
      url2500K = gsub('4000K','2500K',url4000K);
      url1800K = gsub('4000K','1800K',url4000K);
      url = url4000K; # This sets the quality
      # echo(the headline
      # cat("Inintial video url is", url, "\n")
      echo("<tr style='border: 1px solid red' class='headlinestabletr' id='headlinetr",iii,"' onclick='document.getElementById(\"headlinetr",iii,"\").style.background = \"#e0ccff\";'>
        <td id='headline",iii,"' class='headlinestabletd' 
        onclick='document.getElementById(\"videoplayer\").setAttribute(\"src\", \"",url,"\");
        document.getElementById(\"videoplayer\").autoplay=true;'>",headline,"</td>
        <td><a href='" , url , "'  target='_blank'  style='text-decoration: none'>&#8599;</a></td>")
      # echo(the higher and lower quality videos, printed as arrows
      echo("	<td><table style='font-size:.47em'><tr><td onclick='document.getElementById(\"videoplayer\").setAttribute(\"src\", \"",
           url,"\");
        document.getElementById(\"videoplayer\").autoplay=true;'>
        &#x25B2;
        </td></tr><tr>
        <td onclick='document.getElementById(\"videoplayer\").setAttribute(\"src\", \"",url,"\");
        document.getElementById(\"videoplayer\").autoplay=true;'>
        &#x25BC;
        </td></tr></table></td>")
      echo("	</tr>")
    }
  } else {
    #echo('no game yet';
  }
  echo('</table></td><td style="vertical-align:top;">')
  
  # This section makes the video player
  # make video player if any headlines
  if (length(headlines)>0) {
    echo('<video width="1000px" id="videoplayer" controls>
					<source src="<?php echo($urls[0];?>" type="video/mp4">
					Your browser does not support the video tag.
					</video>');
    # No longer need onclick to play/pause since it does it automatically
#     echo('<video width="600px" id="videoplayer" controls  onclick="this.paused ? this.play() : this.pause();">
# 					<source src="<?php echo($urls[0];?>" type="video/mp4">
# 					Your browser does not support the video tag.
# 					</video>');
  } else { # else do something else
    #echo('<img src="http:#mlb.mlb.com/mlb/images/devices/teamBackdrop/teamBackdrop.jpg" />';
    # NO LONGER HAVE IMAGES
    # # This takes images from the following page. Usually it is the probable pitchers before the game starts, or current pitcher and batter if already started
    # # When it is the free game of the day, one of the images just says something about being free game, should try to change that
    # # novideofilename = "http:#gd2.mlb.com/components/game/mlb/year_{$year}/month_{$month}/day_{$day}/gid_{$year}_{$month}_{$day}_{$away_code}mlb_{$home_code}mlb_{$dbh}/atv_preview_noscores.xml";
    # novideofilename = sprintf("http:gd2.mlb.com/components/game/mlb/year_%s/month_%s/day_%s/gid_%s_%s_%s_%smlb_%smlb_%s/atv_preview_noscores.xml",
    #                           year, month, day, year, month, day, away_code, home_code, dbh)
    # # novideohomepage = file_get_contents(novideofilename);
    # # novideoxml = simplexml_load_string(novideohomepage);
    # novideoxml <- XML::xmlToList(XML::xmlParse(novideofilename))
    # echo('<img src="' , novideoxml -> body -> preview -> baseballLineScorePreview -> banners -> imageWithLabels -> image . '" />')
    # echo('<img src="' , novideoxml -> body -> preview -> baseballLineScorePreview -> banners -> imageWithLabels[1] -> image , '" />')
  }
  
  echo('</td></tr></table>')
  if (return_number) {
    list(outstring=outstring,
         number=length(headlines))
  } else {
    outstring
  }
}
# get_highlights("2019", "04", "02", "det", "nya", "1", "567460")
