get_linescore <- function(gin) {
  outstring <- ""
  echo <- function(...) {
    for (i in list(...)) {
      # cat(i)
      outstring <<- paste0(outstring, i)
    }
  }
  
  
  gl <- gin$linescore
  
  
  echo('<table id="toplinescore"><tr>');
  echo('<td><table><tr><td>&nbsp;</td></tr><tr><td>' , toupper(gin$.attrs["away_code"]) ,
       '</td></tr><tr><td>' , toupper(gin$.attrs["home_code"]) , '</td></tr></table></td>');
  # loop through each inning and print the scores
  for(inning_num in which(names(gl) == "inning")) { #$rawboxscore -> linescore -> inning_line_score as $abc) {
    abc <- gl[[inning_num]]
    echo('<td><table>');
    echo('<tr><td style="border-bottom:solid #80ffff">' , inning_num , '</td></tr>');
    echo('<tr><td>');
    if (nchar(abc["away"]) > 0) {
      echo(abc["away"]);
    } else {
      echo('-');
    }
    echo('</td></tr>');
    echo('<tr><td>');
    if (nchar(abc["away"]) > 0) {
      echo(abc["away"]);
    } else {
      echo('-');
    }
    echo('</td></tr>');
    echo('</table></td>');
  }
  # and summary stats
  echo('<td><table style="border-left:thick double #80ffff">');
  echo('<tr><td style="border-bottom:solid #80ffff">R</td></tr>');
  echo('<tr><td>' , gl$r["away"] , '</td></tr>');
  echo('<tr><td>' , gl$r["home"] , '</td></tr>');
  echo('</table></td>');
  echo('<td><table>');
  echo('<tr><td style="border-bottom:solid #80ffff">H</td></tr>');
  echo('<tr><td>' , gl$h["away"] , '</td></tr>');
  echo('<tr><td>' , gl$h["home"] , '</td></tr>');
  echo('</table></td>');
  echo('<td><table>');
  echo('<tr><td style="border-bottom:solid #80ffff">E</td></tr>');
  echo('<tr><td>' , gl$e["away"] , '</td></tr>');
  echo('<tr><td>' , gl$e["home"] , '</td></tr>');
  echo('</table></td>')
  # finish boxscore
  
  outstring
}

if (F) {
  get_linescore(gin)
}