
makeHtmlTable <- function(m) {
  
  innerHtmlTable <- function(x) {
    x <- paste0('<td class="htsTableCell">',trimws(x),'</td>')
    x[1] <- paste0('<tr class="htsTableRow">',x[1])
    x[length(x)] <- paste0(x[length(x)],'</tr>')
    paste(x,collapse = '')
  }
  
  x <- paste0(
    '<table class="htsTable">',
    paste0(apply(m,1,innerHtmlTable),collapse = ''),
    '</table>'
  )  
  
  cat(x)
}


viewHtmlTable <- function(x, useViewer = TRUE, ...){
  # Don't use viewer if in knitr
  if (useViewer &&
      !"package:knitr" %in% search()){
    
    htmlFile <- tempfile(fileext=".html")
    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                      x,
                      "</div>",
                      "</body>",
                      "</html>", sep="\n")
    cat(htmlPage, file=htmlFile)
    
    viewer <- getOption("viewer")
    if (!is.null(viewer) &&
        is.function(viewer)){
      # (code to write some content to the file)
      viewer(htmlFile)
    }else{
      utils::browseURL(htmlFile)
    }
  }else{
    cat(x)
  }
}