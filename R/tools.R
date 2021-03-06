#' Save fetched .md file
#'
#' @param wikiPath wiki.js page path
#' @param content the page content
#'
#' @return text
#' @export
#'
saveMd <- function(wikiPath, content) {
  fnlst <- strsplit(wikiPath, split='/')
  filename= fnlst[[1]][length(fnlst[[1]])]

  pathlst <- strsplit(rstudioapi::documentPath(),split='/')
  pathlst[[1]][length(pathlst[[1]])] <- filename
  subpath=paste(pathlst[[1]],collapse = '/')
  write(content, paste0(trimws(subpath),".md"))
}

#' get part of content
#'
#' @param content all contents
#' @param startText startText
#' @param endText endText
#' @param withStart is output with startText?
#'
#' @return text
#' @export
#'
getPart <- function(content,startText="", endText="", withStart=TRUE) {
	library(stringr)
	if (startText=="") {
		ss <- 1
	} else {
		ss <- str_locate(content,startText)[1]
	}
	if (!withStart) {ss=ss+nchar(startText)}
	if (endText=="") {
		ee=nchar(content)
	} else {
		ee=str_locate(content,endText)[1]-1
	}
	str_sub(content,ss,ee)
}


#' remove part all in content
#'
#' @param content all contents
#' @param startText startText
#' @param endText endText
#'
#' @return text
#' @export
#'
rmParts <- function(content, startText="\\*\\*示例\\*\\*", endText="### ") {
  library(stringr)
  ss=str_locate(content, startText)
  while(!is.na(ss[1])) {
    res1 <- str_sub(content,0,ss[1]-1)
    ee=str_locate(str_sub(content,ss[1],-1),endText)
    res2=''
    if (!is.na(ee[1])) {
      res2 <- str_sub(content, ss[1]+ee[1]-2, -1)
      content <- paste(res1,res2,sep='')
    } else {
      content <- res1
      break
    }
    ss=str_locate(content, startText)
  }
  content
}

