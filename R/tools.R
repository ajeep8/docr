#' Save fetched .md file
#'
#' @param filename filename
#' @param content filecontent
#'
#' @return text
#' @export
#'
savemd <- function(filename, content) {
  fnlst <- strsplit(filename, split='/')
  filename= fnlst[[1]][length(fnlst[[1]])]

  pathlst <- strsplit(rstudioapi::documentPath(),split='/')
  pathlst[[1]][length(pathlst[[1]])] <- filename
  subpath=paste(pathlst[[1]],collapse = '/')
  write(content, paste(subpath,".md"))
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
		ss <- str_locate(res,"## 输入/输出")[1]
	}
	if (!withStart) {ss=ss+nchar(startText)}
	if (endText=="") {
		ee=nchar(content)
	} else {
		ee=str_locate(res,'### 读Transformer')[1]-1
	}
	cat(str_sub(content,ss,ee))
}









