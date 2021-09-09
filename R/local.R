#' embed a .md file, and change title level
#'
#' @param subfile the file you want to embed
#' @param addlevel how many levels you want to add
#'
#' @return text
#' @export
#'
embedMd <- function(subfile, addlevel=0){
	res <- knitr::knit_child(subfile, quiet = TRUE)

		if (addlevel != 0) {
	  titlehead = strrep("#", addlevel+1)
	  res=gsub("^#", titlehead, res)
	}
	paste(res,collapse='\n')
}

#' like embedMd, but can fetch part of it
#'
#' @param subfile the file you want to embed
#' @param startText startText
#' @param endText endText
#' @param withStart withStart
#' @param addlevel how many levels you want to add
#'
#' @return text
#' @export
#'
getMd <- function(subfile, startText="", endText="", withStart=TRUE, addlevel=0){
	res <- knitr::knit_child(subfile, quiet = TRUE)

	if ((startText != "") || (endText != "")) {
    res <- getPart(res, startText, endText, withStart)
	}

	if (addlevel > 0) {
	  titlehead = paste0(strrep("#", addlevel+1)," ")
    res <- gsub("# ", titlehead, res)
	}
	paste(res,collapse='\n')
}


