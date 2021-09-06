#' embed a .md file, and change title level
#'
#' @param subfile the file you want to embed
#' @param addlevel how many levels you want to add
#'
#' @return text
#' @export
#'
embedMd <- function(subfile, addlevel=0){
	res <- readLines(subfile)
	if (addlevel != 0) {
	  titlehead = strrep("#", addlevel+1)
	  res=gsub("^#", titlehead, res)
	}
	paste(res,collapse='\n')
}

#' embed a .Rmd file, and change title level
#'
#' @param subfile the file you want to embed
#' @param addlevel how many levels you want to add
#'
#' @return text
#' @export
#'
embedRmd <- function(subfile, addlevel=0){
	res <- knitr::knit_child(subfile, quiet = TRUE)
	if (addlevel != 0) {
	  titlehead = paste0(strrep("#", addlevel+1)," ")
    res <- gsub("# ", titlehead, res)
	}
	paste(res,collapse='\n')
}
