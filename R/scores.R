#' Is it Gameday?
#'
#' This function returns TRUE if your NHL team plays today
#' and FALSE otherwise
#'
#' You know then problem: You're in your office writing R code and
#' suddenly have the urge to check whether your NHL team has a game today.
#' Before you know it you just wasted 15 minutes browsing the lastest
#' news on your favorite hockey webpage.
#' Suffer no more! You can now ask R directly, without tempting yourself
#' by firing up your web browser.
#' @param date Defaults to "Current date"
#' @return \score of all games if games were played on the date
#' @keywords misc
#' @note returns error if date does not follow yyyy-mm-dd format
#' @export
#' @examples
#' scores()
#' scores("2014-02-14")
#' scores("2014-2-14")
scores <- function(date = Sys.Date()){
	internet_connection <- function() {
		tryCatch({RCurl::getURL("www.google.com"); TRUE},
						 error = function(err) FALSE)
	}
	if(!internet_connection()){stop("There is no internet connection")}
	check_date <- function(date){
		isdate <- as.Date(date)
		if(class(isdate) == "Date"){return(TRUE)}
		else{return(FALSE)}
	}
	check_date(date)
	url  <- paste0('http://live.nhle.com/GameData/GCScoreboard/',
								 date, '.jsonp')
	raw <- RCurl::getURL(url)
	json <- gsub('([a-zA-Z_0-9\\.]*\\()|(\\);?$)', "", raw, perl = TRUE)
	data <- jsonlite::fromJSON(json)$games
	with(data,
			 data.frame(home = paste(htn, htcommon),
			 					 away = paste(atn, atcommon),
			 					 home_score = hts,
			 					 away_score = ats))
}
document()
