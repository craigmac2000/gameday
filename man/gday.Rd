% Generated by roxygen2 (4.0.2): do not edit by hand
\name{gday}
\alias{gday}
\title{Is it Gameday?}
\usage{
gday(team.name = "canucks", date = Sys.Date())
}
\arguments{
\item{team.name}{Defaults to "canucks"}
}
\value{
\code{TRUE} if \code{team.name} has an NHL game on \code{date},
\code{FALSE} otherwise
}
\description{
This function returns TRUE if your NHL team plays today
and FALSE otherwise
}
\details{
You know then problem: You're in your office writing R code and
suddenly have the urge to check whether your NHL team has a game today.
Before you know it you just wasted 15 minutes browsing the lastest
news on your favorite hockey webpage.
Suffer no more! You can now ask R directly, without tempting yourself
by firing up your web browser.
}
\note{
case in \code{team.name} is ignored
}
\examples{
gday()
gday("canadiens")
gday("Bruins")
}
\keyword{misc}

