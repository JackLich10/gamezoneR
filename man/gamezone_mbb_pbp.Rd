% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamezone_pbp.R
\name{gamezone_mbb_pbp}
\alias{gamezone_mbb_pbp}
\title{Get GameZone play-by-play and shot location data}
\usage{
gamezone_mbb_pbp(game_id, sub_parse = FALSE)
}
\arguments{
\item{game_id}{unique GameZone GameID}

\item{sub_parse}{whether or not to attempt to parse substitution and lineup data}
}
\description{
Get GameZone play-by-play and shot location data
}
\examples{
\dontrun{
 gamezone_mbb_pbp(game_id = 2316023)
}

}
\author{
Jack Lichtenstein
}
