# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")

# Initialize first available season in data repository
first_season <- "2014-15"

# Find most recent college basketball season
most_recent_season <- function() {
  end <- ifelse(as.numeric(substr(Sys.Date(), 6, 7)) >= 10,
         as.numeric(substr(Sys.Date(), 1, 4)) + 1,
         as.numeric(substr(Sys.Date(), 1, 4))
  )

  season <- paste0(end - 1, "-", substr(end, 3, 4))
  return(season)
}

# Find available college basketball seasons in data repository
available_seasons <- function() {
  last_season <- most_recent_season()

  seasons <- as.numeric(substr(first_season, 1, 4)):as.numeric(substr(last_season, 1, 4))

  return(paste0(seasons, "-", substr(seasons+1, 3, 4)))
}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
