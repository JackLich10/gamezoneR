#' Information for Division I college basketball teams
#'
#' A dataset containing the team ID's, various team names, colors, and logos
#' of Division I college basketball teams
#'
#' @format A data frame with 359 rows and 24 variables:
#' \describe{
#'   \item{espn_id}{ESPN unique team ID}
#'   \item{game_zone_id}{GameZone unique team ID}
#'   \item{team_name}{A common team name}
#'   \item{game_zone}{GameZone team name}
#'   \item{espn}{ESPN team name}
#'   \item{ncaa}{NCAA team name}
#'   \item{espn_pbp}{ESPN team name in Play-by-Play data}
#'   \item{warren_nolan}{Warren Nolan team name}
#'   \item{trank}{Barttorvik Trank team name}
#'   \item{name_247}{247 Sport team name}
#'   \item{sref_link}{sports-reference.com team link}
#'   \item{sref_name}{sports-reference.com team name}
#'   \item{espn_pbp}{NCAA conference}
#'   \item{color_espn}{ESPN primary color}
#'   \item{alt_color_espn}{ESPN alternate color}
#'   \item{team_logo_espn}{ESPN logo url}
#'   \item{alt_team_logo_espn}{ESPN alternate logo url}
#'   \item{primary_color}{Primary color}
#'   \item{secondary_color}{Secondary color}
#'   \item{tertiary_color}{Tertiary color}
#'   \item{color_4}{Color 4}
#'   \item{color_5}{Color 5}
#'   \item{color_6}{Color 6}
#'   \item{logo_url}{Wikimedia logo url}
#' }
"cbb_team_info"

#' An object for shot chart plotting
#'
#' A list which contains a base court for basketball shot location plotting
#'
#' @format A list with 9 items
"base_court"
