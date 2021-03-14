#' Get GameZone team statistical summary for a given season
#' @author Jack Lichtenstein
#' @param team team name
#' @param season season (of the form "2020-21")
#' @export
#'
#' @examples
#' \dontrun{
#'  get_team_season_stats(team = "Duke", season = "2018-19")
#' }
#'
get_team_season_stats <- function(team, season = "2020-21") {
  # find year
  year <- stringr::str_sub(season, end = 4)

  # find team id
  team_id <- gamezoneR::cbb_team_info %>%
    tidyr::pivot_longer(cols = c(.data$team_name:.data$sref_name),
                        names_to = "organization",
                        values_to = "team_name") %>%
    dplyr::distinct(.data$team_name, .keep_all = T) %>%
    dplyr::select(.data$conference, .data$organization,
                  .data$game_zone_id, .data$team_name,
                  dplyr::everything()) %>%
    dplyr::filter(.data$team_name == team) %>%
    dplyr::pull(.data$game_zone_id)

  # error checking
  if (rlang::is_empty(team_id)) {
    usethis::ui_oops("Team name is not found in dictionary...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  message <- paste0("Scraping ", season, " season summary statistics for: ", team)
  usethis::ui_info(message)

  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/team/"
  append <- "/seasonstats/"
  url <- paste0(base_url, team_id, append, year)

  json <- try(jsonlite::fromJSON(url, flatten = T),
              silent = T)

  if ("try-error" %in% class(json)) {
    usethis::ui_oops(paste0("No ", season, " season statistics available for: ", team))
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  team_stats <- json[["TeamStats"]] %>%
    dplyr::select(-.data$Name) %>%
    dplyr::mutate(Game = as.numeric(.data$Game)) %>%
    tidyr::pivot_wider(names_from = .data$Abbr,
                       values_from = .data$Game) %>%
    janitor::clean_names() %>%
    dplyr::mutate(season = season,
                  team_id = team_id) %>%
    dplyr::select(.data$season, .data$team_id, dplyr::everything()) %>%
    dplyr::as_tibble()

  return(team_stats)
}

#' Get GameZone player statistical summary for a given player
#' @author Jack Lichtenstein
#' @param player_id playerID found in a play-by-play or box score dataframe returned by
#' \code{gamezone_cbb_pbp} or \code{gamezone_cbb_boxscore}, respectively
#' @export
#'
#' @examples
#' \dontrun{
#'  get_player_season_stats(player_id = 1061640)
#' }
#'
get_player_season_stats <- function(player_id) {
  # construct urls
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/player/"
  append <- "/seasonstats"
  url1 <- paste0(base_url, player_id)
  url2 <- paste0(base_url, player_id, append)

  json1 <- try(jsonlite::fromJSON(url1, flatten = T),
              silent = T)

  json2 <- try(jsonlite::fromJSON(url2, flatten = T),
              silent = T)

  if ("try-error" %in% class(json1) || "try-error" %in% class(json2)) {
    usethis::ui_oops(paste0("No ", season, " season statistics available for: ", player_id))
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  # extract player info
  player_info <- json1 %>%
    purrr::map_df(., `[`) %>%
    janitor::clean_names() %>%
    dplyr::transmute(player_id = id,
                     name = paste0(first_name, " ", last_name),
                     jersey_number = as.numeric(uniform),
                     position, height, weight = as.numeric(weight), position,
                     date_of_birth = as.Date(date_of_birth, format = "%m/%d/%Y"),
                     hometown, team_id, team_name)

  # extract player season summary stats
  player_stats <- json2[["Stats"]] %>%
    dplyr::select(-.data$Name) %>%
    dplyr::mutate(Game = as.numeric(.data$Game),
                  Abbr = stringr::str_remove(Abbr, "Stat_")) %>%
    tidyr::pivot_wider(names_from = .data$Abbr,
                       values_from = .data$Game) %>%
    janitor::clean_names() %>%
    dplyr::bind_cols(player_info) %>%
    dplyr::select(.data$player_id, .data$name, .data$jersey_num,
                  dplyr::everything()) %>%
    dplyr::as_tibble()

  return(player_stats)
}



