#' Get GameZone play-by-play and shot location data
#' @author Jack Lichtenstein
#' @param game_id unique GameZone GameID
#' @param sub_parse whether or not to attempt to parse substitution and lineup data
#' @export
#'
#' @examples
#' \dontrun{
#'  gamezone_cbb_pbp(game_id = 2316023)
#' }
#'
gamezone_cbb_pbp <- function(game_id, sub_parse = F) {
  # some error checks
  if (is.na(game_id) || is.null(game_id)) {
    usethis::ui_oops("GameID is missing...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  if (length(game_id) != 1) {
    usethis::ui_oops("Passed in multiple GameID's. This function returns the Play-by-Play of one game at a time...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  message <- paste0("Scraping GameID: ", game_id)
  usethis::ui_todo(message)

  # formulate url
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/events/"
  append <- "/AllEventDataBasketball/?box=true&pbp=true&shotChart=true&shotZones=true&gameState=true&language=en-US"
  url <- paste0(base_url, game_id, append)

  # extract json
  json <- try(jsonlite::fromJSON(url),
              silent = T)

  if ("try-error" %in% class(json)) {
    usethis::ui_oops("GameZone does not have Play-by-Play data for this game...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  if (json[["Period"]] %in% c("Cancelled", "Postponed")) {
    usethis::ui_oops("This game was cancelled or postponed...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  if (json[["Period"]] %in% c("Pre-Game")) {
    usethis::ui_oops("This game has not been played yet...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  if (json[["Period"]] %in% c("1st", "2nd")) {
    usethis::ui_oops("This game is ongoing...")
    usethis::ui_info("Returning Play-by-Play of the incompleted game if available")
  }

  # function to extract home and away team
  extract_home_away <- function(data, home = T) {
    data %>%
      purrr::map_df(., `[`) %>%
      dplyr::select(-c(tidyselect::any_of("Record"))) %>%
      janitor::clean_names() %>%
      dplyr::distinct(.data$id, .keep_all = T) %>%
      dplyr::transmute(team_abbr = .data$abbr,
                       team_id = .data$id,
                       team = .data$location,
                       nickname = .data$name,
                       score = .data$total,
                       location = ifelse(home == T, "home", "away"))
  }

  # extract team information
  teams <- dplyr::bind_rows(extract_home_away(json[["Home"]], home = T),
                            extract_home_away(json[["Away"]], home = F))

  # columns bind to the pbp
  home_away_cols <- teams %>%
    dplyr::select(.data$location, .data$team) %>%
    tidyr::pivot_wider(names_from = .data$location,
                       values_from = .data$team)

  # extract date
  date <- json[["DateTime"]]
  date <- as.Date(unlist(stringr::str_split(date, " \\| "))[2],
                  format = "%B %d, %Y")

  # extract season
  season <- json[["Season"]]
  season <- paste0(season, "-", as.numeric(stringr::str_sub(season, start = 3)) + 1)

  if (length(json[["PBP"]]) == 0) {
    usethis::ui_oops("GameZone does not have Play-by-Play data for this game...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  # extract pbp
  pbp <- json[["PBP"]] %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c(.data$team_id, .data$home_score, .data$away_score),
                                as.numeric))

  # extract shot locations
  shots <- json[["Shotchart"]] %>%
    dplyr::select(-.data$Player) %>%
    janitor::clean_names() %>%
    dplyr::bind_cols(json[["Shotchart"]][["Player"]] %>%
                       janitor::clean_names() %>%
                       dplyr::rename(shooter_id = .data$id))

  # join shots and play-by-play together
  pbp_appended <- pbp %>%
    dplyr::bind_cols(home_away_cols) %>%
    # join in `event_team`
    dplyr::left_join(teams %>%
                       dplyr::select(.data$team_id, event_team = .data$team),
                     by = "team_id") %>%
    # join in shot location info
    dplyr::left_join(shots,
                     by = c("id", "period", "team_id", "time",
                            "text" = "play_text")) %>%
    # arrange in chronological order
    dplyr::arrange(.data$id) %>%
    tidyr::separate(.data$time, into = c("min", "secs"),
                    convert = T, remove = F) %>%
    dplyr::mutate(season = season,
                  date = date,
                  game_id = game_id,
                  score_diff = .data$home_score - .data$away_score,
                  substitution = dplyr::case_when(
                    stringr::str_detect(.data$text, "^Starting Lineup") ~ 1,
                    stringr::str_detect(.data$text, "^Substitution") ~ 1,
                    T ~ 0),
                  # find time remaining
                  half_secs_remaining = (.data$min*60) + .data$secs,
                  game_secs_remaining = (2-.data$period)*20*60+ (.data$min*60) + .data$secs,
                  # fix for OT
                  game_secs_remaining = ifelse(!.data$period %in% c(1, 2),
                                               (.data$min*60) + .data$secs,
                                               .data$game_secs_remaining),
                  # find play length
                  play_length = dplyr::lag(.data$game_secs_remaining) - .data$game_secs_remaining,
                  # paste together shooter
                  shooter = ifelse(!is.na(.data$first_name),
                                   paste0(.data$first_name, " ", .data$last_name),
                                   NA_character_),
                  # shot outcome indicator
                  shot_outcome = dplyr::case_when(
                    .data$is_scoring_play == T ~ "made",
                    stringr::str_detect(.data$text, "makes|dunks") ~ "made",
                    stringr::str_detect(.data$text, "misses|blocks") ~ "missed"),
                  play_length = ifelse(!is.na(.data$shot_outcome) & is.na(.data$play_length),
                                       0, .data$play_length),
                  # three point indicator
                  three_pt = stringr::str_detect(.data$text, "3-point"),
                  # free throw indicator
                  free_throw = stringr::str_detect(.data$text, "free throw"),
                  # extract assist player
                  assist = stringr::str_extract(.data$text, "\\..* with the assist.$"),
                  assist = stringr::str_remove(.data$assist, " with the assist.$"),
                  assist = stringr::str_remove(.data$assist, "^.*\\. ")) %>%
    dplyr::as_tibble() %>%
    # select what is needed
    dplyr::transmute(.data$season, .data$date, .data$game_id,
                     play_id = .data$id, half = .data$period,
                     .data$home, .data$away, .data$home_score, .data$away_score,
                     .data$score_diff, .data$team_id, .data$event_team,
                     .data$game_secs_remaining, .data$half_secs_remaining, .data$play_length,
                     desc = .data$text, .data$shot_outcome,
                     .data$free_throw, .data$three_pt,
                     shot_desc = .data$description, loc_x = .data$y, loc_y = .data$x,
                     .data$shooter_id, .data$shooter, .data$assist, .data$substitution) %>%
    # correct location data
    dplyr::mutate(loc_y = .data$loc_y + 5.25,
                  loc_x = dplyr::case_when(
                    .data$free_throw == T ~ NA_real_,
                    is.na(.data$loc_x) ~ NA_real_,
                    .data$loc_y >= 47 ~ 50 - .data$loc_x,
                    .data$loc_y < 47 ~ .data$loc_x),
                  loc_y = dplyr::case_when(
                    free_throw == T ~ NA_real_,
                    is.na(.data$loc_y) ~ NA_real_,
                    .data$loc_y >= 47 ~ 94 - .data$loc_y,
                    .data$loc_y < 47 ~ .data$loc_y))

  if (isTRUE(sub_parse)) {
    usethis::ui_todo("Attempting to parse lineup and substitution data...\nNote that GameZone does not track all substitutions")
    pbp_appended <- parse_substitutions(pbp_appended)
  }

  message <- paste0("Completed GameID: ", game_id, "\n",
                    home_away_cols$away, " @ ", home_away_cols$home)
  usethis::ui_done(message)

  return(pbp_appended)
}


