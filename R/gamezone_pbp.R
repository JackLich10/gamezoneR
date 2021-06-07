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

  message <- paste0("Scraping Play-by-Play for GameID: ", game_id)
  usethis::ui_todo(message)

  # formulate url
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/events/"
  append <- "/AllEventDataBasketball/?box=true&pbp=true&shotChart=true&shotZones=true&gameState=true&language=en-US"
  url <- paste0(base_url, game_id, append)

  # response <- httr::RETRY("GET", url, quiet = T)
  #
  # content <- jsonlite::fromJSON(httr::content(response, as = "text"))

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

  if (!is.null(json[["TournamentId"]])) {
    neutral <- 1
  } else {
    neutral <- 0
  }

  if (length(json[["PBP"]]) == 0) {
    usethis::ui_oops("GameZone does not have Play-by-Play data for this game...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  # function to extract home and away team
  extract_home_away <- function(data, home = T) {
    data %>%
      purrr::map_df(., `[`) %>%
      dplyr::select(-c(dplyr::any_of("Record"))) %>%
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
    dplyr::select(.data$location, .data$team, .data$nickname) %>%
    tidyr::pivot_wider(names_from = .data$location,
                       values_from = c(.data$team, .data$nickname)) %>%
    dplyr::rename_with(.cols = dplyr::starts_with("team_"),
                       ~ stringr::str_remove(., "team_"))

  # extract date
  date <- json[["DateTime"]]
  date <- as.Date(unlist(stringr::str_split(date, " \\| "))[2],
                  format = "%B %d, %Y")

  # extract season
  season <- json[["Season"]]
  season <- paste0(season, "-", as.numeric(stringr::str_sub(season, start = 3)) + 1)

  # extract play-by-play
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
  pbp <- pbp %>%
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
                  #### ADDING IN NEUTRAL COURT FLAG
                  neutral = neutral,
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
                  assist = stringr::str_remove(.data$assist, "^.*\\. "),
                  dplyr::across(c(.data$shooter, .data$assist),
                                ~ dplyr::na_if(., " ")),
                  dplyr::across(c(.data$shooter, .data$assist),
                                ~ dplyr::na_if(., ""))) %>%
    dplyr::as_tibble() %>%
    # select what is needed
    dplyr::select(dplyr::any_of(c(
      "season", "date", "game_id", "play_id" = "id", "neutral",
      "half" = "period", "home", "away",
      "home_name" = "nickname_home", "away_name" = "nickname_away",
      "home_score", "away_score", "score_diff", "team_id", "event_team",
      "game_secs_remaining", "half_secs_remaining", "play_length",
      "desc" = "text", "shot_outcome", "free_throw", "three_pt",
      "shot_desc" = "description", "loc_x" = "y", "loc_y" = "x",
      "shooter_id", "shooter", "assist", "substitution"
    ))) %>%
    # correct location data
    dplyr::mutate(loc_y = .data$loc_y + 5.25,
                  loc_x = dplyr::case_when(
                    .data$free_throw == T ~ NA_real_,
                    is.na(.data$loc_x) ~ NA_real_,
                    .data$loc_y >= 47 ~ 50 - .data$loc_x,
                    .data$loc_y < 47 ~ .data$loc_x),
                  loc_y = dplyr::case_when(
                    .data$free_throw == T ~ NA_real_,
                    is.na(.data$loc_y) ~ NA_real_,
                    .data$loc_y >= 47 ~ 94 - .data$loc_y,
                    .data$loc_y < 47 ~ .data$loc_y))

  # parse timeouts
  pbp <- pbp %>%
    dplyr::mutate(home_timeouts = ifelse(.data$half <= 2, 4, NA_real_),
                  away_timeouts = ifelse(.data$half <= 2, 4, NA_real_),
                  # add a timeout for start of each overtime period
                  add_timeout_ot = ifelse(stringr::str_detect(.data$desc, "Start of the .*Overtime"),
                                          .data$half - 2, NA_real_),
                  # fill in `event_team` for timeouts and team turnovers, rebounds
                  event_team = dplyr::case_when(
                    is.na(.data$team_id) & stringr::str_detect(.data$desc, .data$home_name) ~ .data$home,
                    is.na(.data$team_id) & stringr::str_detect(.data$desc, .data$away_name) ~ .data$away,
                    T ~ .data$event_team),
                  # all timeouts
                  timeout = ifelse(stringr::str_detect(desc, stringr::fixed("timeout")),
                                   1, 0),
                  # home/away timeouts
                  home_timeout = ifelse(.data$timeout == 1 & .data$event_team == .data$home &
                                          !stringr::str_detect(desc, stringr::fixed("Official")),
                                        1, 0),
                  away_timeout = ifelse(.data$timeout == 1 & .data$event_team == .data$away &
                                          !stringr::str_detect(desc, stringr::fixed("Official")),
                                        1, 0)) %>%
    # timeouts remaining by half
    dplyr::group_by(.data$half) %>%
    dplyr::mutate(home_timeouts = .data$home_timeouts - cumsum(.data$home_timeout),
                  away_timeouts = .data$away_timeouts - cumsum(.data$away_timeout)) %>%
    dplyr::ungroup() %>%
    # initialize timeouts remaining for start of overtime
    dplyr::mutate(dplyr::across(dplyr::ends_with("_timeouts_remaining"),
                                ~ ifelse(.data$desc == "Start of the Overtime",
                                         dplyr::lag(.),
                                         .))) %>%
    # fill in timeouts remaining downward
    tidyr::fill(.data$home_timeouts,
                .data$away_timeouts,
                .data$add_timeout_ot,
                .direction = "down") %>%
    # fix timeouts remaining for overtime
    dplyr::mutate(home_timeouts = ifelse(.data$half > 2,
                                         .data$home_timeouts + .data$add_timeout_ot - cumsum(.data$home_timeout & .data$half > 2),
                                         .data$home_timeouts),
                  away_timeouts = ifelse(.data$half > 2,
                                         .data$away_timeouts + .data$add_timeout_ot - cumsum(.data$away_timeout & .data$half > 2),
                                         .data$away_timeouts)) %>%
    # fill in `team_id` when missing
    dplyr::group_by(.data$event_team) %>%
    tidyr::fill(.data$team_id, .direction = "downup") %>%
    dplyr::ungroup() %>%
    # select what is needed
    dplyr::relocate(dplyr::ends_with("_timeouts"),
                    .after = .data$away_name) %>%
    dplyr::select(-c(.data$add_timeout_ot:.data$away_timeout)) %>%
    # add a flag for stoppage of play
    dplyr::mutate(stoppage = stringr::str_detect(.data$desc, "Start of the|timeout|End of the|Game is in Delay"))

  # parse possessions, joining because want to get rid of stoppages
  # so that there are no NA values in dplyr::lead, dplyr::lag for
  # `poss_before`, `poss_after`
  pbp_appended <- pbp %>%
    dplyr::left_join(pbp %>%
                       # filter out all stoppages
                       dplyr::filter(!(.data$substitution == 1 | .data$stoppage == T)) %>%
                       # indicators for various plays
                       dplyr::mutate(d_reb = stringr::str_detect(.data$desc, stringr::fixed("defensive rebound")),
                                     o_reb = stringr::str_detect(.data$desc, stringr::fixed("offensive rebound")),
                                     turnover = stringr::str_detect(.data$desc, stringr::fixed("turnover")),
                                     steal = stringr::str_detect(.data$desc, stringr::fixed("steal")),
                                     off_foul = stringr::str_detect(.data$desc, stringr::fixed("Offensive foul")),
                                     shooting_foul = stringr::str_detect(.data$desc, stringr::fixed("Shooting foul")),
                                     personal_foul = stringr::str_detect(.data$desc, stringr::fixed("Personal foul")),
                                     technical_foul = stringr::str_detect(.data$desc, stringr::fixed("Technical foul")),
                                     flagrant_foul = stringr::str_detect(.data$desc, stringr::fixed("Flagrant foul")),
                                     front_end = stringr::str_detect(.data$desc, "1 of 2|1 of 3|2 of 3"),
                                     one_and_one = stringr::str_detect(.data$desc, stringr::fixed("1st of 1-and-1")),
                                     technical_ft = stringr::str_detect(.data$desc, stringr::fixed("free throw technical")),
                                     poss_before = dplyr::case_when(
                                       # steals mean possession is with `event_team`
                                       # steals are coded as having possession of the team that
                                       # turns the ball over, not the stealing team
                                       .data$steal == T ~ .data$event_team,
                                       # turnover means possession is with `event_team`
                                       .data$turnover == T ~ .data$event_team,
                                       # offensive foul means possession is with `event_team`
                                       .data$off_foul == T ~ .data$event_team,
                                       # shooting or personal fouls mean possession is the other team
                                       .data$shooting_foul + .data$personal_foul >= 1 & .data$event_team == .data$away ~ .data$home,
                                       .data$shooting_foul + .data$personal_foul >= 1 & .data$event_team == .data$home ~ .data$away,
                                       # offensive rebounds means possession is with `event_team`
                                       .data$o_reb == T ~ .data$event_team,
                                       # defensive rebounds means possession was with the other team
                                       .data$d_reb == T & .data$event_team == .data$away ~ .data$home,
                                       .data$d_reb == T & .data$event_team == .data$home ~ .data$away,
                                       # otherwise, possession before is the `event_team`
                                       !is.na(.data$shot_outcome) ~ .data$event_team,
                                       T ~ NA_character_),
                                     poss_after = dplyr::case_when(
                                       # offensive and defensive rebounds mean possession is with `event_team`
                                       .data$d_reb == T ~ .data$event_team,
                                       .data$o_reb == T ~ .data$event_team,
                                       # all technical free throws keep possession
                                       .data$technical_ft == T ~ .data$poss_before,
                                       # turnover means possession changes teams
                                       .data$turnover == T & .data$event_team == .data$away ~ .data$home,
                                       .data$turnover == T & .data$event_team == .data$home ~ .data$away,
                                       # an offensive foul followed by turnover by same team keeps possession
                                       # the next row (turnover row) will change the possession
                                       .data$off_foul == T & .data$event_team == dplyr::lead(.data$event_team) &
                                         dplyr::lead(.data$turnover == T) ~ .data$event_team,
                                       # offensive foul means possession changes teams
                                       .data$off_foul == T & .data$event_team == .data$away ~ .data$home,
                                       .data$off_foul == T & .data$event_team == .data$home ~ .data$away,
                                       # shooting or personal fouls mean possession stays with the other team
                                       .data$shooting_foul + .data$personal_foul >= 1 ~ .data$poss_before,
                                       # and-one's keep possession
                                       .data$shot_outcome == "made" & dplyr::lead(.data$shooting_foul == T) & .data$event_team != dplyr::lead(.data$event_team) ~ .data$event_team,
                                       # front ends of free throw's keep possession
                                       .data$front_end == T ~ .data$poss_before,
                                       # made front ends of 1-and-1 free throw's keep possession
                                       .data$one_and_one == T & shot_outcome == "made" ~ .data$poss_before,
                                       # made fga's that are not and-one's change possession
                                       .data$shot_outcome == "made" & .data$event_team == .data$away ~ .data$home,
                                       .data$shot_outcome == "made" & .data$event_team == .data$home ~ .data$away,
                                       # otherwise, change to the next `poss_before`
                                       # poor logic for finding the next non-NA `poss_before`
                                       dplyr::lead(is.na(.data$poss_before)) ~ dplyr::lead(.data$poss_before, n = 2),
                                       T ~ dplyr::lead(.data$poss_before)),
                                     # if there's a technical or flagrant foul, `poss_before` is the previous `poss_after`
                                     poss_before = dplyr::case_when(
                                       .data$technical_foul + .data$flagrant_foul >= 1 ~ dplyr::lag(.data$poss_after),
                                       T ~ .data$poss_before),
                                     # indicator for when a possession changes teams
                                     poss_change = dplyr::if_else(.data$poss_before != .data$poss_after,
                                                                  1, 0, missing = 0)) %>%
                       dplyr::select(.data$game_id, .data$half, .data$play_id,
                                     .data$poss_before, .data$poss_after, .data$poss_change) %>%
                       dplyr::distinct(),
                     by = c("game_id", "half", "play_id")) %>%
    # initialize start of each half/ot as `poss_after` being the next `poss_before`
    dplyr::mutate(poss_after = ifelse(stringr::str_detect(.data$desc, stringr::fixed("Start of the")),
                                      dplyr::lead(.data$poss_before),
                                      .data$poss_after),
                  # make end of games have last possession
                  dplyr::across(c(.data$poss_before, .data$poss_after),
                                ~ ifelse(stringr::str_detect(.data$desc, stringr::fixed("End of the Game")),
                                         dplyr::last(stats::na.omit(.data$poss_after)),
                                         .)),
                  # possession change
                  poss_change = tidyr::replace_na(.data$poss_change, 0),
                  poss_change = ifelse(stringr::str_detect(.data$desc, stringr::fixed("Start of the")),
                                       1, .data$poss_change),
                  # add in possession number
                  poss_number = dplyr::lag(cumsum(.data$poss_change)),
                  poss_number = dplyr::case_when(
                    .data$poss_number == 0 ~ 1,
                    stringr::str_detect(.data$desc, stringr::fixed("End of the Game")) ~ NA_real_,
                    T ~ .data$poss_number),
                  dplyr::across(dplyr::starts_with("poss_"),
                                ~ ifelse((.data$substitution == 1 | .data$stoppage == T) &
                                           !stringr::str_detect(.data$desc, stringr::fixed("Start of the")) &
                                           !stringr::str_detect(.data$desc, stringr::fixed("End of the Game")),
                                         NA_character_,
                                         .))) %>%
    dplyr::select(-dplyr::any_of(c("stoppage", "poss_change")))

  if (isTRUE(sub_parse)) {
    usethis::ui_todo("Attempting to parse lineup and substitution data...\nNote that GameZone does not track all substitutions")
    pbp_appended <- parse_substitutions(pbp_appended)
  }

  message <- paste0("Completed GameID: ", game_id, "\n",
                    home_away_cols$away, " @ ", home_away_cols$home)
  usethis::ui_done(message)

  return(pbp_appended)
}
