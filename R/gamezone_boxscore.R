#' Get GameZone boxscore
#' @author Jack Lichtenstein
#' @param game_id unique GameZone GameID
#' @export
#'
#' @examples
#' \dontrun{
#'  gamezone_cbb_boxscore(game_id = 2316023)
#' }
#'
gamezone_cbb_boxscore <- function(game_id) {
  # some error checks
  if (is.na(game_id) || is.null(game_id)) {
    usethis::ui_oops("GameID is missing...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  if (length(game_id) != 1) {
    usethis::ui_oops("Passed in multiple GameID's. This function returns the box score of one game at a time...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  message <- paste0("Scraping box score for GameID: ", game_id)
  usethis::ui_todo(message)

  # formulate url
  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/events/"
  append <- "/boxscore"
  url <- paste0(base_url, game_id, append)

  # extract json
  json <- try(jsonlite::fromJSON(url),
              silent = T)

  if ("try-error" %in% class(json) || length(json[["Boxscore"]]) == 0) {
    usethis::ui_oops("GameZone does not have box score data for this game...")
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  # extract box score
  box <- json[["Boxscore"]]

  # extract date
  date <- json[["DateTime"]]
  date <- as.Date(unlist(stringr::str_split(date, " \\| "))[2],
                  format = "%B %d, %Y")

  # extract season
  season <- json[["Season"]]
  season <- paste0(season, "-", as.numeric(stringr::str_sub(season, start = 3)) + 1)

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

  unnested <- box %>%
    tidyr::unnest(.data$Players)

  boxscore <- unnested[["Player"]] %>%
    dplyr::bind_cols(unnested %>%
                       dplyr::select(-.data$Player)) %>%
    tidyr::unnest(.data$Stats) %>%
    tidyr::pivot_wider(names_from = .data$Abbr,
                       values_from = .data$Game) %>%
    janitor::clean_names() %>%
    tidyr::separate(.data$or_tr, into = c("o_reb", "tot_reb"),
                    sep = "-", convert = T, remove = T) %>%
    dplyr::left_join(teams,
                     by = "team_id") %>%
    dplyr::transmute(season = season, date = date,
                     .data$team_id, .data$team, .data$location,
                     .data$score, player_id = .data$id,
                     jersey_number = as.numeric(.data$uniform),
                     position = .data$pos, starter = .data$is_starter,
                     name = paste0(.data$first_name, " ", .data$last_name),
                     dplyr::across(c(.data$min, .data$pts), as.numeric),
                     .data$fg_fga, .data$ft_fta, .data$x3p_3pa, .data$o_reb, .data$tot_reb,
                     dplyr::across(.data$ast:.data$pf, as.numeric))

  message <- paste0("Completed box score collection for GameID: ", game_id, "\n",
                    teams$team[teams$location == "away"], " @ ",
                    teams$team[teams$location == "home"])
  usethis::ui_done(message)

  return(boxscore)
}
