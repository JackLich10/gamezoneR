#' Get GameZone master schedule data by date
#' @author Jack Lichtenstein
#' @param date date (of the form %Y-%m-%d)
#' @param ranked_games whether or not to only grab the games where at least
#' one team is ranked (much faster, since it uses the API)
#' @export
#'
#' @examples
#' \dontrun{
#'  get_master_schedule(date = "2021-03-01", ranked_games = T)
#'}
#'
get_master_schedule <- function(date, ranked_games = F) {
  # error checking
  if (is.na(as.Date(date, format = "%Y-%m-%d"))) {
    usethis::ui_oops("Date is not in the format %Y-%m-%d...\nReturning NULL")
    return(NULL)
  }

  if (isTRUE(ranked_games)) {
    message <- paste0("Scraping ranked games master schedule for: ", date)
    usethis::ui_info(message)

    # formulate url
    base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/events/"
    append <- "?eventTypeId=1&language=en-US&timezone=Eastern+Standard+Time"
    url <- paste0(base_url, date, append)

    json <- try(jsonlite::fromJSON(url, flatten = T),
                silent = T)

    if ("try-error" %in% class(json)) {
      usethis::ui_oops(paste0("No ranked games for: ", date))
      # usethis::ui_info("It is possible that the API is down. Check back later")
      usethis::ui_info("Returning Null")
      return(NULL)
    }

    schedule <- json %>%
      janitor::clean_names() %>%
      dplyr::mutate(tv = dplyr::na_if(.data$tv, y = ""),
                    home_record = paste0(.data$home_record_wins, "-", .data$home_record_losses),
                    away_record = paste0(.data$away_record_wins, "-", .data$away_record_losses),
                    dplyr::across(dplyr::ends_with("_total"), as.numeric)) %>%
      dplyr::select(.data$date_time, game_id = .data$event_id, .data$tv,
                    dplyr::starts_with("home_"), dplyr::starts_with("away_"),
                    -c(dplyr::contains("timeouts"), dplyr::ends_with("_wins"),
                       dplyr::ends_with("_losses"))) %>%
      dplyr::as_tibble()

  } else {
    if (as.Date(date) < as.Date("2020-11-25")) {
      usethis::ui_oops("GameZone has a master schedule from 2020-21 season to present...\nReturning NULL")
      return(NULL)
    }

    message <- paste0("Scraping master GameZone schedule for: ", date,
                      "\n Returning only completed games...")
    usethis::ui_info(message)

    # formulate url
    base_url <- "http://gamezone.stats.com/cbk/scoreboard.asp?conf=-1&day="
    url <- paste0(base_url, stringr::str_remove_all(date, "-"))

    # read in html of url
    html <- xml2::read_html(url)

    # extract teams
    teams <- html %>%
      rvest::html_nodes(".teamName") %>%
      rvest::html_text()

    # extract game links
    links <- html %>%
      rvest::html_nodes("#shsCBKScoreboard .shsGZLink a") %>%
      rvest::html_attr("href") %>%
      unique()

    # extract table of game scores and final
    tables <- html %>%
      rvest::html_nodes(".shsLinescore") %>%
      rvest::html_table(fill = T)

    # convert tables into box scores
    box <- purrr::map_df(seq_along(tables), function(index) {
      tables[[index]] %>%
        dplyr::select(1:4) %>%
        dplyr::slice(2:4) %>%
        dplyr::mutate(final = ifelse(dplyr::row_number() == 1 & stringr::str_detect(.data$X1, "Final"),
                                     "Complete", NA_character_)) %>%
        tidyr::fill(.data$final, .direction = "down") %>%
        janitor::row_to_names(row_number = 1) %>%
        janitor::clean_names() %>%
        dplyr::rename(team = 1, half1 = 2, half2 = 3,
                      total = 4, final = 5) %>%
        dplyr::mutate(game = index,
                      dplyr::across(.data$half1:.data$total,
                                    ~ suppressWarnings(as.numeric(.)))) %>%
        dplyr::select(-c(.data$half1, .data$half2))
    })

    # create schedule
    schedule <- dplyr::tibble(team = teams) %>%
      dplyr::mutate(home = ifelse(dplyr::row_number() %% 2 == 1,
                                  "home", "away"),
                    game = cumsum(.data$home == "home")) %>%
      dplyr::left_join(box,
                       by = c("team", "game")) %>%
      tidyr::pivot_wider(names_from = .data$home,
                         values_from = c(.data$team, .data$total)) %>%
      dplyr::rename(home = .data$team_home, away = .data$team_away,
                    home_total = .data$total_home, away_total = .data$total_away) %>%
      dplyr::mutate(dplyr::across(c(.data$home, .data$away),
                                  ~ stringr::str_extract(., "\\d{1,2}"),
                                  .names = "{col}_rank"),
                    dplyr::across(c(.data$home, .data$away),
                                  ~ stringr::str_trim(stringr::str_remove(., "\\d{1,2} "))),
                    dplyr::across(dplyr::ends_with("_total"), as.numeric),
                    dplyr::across(dplyr::ends_with("_rank"), as.numeric),
                    link = links,
                    game_id = stringr::str_extract(.data$link, "\\d{7}")) %>%
      dplyr::filter(.data$final == "Complete") %>%
      dplyr::transmute(game_date = date, game_id = as.numeric(.data$game_id),
                       .data$home, .data$away, .data$home_total, .data$away_total,
                       home_ap_ranking = .data$home_rank, away_ap_ranking = .data$away_rank) %>%
      dplyr::mutate(year = lubridate::year(.data$game_date),
                    month = lubridate::month(.data$game_date),
                    season = ifelse(.data$month < 10, .data$year - 1),
                    season = paste0(.data$season, "-",
                                    as.numeric(stringr::str_sub(.data$season, 3)) + 1)) %>%
      dplyr::select(.data$season, dplyr::everything(),
                    -c(.data$year, .data$month))
  }
  return(schedule)
}

#' Get GameZone team schedule data for a given season
#' @author Jack Lichtenstein
#' @param team team name
#' @param season season (of the form "2020-21")
#' @export
#'
#' @examples
#' \dontrun{
#'  get_team_schedule(team = "Duke", season = "2018-19")
#'}
#'
get_team_schedule <- function(team, season = "2020-21") {
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

  message <- paste0("Scraping ", season, " season for: ", team)
  usethis::ui_info(message)

  base_url <- "http://api.gamezone.stats.com/Basketball/Service.svc/league/cbk/eventsForTeam/"
  append <- "?season="
  url <- paste0(base_url, team_id, append, year)

  json <- try(jsonlite::fromJSON(url, flatten = T),
              silent = T)

  if ("try-error" %in% class(json)) {
    usethis::ui_oops(paste0("No ", season, " season available for: ", team))
    usethis::ui_info("Returning NULL")
    return(NULL)
  }

  schedule <- json %>%
    janitor::clean_names() %>%
    tidyr::separate(.data$date_time, into = c("start_time", "date"),
                    sep = " \\| ", remove = T) %>%
    dplyr::mutate(season = season,
                  date = as.Date(.data$date, format = "%B %d, %Y"),
                  tv = dplyr::na_if(.data$tv, y = ""),
                  home_total = dplyr::na_if(.data$home_total, y = ""),
                  away_total = dplyr::na_if(.data$away_total, y = ""),
                  dplyr::across(c(.data$event_id, .data$home_total, .data$away_total),
                                as.numeric),
                  home_record = paste0(.data$home_record_wins, "-", .data$home_record_losses),
                  away_record = paste0(.data$away_record_wins, "-", .data$away_record_losses)) %>%
    dplyr::select(.data$season, .data$start_time, game_date = .data$date,
                  game_id = .data$event_id, .data$tv,
                  home = .data$home_location, away = .data$away_location,
                  dplyr::starts_with("home_"), dplyr::starts_with("away_"),
                  -c(dplyr::contains("timeouts"), dplyr::ends_with("_wins"),
                     dplyr::ends_with("_losses"))) %>%
    dplyr::as_tibble()

  return(schedule)
}

