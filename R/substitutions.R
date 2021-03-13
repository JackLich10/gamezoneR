#' Parse GameZone substitutions given a play-by-play dataframe
#' @author Jack Lichtenstein
#' @param pbp play-by-play dataframe from \code{gamezone_cbb_pbp}
#' @export
#'
#' @examples
#' \dontrun{
#'  parse_substitutions(pbp = gamezone_cbb_pbp(game_id = 2316023))
#' }
#'
parse_substitutions <- function(pbp) {
  # find all substitutions
  substitutions <- pbp %>%
    dplyr::filter(.data$substitution == 1)

  # get the starters for each team
  starters <- substitutions %>%
    dplyr::filter(stringr::str_detect(.data$desc, "^Starting")) %>%
    dplyr::mutate(player = stringr::str_extract(.data$desc, "(?<=\\- ).*")) %>%
    dplyr::select(.data$play_id, .data$home, .data$away,
                  .data$event_team, .data$player) %>%
    dplyr::group_by(.data$event_team) %>%
    dplyr::mutate(player_num = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(home = ifelse(.data$home == .data$event_team, "home", "away")) %>%
    dplyr::arrange(desc(.data$home), .data$player_num) %>%
    dplyr::select(.data$play_id, .data$home, .data$player_num, .data$player)

  # get each row of substitutions in game
  subs <- substitutions %>%
    dplyr::filter(!stringr::str_detect(.data$desc, "^Starting")) %>%
    dplyr::mutate(player = stringr::str_remove(stringr::str_extract(.data$desc, "(?<=\\: ).*"),
                                        "\\.$"),
           home = ifelse(.data$home == .data$event_team, "home", "away")) %>%
    tidyr::separate(.data$player, into = c("sub", "out"),
                    sep = " in for ") %>%
    dplyr::select(.data$play_id, .data$home, .data$sub, .data$out)

  # error checking for no substitution data
  if(nrow(subs) == 0) {
    usethis::ui_oops("There are no marked substitutions in the Play-by-Play...")
    usethis::ui_info("Returning original Play-by-Play")
    return(pbp)
  }

  # function to recursively substitute players if possible
  # data is not complete because not all subs are tracked
  sub_players <- function(subs, curr_players) {
    # find players currently in the game
    in_game <- curr_players %>%
      dplyr::filter(.data$play_id == max(.data$play_id)) %>%
      dplyr::pull(.data$player)

    # perform substitution
    new_players <- subs %>%
      dplyr::slice(1) %>%
      dplyr::rename(home_curr = .data$home, play_id_curr = .data$play_id) %>%
      dplyr::bind_cols(curr_players %>%
                         dplyr::filter(.data$play_id == max(.data$play_id))) %>%
      # replace player if someone in the game is being replaced
      # and the substitute isn't already in the game
      dplyr::mutate(player = ifelse(.data$player == .data$out & .data$home == .data$home &
                                      !.data$sub %in% in_game,
                                    .data$sub, .data$player),
                    sub_error = dplyr::case_when(
                      .data$sub %in% in_game ~ "Sub already in game",
                      !.data$out %in% in_game ~ "Player leaving not previously in game",
                      T ~ NA_character_)) %>%
      dplyr::select(play_id = .data$play_id_curr, .data$home,
                    .data$player_num, .data$player, .data$sub_error)

    # bind together to create running df of players
    players <- dplyr::bind_rows(curr_players, new_players)

    # if this was the last substitution, return
    if (nrow(subs) == 1) {
      return(players)
    }
    # remove completed row
    subs <- subs %>%
      dplyr::slice(2:n())

    # recurse
    sub_players(subs, players)
  }

  # find all substitutions
  complete_subs <- sub_players(subs, curr_players = starters)

  # pivot to wide format for joining on pbp
  wide_players <- complete_subs %>%
    dplyr::transmute(.data$play_id, .data$sub_error, .data$player,
                     name = paste0(.data$home, "_", .data$player_num)) %>%
    tidyr::pivot_wider(names_from = .data$name,
                       values_from = .data$player)

  # join in lineups by play_id and fill NA's downward
  pbp_appended <- pbp %>%
    dplyr::left_join(wide_players, by = "play_id") %>%
    tidyr::fill(dplyr::starts_with("home_"), dplyr::starts_with("away_"),
                .direction = "down")

  return(pbp_appended)
}
