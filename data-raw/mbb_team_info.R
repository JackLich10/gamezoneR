### Make team information dictionary

# Gather ESPN names, ids, colors, logos
espn_ids <- hoopR::espn_mbb_teams() %>%
  janitor::clean_names() %>%
  dplyr::transmute(espn_id = as.numeric(team_id),
                   espn_name = team,
                   espn_pbp = nickname,
                   espn = short_name,
                   dplyr::across(c(color, alternate_color),
                                 ~ ifelse(. == "NA", NA, paste0("#", .))),
                   team_logo_espn = logo,
                   alt_team_logo_espn = logo_dark)
# tidyr::pivot_longer(cols = c(espn_name, espn_pbp, espn),
#                     names_to = "name_type",
#                     values_to = "team_name") %>%
# dplyr::distinct(espn_id, team_name, .keep_all = TRUE) %>%
# dplyr::select(team_name, dplyr::everything(), -name_type)

# Dictionary of names from many sites
name_dictionary <- ncaahoopR::dict %>%
  janitor::clean_names() %>%
  cbbdbR::join_cbb_team_info(join_name = "ncaa") %>%
  dplyr::rename(conference = conference.x) %>%
  dplyr::select(-c(conference.y, team_name, organization,
                   dplyr::contains("team_logo_espn"), dplyr::contains("color_espn"))) %>%
  dplyr::mutate(espn_id = as.numeric(espn_id))

espn_ids %>%
  filter(stringr::str_detect(espn_name, "Centenary"))

# Game Zone IDs
gamezone_ids <- gamezoneR::mbb_team_info %>%
  dplyr::distinct(game_zone_id, game_zone) %>%
  dplyr::bind_rows(dplyr::tibble(game_zone_id = 1730,
                                 game_zone = "St. Thomas (MN)")) %>%
  dplyr::arrange(game_zone_id)

# Join together
dictionary <- name_dictionary %>%
  dplyr::mutate(
    game_zone_id = dplyr::case_when(
      espn == "St. Thomas - Minnesota" ~ 1730,
      TRUE ~ game_zone_id
    ),
    espn_id = dplyr::case_when(
      espn == "Bethune-Cookman" ~ 2065,
      espn == "Birmingham-Southern" ~ 3,
      espn == "Centenary" ~ 2113,
      espn == "LIU" ~ 112358,
      espn == "St. Thomas - Minnesota" ~ 2900,
      espn == "MD-E Shore" ~ 2379,
      TRUE ~ espn_id),
    espn_pbp = dplyr::case_when(
      espn_pbp == "CS Fullerton" ~ "CSU Fullerton",
      espn_pbp == "Mcneese St" ~ "McNeese",
      espn_pbp == "Ul Monroe" ~ "UL Monroe",
      espn_pbp == "Mt. St. Mary'S" ~ "Mt. St. Mary's",
      espn_pbp == "Georgia St" ~ "Georgia State",
      espn_pbp == "Arkansas St" ~ "Arkansas State",
      espn_pbp == "Massachusetts" ~ "UMass",
      espn_pbp == "UT San Antonio" ~ "UTSA",
      espn_pbp == "Arkansas-Little Rock" ~ "Little Rock",
      espn_pbp == "Connecticut" ~ "UConn",
      espn_pbp == "Presbyterian College" ~ "Presbyterian",
      TRUE ~ espn_pbp)) %>%
  dplyr::left_join(espn_ids %>%
                     dplyr::select(-c(espn_name, espn_pbp, espn)),
                   by = c("espn_id")) %>%
  dplyr::left_join(gamezone_ids,
                   by = c("game_zone_id"))

# Most common names across sources
most_common_names <- dictionary %>%
  dplyr::filter(!is.na(espn_id)) %>%
  tidyr::pivot_longer(cols = c(ncaa:sref_name, game_zone),
                      names_to = "name_type",
                      values_to = "team_name",
                      values_drop_na = TRUE) %>%
  dplyr::group_by(espn_id, game_zone_id) %>%
  dplyr::summarise(team_name = collapse::fmode(team_name),
                   .groups = "drop")

# Make final dictionary
mbb_team_info <- dictionary %>%
  dplyr::left_join(most_common_names, by = c("espn_id", "game_zone_id")) %>%
  dplyr::select(espn_id, game_zone_id, team_name, game_zone,
                ncaa:sref_name, conference,
                color_espn = color, alt_color_espn = alternate_color,
                team_logo_espn, alt_team_logo_espn,
                dplyr::everything()) %>%
  dplyr::arrange(game_zone_id)

mbb_team_info %>% colnames()

# Use the data
usethis::use_data(mbb_team_info, overwrite = TRUE)

