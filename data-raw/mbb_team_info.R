# read in local dictionary
dictionary <- readRDS("~/Desktop/RStudio/gamezoneR/data/dictionary.rds")

# gather ESPN names, ids, colors, logos
long <- kenpomR::cbb_espn_teams() %>%
  janitor::clean_names() %>%
  dplyr::transmute(espn_id = id,
                   espn_name = location,
                   espn_pbp = nickname,
                   espn = short_display_name,
                   dplyr::across(c(color, alternate_color),
                                 ~ paste0("#", .)),
                   team_logo_espn = logos_href_1,
                   alt_team_logo_espn = logos_href_2) %>%
  tidyr::pivot_longer(cols = c(espn_name, espn_pbp, espn),
                      names_to = "name_type",
                      values_to = "team_name") %>%
  dplyr::distinct(espn_id, team_name, .keep_all = T) %>%
  dplyr::select(team_name, dplyr::everything(), -name_type)

# join together
cbb_team_info <- dictionary %>%
  dplyr::mutate(espn_pbp = dplyr::case_when(
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
    T ~ espn_pbp)) %>%
  dplyr::left_join(long,
                   by = c("espn_pbp" = "team_name")) %>%
  dplyr::as_tibble() %>%
  dplyr::select(espn_id, game_zone_id = team_id,
                team_name, game_zone:sref_name, conference,
                color_espn = color, alt_color_espn = alternate_color,
                team_logo_espn, alt_team_logo_espn,
                dplyr::everything()) %>%
  dplyr::arrange(game_zone_id)

mbb_team_info <- cbbdbR::cbb_team_info

# use the data
usethis::use_data(mbb_team_info, overwrite = T)

