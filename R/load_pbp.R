
#' Load cleaned gamezoneR play-by-play from the data repository
#' @author Jack Lichtenstein
#' @param seasons A vector of years (formatted like 2020-21) associated with men's college basketball seasons
#' @export
#'
#' @examples
#' \dontrun{
#' future::plan("multisession")
#' load_gamezone_pbp(c("2017-18", "2018-19", "2019-20", "2020-21"))
#' }
#'
load_gamezone_pbp <- function(seasons) {
  avail_seasons <- available_seasons()

  last_season <- most_recent_season()

  if (!all(seasons %in% avail_seasons)) {
    usethis::ui_stop("Please pass valid seasons between 2017-18 and {last_season}. Ensure the format of the season is correct.")
  }

  if (length(seasons) > 1 && is_sequential()) {
    usethis::ui_info(c(
      "It is recommended to use parallel processing when trying to load multiple seasons.",
      "Please consider running {usethis::ui_code('future::plan(\"multisession\")')}!",
      "Will go on sequentially..."
    ))
  }

  p <- progressr::progressor(along = seasons)

  out <- furrr::future_map_dfr(seasons, single_season, p = p)

  return(out)
}

single_season <- function(season, p) {

  season <- stringr::str_remove(season, "-")

  .url <- paste0("https://raw.githubusercontent.com/JackLich10/gamezoneR-data/main/data/play_by_play/rds/pbp_", season, ".rds")
  con <- url(.url)
  pbp <- readRDS(con)
  close(con)

  p(sprintf("season=%s", season))
  return(pbp)
}

