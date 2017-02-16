#' Adjust storm data
#'
#' This function adjusts a set of storm data based on user selections on
#' date range, distance limit to a storm, etc.
#'
#' @param storm_data A dataset of storm data.
#' @param event_type Specifies the types of storm events should be included.
#' @param dist_limit If selected, the distance (in kilometers) that a county
#'    must be from the storm's path to be included.
#' @inheritParams create_storm_data
#'
#' @importFrom dplyr %>%
adjust_storm_data <- function(storm_data, date_range = NULL,
                              event_type = NULL, dist_limit = NULL,
                              storm = NULL) {

  utils::data(county.regions, package = "choroplethrMaps")
  county.regions <- county.regions %>%
    tidyr::unite_("state_county_name", c("state.name", "county.name"), sep = " ")

  # Clean up storm events reported by forecast zone rather than county
  # (cz_type == "Z")
  storm_data <- dplyr::tbl_df(storm_data)
  storm_data_z <- storm_data %>%
    dplyr::filter_(~ cz_type == "Z") %>%
    match_forecast_county()

  storm_data_c <- storm_data %>%
    dplyr::filter_(~ cz_type == "C") %>%
    dplyr::mutate_(fips = ~ as.numeric(paste0(state_fips, sprintf("%03d", cz_fips))))

  storm_data <- dplyr::bind_rows(storm_data_c, storm_data_z) %>%
    dplyr::rename_(type = ~ event_type) %>%
    dplyr::mutate_(begin_day = ~ sprintf("%02d", begin_day),
                   end_day =  ~ sprintf("%02d", end_day)) %>%
    tidyr::unite_("begin_date", c("begin_yearmonth", "begin_day"), sep = "") %>%
    tidyr::unite_("end_date", c("end_yearmonth", "end_day"), sep = "") %>%
    dplyr::mutate_(begin_date = ~ lubridate::ymd(begin_date),
                   end_date = ~ lubridate::ymd(end_date))%>%
    dplyr::filter_(~ !is.na(fips)) %>%
    dplyr::arrange_(~ begin_date)

  # Pull closest distance data if the storm is provided
  if(!is.null(storm)){
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm) %>%
      dplyr::select_(~ -closest_time_utc, ~ -local_time) %>%
      dplyr::mutate_(closest_date = ~ lubridate::ymd(closest_date),
                     earliest_date = ~ closest_date - lubridate::ddays(2),
                     latest_date = ~ closest_date + lubridate::ddays(2))
    storm_data <- storm_data %>%
      dplyr::left_join(distance_df, by = "fips")
  }

  # If a date range is included, filter only on that for date
  if(!is.null(date_range)){
    storm_data <- storm_data %>%
      dplyr::filter_(~ !is.na(begin_date) &
                      begin_date %within% lubridate::interval(date_range[1],
                                                              date_range[2]))
  } else if (!is.null(storm)){ ## Otherwise, if possible use the storm dates from "closest_dates" to pick dates
   storm_data <- storm_data %>%
     dplyr::filter_(~ !is.na(begin_date)) %>%
     dplyr::filter_(~ earliest_date <= begin_date &
                      begin_date <= latest_date)
  }

  # If a distance limit is specified, filter by that
  if(!is.null(dist_limit)){
    if(is.null(storm)){
      stop("To use `dist_limit`, `storm` must be specified.")
    }
    storm_data <- storm_data %>%
      dplyr::filter_(~ storm_dist <= dist_limit)
  }

  # Clean up some extras put in `storm_data` to filter by time and location
  if(!is.null(storm)){
    storm_data <- storm_data %>%
      dplyr::select_(~ -storm_dist, ~ -closest_date, ~ -earliest_date,
                     ~ -latest_date)
  }

  # If the event_type is specified, filter by that
  if(!is.null(event_type)) {
    storm_data <- suppressWarnings(dplyr::filter_(storm_data,
                                                  ~ tolower(type) ==
                                                    tolower(event_type)))
  }

  storm_data <- storm_data %>%
    dplyr::mutate_(state = ~ stringr::str_to_title(state))


  return(storm_data)
}

#' Match events by forecast zone to county
#'
#' For events reported by forecast zone, use regular expressions to match
#' as many as possible to counties.
#'
#' @param storm_data_z A dataframe of storm events reported by forecast zone
#'    (i.e., \code{cz_type == "Z"}) rather than county. This dataframe should
#'    include the columns:
#'    \begin{itemize}
#'      \item{\code{state}:}{State name, in lowercase}
#'      \item{\code{cz_name}:}{Location name, in lowercase}
#'      \item{\code{cz_fips}:}{Forecast zone FIPS}
#'    \end{itemize}
#'
#' @return The dataframe of events input to the function, with county FIPS
#'    added for events matched to a county in the \code{fips} column. Events
#'    that could not be matched are kept in the dataframe, but the \code{fips}
#'    code is set to \code{NA}.
#'
#' @note This function does not provide any matches for events outside
#'    of the continental U.S.
#'
#' @details This function tries to match the \code{cz_name} of each event to
#'    a state and county name from the \code{county.fips} dataframe that comes
#'    with the \code{maps} package.
#'
#' @export
match_forecast_county <- function(storm_data_z){
  # Use the data `county.fips` to get county FIPS codes
  utils::data(county.fips, package = "maps")
  county.fips <- county.fips %>%
    tidyr::separate_("polyname", c("state", "cz_name"), sep = ",") %>%
    dplyr::mutate_(cz_name = ~ stringr::str_replace(cz_name, ":.+", "")) %>%
    dplyr::distinct_()

  small_data <- storm_data_z %>%
    dplyr::tbl_df() %>%
    dplyr::select_("event_id", "state", "cz_name") %>%
    dplyr::filter_(~ !(state %in% c("GULF OF MEXICO", "GUAM", "ATLANTIC NORTH",
                                    "LAKE HURON", "LAKE ST CLAIR", "AMERICAN SAMOA",
                                    "LAKE SUPERIOR", "ATLANTIC SOUTH", "LAKE MICHIGAN",
                                    "HAWAII WATERS", "PUERTO RICO", "E PACIFIC", "LAKE ERIE",
                                    "LAKE ONTARIO", "VIRGIN ISLANDS", "HAWAII", "ALASKA"))) %>%
    dplyr::mutate_(state = ~ stringr::str_to_lower(state),
                   cz_name = ~ stringr::str_to_lower(cz_name),
                   cz_name = ~stringr::str_replace_all(cz_name, "[.'``]", ""))

  # First, try to match `cz_name` to county name in `county.fips`
  a <- small_data %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::mutate_(fips = ~ ifelse(state == "district of columbia", 11001, fips),
                   fips = ~ ifelse(state == "virginia" & cz_name == "chesapeake", 51550, fips),
                   fips = ~ ifelse(state == "alabama" & cz_name == "dekalb", 1049, fips)) %>%
    dplyr::filter_(~ !is.na(fips)) %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% a$event_id))

  # Next, for county names with 'county' in them, try to match the word before 'county'
  # to county name in `county.fips`. Then check the two words before 'county', then the
  # one and two words before 'counties'
  b <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "([a-z]*+)\\s(county|cnty)")[ , 2]) %>%
    dplyr::filter_(~ !is.na(cz_name)) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% b$event_id))

  c <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "([a-z]*+\\s[a-z]*+)\\scounty")[ , 2]) %>%
    dplyr::filter_(~ !is.na(cz_name)) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% c$event_id))

  d <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "([a-z]*+)\\scounties")[ , 2]) %>%
    dplyr::filter_(~ !is.na(cz_name)) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% d$event_id))

  e <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "([a-z]*+\\s[a-z]*+)\\scounties")[ , 2]) %>%
    dplyr::filter_(~ !is.na(cz_name)) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% e$event_id))

  # Next, pull out the last word in `cz_name` and try to match it to the county name
  # in `county.fips`. The check the last two words in `cz_name`.
  f <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "[a-z]*+$")[ , 1]) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% f$event_id))

  g <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "[a-z]*+\\s[a-z]*+$")[ , 1]) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% g$event_id))

  h <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "[a-z]*+\\s[a-z]*+\\s[a-z]*+$")[ , 1]) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% h$event_id))

  # Next, pull any words right before a slash and check that against the county name.
  # Then try removing anything in parentheses in `cz_name` before matching.
  i <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_match(cz_name, "^([a-z]*+)/")[ , 2]) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% i$event_id))

  j <- small_data %>%
    dplyr::mutate_(cz_name = ~ stringr::str_replace(cz_name, "\\s\\(.+", "")) %>%
    dplyr::left_join(county.fips, by = c("state", "cz_name")) %>%
    dplyr::filter_(~ !is.na(fips))  %>%
    dplyr::select_("event_id", "fips")
  small_data <- dplyr::filter_(small_data, ~ !(event_id %in% j$event_id))

  matched_data <- dplyr::bind_rows(a, b, c, d, e, f, g, h, i, j) %>%
    mutate(fips = ifelse(fips == 49049, NA, fips)) # Utah County, Utah is getting wrong matches
  storm_data_z <- storm_data_z %>%
    dplyr::left_join(matched_data, by = "event_id")

  return(storm_data_z)
}
