#' Clean storm dataset
#'
#' Cleans the storm dataset to prepare for further processing. This includes changing all
#' variable names to lowercase, removing some unneeded columns, and removing the narratives
#' if requested by the user.
#'
#' @inheritParams find_events
#' @inheritParams adjust_storm_data
#'
#' @return A cleaned version of the dataset input to the function.
#'
#' @importFrom dplyr %>%
clean_storm_data <- function(storm_data, include_narratives){
  storm_data <- storm_data %>%
    stats::setNames(tolower(names(.))) %>%
    dplyr::select_(~ begin_yearmonth, ~ begin_day, ~ end_yearmonth, ~ end_day,
                   ~ episode_id, ~ event_id, ~ state, ~ cz_type, ~ cz_name,
                   ~ event_type, ~ state_fips, ~ cz_fips, ~ source,
                   ~ injuries_direct, ~ injuries_indirect, ~ deaths_direct,
                   ~ deaths_indirect, ~ damage_property, ~ damage_crops,
                   ~ episode_narrative, ~ event_narrative) %>%
    dplyr::mutate_(state = ~ stringr::str_to_title(state),
                   cz_name = ~ stringr::str_to_title(cz_name),
                   damage_property = ~ parse_damage(damage_property),
                   damage_crops = ~ parse_damage(damage_crops))

  if(!include_narratives){
    storm_data <- storm_data %>%
      dplyr::select_(~ -event_narrative, ~ -episode_narrative)
  }

  return(storm_data)
}

#' Adjust storm data
#'
#' Adjusts storm data based on user selections on date range, distance limit to a storm, etc.
#'
#' @param storm_data A dataset of storm data. This dataset must include certain columns given
#'    in the NOAA Storm Events datasets for which this package was created.
#' @param event_types Character vector with the types of storm events that should be kept.
#'    The default value (NULL) keeps all types of events. See the "Details" vignette for this
#'    package for more details on possible event types.
#' @param dist_limit A numeric scalar with the distance (in kilometers) that a county
#'    must be from the storm's path to be included. The default (NULL) does not eliminate any
#'    events based on distance from a storm's path. This option should only be used when also
#'    specifying a storm with the \code{storm} parameter.
#' @inheritParams create_storm_data
#'
#' @importFrom dplyr %>%
adjust_storm_data <- function(storm_data, date_range = NULL,
                              event_types = NULL, dist_limit = NULL,
                              storm = NULL) {

  # Clean up storm events reported by forecast zone rather than county
  # (cz_type == "Z")
  storm_data_z <- storm_data %>%
    dplyr::filter_(~ cz_type == "Z") %>%
    match_forecast_county()

  storm_data_c <- storm_data %>%
    dplyr::filter_(~ cz_type == "C") %>%
    dplyr::mutate_(fips = ~ as.numeric(paste0(state_fips, sprintf("%03d", cz_fips))))

  storm_data <- dplyr::bind_rows(storm_data_c, storm_data_z) %>%
    tidyr::unite_("begin_date", c("begin_yearmonth", "begin_day"), sep = "-") %>%
    tidyr::unite_("end_date", c("end_yearmonth", "end_day"), sep = "-") %>%
    dplyr::mutate_(begin_date = ~ lubridate::ymd(begin_date),
                   end_date = ~ lubridate::ymd(end_date)) %>%
    dplyr::filter_(~ !is.na(fips)) %>%
    dplyr::arrange_(~ begin_date)

  # Pull closest distance data if the storm is provided
  if(!is.null(storm)){
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm) %>%
      dplyr::select_(~ -closest_time_utc, ~ -local_time) %>%
      dplyr::mutate_(closest_date = ~ lubridate::ymd(closest_date),
                     earliest_date = ~ closest_date - lubridate::ddays(2),
                     latest_date = ~ closest_date + lubridate::ddays(2),
                     fips = ~ as.numeric(fips))
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

  # If the `event_types`` is specified, filter by that
  if(!is.null(event_types)) {
    storm_data <- storm_data %>%
      dplyr::filter_(~ tolower(event_type) %in% stringr::str_to_lower(event_types))
  }

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
#'    \itemize{
#'      \item{\code{state}: }{State name, in lowercase}
#'      \item{\code{cz_name}: }{Location name, in lowercase}
#'      \item{\code{cz_fips}: }{Forecast zone FIPS}
#'    }
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
#'    with the \code{maps} package. The following steps are taken to try to
#'    match each \code{cz_name} to a state and county name from \code{county.fips}:
#'    \enumerate{
#'      \item Tries to match \code{cz_name} to the county name in \code{county.fips}
#'        after removing any periods or apostrophes in \code{cz_name}.
#'      \item Next, for county names with "county" in them, try to match the word before
#'         "county" to county name in \code{county.fips}. Then check the two words before
#'         "county", then the one and two words before "counties".
#'      \item Next, pull out the last word in \code{cz_name} and try to match it to the county
#'         name in \code{county.fips}. The check the last two words in \code{cz_name}, then check
#'         the last three words in \code{cz_name}.
#'      \item Next, pull any words right before a slash and check that against the county name.
#'      \item Finally, try removing anything in parentheses in \code{cz_name} before matching.
#'    }
#'
#' @note You may want to hand-check that event listings with names like "Lake", "Mountain", and
#'    "Park" have not been unintentionally linked to a county like "Lake County". While such
#'    examples seem rare in the example data used to develop this function (NOAA Storm Events
#'    for 2015), it can sometimes happen. To do so, you can use the \code{str_detect} function
#'    from the \code{stringr} package.
#'
#' @examples
#' counties_to_parse <- dplyr::data_frame(
#'                                    event_id = c(1:19),
#'                                    cz_name = c("Suffolk",
#'                                                "Eastern Greenbrier",
#'                                                "Ventura County Mountains",
#'                                                "Central And Southeast Montgomery",
#'                                                "Western Cape May",
#'                                                "San Diego County Coastal Areas",
#'                                                "Blount/Smoky Mountains",
#'                                                "St. Mary's",
#'                                                "Central & Eastern Lake County",
#'                                                "Mountains Southwest Shasta County To Northern Lake County",
#'                                                "Kings (Brooklyn)",
#'                                                "Lower Bucks",
#'                                                "Central St. Louis",
#'                                                "Curry County Coast",
#'                                                "Lincoln County Except The Sheep Range",
#'                                                "Shasta Lake/North Shasta County",
#'                                                "Coastal Palm Beach County",
#'                                                "Larimer & Boulder Counties Between 6000 & 9000 Feet",
#'                                                "Yellowstone National Park"),
#'                                    state = c("Virginia",
#'                                              "West Virginia",
#'                                              "California",
#'                                              "Maryland",
#'                                              "New Jersey",
#'                                              "California",
#'                                              "Tennessee",
#'                                              "Maryland",
#'                                              "Oregon",
#'                                              "California",
#'                                              "New York",
#'                                              "Pennsylvania",
#'                                              "Minnesota",
#'                                              "Oregon",
#'                                              "Nevada",
#'                                              "California",
#'                                              "Florida",
#'                                              "Colorado",
#'                                              "Wyoming"))
#' match_forecast_county(counties_to_parse)
#' @importFrom dplyr %>%
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
    dplyr::mutate_(fips = ~ ifelse(fips == 49049, NA, fips)) # Utah County, Utah is getting wrong matches

  storm_data_z <- storm_data_z %>%
    dplyr::left_join(matched_data, by = "event_id") %>%
    dplyr::mutate_(fips = ~ ifelse(stringr::str_detect(stringr::str_to_lower(cz_name),
                                                       "national park"),
                                   NA, fips)) # Park County, Wyoming is getting wrong matches. Likely, this may happen in other states, as well.

  return(storm_data_z)
}

#' Parse damage values
#'
#' Take damage values that include letters for order of magnitude (e.g., "2K" for
#' $2,000) and return a numeric value of damage.
#'
#' @param damage_vector A character vector with damage values (e.g., the \code{damage_crops}
#'    or \code{damage_property} columns in the NOAA Storm Events data). This vector should
#'    give numbers except for specific abbreviations specifying order of magnitude (see Details).
#'
#' @return The input vector, parsed to a numeric, with abbreviations for orders of magnitude
#'    appropriately interpreted (e.g., "2K" in the input vector becomes the numeric 2000 in the
#'    output vector).
#'
#' @details This function parses the following abbreviations for order of magnitude:
#'   \itemize{
#'     \item{"K":}{  1,000 (thousand)}
#'     \item{"M":}{  1,000,000 (million)}
#'     \item{"B":}{  1,000,000,000 (billion)}
#'     \item{"T":}{  1,000,000,000,000 (trillion)}
#'   }
#'
#' @examples
#' damage_crops <- c("150", "2K", "3.5B", NA)
#' parse_damage(damage_crops)
#'
#' @importFrom dplyr %>%
#'
#' @export
parse_damage <- function(damage_vector){
  value_table <- dplyr::data_frame(letter_damage = c(NA, "K", "M", "B", "T"),
                                   value_damage = 10 ^ c(0, 3, 6, 9, 12))

  out <- data_frame(damage_vector) %>%
    dplyr::mutate_(num_damage = ~ stringr::str_extract(damage_vector, "[0-9.]+"),
                   num_damage = ~ as.numeric(num_damage),
                   letter_damage = ~ stringr::str_extract(damage_vector, "[A-Z]+"),
                   letter_damage = ~ stringr::str_to_upper(letter_damage)) %>%
    dplyr::left_join(value_table, by = "letter_damage") %>%
    dplyr::mutate_(damage_vector = ~ num_damage * value_damage,
                   damage_vector = ~ ifelse(is.na(damage_vector), 0, damage_vector))

  return(out$damage_vector)
}
