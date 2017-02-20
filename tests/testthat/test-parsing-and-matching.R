context("parsing-and-matching")

test_that("damage parsing (e.g., K to 1,000) works",
          {
            expected_damage <- c(150, 2000, 3500000000, 0)
            damage_crops <- c("150", "2K", "3.5B", NA)
            damage_crops <- parse_damage(damage_crops)
            expect_equal(expected_damage, damage_crops)
            }
          )

test_that("matching cz_names to county names to get FIPS for events listed by forecast zone",
          {
            expected_fips <- c(51800, # Suffolk, Virginia
                               54025, # Greenbrier, West Virginia
                                6111, # Ventura, California
                               24031, # Montgomery, Maryland
                               34009, # Cape May, New Jersey
                                6073, # San Diego, California
                               47009, # Blount, Tennessee
                               24037, # St. Mary's, Maryland
                               41037, # Lake, Oregon
                                6089, # Shasta, California
                               36047, # Kings, New York
                               42017, # Bucks, Pennsylvania
                               27137, # St. Louis, Minnesota
                               41015, # Curry, Oregon
                               32017, # Lincoln, Nevada
                                6089, # Shasta, California
                               12099, # Palm Beach, Florida
                                8013, # Larimer, Colorado
                                  NA  # Yellowstone National Park should not match anything
                               )

            counties_to_parse <- dplyr::data_frame(event_id = c(1:19),
                      cz_name = c("Suffolk", # Should match city in Virginia to a county FIPS
                                  "Eastern Greenbrier", # Should match with qualifier ("Eastern")
                                  "Ventura County Mountains", # Should match word before "County"
                                  "Central And Southeast Montgomery", # Should match with qualifiers ("Central and Southeast")
                                  "Western Cape May", # Should match with two-word county name and qualifier ("Western")
                                  "San Diego County Coastal Areas", # Should match two words before "County"
                                  "Blount/Smoky Mountains", # Should match before /
                                  "St. Mary's", # Should take out punctuation to match
                                  "Central & Eastern Lake County", # Should match word before "County"
                                  "Mountains Southwest Shasta County To Northern Lake County", # Should match word before "County"
                                  "Kings (Brooklyn)", # Should take out everything in parentheses before matching
                                  "Lower Bucks", # Should match with qualifier ("Lower")
                                  "Central St. Louis", # Should match with qualifier ("Central"), punctuation, and two-word county
                                  "Curry County Coast", # Should match word before "County"
                                  "Lincoln County Except The Sheep Range", # Should match word before "County"
                                  "Shasta Lake/North Shasta County", # Should match word before "County"
                                  "Coastal Palm Beach County", # # Should match two words before "County"
                                  "Larimer & Boulder Counties Between 6000 & 9000 Feet", # Should match first word
                                  "Yellowstone National Park"), # National park, so should not match (FIPS = NA)
                                  state = c("Virginia",
                                           "West Virginia",
                                           "California",
                                           "Maryland",
                                           "New Jersey",
                                           "California",
                                           "Tennessee",
                                           "Maryland",
                                           "Oregon",
                                           "California",
                                           "New York",
                                           "Pennsylvania",
                                           "Minnesota",
                                           "Oregon",
                                           "Nevada",
                                           "California",
                                           "Florida",
                                           "Colorado",
                                           "Wyoming"))

            matched_counties <- match_forecast_county(counties_to_parse)

            expect_equal(expected_fips, matched_counties$fips)
          }
)
