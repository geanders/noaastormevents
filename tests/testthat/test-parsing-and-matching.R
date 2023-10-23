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

            counties_to_parse <- dplyr::tibble(event_id = c(1:19),
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

            matched_counties <- old_match_forecast_county(counties_to_parse)

            expect_equal(expected_fips, matched_counties$fips)
          }
)

test_that("matching forecast zone codes to get county FIPS codes",
          {
            expected_fips <- c(8029, 8077, 8085, 8091, 8113, #Delta, Mesa, Mountrose, Ouray, San Miguel counties for UNCOMPAHGRE PLATEAU AND DALLAS DIVIDE
                               56001, 56007, #Albany, Carbon counties for NORTH SNOWY RANGE FOOTHILLS
                               30067, 30097, 30107, #Park, Sweet Grass, Wheatlans counties for CRAZY MOUNTAINS
                               32007, 32033, #Elko, White Pine for RUBY MOUNTAINS/E HUMBOLDT RANGE
                               41021, 41055, 41065, #Gilliam, Sherman, Wasco for EASTERN COLUMBIA RIVER GORGE
                               36081, #Queens for Southern Queens
                               8007, 8033, 8053, 8067, 8083, 8111, 8113, #Archuleta, Dolores, Hinsdale, La Plata, Montezuma, San Juan, San Miguel for SOUTHWESTERN SAN JUAN MOUNTAINS
                               45077, #Pickens for Pickens Mountains
                               51199, 51199, 51735, #York, City of Poquoson for York
                               49003, 49005, 49011, 49029, 49033, 49035, 49043, 49057, #Box Elder, Cache, Davis, Morgan, Rich, Salt Lake, Summit, Weber for Wasatch Mountains I-80 North
                               30049, 30099, #Lewis and Clark, Teton for Southern Rocky Mountain Front
                               16019, 16043, 16051, 16065, #Bonneville, Fremont, Jefferson, Madison for Upper Snake River Plain
                               35028, 35039, 35043, #Los Alamos, Rio Arriba, Sandoval for Jemez Mountains
                               6029, #Kern for Bakersfield
                               24023, #Garrett for Garrett
                               48303, #Lubbock for Lubbock
                               54007, #Braxton for Braxton
                               31013 #Box Butte for Box Butte
            )
            #"event_id", "state", "cz_name", "state_fips", "cz_fips"
            counties_to_parse <- dplyr::tibble(event_id = c(1:18),
                                               cz_name = c("UNCOMPAHGRE PLATEAU AND DALLAS DIVIDE",
                                                           "NORTH SNOWY RANGE FOOTHILLS",
                                                           "CRAZY MOUNTAINS",
                                                           "RUBY MOUNTAINS/E HUMBOLDT RANGE",
                                                           "EASTERN COLUMBIA RIVER GORGE",
                                                           "SOUTHERN QUEENS",
                                                           "SOUTHWESTERN SAN JUAN MOUNTAINS",
                                                           "PICKENS MOUNTAINS",
                                                           "YORK",
                                                           "WASATCH MOUNTAINS I-80 NORTH",
                                                           "SOUTHERN ROCKY MOUNTAIN FRONT",
                                                           "UPPER SNAKE RIVER PLAIN",
                                                           "JEMEZ MOUNTAINS",
                                                           "BAKERSFIELD",
                                                           "GARRETT",
                                                           "LUBBOCK",
                                                           "BRAXTON",
                                                           "BOX BUTTE"
                                                           ), # National park, so should not match (FIPS = NA)
                                               state = c("COLORADO",
                                                         "WYOMING",
                                                         "MONTANA",
                                                         "NEVADA",
                                                         "OREGON",
                                                         "NEW YORK",
                                                         "COLORADO",
                                                         "SOUTH CAROLINA",
                                                         "VIRGINIA",
                                                         "UTAH",
                                                         "MONTANA",
                                                         "IDAHO",
                                                         "NEW MEXICO",
                                                         "CALIFORNIA",
                                                         "MARYLAND",
                                                         "TEXAS",
                                                         "WEST VIRGINIA",
                                                         "NEBRASKA"
                                                         ),
                                               state_fips = c(8,
                                                              56,
                                                              30,
                                                              32,
                                                              41,
                                                              36,
                                                              8,
                                                              45,
                                                              51,
                                                              49,
                                                              30,
                                                              16,
                                                              35,
                                                              6,
                                                              24,
                                                              48,
                                                              54,
                                                              31
                                                              ),
                                               cz_fips = c(17,
                                                           110,
                                                           68,
                                                           34,
                                                           41,
                                                           178,
                                                           19,
                                                           2,
                                                           91,
                                                           7,
                                                           48,
                                                           20,
                                                           511,
                                                           314,
                                                           1,
                                                           35,
                                                           28,
                                                           3
                                                          )
                                               )

            matched_counties <- match_forecast_county(counties_to_parse)

            expect_equal(expected_fips, matched_counties$FIPS)
          }
)
