
<!-- [![Build Status](https://travis-ci.org/geanders/noaastormevents.svg?branch=master)](https://travis-ci.org/zailchen/noaastormevents) -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Description of package

This package can be used to explore and map data from [NOAA’s Storm
Events Database](https://www.ncdc.noaa.gov/stormevents/). This storm
event database is maintained by NOAA’s National Centers for
Environmental Information and aims to provide information, including
estimates of damage and human health impacts, for severe storm events
that affect the U.S. It has aggregated storm event listings for
tornadoes since the 1950s and for a broad range of event types (e.g.,
snow storms, heat waves, droughts, wildfires, floods) since 1996. This
database has been used either alone or in conjunction with other data
for a number of scientific studies. It is available for downloading at
<https://www.ncdc.noaa.gov/stormevents/ftp.jsp>, with three files (one
with event details, one with fatality details, and with with location
details) available per year.

While the online database does not have a structured API, this package
uses regular expressions to search the listings of available files to
identify the filename for a queried year and download that year’s data
to a user’s R session. The package functions then filter the downloaded
storm event listings based to the dates, locations, event types, and
other search limitations specified by the user. In particular, this
package can be used to identify storm event listings that were close in
location and time to Atlantic basin tropical storm tracks.

The `noaastormevents` package includes options that allow users to find
events based on proximity to a tropical storm. To use this
functionality, the user must have the `hurricaneexposuredata` package,
available from a drat repository, installed locally. This package can be
installed by running:

``` r
library(drat)
addRepo("geanders")
install.packages("hurricaneexposuredata")
```

For more details on this package and how it can be used, please see its
online documentation at: <https://github.com/geanders/noaastormevents>.
