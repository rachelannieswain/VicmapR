
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VicmapR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/JustinCally/VicmapR/branch/master/graph/badge.svg)](https://codecov.io/gh/JustinCally/VicmapR?branch=master)
<!-- badges: end -->

The goal of VicmapR is to provide functions to easily access Victorin
Government spatial data through their WFS (Web Feature Service). The
package is currently in an early development stage.

## Installation

You can install the the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JustinCally/VicmapR")
```

## Example

### Searching for data

``` r
library(VicmapR)
#> 
#> Attaching package: 'VicmapR'
#> The following object is masked from 'package:stats':
#> 
#>     filter

listLayers(pattern = stringr::regex("trees", ignore_case = T))
#>                                Name
#> 1 datavic:WATER_ISC2010_LARGE_TREES
#>                                                           Title
#> 1 2010 Index of Stream Condition - Large Trees polygon features
```

### Reading in data

As of VicmapR version `0.1.0` data is read in using a lazy evaluation
method with the convenience of pipe operators (`%>%`). A lot of the
methods and code have already been written for a similar package
([bcdata](https://github.com/bcgov/bcdata)) that downloads data from the
British Columbia WFS catalogue. Using a similar approach to
[bcdata](https://github.com/bcgov/bcdata), VicmapR allows users to
construct a WFS query in a step-wise format. In doing so a query is
reserved until `collect()` is used on the `vicmap_promise`. The example
below shows an extensive example of how the to easily read in spatial
data:

``` r
# Read in an example shape to restrict our query to using geometric filtering
melbourne <- sf::st_read(system.file("shapes/melbourne.geojson", package="VicmapR"), quiet = T)

# Obtain a promise of what data will be returned for a given layer
vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN")
#> * Using collect() on this object will return 187445 features and 16
#> * fields
#> * At most six rows of the record are printed here
#> -------------------------------------------------------------------------------------------------------
#> Simple feature collection with 6 features and 15 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: 142.7675 ymin: -35.06905 xmax: 143.324 ymax: -35.04559
#> geographic CRS: GDA94
#> # A tibble: 6 x 16
#>   id       PFI    UFI FEATURE_TYPE_CO~ NAME  NAMED_FEATURE_ID ORIGIN
#>   <chr>  <int>  <int> <chr>            <chr> <chr>            <chr> 
#> 1 VMHY~ 8.55e6 2.55e6 watercourse_cha~ <NA>  <NA>             2     
#> 2 VMHY~ 8.55e6 2.55e6 watercourse_cha~ <NA>  <NA>             2     
#> 3 VMHY~ 8.55e6 2.55e6 watercourse_cha~ <NA>  <NA>             2     
#> 4 VMHY~ 8.55e6 2.55e6 watercourse_cha~ <NA>  <NA>             2     
#> 5 VMHY~ 8.55e6 2.55e6 watercourse_cha~ <NA>  <NA>             2     
#> 6 VMHY~ 8.55e6 2.55e6 watercourse_cha~ <NA>  <NA>             2     
#> # ... with 9 more variables: CONSTRUCTION <chr>, USAGE <chr>, HIERARCHY <chr>,
#> #   FEATURE_QUALITY_ID <int>, CREATE_DATE_PFI <dttm>, SUPERCEDED_PFI <chr>,
#> #   CREATE_DATE_UFI <dttm>, OBJECTID <int>, geometry <LINESTRING [°]>

# Build a more specific query and collect the results
vicmap_query(layer = "datavic:VMHYDRO_WATERCOURSE_DRAIN") %>% # layer to query
  filter(HIERARCHY == "L") %>% # simple filter for a column
  filter(INTERSECTS(melbourne)) %>% # more advanced geometric filter
  select(HIERARCHY, PFI) %>% 
  collect()
#> Warning: The object is too large to perform exact spatial operations using VicmapR.
#>              To simplify the polygon, sf::st_simplify() was used to reduce the size of the query
#> Simple feature collection with 8 features and 5 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: 144.909 ymin: -37.81511 xmax: 144.9442 ymax: -37.78198
#> geographic CRS: GDA94
#> # A tibble: 8 x 6
#>   id               PFI    UFI HIERARCHY OBJECTID                        geometry
#>   <chr>          <int>  <int> <chr>        <int>                <LINESTRING [°]>
#> 1 VMHYDRO_WAT~  1.46e7 3.63e7 L          1605003 (144.9365 -37.81511, 144.9359 ~
#> 2 VMHYDRO_WAT~  1.46e7 3.63e7 L          1582117 (144.929 -37.81409, 144.9294 -~
#> 3 VMHYDRO_WAT~  1.46e7 3.63e7 L          1582120 (144.9288 -37.81417, 144.9292 ~
#> 4 VMHYDRO_WAT~  1.46e7 4.90e7 L          2432411 (144.9403 -37.78253, 144.9401 ~
#> 5 VMHYDRO_WAT~  1.75e7 4.90e7 L          2432413 (144.9415 -37.78232, 144.9414 ~
#> 6 VMHYDRO_WAT~  1.46e7 4.90e7 L          2432415 (144.9442 -37.78198, 144.9441 ~
#> 7 VMHYDRO_WAT~  1.93e7 5.44e7 L          2698790 (144.9287 -37.8033, 144.9186 -~
#> 8 VMHYDRO_WAT~  1.46e7 5.44e7 L          2698805 (144.9201 -37.79069, 144.9202 ~
```
