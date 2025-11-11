# List all system levels and their available time periods

Retrieves all available NHS system levels from the CVDPREVENT API, along
with the reporting periods (time periods) in which each system level has
data available.

This function is the inverse of
[`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md),
allowing you to see, for each system level (e.g., National, Region, ICB,
PCN, Practice), the set of time periods for which data exists.

## Usage

``` r
cvd_area_system_level_time_periods()
```

## Value

A tibble with one row per system level and time period, with the
following columns:

- IsVisible:

  Logical or character. Indicates whether the time period is visible in
  the API or dashboard ("Y" or "N").

- NationalLevel:

  Logical or character. Indicates whether the data is available at the
  national level ("Y" or "N").

- SystemLevelID:

  Integer. Unique identifier for the system level (e.g., 1 = England, 4
  = PCN).

- SystemLevelName:

  Character. Name of the system level (e.g., "England", "ICB",
  "Sub-ICB", "STP").

- EndDate:

  POSIXct. End date of the reporting period (e.g., "2023-06-30").

- StartDate:

  POSIXct. Start date of the reporting period. Typically set to a
  default baseline (e.g., "1900-01-01").

- TimePeriodID:

  Integer. Unique identifier for the time period.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To June 2025",
  "Apr 2022 – Mar 2023").

If no data is found, returns a tibble describing the error.

## Details

Use this function to determine which reporting periods are available for
each NHS system level. This is useful for dynamically generating data
selections or validating user input in dashboards or scripts.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

[CVDPREVENT API documentation: All system levels and time
periods](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*%2Farea%2FsystemLevel%2FtimePeriods)
for details.

## See also

[`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md),
[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md)

## Examples

``` r
# \donttest{
# List the latest four reporting periods at GP practice level
cvd_area_system_level_time_periods() |>
  dplyr::filter(SystemLevelName == "Practice") |>
  dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
  dplyr::select(SystemLevelName, TimePeriodID, TimePeriodName)
#> # A tibble: 4 × 3
#>   SystemLevelName TimePeriodID TimePeriodName   
#>   <chr>                  <int> <chr>            
#> 1 Practice                  26 To June 2025     
#> 2 Practice                  24 To March 2025    
#> 3 Practice                  22 To December 2024 
#> 4 Practice                  20 To September 2024

# Explore all system levels and their available time periods
cvd_area_system_level_time_periods()
#> # A tibble: 130 × 8
#>    IsVisible NationalLevel SystemLevelID SystemLevelName EndDate       StartDate
#>    <chr>     <chr>                 <int> <chr>           <chr>         <chr>    
#>  1 Y         Y                         1 England         Tue, 31 Mar … Mon, 01 …
#>  2 Y         Y                         1 England         Wed, 31 Mar … Mon, 01 …
#>  3 Y         Y                         1 England         Thu, 30 Sep … Mon, 01 …
#>  4 Y         Y                         1 England         Thu, 31 Mar … Mon, 01 …
#>  5 Y         Y                         1 England         Thu, 30 Jun … Mon, 01 …
#>  6 Y         Y                         1 England         Fri, 30 Sep … Mon, 01 …
#>  7 Y         Y                         1 England         Sat, 31 Dec … Mon, 01 …
#>  8 Y         Y                         1 England         Fri, 31 Mar … Mon, 01 …
#>  9 Y         Y                         1 England         Fri, 30 Jun … Mon, 01 …
#> 10 Y         Y                         1 England         Sat, 30 Sep … Mon, 01 …
#> # ℹ 120 more rows
#> # ℹ 2 more variables: TimePeriodID <int>, TimePeriodName <chr>
# }
```
