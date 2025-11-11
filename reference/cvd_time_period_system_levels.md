# List available time periods and associated system levels

Retrieves all available reporting periods from the CVDPREVENT API, along
with the NHS system levels included in each time period.

This function is useful to determine which system levels (e.g.,
national, region, ICB, PCN, practice) have data available for each
reporting period.

## Usage

``` r
cvd_time_period_system_levels()
```

## Value

A tibble containing time periods and the corresponding system levels
with the following columns:

A tibble with the following columns:

- EndDate:

  POSIXct. End date of the reporting period (e.g., "2023-12-31").

- StartDate:

  POSIXct. Start date of the reporting period. Typically set to a
  default baseline (e.g., "1900-01-01").

- TimePeriodID:

  Integer. Unique identifier for the time period.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To December
  2023", "Apr 2022 – Mar 2023").

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

  Character. Name of the system level (e.g., "England", "Region",
  "Sub-ICB", "PCN").

If no data is found, returns a tibble describing the error.

## Details

This function is helpful for understanding the data structure of each
reporting period, especially if you need to filter or subset data by
system level and time period in downstream API calls.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Time period system
levels](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#*Proposed*-%2FtimePeriod%2FsystemLevels)
for technical details.

## See also

[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md),
[`cvd_area_system_level_time_periods()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level_time_periods.md)

## Examples

``` r
# \donttest{
# Retrieve all time periods and associated system levels
periods_levels <- cvd_time_period_system_levels()

# Show available system levels for the latest time period
periods_levels |>
  dplyr::slice_max(order_by = TimePeriodID) |>
  dplyr::select(TimePeriodID, TimePeriodName, SystemLevelID, SystemLevelName)
#> # A tibble: 3 × 4
#>   TimePeriodID TimePeriodName      SystemLevelID SystemLevelName
#>          <int> <chr>                       <int> <chr>          
#> 1           27 Apr 2024 - Mar 2025             1 England        
#> 2           27 Apr 2024 - Mar 2025             6 Region         
#> 3           27 Apr 2024 - Mar 2025             7 ICB            
# }
```
