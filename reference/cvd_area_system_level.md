# List system levels available for a specific time period

Retrieves all available NHS system levels (e.g., National, Region, ICB,
PCN, Practice) for a specified reporting time period from the CVDPREVENT
API.

This function helps users determine which system levels are available
for data extraction in a given reporting period.

## Usage

``` r
cvd_area_system_level(time_period_id)
```

## Arguments

- time_period_id:

  Integer (required). The ID of the reporting time period for which
  system levels should be returned. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

## Value

A tibble containing system level details for the specified time period,
with the following columns:

- IsVisible:

  Logical or character. Indicates whether the system level is visible in
  the API or dashboard ("Y" or "N").

- NationalLevel:

  Logical or character. Indicates whether the system level represents
  national coverage ("Y" or "N").

- SystemLevelID:

  Integer. Unique identifier for the system level (e.g., 1 = England, 4
  = PCN).

- SystemLevelName:

  Character. Name of the system level (e.g., "England", "Region", "ICB",
  "Practice").

- SystemLevelOrder:

  Integer. Display order for the system level in dashboards or reports.

If no data is found, returns a tibble describing the error.

## Details

This function is useful in workflows where you need to filter or subset
NHS areas or data by both time period and system level. It is often used
as a precursor to more detailed area or indicator queries.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: System levels per time
period](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2FsystemLevel)

## See also

[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md),
[`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)

## Examples

``` r
# \donttest{
# List all system levels available for time period 4 (activity to March 2022)
cvd_area_system_level(time_period_id = 4) |>
  dplyr::select(SystemLevelID, SystemLevelName)
#> # A tibble: 6 × 2
#>   SystemLevelID SystemLevelName
#>           <int> <chr>          
#> 1             1 England        
#> 2             6 Region         
#> 3             2 STP            
#> 4             3 CCG            
#> 5             4 PCN            
#> 6             5 Practice       

# Find valid time period IDs, then get system levels for the latest one
latest_period <-
  cvd_time_period_list() |>
  dplyr::pull(TimePeriodID) |>
  max()

cvd_area_system_level(time_period_id = latest_period) |>
  dplyr::select(SystemLevelID, SystemLevelName)
#> # A tibble: 3 × 2
#>   SystemLevelID SystemLevelName
#>           <int> <chr>          
#> 1             1 England        
#> 2             6 Region         
#> 3             7 ICB            
# }
```
