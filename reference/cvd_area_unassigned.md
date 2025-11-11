# List NHS areas without parent assignments for a given time period

Retrieves all NHS areas that have data in the specified reporting time
period but do not have any parent areas assigned. These "unassigned"
areas are unreachable via standard heirarchical navigation and may
represent data issues or exceptional cases (e.g., England as the
highest-level system).

## Usage

``` r
cvd_area_unassigned(time_period_id, system_level_id = NULL)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period (time period) for which to
  find unassigned areas. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to obtain valid IDs.

- system_level_id:

  Integer (optional). Restrict the search to areas at a specific system
  level (e.g., Practice, PCN, ICB). Use
  [`cvd_area_system_level()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level.md)
  to find valid IDs for a given time period.

## Value

A tibble containing details for all areas without parent assignments in
the selected time period (and system level, if specified). Typical
columns include:

- AreaCode:

  Character. Unique code for the NHS area (e.g., "L81117").

- AreaID:

  Integer. Unique identifier for the NHS area

- AreaName:

  Character. Name of the NHS area (e.g., "Pilning Surgery").

- OdsCode:

  Character. ODS (Organisation Data Service) code for the practice, if
  available. Often blank.

- SystemLevelID:

  Integer. Identifier for the system level (5 = GP practices).

- SystemLevelName:

  Character. Name of the system level (e.g., "Practice").

If no data is found, returns a tibble describing the error.

## Details

- Use this function to identify "orphaned" NHS areas or to understand
  top-level areas (e.g., England).

- If `system_level_id = 1` (England), expect the only result to be
  England, since it has no parent.

- This can help with data quality checks or to ensure all areas are
  accessible via parent/child navigation.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Areas
unassigned](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Funassigned)

## See also

[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md),
[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md)

## Examples

``` r
# \donttest{
# Report four GP practices (system level ID = 5) without parent PCN details for time period 17:
cvd_area_unassigned(time_period_id = 17, system_level_id = 5) |>
  dplyr::slice_head(n = 4) |>
  dplyr::select(SystemLevelName, AreaID, AreaName)
#> # A tibble: 4 × 3
#>   SystemLevelName AreaID AreaName                  
#>   <chr>            <int> <chr>                     
#> 1 Practice          6037 15 Sefton Road            
#> 2 Practice          4877 27@Northgate              
#> 3 Practice          1626 49 Marine Avenue Surgery  
#> 4 Practice          3765 Aldersbrook Medical Centre

# List unassigned top-level areas (system level ID = 1, England) for time period 17:
cvd_area_unassigned(time_period_id = 17, system_level_id = 1) |>
  dplyr::select(SystemLevelName, AreaID, AreaName)
#> # A tibble: 1 × 3
#>   SystemLevelName AreaID AreaName
#>   <chr>            <int> <chr>   
#> 1 England              1 England 
# }
```
