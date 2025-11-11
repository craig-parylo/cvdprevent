# List NHS areas for a given time period and parent or system level

Retrieves all NHS geographical areas (e.g., England, Region, ICB, PCN,
Practice) that have data avialable for a specified reporting period and
either a pareent area or system level. Only areas with data for the
chosen time period are returned.

You must specify at least one of `parent_area_id` or `system_level_id`.
If both are provided, `parent_area_id` takes precedence and
`system_level_id` is ignored.

- If `parent_area_id` is specified, returns all child areas of the
  specified parent area.

- If `system_level_id` is specified, returns all areas within that
  system level.

## Usage

``` r
cvd_area_list(time_period_id, parent_area_id = NULL, system_level_id = NULL)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period ID for which to return areas.
  Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- parent_area_id:

  Integer (optional). The AreaID for which all children will be
  returned. If provided, this takes precedence over `system_level_id`.

- system_level_id:

  Integer (optional). The system level ID for which to return all areas
  (e.g., Practice, PCN, ICB). Ignored if `parent_area_id` is specified.
  Use
  [`cvd_area_system_level()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level.md)
  to find valid IDs for a given time period.

## Value

A tibble containing area details for the specified criteria with the
following columns:

- AreaCode:

  Character. ONS code for the NHS area (e.g., "U91471").

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Inclusive Health PCN").

- AreaOdsCode:

  Character. ODS (Organisation Data Service) code for the area, if
  available. Often blank.

- ParticipationRate:

  Numeric. Percentage of practices or organisations participating in the
  CVDPREVENT program within the area.

- PopulationRate:

  Numeric. Percentage of the population covered by participating
  organisations in the area.

- SystemLevelID:

  Integer. Unique identifier for the system level (e.g., 4 = PCN).

- SystemLevelName:

  Character. Name of the system level (e.g., "PCN").

- Parents:

  Integer. ID of the parent organisation or grouping (e.g., ICB or
  region).

If no data is found, returns a tibble describing the error.

## Details

- At least one of `parent_area_id` or `system_level_id` must be
  supplied, otherwise an error is thrown.

- This function is commonly used to list all practices within a given
  PCN, all PCNs within an ICB, or all areas at a specific system level
  for a chosen time period.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Area
lists](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea)

## See also

[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md)

## Examples

``` r
# \donttest{
# List four PCNs (system level 4) with data available at time period 17
cvd_area_list(time_period_id = 17, system_level_id = 4) |>
  dplyr::select(SystemLevelName, AreaID, AreaCode, AreaName) |>
  dplyr::slice_head(n = 4)
#> # A tibble: 4 × 4
#>   SystemLevelName AreaID AreaCode AreaName       
#>   <chr>            <int> <chr>    <chr>          
#> 1 PCN               1103 U60176   3 Centres PCN  
#> 2 PCN               1103 U60176   3 Centres PCN  
#> 3 PCN                920 U72999   4 Doncaster PCN
#> 4 PCN                920 U72999   4 Doncaster PCN

# List all child areas of parent area 8037 (e.g., an ICB) in time period 17
cvd_area_list(time_period_id = 17, parent_area_id = 8037)
#> # A tibble: 3 × 9
#>   AreaCode  AreaID AreaName         AreaOdsCode ParticipationRate PopulationRate
#>   <chr>      <int> <chr>            <chr>       <lgl>             <lgl>         
#> 1 E38000051   7994 NHS Leicester, … 03W         NA                NA            
#> 2 E38000097   8009 NHS Leicester, … 04C         NA                NA            
#> 3 E38000201   8014 NHS Leicester, … 04V         NA                NA            
#> # ℹ 3 more variables: SystemLevelID <int>, SystemLevelName <chr>, Parents <int>

# Attempting to call without either optional argument will result in a
# tibble explaining the error.
# cvd_area_list(time_period_id = 17)
# }
```
