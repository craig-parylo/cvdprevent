# Retrieve details for a specific NHS area and time period

Returns detailed information about a single NHS area for a given
reporting period, including its own details, as well as any parent and
child areas. This allows you to understand the heirarchical context
(e.g., parent ICB, child PCNs or Practices) for the specified area.

## Usage

``` r
cvd_area_details(time_period_id, area_id)
```

## Arguments

- time_period_id:

  Integer (required). The reporting period for which area details should
  be returned. Use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to find valid IDs.

- area_id:

  Integer (required). The AreaID to return details for. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

## Value

A named list with up to three tibbles:

- area_details:

  A tibble with details about the specified area.

- area_parent_details:

  A tibble with details about the parent area(s), if available.

- area_child_details:

  A tibble with details about child area(s), if available.

If no data is found, returns a tibble describing the error.

**area_details**, **area_parent_details** and **area_child_details**
typically contain the following columns:

- AreaCode:

  Character. ONS or internal code for the NHS area (e.g., "E54000015").

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Full name of the NHS area (e.g., "NHS Leicester,
  Leicestershire and Rutland Integrated Care Board").

- AreaOdsCode:

  Character. ODS (Organisation Data Service) code for the area (e.g.,
  "QK1").

- SystemLevelID:

  Integer. Unique identifier for the system level (e.g., 7 = ICB).

- SystemLevelName:

  Character. Name of the system level (e.g., "ICB").

## Details

This function is useful for navigating NHS area heirarchies, such as
finding all practices within a PCN, or determining the parent ICB for a
given area. The result is a list of tibbles, so you can extract and work
with each component separately.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Area
details](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2Fdetails)

## See also

[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md)

## Examples

``` r
# \donttest{
# Retrieve details for 'Leicester, Leicestershire and Rutland ICB' (area_id = 8037)
# in time period 17
returned_list <- cvd_area_details(time_period_id = 17, area_id = 8037)

# View details for the area
returned_list$area_details |> dplyr::select(AreaCode, AreaName)
#> # A tibble: 1 × 2
#>   AreaCode  AreaName                                                       
#>   <chr>     <chr>                                                          
#> 1 E54000015 NHS Leicester, Leicestershire and Rutland Integrated Care Board

# View details for the parent area(s)
returned_list$area_parent_details |> dplyr::select(AreaID, AreaName, SystemLevelID)
#> # A tibble: 1 × 3
#>   AreaID AreaName SystemLevelID
#>    <int> <chr>            <int>
#> 1   7922 Midlands             6

# View details for the child area(s)
returned_list$area_child_details |> dplyr::select(AreaID, AreaName, SystemLevelID)
#> # A tibble: 3 × 3
#>   AreaID AreaName                                            SystemLevelID
#>    <int> <chr>                                                       <int>
#> 1   7994 NHS Leicester, Leicestershire and Rutland ICB - 03W             8
#> 2   8009 NHS Leicester, Leicestershire and Rutland ICB - 04C             8
#> 3   8014 NHS Leicester, Leicestershire and Rutland ICB - 04V             8
# }
```
