# Search for NHS areas by partial name and time period

Searches for NHS areas whose names match a given partial string, within
a specified reporting time period. This function uses a case-insensitive
"LIKE" search (i.e., matches any area containing the search term) and
returns only areas for which data is available in the specified period.

## Usage

``` r
cvd_area_search(partial_area_name, time_period_id)
```

## Arguments

- partial_area_name:

  String (required). The substring to search for within area names
  (case-insensitive). This may be any part of an area name, e.g.,
  "practice", "PCN", or a specific place.

- time_period_id:

  Integer (required). The reporting period (time period) to restrict the
  search to areas with data. use
  [`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
  to obtain valid IDs.

## Value

A tibble containing details of areas matching the search term and having
data for the specified time period. Typical columns include:

- AreaCode:

  Character. Unique code for the NHS area (e.g., "P86619").

- AreaID:

  Integer. Unique identifier for the NHS area

- AreaName:

  Character. Name of the NHS area (e.g., "Dr Mb Ghafoor & Partners").

- IsVisible:

  Logical or character. Indicates whether the area is visible in the API
  or dashboard ("Y" or "N").

- NationalLevel:

  Logical or character. Indicates whether the area is included in
  national-level aggregations ("Y" or "N").

- OdsCode:

  Character. ODS (Organisation Data Service) code for the area, if
  available. Often blank.

- SystemLevelID:

  Integer. Identifier for the system level (5 = GP Practices).

- SystemLevelName:

  Character. Name of the system level (e.g., "Practice").

- SystemLevelOrder:

  Integer. Display order for the system level in dashboards or reports.

If no data is found, returns a tibble describing the error.

## Details

- The search is case-insensitive and matches anywhere in the area name.

- Only areas with available data in the chosen time period will be
  returned.

- Use this function to quickly locate AreaIDs or codes for use in other
  `cvdprevent` API calls.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Area
search](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2Fsearch)

## See also

[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md),
[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md)

## Examples

``` r
# \donttest{
# Search for areas containing "practice" in their name for time period 17
cvd_area_search(partial_area_name = "practice", time_period_id = 17) |>
  dplyr::select(AreaID, AreaName, AreaCode)
#> # A tibble: 1,447 × 3
#>    AreaID AreaName                             AreaCode
#>     <int> <chr>                                <chr>   
#>  1   7735 1/Monton Medical Practice            P87620  
#>  2   7788 1/SALFORD MEDICAL PRACTICE           P87004  
#>  3   6396 3/Springfield House Medical Practice P87024  
#>  4   6794 ABBEY GRANGE MEDICAL PRACTICE        B86068  
#>  5   4804 Abbey House Medical Practice         K83032  
#>  6   4814 Abbey Medical Practice               K83047  
#>  7   5262 Abbey Medical Practice               M81094  
#>  8   2327 Abbey Medical Practice               C83051  
#>  9   3607 Abbey Road Medical Practice          F84111  
#> 10   5927 Abercromby Family Practice           N82054  
#> # ℹ 1,437 more rows

# Search for areas containing "PCN" for time period 17
cvd_area_search(partial_area_name = "PCN", time_period_id = 17) |>
  dplyr::select(AreaID, AreaName, AreaCode)
#> # A tibble: 1,275 × 3
#>    AreaID AreaName                  AreaCode
#>     <int> <chr>                     <chr>   
#>  1   1103 3 Centres PCN             U60176  
#>  2    920 4 Doncaster PCN           U72999  
#>  3   1066 4PCN (Bnssg) PCN          U16600  
#>  4    923 A1 Network PCN            U89554  
#>  5    640 A31 Group PCN             U72748  
#>  6   1038 A34 West Berkshire PCN    U16983  
#>  7    471 Abbey Field PCN           U51488  
#>  8    222 Abbey Health PCN          U06079  
#>  9    174 Abc PCN                   U47228  
#> 10    889 Abingdon And District PCN U57321  
#> # ℹ 1,265 more rows
# }
```
