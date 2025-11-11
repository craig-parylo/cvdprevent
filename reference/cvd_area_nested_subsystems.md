# Retrieve nested sub-systems for an NHS area

Returns a hierarchical (nested) structure of the specified NHS area and
all of its descendent (child) areas from the CVDPREVENT API. This
function is useful for exploring the parent-child relationships within
NHS geographies, such as seeing a PCN and all of its practices, or an
ICB with all subordinate structures.

The output is a list of tibbles, one for each "level" in the heirarchy,
named as `level_1`, `level_2`, etc. Each tibble contains the details for
the areas at that level.

## Usage

``` r
cvd_area_nested_subsystems(area_id)
```

## Arguments

- area_id:

  Integer (required). The AreaID for which to retrieve nested sub-system
  data. Use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

## Value

A named list of tibbles, where each element (`level_1`, `level_2`, etc.)
contains details for the specified area and each subsequent child level.
If no data is found, returns a tibble describing the error.

Each tibble contains the following columns:

- AreaCode:

  Character. Unique code for the NHS area (e.g., "U11103").

- AreaID:

  Integer. Unique identifier for the NHS area.

- AreaName:

  Character. Name of the NHS area (e.g., "Yeovil PCN").

- AreaOdsCode:

  Character. ODS (Organisation Data Service) code for the area, if
  available. Often blank.

- ParentAreaID:

  Integer. ID of the parent NHS area or organisation (e.g., ICB or
  region).

- SystemLevelID:

  Integer. Identifier for the system level (e.g., 4 = PCN).

- SystemLevelName:

  Character. Name of the system level (e.g., "PCN").

## Details

This function is helpful for visualising or programmatically traversing
the full nested structure beneath a given NHS area. For example, given
an ICB, you can see all PCNs, then all practices beneath those PCNs, and
so on.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Area nested
subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FnestedSubSystems)

## See also

[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md),
[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_flat_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_flat_subsystems.md)

## Examples

``` r
# \donttest{
# View the nested structure for Somerset STP (area_id = 5)
returned_list <- cvd_area_nested_subsystems(area_id = 5)
returned_list |> summary()
#>         Length Class  Mode
#> level_1 7      tbl_df list
#> level_2 7      tbl_df list
#> level_3 7      tbl_df list
#> level_4 7      tbl_df list

# See details for the first five immediate children of Somerset STP
returned_list$level_2 |> dplyr::slice_head(n = 5)
#> # A tibble: 1 × 7
#>   AreaCode  AreaID AreaName         AreaOdsCode ParentAreaID SystemLevelID
#>   <chr>      <int> <chr>            <chr>              <int>         <int>
#> 1 E38000150    105 NHS Somerset CCG 11X                    5             3
#> # ℹ 1 more variable: SystemLevelName <chr>

# View the nested structure for Leicester Central PCN (area_id = 701)
returned_list <- cvd_area_nested_subsystems(area_id = 701)
returned_list |> summary()
#>         Length Class  Mode
#> level_1 6      tbl_df list
#> level_2 7      tbl_df list

# See the GP practice children of the PCN
returned_list$level_2
#> # A tibble: 5 × 7
#>   AreaCode AreaID AreaName                AreaOdsCode ParentAreaID SystemLevelID
#>   <chr>     <int> <chr>                   <lgl>              <int>         <int>
#> 1 C82642     2278 Highfields Medical Cen… NA                   701             5
#> 2 C82643     2279 Community Health Centr… NA                   701             5
#> 3 Y02469     6621 Heron Gp Practice       NA                   701             5
#> 4 Y02686     6653 Bowling Green Street S… NA                   701             5
#> 5 C82080     7323 SHEFA MEDICAL PRACTICE  NA                   701             5
#> # ℹ 1 more variable: SystemLevelName <chr>
# }
```
