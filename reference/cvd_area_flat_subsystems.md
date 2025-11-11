# Retieve flat sub-systems for an NHS area, grouped by system level

Returns a "flat" list of the specified NHS area and all its immediate
child areas from the CVDPREVENT API, with child areas grouped by their
system level rather than by strict heirarchical nesting. This function
provides a convenient overview when you want to see all sub-areas
organised by level (e.g., all PCNs and all GP practices beneath an ICB)
without traversing the full heirarchy.

The output is a tibble where each row represents an area or sub-area,
and child areas are included as columns (with system level information).

## Usage

``` r
cvd_area_flat_subsystems(area_id)
```

## Arguments

- area_id:

  Integer (required). The AreaID for which to retrieve flat sub-system
  data. use
  [`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
  or
  [`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
  to find valid IDs.

## Value

A tibble containing details for the specified area and its child areas
and child area details (e.g., via `SubSystems_*` columns). Typical
columns include:

- AreaCode:

  Character. Code for the parent NHS area (e.g., "E54000038").

- AreaID:

  Integer. Unique identifier for the parent NHS area.

- AreaName:

  Character. Name of the parent NHS area (e.g., "Somerset").

- AreaOdsCode:

  Character. ODS (Organisation Data Service) code for the parent area
  (e.g., "QSL").

- ParentAreaID:

  Integer. Identifier for the higher-level parent area (e.g., regional
  or national grouping).

- SubSystems_AreaCode:

  Character. Code for the subsystem NHS area (e.g., practice or PCN).

- SubSystems_AreaID:

  Integer. Unique identifier for the subsystem NHS area.

- SubSystems_AreaName:

  Character. Name of the subsystem NHS area (e.g., "Church Street
  Surgery, Martock").

- SubSystems_AreaOdsCode:

  Character. ODS code for the subsystem area, if available. Often blank.

- SubSystems_ParentAreaID:

  Integer. ID of the immediate parent area for the subsystem (e.g., PCN
  or ICB).

- SubSystems_SystemLevelID:

  Integer. Identifier for the system level of the subsystem (e.g., 5 =
  Practice, 4 = PCN).

- SubSystems_SystemLevelName:

  Character. Name of the system level for the subsystem (e.g.,
  "Practice", "PCN").

- SystemLevelID:

  Integer. Identifier for the system level of the parent area (e.g., 2 =
  STP).

- SystemLevelName:

  Character. Name of the system level for the parent area (e.g., "STP").

If no data is found, returns a tibble describing the error.

## Details

This function is useful for quickly listing all areas beneath a parent,
grouped by system level, for reporting or selection purposes. For a
fully nested view, see
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md).

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Area flat
subsystems](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2Farea%2F%3Carea_id%3E%2FflatSubSystems)

## See also

[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md),
[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md),
[`cvd_area_unassigned()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_unassigned.md),
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md),
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md)

## Examples

``` r
# \donttest{
# View flat sub-systems for Somerset STP (area_id = 5)
cvd_area_flat_subsystems(area_id = 5) |> dplyr::glimpse()
#> Rows: 74
#> Columns: 14
#> $ AreaCode                   <chr> "E54000038", "E54000038", "E54000038", "E54…
#> $ AreaID                     <int> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5…
#> $ AreaName                   <chr> "Somerset", "Somerset", "Somerset", "Somers…
#> $ AreaOdsCode                <chr> "QSL", "QSL", "QSL", "QSL", "QSL", "QSL", "…
#> $ ParentAreaID               <int> 7670, 7670, 7670, 7670, 7670, 7670, 7670, 7…
#> $ SubSystems_AreaCode        <chr> "E38000150", "U47425", "U17153", "U84175", …
#> $ SubSystems_AreaID          <int> 105, 528, 613, 677, 688, 743, 786, 868, 963…
#> $ SubSystems_AreaName        <chr> "NHS Somerset CCG", "West Somerset PCN", "W…
#> $ SubSystems_AreaOdsCode     <chr> "11X", NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ SubSystems_ParentAreaID    <int> 5, 105, 105, 105, 105, 105, 105, 105, 105, …
#> $ SubSystems_SystemLevelID   <int> 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5…
#> $ SubSystems_SystemLevelName <chr> "CCG", "PCN", "PCN", "PCN", "PCN", "PCN", "…
#> $ SystemLevelID              <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
#> $ SystemLevelName            <chr> "STP", "STP", "STP", "STP", "STP", "STP", "…

# View flat sub-systems for Lincolnshire ICB (area_id = 8042)
cvd_area_flat_subsystems(area_id = 8042) |> dplyr::glimpse()
#> Rows: 94
#> Columns: 14
#> $ AreaCode                   <chr> "E54000013", "E54000013", "E54000013", "E54…
#> $ AreaID                     <int> 8042, 8042, 8042, 8042, 8042, 8042, 8042, 8…
#> $ AreaName                   <chr> "NHS Lincolnshire Integrated Care Board", "…
#> $ AreaOdsCode                <chr> "QJM", "QJM", "QJM", "QJM", "QJM", "QJM", "…
#> $ ParentAreaID               <int> 7922, 7922, 7922, 7922, 7922, 7922, 7922, 7…
#> $ SubSystems_AreaCode        <chr> "U38661", "U93726", "U56215", "U58435", "U1…
#> $ SubSystems_AreaID          <int> 159, 512, 515, 693, 736, 1007, 1090, 1124, …
#> $ SubSystems_AreaName        <chr> "Lincoln Health Partnership PCN", "South Li…
#> $ SubSystems_AreaOdsCode     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ SubSystems_ParentAreaID    <int> 8005, 8005, 8005, 8005, 8005, 8005, 8005, 8…
#> $ SubSystems_SystemLevelID   <int> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5…
#> $ SubSystems_SystemLevelName <chr> "PCN", "PCN", "PCN", "PCN", "PCN", "PCN", "…
#> $ SystemLevelID              <int> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7…
#> $ SystemLevelName            <chr> "ICB", "ICB", "ICB", "ICB", "ICB", "ICB", "…
# }
```
