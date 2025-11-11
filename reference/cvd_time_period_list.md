# List available time periods for CVD indicators

Retrieves all available reporting periods from the CVDPREVENT API.
Optionally, you can filter periods by a specific indicator type (e.g.,
standard, outcome) using the `indicator_type_id` parameter.

## Usage

``` r
cvd_time_period_list(indicator_type_id = NULL)
```

## Arguments

- indicator_type_id:

  Optional integer. If provided, restricts the returned time periods to
  those containing data of the given indicator type.

## Value

A tibble containing details of available time periods with the following
columns:

- EndDate:

  POSIXct. End date of the reporting period (e.g., "2025-06-30").

- IndicatorTypeID:

  Integer. Unique identifier for the indicator type (e.g., 1 = Standard,
  2 = Outcomes).

- IndicatorTypeName:

  Character. Descriptive name of the indicator type (e.g., "Standard",
  "Outcomes").

- StartDate:

  POSIXct. Start date of the reporting period. Typically set to a
  default baseline (e.g., "1900-01-01").

- TimePeriodID:

  Integer. Unique identifier for the time period.

- TimePeriodName:

  Character. Display label for the time period (e.g., "To June 2025",
  "Apr 2024 – Mar 2025").

If no data is found, returns a tibble describing the error.

## Details

This function is often used to determine valid values for time period
parameters in other API queries. It is a building block for most
higher-level data retrieval functions in this package.

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: Time
period](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FtimePeriod)
for endpoint details.

## See also

[`cvd_indicator_types()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_types.md),
[`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)

## Examples

``` r
# NB, the following examples are not tested because they take longer than
# expected to return the results

# List all available time periods
cvd_time_period_list()
#> # A tibble: 27 × 6
#>    EndDate              IndicatorTypeID IndicatorTypeName StartDate TimePeriodID
#>    <chr>                          <int> <chr>             <chr>            <int>
#>  1 Mon, 30 Jun 2025 00…               1 Standard          Mon, 01 …           26
#>  2 Mon, 30 Jun 2025 00…               2 Outcomes          Mon, 01 …           27
#>  3 Mon, 31 Mar 2025 00…               2 Outcomes          Mon, 01 …           25
#>  4 Mon, 31 Mar 2025 00…               1 Standard          Mon, 01 …           24
#>  5 Tue, 31 Dec 2024 00…               2 Outcomes          Mon, 01 …           23
#>  6 Tue, 31 Dec 2024 00…               1 Standard          Mon, 01 …           22
#>  7 Mon, 30 Sep 2024 00…               1 Standard          Mon, 01 …           20
#>  8 Mon, 30 Sep 2024 00…               2 Outcomes          Mon, 01 …           21
#>  9 Sun, 30 Jun 2024 00…               1 Standard          Mon, 01 …           18
#> 10 Sun, 30 Jun 2024 00…               2 Outcomes          Mon, 01 …           19
#> # ℹ 17 more rows
#> # ℹ 1 more variable: TimePeriodName <chr>

# List time periods with data for a specific indicator type (e.g., Standard)
cvd_time_period_list(indicator_type_id = 1)
#> # A tibble: 17 × 6
#>    EndDate              IndicatorTypeID IndicatorTypeName StartDate TimePeriodID
#>    <chr>                          <int> <chr>             <chr>            <int>
#>  1 Mon, 30 Jun 2025 00…               1 Standard          Mon, 01 …           26
#>  2 Mon, 31 Mar 2025 00…               1 Standard          Mon, 01 …           24
#>  3 Tue, 31 Dec 2024 00…               1 Standard          Mon, 01 …           22
#>  4 Mon, 30 Sep 2024 00…               1 Standard          Mon, 01 …           20
#>  5 Sun, 30 Jun 2024 00…               1 Standard          Mon, 01 …           18
#>  6 Sun, 31 Mar 2024 00…               1 Standard          Mon, 01 …           17
#>  7 Sun, 31 Dec 2023 00…               1 Standard          Mon, 01 …           15
#>  8 Sat, 30 Sep 2023 00…               1 Standard          Mon, 01 …           10
#>  9 Fri, 30 Jun 2023 00…               1 Standard          Mon, 01 …            9
#> 10 Fri, 31 Mar 2023 00…               1 Standard          Mon, 01 …            8
#> 11 Sat, 31 Dec 2022 00…               1 Standard          Mon, 01 …            7
#> 12 Fri, 30 Sep 2022 00…               1 Standard          Mon, 01 …            6
#> 13 Thu, 30 Jun 2022 00…               1 Standard          Mon, 01 …            5
#> 14 Thu, 31 Mar 2022 00…               1 Standard          Mon, 01 …            4
#> 15 Thu, 30 Sep 2021 00…               1 Standard          Mon, 01 …            3
#> 16 Wed, 31 Mar 2021 00…               1 Standard          Mon, 01 …            2
#> 17 Tue, 31 Mar 2020 00…               1 Standard          Mon, 01 …            1
#> # ℹ 1 more variable: TimePeriodName <chr>
```
