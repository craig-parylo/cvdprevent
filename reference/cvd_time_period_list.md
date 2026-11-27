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
if (FALSE) cvd_time_period_list() # \dontrun{}

# List time periods with data for a specific indicator type (e.g., Standard)
if (FALSE) cvd_time_period_list(indicator_type_id = 1) # \dontrun{}
```
