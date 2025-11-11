# Retrieve available indicator types

Returns a tibble of indicator type IDs and their corresponding names,
used to categorise CVD indicators in the CVDPREVENT API. This function
is primarily a helper for
[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md),
which accepts `indicator_type_id` as an optional parameter.

## Usage

``` r
cvd_indicator_types()
```

## Value

A tibble of indicator types with the following columns:

- IndicatorTypeID:

  Integer. Unique identifier for the indicator type.

- IndicatorTypeName:

  Character. Name of the indicator type (e.g., "Standard", "Outcomes").

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## See also

[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)

## Examples

``` r
# NB, the following example is not tested because it takes longer than
# expected to return the results

# List available indicator types
cvd_indicator_types()
```
