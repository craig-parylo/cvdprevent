# Retrieve metadata for external resources linked to CVDPREVENT

Returns a tibble containing metadata for all external resources
referenced by the CVDPREVENT programme. These resources may include
clinical guidelines, research papers, policy documents, and third-party
datasets used to support indicator definitions and reporting.

## Usage

``` r
cvd_external_resource()
```

## Value

A tibble where each row represents an external resource referenced by
CVDPREVENT. Columns include:

- ExternalResourceCategory:

  Character. Thematic category of the resource (e.g., "Toolkits",
  "Resources for patients").

- ExternalResourceID:

  Integer. Unique identifier for the resource.

- ExternalResourceOrder:

  Integer. Display order for the resource within its category.

- ExternalResourceSource:

  Character. Organisation or publisher of the resource (e.g., "NHS
  England", "UCLPartners").

- ExternalResourceTitle:

  Character. Title of the resource (e.g., "Cardiovascular Disease
  Prevention Data Packs").

- ExternalResourceType:

  Character. Type of resource (e.g., "website", "document").

- ExternalResourceURL:

  Character. Direct URL to the resource.

- Tags:

  List-column of data frames. Each entry contains one or more indicator
  tags associated with the resource, including:

  IndicatorTagID

  :   Integer. Unique identifier for the tag.

  IndicatorTagName

  :   Character. Descriptive name of the tag (e.g., "prevention",
      "digital tools").

  May be empty or contain NA if no tags are assigned.

If the request fails, a tibble describing the error is returned instead.

## Details

Each resource is categorised and includes source information, title, and
descriptive metadata. This function is useful for:

- Auditing external references used in CVDPREVENT indicators

- Linking indicators to supporting evidence or policy

- Building documentation or dashboards that reference external sources

## Note

This function may take longer than 5 seconds to complete due to API
response time.

## API Documentation

See the [CVDPREVENT API documentation: External
resources](https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#%2FexternalResource)

## See also

[`cvd_data_availability()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_data_availability.md)
for checking data coverage across indicators

## Examples

``` r
# \donttest{
# Show the first five external resources grouped by category
cvd_external_resource() |>
  dplyr::filter(ExternalResourceID < 10) |>
  dplyr::select(ExternalResourceCategory, ExternalResourceSource, ExternalResourceTitle) |>
  dplyr::group_by(ExternalResourceCategory)
#> # A tibble: 5 × 3
#> # Groups:   ExternalResourceCategory [3]
#>   ExternalResourceCategory ExternalResourceSource          ExternalResourceTitle
#>   <chr>                    <chr>                           <chr>                
#> 1 Data Packs               Public Health England           Cardiovascular Disea…
#> 2 Data Packs               NHS England                     Equality and Health …
#> 3 Toolkits                 NHS Digital                     Weight Management Pr…
#> 4 Toolkits                 Primary Care Cardiovascular So… CVD – identification…
#> 5 Stratification tools     UCL Partners                    Search and risk stra…
# }
```
