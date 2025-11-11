# Using CVD Prevent

``` r
# avoid warnings about the different heading
options(rmarkdown.html_vignette.check_title = FALSE)

# load the package library
library(cvdprevent)
```

## Time periods

### Listing available time periods

The CVD Prevent audit publishes data approximately four times per year.
To view the four most recent reporting periods for **standard
indicators**, use the
[`cvd_time_period_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_list.md)
function:

``` r
cvd_time_period_list() |>
  dplyr::filter(IndicatorTypeName == 'Standard') |>
  dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
  dplyr::select(TimePeriodID, TimePeriodName) |>
  gt::gt()
```

| TimePeriodID | TimePeriodName    |
|--------------|-------------------|
| 26           | To June 2025      |
| 24           | To March 2025     |
| 22           | To December 2024  |
| 20           | To September 2024 |

There are two main types of indicator reported: - “Standard”
(IndicatorTypeID = 1)

- “Outcomes” (IndicatorTypeID = 2)

To retrieve time periods specific to an indicator type, pass the
relevant ID to the optional `indicator_type_id` parameter:

``` r
cvd_time_period_list(indicator_type_id = 2) |>
  dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
  dplyr::select(
    IndicatorTypeID,
    IndicatorTypeName,
    TimePeriodID,
    TimePeriodName
  ) |>
  gt::gt()
```

| IndicatorTypeID | IndicatorTypeName | TimePeriodID | TimePeriodName      |
|-----------------|-------------------|--------------|---------------------|
| 2               | Outcomes          | 27           | Apr 2024 - Mar 2025 |
| 2               | Outcomes          | 25           | Jan 2024 - Dec 2024 |
| 2               | Outcomes          | 23           | Oct 2023 - Sep 2024 |
| 2               | Outcomes          | 21           | Jul 2023 - Jun 2024 |

### Listing system levels by time period

Audit data is reported across multiple geographic levels (referred to as
**System Levels**), ranging from individual GP practices to
national-level aggregates (e.g., England). To view which system levels
are available for each time period, use the
[`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)
function:

``` r
cvd_time_period_system_levels() |>
  dplyr::slice_max(order_by = TimePeriodID) |>
  dplyr::select(TimePeriodID, TimePeriodName, SystemLevelID, SystemLevelName) |>
  gt::gt()
```

| TimePeriodID | TimePeriodName      | SystemLevelID | SystemLevelName |
|--------------|---------------------|---------------|-----------------|
| 27           | Apr 2024 - Mar 2025 | 1             | England         |
| 27           | Apr 2024 - Mar 2025 | 6             | Region          |
| 27           | Apr 2024 - Mar 2025 | 7             | ICB             |

## Areas

### List available system levels for a given time period

To find out which system levels are reported for a specific time period
you can use the
[`cvd_area_system_level()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level.md)
function, providing the required time_period_id, (NB, time_period_id
defaults to the first time period if not supplied).

``` r
cvd_area_system_level(time_period_id = 17) |>
  dplyr::select(SystemLevelID, SystemLevelName) |>
  gt::gt()
```

| SystemLevelID | SystemLevelName |
|---------------|-----------------|
| 1             | England         |
| 6             | Region          |
| 7             | ICB             |
| 8             | Sub-ICB         |
| 4             | PCN             |
| 5             | Practice        |

### List all available reporting periods for each system level

Use the
[`cvd_area_system_level_time_periods()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_system_level_time_periods.md)
function to show a list of all reporting periods for each system level.
Here we show the latest four reporting periods at the GP practice system
level.

``` r
cvd_area_system_level_time_periods() |>
  dplyr::filter(SystemLevelName == 'Practice') |>
  dplyr::slice_max(order_by = TimePeriodID, n = 4) |>
  dplyr::select(SystemLevelID, SystemLevelName, TimePeriodID, TimePeriodName) |>
  gt::gt()
```

| SystemLevelID | SystemLevelName | TimePeriodID | TimePeriodName    |
|---------------|-----------------|--------------|-------------------|
| 5             | Practice        | 26           | To June 2025      |
| 5             | Practice        | 24           | To March 2025     |
| 5             | Practice        | 22           | To December 2024  |
| 5             | Practice        | 20           | To September 2024 |

### List areas for a time period and system level or parent area

To list four of the Primary Care Networks (PCN) (SystemLevelID = 4) for
which data is available at time period 17 use the
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md)
function.

``` r
cvd_area_list(time_period_id = 17, system_level_id = 4) |>
  dplyr::select(SystemLevelName, AreaID, AreaCode, AreaName) |>
  dplyr::slice_head(n = 4) |>
  gt::gt()
```

| SystemLevelName | AreaID | AreaCode | AreaName        |
|-----------------|--------|----------|-----------------|
| PCN             | 1103   | U60176   | 3 Centres PCN   |
| PCN             | 1103   | U60176   | 3 Centres PCN   |
| PCN             | 920    | U72999   | 4 Doncaster PCN |
| PCN             | 920    | U72999   | 4 Doncaster PCN |

Either parent area or system level **must** be specified.

### View details for a specific area

To view details for a specific area use
[`cvd_area_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_details.md)
with the required parameters of ‘time_period_id’ and ‘area_id’. The
return from this function is a list of three named tibbles:

- `area_details`,

- `area_child_details` (where appropriate), and

- `area_parent_details` (where appropriate).

``` r
# get the list from the function
returned_list <- cvd_area_details(time_period_id = 17, area_id = 7922)
```

The tibbles first need extracting from the returned list object, done
here using the `list$object` notation. To view area details:

``` r
returned_list$area_details |>
  dplyr::select(AreaCode, AreaName, SystemLevelID) |>
  gt::gt()
```

| AreaCode  | AreaName | SystemLevelID |
|-----------|----------|---------------|
| E40000011 | Midlands | 6             |

View details for the parent of this area:

``` r
returned_list$area_parent_details |>
  dplyr::select(AreaID, AreaName, SystemLevelID) |>
  gt::gt()
```

| AreaID | AreaName | SystemLevelID |
|--------|----------|---------------|
| 1      | England  | 1             |

View details for the children of this area:

``` r
returned_list$area_child_details |>
  dplyr::select(AreaID, AreaName, SystemLevelID) |>
  gt::gt()
```

| AreaID | AreaName                                                        | SystemLevelID |
|--------|-----------------------------------------------------------------|---------------|
| 8032   | NHS Herefordshire and Worcestershire Integrated Care Board      | 7             |
| 8033   | NHS Staffordshire and Stoke-on-Trent Integrated Care Board      | 7             |
| 8037   | NHS Leicester, Leicestershire and Rutland Integrated Care Board | 7             |
| 8039   | NHS Shropshire, Telford and Wrekin Integrated Care Board        | 7             |
| 8042   | NHS Lincolnshire Integrated Care Board                          | 7             |
| 8043   | NHS Nottingham and Nottinghamshire Integrated Care Board        | 7             |
| 8044   | NHS Coventry and Warwickshire Integrated Care Board             | 7             |
| 8045   | NHS Northamptonshire Integrated Care Board                      | 7             |
| 8052   | NHS Birmingham and Solihull Integrated Care Board               | 7             |
| 8053   | NHS Derby and Derbyshire Integrated Care Board                  | 7             |
| 8056   | NHS Black Country Integrated Care Board                         | 7             |

### List areas without parent details

Some areas do *not* have parent details but *do* have data reported in a
given period, which may mean they are missed when searching for areas.
This function provides a convenient way of accessing these details for a
given ‘time_period_id’ and ‘system_level_id’.

Here we report four GP practices without any parent PCN:

``` r
cvd_area_unassigned(time_period_id = 17, system_level_id = 5) |>
  dplyr::slice_head(n = 4) |>
  dplyr::select(SystemLevelName, AreaID, AreaName) |>
  gt::gt()
```

| SystemLevelName | AreaID | AreaName                   |
|-----------------|--------|----------------------------|
| Practice        | 6037   | 15 Sefton Road             |
| Practice        | 4877   | 27@Northgate               |
| Practice        | 1626   | 49 Marine Avenue Surgery   |
| Practice        | 3765   | Aldersbrook Medical Centre |

The top system_level (England) does not have a parent either:

``` r
cvd_area_unassigned(time_period_id = 17, system_level_id = 1) |>
  dplyr::select(SystemLevelName, AreaID, AreaName) |>
  gt::gt()
```

| SystemLevelName | AreaID | AreaName |
|-----------------|--------|----------|
| England         | 1      | England  |

### Search for area by keyword

To find details for an area where you don’t know its ID number you can
perform a partial name search using
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md)
and specify a time period to search.

``` r
cvd_area_search(partial_area_name = 'foo', time_period_id = 17) |>
  dplyr::select(AreaID, AreaName, AreaCode) |>
  gt::gt()
```

| AreaID | AreaName                   | AreaCode |
|--------|----------------------------|----------|
| 7327   | HILLFOOT HEALTH            | N82116   |
| 1991   | Hillfoot Surgery           | B86011   |
| 6155   | Waterfoot Medical Practice | P81132   |

### Area details

#### Nested

To retrieve details for a specific area, including all nested system
areas within it, you can use the function
[`cvd_area_nested_subsystems()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_nested_subsystems.md).
This function requires an `area_id` as input.

In the example below, we request details for Greater Manchester ICB
(`area_id = 8030`), which returns a named list of tibbles representing
heirarchical levels:

``` r
return_list <- cvd_area_nested_subsystems(area_id = 8030)
return_list |> summary()
#>         Length Class  Mode
#> level_1 7      tbl_df list
#> level_2 7      tbl_df list
#> level_3 7      tbl_df list
#> level_4 7      tbl_df list
```

The tibble `level_1` provides details about our requested area itself.
you can view it using:

``` r
return_list$level_1 |>
  gt::gt()
```

| AreaCode  | AreaID | AreaName                                     | AreaOdsCode | ParentAreaID | SystemLevelID | SystemLevelName |
|-----------|--------|----------------------------------------------|-------------|--------------|---------------|-----------------|
| E54000057 | 8030   | NHS Greater Manchester Integrated Care Board | QOP         | 8152         | 7             | ICB             |

The next level, `level_2`, contains areas that are direct children of
the requested area. Here, we display hte first four entries:

``` r
return_list$level_2 |>
  dplyr::slice_head(n = 4) |>
  gt::gt()
```

| AreaCode  | AreaID | AreaName          | AreaOdsCode | ParentAreaID | SystemLevelID | SystemLevelName |
|-----------|--------|-------------------|-------------|--------------|---------------|-----------------|
| E38000174 | 58     | NHS Stockport CCG | 01W         | 8030         | 3             | CCG             |
| E38000135 | 60     | NHS Oldham CCG    | 00Y         | 8030         | 3             | CCG             |
| E38000187 | 71     | NHS Trafford CCG  | 02A         | 8030         | 3             | CCG             |
| E38000143 | 73     | NHS Salford CCG   | 01G         | 8030         | 3             | CCG             |

Subsequent levels (`level_3`, `level_4`, etc.) represent further nested
areas - children of children - and can be explored in the same way.

#### Flat

Alternatively, you can request a flat output grouped on system level
using the `cvd_area_flat_subsystem()` function:

``` r
cvd_area_flat_subsystems(area_id = 5) |>
  dplyr::glimpse()
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
```

## Indicators

An ***Indicator*** represents a cardiovascular disease health indicator
as defined by NHS England. An example of an Indicator is CVDP001AF,
‘Prevalence of GP recorded atrial fibrillation in patients aged 18 and
over’. Indicators have unique Indicator IDs. Each indicator is further
broken down into *Metrics*.

A ***Metric*** represents a further breakdown of an indicator by
inequality markers. An example of an inequality marker is ‘Age Group -
Male, 40-59’. Metrics have unique Metric IDs with each representing a
combination of *Indicator* and *Metric Category*.

A ***Metric Category*** describes the inequality markers which the
*Metric* applies to. Each *Metric Category* has a unique ID for each
combination of Name and *Metric Category Type*.

A *Metric Category* belongs to a ***Metric Category Type*** which groups
the *Metric Categories* into one entity. Each *Metric Category Type* has
a unique ID.

For example, ‘Male - 40-59’ is a *Metric Category* in the ‘Age Group’
*Metric Category Type*. Age Group will also contain categories for
‘18-39’, ‘40-59’, ‘60-79’, ‘80+’. Assigning *Metric Categories* to
*Metric Category Type* allows comparison of all metrics in one
inequality marker (in this case Age Group), or displaying *Metric
Categories* in the same *Metric Category Type* alongside each other.

### Listing indicators

To get a list of all available indicators for a given time period and
system level you can use the
[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
function and specify which time period and system level you are
interested in.

Here we access the first four indicators for time point 17 and GP
practice level (system level 5).

``` r
cvd_indicator_list(time_period_id = 17, system_level_id = 5) |>
  dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
  dplyr::slice_head(n = 4) |>
  gt::gt()
```

| IndicatorID | IndicatorCode | IndicatorShortName                                                       |
|-------------|---------------|--------------------------------------------------------------------------|
| 2           | CVDP002HYP    | Hypertension: Treated to appropriate threshold (age \< 80) (CVDP002HYP)  |
| 3           | CVDP003HYP    | Hypertension: Treated to appropriate threshold (age \>= 80) (CVDP003HYP) |
| 4           | CVDP004HYP    | Hypertension: BP monitoring (CVDP004HYP)                                 |
| 7           | CVDP002AF     | AF: Treated with anticoagulants (CVDP002AF)                              |

### Listing metrics for each indicator

To list all metrics for each indicator you can use the
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
function and specify which time period and system level you are
interested in.

Here we access the metrics for the prevalence of atrial fibrillation
(Indicator ID 1) and focussing on just those metrics available for the
40-59 years age group:

``` r
cvd_indicator_metric_list(time_period_id = 17, system_level_id = 1) |>
  dplyr::filter(IndicatorID == 1, MetricCategoryName == '40-59') |>
  dplyr::count(
    IndicatorID,
    IndicatorShortName,
    MetricID,
    MetricCategoryName,
    CategoryAttribute
  ) |>
  dplyr::select(-n) |>
  gt::gt()
```

| IndicatorID | IndicatorShortName         | MetricID | MetricCategoryName | CategoryAttribute |
|-------------|----------------------------|----------|--------------------|-------------------|
| 1           | AF: Prevalence (CVDP001AF) | 173      | 40-59              | Female            |
| 1           | AF: Prevalence (CVDP001AF) | 175      | 40-59              | Male              |
| 1           | AF: Prevalence (CVDP001AF) | 187      | 40-59              | Persons           |

### List all indicator data for a given area

To access all indicator data for a given area and time period you can
use the
[`cvd_indicator()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator.md)
function. The return from the API includes four sets of data which are
grouped together as four tibbles within a named list object. The four
tibbles include:

- indicators,

- metric categories,

- metric data,

- time-series data,

Here we look at ‘3 Centres PCN’ (area ID 1103) for time period 17:

``` r
returned_list <- cvd_indicator(time_period_id = 17, area_id = 1103)
returned_list |> summary()
#>                   Length Class  Mode
#> indicators        14     tbl_df list
#> metric_categories  7     tbl_df list
#> metric_data       19     tbl_df list
#> timeseries_data    7     tbl_df list
```

#### Indicators

We can extract the tibbles using the list\$object notation. To
illustrate, we obtain the first four indicators from the
`return_list$indicators` list item:

``` r
returned_list$indicators |>
  dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
  dplyr::arrange(IndicatorID) |>
  dplyr::slice_head(n = 4) |>
  gt::gt()
```

| IndicatorID | IndicatorCode | IndicatorShortName                                                       |
|-------------|---------------|--------------------------------------------------------------------------|
| 2           | CVDP002HYP    | Hypertension: Treated to appropriate threshold (age \< 80) (CVDP002HYP)  |
| 3           | CVDP003HYP    | Hypertension: Treated to appropriate threshold (age \>= 80) (CVDP003HYP) |
| 4           | CVDP004HYP    | Hypertension: BP monitoring (CVDP004HYP)                                 |
| 7           | CVDP002AF     | AF: Treated with anticoagulants (CVDP002AF)                              |

#### Category

Here we access category details for:

- indicator 7: (AF: treatment with anticoagulants)

- metric categories 7 & 8 (people aged 40-59 years by gender)

``` r
returned_list$metric_categories |>
  dplyr::filter(IndicatorID == 7, MetricCategoryID %in% c(7, 8)) |>
  dplyr::select(
    IndicatorID,
    MetricCategoryTypeName,
    CategoryAttribute,
    MetricCategoryName,
    MetricID
  ) |>
  gt::gt()
```

| IndicatorID | MetricCategoryTypeName | CategoryAttribute | MetricCategoryName | MetricID |
|-------------|------------------------|-------------------|--------------------|----------|
| 7           | Age group              | Male              | 40-59              | 126      |
| 7           | Age group              | Female            | 40-59              | 132      |

#### Category data

Here we access the data for each of the above categories:

``` r
returned_list$metric_data |>
  dplyr::filter(MetricID %in% c(126, 132)) |>
  dplyr::select(MetricID, Value, Numerator, Denominator) |>
  gt::gt()
```

| MetricID | Value | Numerator | Denominator |
|----------|-------|-----------|-------------|
| 126      | 100   | 15        | 15          |
| 132      | 100   | 10        | 10          |

#### Time series

A set of time-series data area also supplied for returned metrics, which
provide results for all available time periods. This data is accessible
from the `timeseries_data` object.

Here we show the time-series for the two categories of metric shown
above as a plot.

``` r
returned_list$timeseries_data |>
  dplyr::filter(MetricID %in% c(126, 132), !is.na(Value)) |>
  # prepare for plotting
  dplyr::mutate(
    EndDate = as.Date(EndDate, format = '%a, %d %b %Y 00:00:00 GMT'),
    MetricID = MetricID |> as.factor()
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = EndDate,
    y = Value,
    group = MetricID,
    colour = MetricID
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line()
```

![](using_cvdprevent_files/figure-html/time%20series-1.png)

#### Tags

Indicators are searchable by one or more *Tag*, supplied by the optional
argument tag_id. Tag ID can be a single ID number or a vector or IDs,
shown here as a vector of IDs 12 and 13 (hypertension and blood pressure
measures).

``` r
returned_list <-
  cvd_indicator(time_period_id = 17, area_id = 3, tag_id = c(3, 4))
```

``` r
if (!is.null(returned_list$indicators)) {
  returned_list$indicators |>
    dplyr::select(IndicatorID, IndicatorCode, IndicatorShortName) |>
    dplyr::arrange(IndicatorID) |>
    dplyr::slice_head(n = 4) |>
    gt::gt()
}
```

| IndicatorID | IndicatorCode | IndicatorShortName                                                       |
|-------------|---------------|--------------------------------------------------------------------------|
| 2           | CVDP002HYP    | Hypertension: Treated to appropriate threshold (age \< 80) (CVDP002HYP)  |
| 3           | CVDP003HYP    | Hypertension: Treated to appropriate threshold (age \>= 80) (CVDP003HYP) |
| 4           | CVDP004HYP    | Hypertension: BP monitoring (CVDP004HYP)                                 |
| 11          | CVDP001HYP    | Hypertension: Prevalence (CVDP001HYP)                                    |

See also [List indicator tags](#list-indicator-tags).

### List indicator tags

Indicators have one or more tags that can be used to filter related
indicators. To get a list of these tags you can use the
[`cvd_indicator_tags()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_tags.md)
function

``` r
cvd_indicator_tags() |>
  dplyr::arrange(IndicatorTagID) |>
  dplyr::slice_head(n = 5) |>
  gt::gt()
```

| IndicatorTagID | IndicatorTagName        |
|----------------|-------------------------|
| 1              | prevalence              |
| 2              | atrial fibrillation     |
| 3              | hypertension            |
| 4              | blood pressure measures |
| 5              | cardiovascular disease  |

### Get meta-data for an indicator

To get details and meta-data for a specific indicator, use the
[`cvd_indicator_details()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_details.md)
function, specifying the indicator ID of interest. Meta-data includes
details such as copyright information, source of the data, definitions
and method of calculating confidence intervals.

``` r
cvd_indicator_details(indicator_id = 7) |>
  dplyr::select(IndicatorID, MetaDataTitle, MetaData) |>
  dplyr::slice_head(n = 5) |>
  gt::gt()
```

| IndicatorID | MetaDataTitle         | MetaData                                                                                                                                                                                                                                             |
|-------------|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 7           | Copyright             | Produced by Office for Health Improvement and Disparities, and NHS Benchmarking Network. © Copyright NHS England. All rights reserved.                                                                                                               |
| 7           | Data source           | Cardiovascular Disease Prevention Audit (CVDPREVENT)                                                                                                                                                                                                 |
| 7           | Definition            | Percentage of patients aged 18 and over with GP recorded atrial fibrillation (AF) and a record of a CHADS2 or CHA2DS2-VASc score of 2 or more, who have received a prescription for any oral anticoagulation drug therapy in the preceding 6 months. |
| 7           | Indicator ID          | CVDP002AF                                                                                                                                                                                                                                            |
| 7           | Indicator short title | AF: Treated with anticoagulants (CVDP002AF)                                                                                                                                                                                                          |

### Sibling area data

To find performance for sibling areas (areas that share a common parent
area, such as GP practices within a PCN or PCNs within an ICB), you can
use the
[`cvd_indicator_sibling()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_sibling.md)
function, specifying a time period, area and metric.

Here we get data for Metric 126 (relating to indicator 2- Hypertension
treatment) for siblings to ‘3 Centres PCN’ (ID 1103):

``` r
cvd_indicator_sibling(time_period_id = 17, area_id = 1103, metric_id = 126) |>
  dplyr::select(
    AreaID,
    AreaName,
    Value,
    LowerConfidenceLimit,
    UpperConfidenceLimit
  ) |>
  gt::gt()
```

| AreaID | AreaName                             | Value  | LowerConfidenceLimit | UpperConfidenceLimit |
|--------|--------------------------------------|--------|----------------------|----------------------|
| 1103   | 3 Centres PCN                        | 100.00 | 84.21                | 100.00               |
| 1023   | The Valleys Health & Social Care PCN | 93.75  | 73.68                | 100.00               |
| 315    | Tolson Care Partnership PCN          | 90.91  | 64.29                | 100.00               |
| 709    | Spen Health & Wellbeing PCN          | 90.91  | 72.00                | 96.00                |
| 523    | Dewsbury & Thornhill PCN             | 88.89  | 58.33                | 100.00               |
| 607    | The Mast PCN                         | 88.89  | 58.33                | 100.00               |
| 606    | Batley Birstall PCN                  | 86.67  | 61.11                | 94.44                |
| 1055   | Greenwood PCN                        | 86.67  | 61.11                | 94.44                |
| 1334   | Viaduct Care PCN                     | 69.23  | 43.75                | 87.50                |

### Child area data

To find performance for child areas (areas that are organisationally one
level lower than the specified area), you can use the
[`cvd_indicator_child_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_child_data.md)
function, specifying a time period, area and metric.

Here we get data for Metric 126 (relating to indicator 17 - Hypertension
treatment) for the children of ‘NHS Shropshire, Telford and Wrekin CCG’
(ID 126):

``` r
cvd_indicator_child_data(time_period_id = 17, area_id = 74, metric_id = 126) |>
  dplyr::select(
    AreaID,
    AreaName,
    Value,
    LowerConfidenceLimit,
    UpperConfidenceLimit
  ) |>
  gt::gt()
```

| AreaID | AreaName                | Value  | LowerConfidenceLimit | UpperConfidenceLimit |
|--------|-------------------------|--------|----------------------|----------------------|
| 226    | Wrekin PCN              | 100.00 | 80.00                | 100.00               |
| 899    | Se Shropshire PCN       | 100.00 | 88.00                | 100.00               |
| 435    | Newport And Central PCN | 93.33  | 72.22                | 100.00               |
| 1373   | Shrewsbury PCN          | 92.31  | 75.86                | 96.55                |
| 1201   | North Shropshire PCN    | 91.30  | 73.08                | 96.15                |
| 848    | Sw Shropshire PCN       | 90.91  | 64.29                | 100.00               |
| 1338   | Teldoc PCN              | 80.00  | 60.71                | 89.29                |
| 731    | South East Telford PCN  | 77.78  | 57.14                | 90.48                |

### All metric data for a given time, area and indicator

To access all metric data for a given area, time period and indicator
you can use the
[`cvd_indicator_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_data.md)
function. The return from the API includes three sets of data which are
grouped together as three tibbles within a named list object. The three
tibbles include:

- indicator metrics,

- area data,

- national data,

Here we look at ‘AF: treatment with anticoagulants’ (indicator ID 7) in
time period 17 for ‘Leicester Central PCN’ (area_id 701) focussed on
metrics by gender:

``` r
returned_list <- cvd_indicator_data(
  time_period_id = 17,
  indicator_id = 7,
  area_id = 701
)
returned_list |> summary()
#>                   Length Class  Mode
#> indicator_metrics 17     tbl_df list
#> area_data         27     tbl_df list
#> national_data     26     tbl_df list
```

#### Indicator metrics

We can extract the tibbles using the list\$object notation. Here we can
see definitions for the indicator and its associated metrics.

``` r
returned_list$indicator_metrics |> dplyr::glimpse()
#> Rows: 22
#> Columns: 17
#> $ AxisCharacter          <chr> "%", "%", "%", "%", "%", "%", "%", "%", "%", "%…
#> $ FormatDisplayName      <chr> "Proportion %", "Proportion %", "Proportion %",…
#> $ IndicatorCode          <chr> "CVDP002AF", "CVDP002AF", "CVDP002AF", "CVDP002…
#> $ IndicatorFormatID      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ IndicatorID            <int> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,…
#> $ IndicatorName          <chr> "Patients with GP recorded atrial fibrillation …
#> $ IndicatorOrder         <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
#> $ IndicatorShortName     <chr> "AF: Treated with anticoagulants (CVDP002AF)", …
#> $ NotificationCount      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ TimePeriodID           <dbl> 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,…
#> $ TimePeriodName         <chr> "To March 2024", "To March 2024", "To March 202…
#> $ CategoryAttribute      <chr> "Persons", "Female", "Male", "Male", "Female", …
#> $ MetricCategoryID       <int> 30, 28, 29, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1…
#> $ MetricCategoryName     <chr> "Persons", "Female", "Male", "18-39", "18-39", …
#> $ MetricCategoryOrder    <int> 1, 2, 3, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
#> $ MetricCategoryTypeName <chr> "Sex", "Sex", "Sex", "Age group", "Age group", …
#> $ MetricID               <int> 150, 125, 124, 127, 131, 134, 126, 132, 135, 12…
```

#### Area data

Here we can obtain rates of treatment with anticoagulants broken down by
sex for Leicester Central PCN:

``` r
returned_list$area_data |>
  dplyr::filter(MetricCategoryTypeName == "Sex") |>
  dplyr::select(
    AreaID,
    CategoryAttribute,
    Value,
    LowerConfidenceLimit,
    UpperConfidenceLimit
  ) |>
  gt::gt()
```

| AreaID | CategoryAttribute | Value | LowerConfidenceLimit | UpperConfidenceLimit |
|--------|-------------------|-------|----------------------|----------------------|
| 701    | Persons           | 95.40 | 91.53                | 97.74                |
| 701    | Female            | 94.67 | 87.18                | 97.44                |
| 701    | Male              | 95.96 | 90.20                | 98.04                |

#### National data

And we can also obtain the same breakdown for national (England-wide):

``` r
returned_list$national_data |>
  dplyr::filter(MetricCategoryTypeName == "Sex") |>
  dplyr::select(
    AreaID,
    CategoryAttribute,
    Value,
    LowerConfidenceLimit,
    UpperConfidenceLimit
  ) |>
  gt::gt()
```

| AreaID | CategoryAttribute | Value | LowerConfidenceLimit | UpperConfidenceLimit |
|--------|-------------------|-------|----------------------|----------------------|
| 1      | Persons           | 91.48 | 91.43                | 91.54                |
| 1      | Female            | 91.17 | 91.08                | 91.25                |
| 1      | Male              | 91.74 | 91.67                | 91.82                |

These splits by local and national level allow for fine-grained
benchmarking.

### All indicator metric data for system level and time period

Perhaps one of the most useful functions when looking to do a
cross-sectional system-wide analysis,
[`cvd_indicator_raw_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_raw_data.md)
returns all metric data for a specified indicator system level and time
period.

Here we return all metric data for indicator ‘AF: treatment with
anticoagulants’ (indicator ID 7) in time period 17 at GP practice level
(system level ID 5):

``` r
cvd_indicator_raw_data(
  indicator_id = 7,
  time_period_id = 17,
  system_level_id = 5
) |>
  dplyr::slice_head(n = 5) |>
  dplyr::select(AreaCode, AreaName, Value) |>
  gt::gt()
```

| AreaCode | AreaName                                 | Value |
|----------|------------------------------------------|-------|
| P87657   | (IRLAM) SALFORD CARE CTRS MEDICAL PRACTI | 97.01 |
| P87620   | 1/Monton Medical Practice                | 92.09 |
| P87004   | 1/SALFORD MEDICAL PRACTICE               | 97.37 |
| N84035   | 15 Sefton Road                           | 87.85 |
| L81051   | 168 Medical Group                        | 91.84 |

### Benchmark metric data with national results

To return metric data for a given area and time period along with
national results to compare against use function
[`cvd_indicator_nationalarea_metric_data()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_nationalarea_metric_data.md).

The output is a list of named tibbles. The two possible tibbles are:

- `area`: contains metric data for the specified area in comparison with
  national metric data.

- `target`: contains details on how to reach target values, including:

  - target value as a percentage (stored as a whole number up to 100)

  - target patients (the number of additional patients needed to reach
    the target percentage)

Note that the `target` tibble is only provided if data is available for
both national and the chosen area.

Here we compare performance against metric 150 (AF: treatment with
anticoagulants - all people) in ‘Chester South PCN’ (area ID 553) with
national performance:

``` r
returned_list <- cvd_indicator_nationalarea_metric_data(
  metric_id = 150,
  time_period_id = 17,
  area_id = 553
)

returned_list |> summary()
#>        Length Class  Mode
#> area   7      tbl_df list
#> target 3      tbl_df list
```

#### Area data

We can extract the tibbles using the `list$object` notation. Here we
extract the area metric comparison with national data:

``` r
area_data <- returned_list$area
area_data |> gt::gt()
```

| AreaCode  | AreaID | AreaName          | HighestPriorityNotificationType | NationalLevel | NotificationCount | Value |
|-----------|--------|-------------------|---------------------------------|---------------|-------------------|-------|
| E92000001 | 1      | England           | NA                              | Y             | 0                 | 91.48 |
| U68943    | 553    | Chester South PCN | NA                              | N             | 0                 | 88.57 |

#### Target data

Here we extract the target details:

``` r
target_data <- returned_list$target
target_data |> gt::gt()
```

| TargetLabel             | TargetPatients | TargetValue |
|-------------------------|----------------|-------------|
| Upper threshold for QOF | 45             | 95          |

`Target value` is a percentage figure indicating the national target for
this metric. `Target patients` is the number of additional patients
needed by Chester South PCN to reach the target value.

NB, `target` data is only provided where there are data for both
national and the specified area.

### List priority group indicators

Priority groups are collections of *Indicators* which are displayed in
the [Regional & ICS insights](https://data.cvdprevent.nhs.uk/insights)
page of the CVD PREVENT website and can be accessed from the API using
the
[`cvd_indicator_priority_groups()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_priority_groups.md)
function.

Here we return one indicator from each of the priority groups:

``` r
cvd_indicator_priority_groups() |>
  dplyr::select(
    PriorityGroup,
    PathwayGroupName,
    PathwayGroupID,
    IndicatorID,
    IndicatorName
  ) |>
  dplyr::slice_head(by = PathwayGroupID) |>
  gt::gt(row_group_as_column = T)
```

| PriorityGroup   | PathwayGroupName               | PathwayGroupID | IndicatorID | IndicatorName                                                                                                                                                                                                                        |
|-----------------|--------------------------------|----------------|-------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ABC             | Hypertension                   | 5              | 4           | Patients with GP recorded hypertension, with a record of a blood pressure reading in the preceding 12 months.                                                                                                                        |
| ABC             | Cholesterol                    | 6              | 22          | Patients with no GP recorded CVD and a GP recorded QRISK score of 10% or more, who are currently treated with lipid lowering therapy                                                                                                 |
| ABC             | Atrial Fibrilation             | 7              | 7           | Patients with GP recorded atrial fibrillation and with a CHADS2 or CHA2DS2-VASc score of 2 or more, who are currently treated with any oral anticoagulant.                                                                           |
| ABC             | NA                             | NA             | 54          | Patients with GP recorded CVD (narrow definition), whose most recent blood cholesterol level is LDL-cholesterol less than or equal to 2.0 mmol/l or non-HDL cholesterol less than or equal to 2.6 mmol/l, in the preceding 12 months |
| CKD             | Chronic Kidney Disease         | 9              | 29          | Patients with GP recorded CKD (G3a to G5), with a record of an eGFR test in the preceding 12 months.                                                                                                                                 |
| Prevalence      | Familial Hypercholesterolaemia | 8              | 9           | Prevalence of GP recorded possible, probable or genetically confirmed familial hypercholesterolaemia, in patients all ages, calculated as a crude rate per 10,000 patients.                                                          |
| Smoking and BMI | Smoking                        | 10             | 24          | Patients with GP recorded CVD or CVD risk factors who are GP recorded current smokers or have no smoking status recorded, whose notes record smoking status in the preceding 12 months.                                              |

### List pathway group indicators

Pathway groups are sub-groupings of Priority Groups visible in the
[Regional & ICS insights](https://data.cvdprevent.nhs.uk/insights) page
of the CVD PREVENT website and can be accessed from the API using the
[`cvd_indicator_pathway_group()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_pathway_group.md)
function.

Here we return the indicators within the ‘Chronic Kidney Disease’
pathway group (ID 9):

``` r
cvd_indicator_pathway_group(pathway_group_id = 9) |>
  dplyr::select(
    PathwayGroupName,
    PathwayGroupID,
    IndicatorCode,
    IndicatorID,
    IndicatorName
  ) |>
  dplyr::group_by(PathwayGroupName) |>
  gt::gt(row_group_as_column = T)
```

|                        | PathwayGroupID | IndicatorCode | IndicatorID | IndicatorName                                                                                                                                               |
|------------------------|----------------|---------------|-------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Chronic Kidney Disease | 9              | CVDP002CKD    | 13          | Patients whose last two eGFRs are less than 60ml/min/1.73m2 (uncoded CKD), who do not have a record of GP recorded CKD (G3a to G5)                          |
|                        | 9              | CVDP001CKD    | 8           | Prevalence of GP recorded CKD (G3a to G5)                                                                                                                   |
|                        | 9              | CVDP006CKD    | 29          | Patients with GP recorded CKD (G3a to G5), with a record of an eGFR test in the preceding 12 months.                                                        |
|                        | 9              | CVDP005CKD    | 19          | Patients with GP recorded CKD (G3a to G5) and hypertension and proteinuria, who are currently treated with renin-angiotensin system antagonists             |
|                        | 9              | CVDP007CKD    | 31          | Patients with GP recorded CKD (G3a to G5) with an ACR of less than 70 mg/mmol, whose last blood pressure reading is to the appropriate treatment threshold. |
|                        | 9              | CVDP010CHOL   | 23          | Patients with GP recorded chronic kidney disease (G3a to G5), who are currently treated with lipid lowering therapy.                                        |

### List Indicator Group indicators

Indicator Groups are further groups of indicators which are classified
by `IndicatorGroupTypeID` which indicates what *type* of indicator,
e.g. Priority Group.

Here we list the indicators under Indicator Group ID 13 (Monitoring)
which lists ‘Key Question’ Indicator Group indicators:

``` r
cvd_indicator_group(indicator_group_id = 13) |>
  dplyr::select(
    IndicatorGroupID,
    IndicatorGroupName,
    IndicatorGroupTypeName,
    IndicatorID,
    IndicatorName
  ) |>
  dplyr::group_by(IndicatorGroupID, IndicatorGroupName) |>
  gt::gt()
```

| IndicatorGroupTypeName | IndicatorID | IndicatorName                                                                                                 |
|------------------------|-------------|---------------------------------------------------------------------------------------------------------------|
| 13 - Monitoring        |             |                                                                                                               |
| Key Question           | 4           | Patients with GP recorded hypertension, with a record of a blood pressure reading in the preceding 12 months. |
| Key Question           | 29          | Patients with GP recorded CKD (G3a to G5), with a record of an eGFR test in the preceding 12 months.          |

### Return time series data for a metric and area

To get data over all available time points for a given metric use the
[`cvd_indicator_metric_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_timeseries.md)
function whilst specifying an area ID.

Here we visualise data for Salford South East PCN (area ID 705) for ‘AF:
treatment with anticoagulants’ for women people aged 60-79 years (metric
ID 130):

``` r
cvd_indicator_metric_timeseries(metric_id = 130, area_id = 705) |>
  # prepare for plotting
  dplyr::mutate(
    TimePeriodName = stats::reorder(TimePeriodName, TimePeriodID)
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = TimePeriodName,
    y = Value,
    group = AreaName,
    colour = AreaName
  )) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )
```

![](using_cvdprevent_files/figure-html/return%20time%20series%20data%20for%20a%20metric%20and%20area%201-1.png)

Or we can view the details of the time-series performance for this
metric:

``` r
cvd_indicator_metric_timeseries(metric_id = 130, area_id = 705) |>
  dplyr::select(AreaName, TimePeriodName, TimePeriodID, Value) |>
  tidyr::pivot_wider(
    names_from = AreaName,
    values_from = Value
  ) |>
  gt::gt()
```

| TimePeriodName    | TimePeriodID | England | Salford South East PCN |
|-------------------|--------------|---------|------------------------|
| To March 2020     | 1            | 88.20   | 85.90                  |
| To March 2021     | 2            | 88.60   | 86.00                  |
| To September 2021 | 3            | 88.90   | 88.80                  |
| To March 2022     | 4            | 89.33   | 90.00                  |
| To June 2022      | 5            | 89.37   | 90.24                  |
| To September 2022 | 6            | 89.64   | 90.60                  |
| To December 2022  | 7            | 89.99   | 91.72                  |
| To March 2023     | 8            | 90.97   | 90.12                  |
| To June 2023      | 9            | 90.99   | 90.96                  |
| To December 2023  | 15           | 91.15   | 92.22                  |
| To March 2024     | 17           | 92.21   | 93.92                  |
| To June 2024      | 18           | 92.23   | 92.93                  |
| To September 2024 | 20           | 92.00   | 93.12                  |
| To December 2024  | 22           | 92.05   | 92.75                  |
| To March 2025     | 24           | 92.52   | 94.65                  |
| To June 2025      | 26           | 92.41   | 94.65                  |

### Return time series data for an indicator and area - inequalities

To see time series data for all metric breakdowns for an indicator use
the
[`cvd_indicator_person_timeseries()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_person_timeseries.md)
function along with the indicator ID and specify an area.

Here we view time-series metric data for indicator ‘AF: treatment with
anticoagulants’ (ID 7) in Salford South East PCN (area ID 705), focussed
just on the age group inequalities metrics:

``` r
cvd_indicator_person_timeseries(indicator_id = 7, area_id = 705) |>
  dplyr::filter(
    MetricCategoryTypeName == 'Age group',
    !is.na(Value)
  ) |>
  dplyr::mutate(
    TimePeriodName = stats::reorder(TimePeriodName, TimePeriodID)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = TimePeriodName,
      y = Value,
      group = MetricCategoryName,
      colour = MetricCategoryName
    )
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )
```

![](using_cvdprevent_files/figure-html/return%20time%20series%20data%20for%20an%20indicator%20and%20area%20-%20inequalities%201-1.png)

Or we can view the details of the time-series performance for these
metrics:

``` r
cvd_indicator_person_timeseries(indicator_id = 7, area_id = 705) |>
  dplyr::filter(
    MetricCategoryTypeName == 'Age group',
    !is.na(Value)
  ) |>
  dplyr::select(MetricCategoryName, TimePeriodName, TimePeriodID, Value) |>
  tidyr::pivot_wider(
    names_from = MetricCategoryName,
    values_from = Value
  ) |>
  gt::gt()
```

| TimePeriodName    | TimePeriodID | 40-59 | 60-79 | 80+   |
|-------------------|--------------|-------|-------|-------|
| To March 2020     | 1            | 78.30 | 82.30 | 85.00 |
| To March 2021     | 2            | 91.30 | 82.30 | 84.80 |
| To September 2021 | 3            | 78.00 | 86.90 | 87.20 |
| To March 2022     | 4            | 76.60 | 86.81 | 90.23 |
| To June 2022      | 5            | 82.35 | 86.82 | 88.56 |
| To September 2022 | 6            | 80.00 | 86.56 | 90.60 |
| To December 2022  | 7            | 76.32 | 87.50 | 90.58 |
| To March 2023     | 8            | 72.73 | 87.42 | 91.06 |
| To June 2023      | 9            | 71.11 | 88.51 | 91.29 |
| To December 2023  | 15           | 74.42 | 88.94 | 90.61 |
| To March 2024     | 17           | 80.49 | 91.51 | 92.45 |
| To June 2024      | 18           | 82.93 | 91.63 | 92.24 |
| To September 2024 | 20           | 80.43 | 91.62 | 91.88 |
| To December 2024  | 22           | 85.71 | 91.38 | 91.64 |
| To March 2025     | 24           | 85.71 | 93.31 | 91.90 |
| To June 2025      | 26           | 85.71 | 92.87 | 92.16 |

### Return system level comparisons for a metric

Use the
[`cvd_indicator_metric_systemlevel_comparison()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_systemlevel_comparison.md)
function to return metric performance for a specified time period for
all organisations in the same system level, for example for all GP
practices or for all PCNs.

Here we return performance for metric ‘AF: DOAC & VitK’ in people aged
40-59 years (metric ID 1270) in time period 17 for
Salford South East PCN (area ID 705) *and all other* PCNs - truncated to
a sample of four PCN performances:

``` r
cvd_indicator_metric_systemlevel_comparison(
  metric_id = 1270,
  time_period_id = 17,
  area_id = 705
) |>
  dplyr::filter(AreaID %in% c(705:709), !is.na(Value)) |>
  dplyr::select(SystemLevelName, AreaID, AreaName, Value) |>
  gt::gt()
```

| SystemLevelName | AreaID | AreaName                    | Value  |
|-----------------|--------|-----------------------------|--------|
| PCN             | 705    | Salford South East PCN      | 80.49  |
| PCN             | 707    | Haringey - North East PCN   | 88.00  |
| PCN             | 708    | Teesdale PCN                | 100.00 |
| PCN             | 709    | Spen Health & Wellbeing PCN | 85.71  |

### Metric area breakdown

Use
[`cvd_indicator_metric_area_breakdown()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_area_breakdown.md)
function to return the data Area Breakdown chart.

Here we return performance for metric ‘AF: DOAC & VitK’ in men aged
60-79 years (metric ID 128) in time period 17 for Salford South East PCN
(area ID 705):

``` r
cvd_indicator_metric_area_breakdown(
  metric_id = 128,
  time_period_id = 17,
  area_id = 705
) |>
  dplyr::select(SystemLevelName, AreaID, AreaName, Value) |>
  gt::gt()
```

| SystemLevelName | AreaID | AreaName               | Value |
|-----------------|--------|------------------------|-------|
| England         | 1      | England                | 92.49 |
| PCN             | 705    | Salford South East PCN | 90.00 |

## Misc

### List external resources

To return a list of all external resources used by the API use function
[`cvd_external_resource()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_external_resource.md).
Here we show the first five external resources:

``` r
cvd_external_resource() |>
  dplyr::filter(ExternalResourceID < 10) |>
  dplyr::select(
    ExternalResourceCategory,
    ExternalResourceSource,
    ExternalResourceTitle
  ) |>
  dplyr::group_by(ExternalResourceCategory) |>
  gt::gt(row_group_as_column = T)
```

|                      | ExternalResourceSource              | ExternalResourceTitle                                     |
|----------------------|-------------------------------------|-----------------------------------------------------------|
| Data Packs           | Public Health England               | Cardiovascular Disease Prevention Data Packs              |
|                      | NHS England                         | Equality and Health Inequality Packs                      |
| Toolkits             | NHS Digital                         | Weight Management Programme General Practice Toolkit      |
|                      | Primary Care Cardiovascular Society | CVD – identification and treatment of people at high risk |
| Stratification tools | UCL Partners                        | Search and risk stratification tools                      |

### Show data availability

To show data availability for a given system level in a given time
period then use the
[`cvd_data_availability()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_data_availability.md)
function.

NB, data appears to be only available up to time period 6 (to September
2022).

``` r
cvd_data_availability(time_period_id = 6, system_level_id = 5) |>
  dplyr::select(
    IndicatorShortName,
    IsAvailable,
    SystemLevelName,
    MetricCategoryTypeID
  ) |>
  dplyr::slice_head(n = 5) |>
  gt::gt()
```

| IndicatorShortName                                                      | IsAvailable | SystemLevelName | MetricCategoryTypeID |
|-------------------------------------------------------------------------|-------------|-----------------|----------------------|
| AF: Prevalence (CVDP001AF)                                              | N           | Practice        | 4                    |
| AF: Prevalence (CVDP001AF)                                              | N           | Practice        | 2                    |
| AF: Prevalence (CVDP001AF)                                              | N           | Practice        | 1                    |
| AF: Prevalence (CVDP001AF)                                              | N           | Practice        | 3                    |
| Hypertension: Treated to appropriate threshold (age \< 80) (CVDP002HYP) | Y           | Practice        | 4                    |
