# Example 2: Time-series monitoring

``` r
library(cvdprevent)

# We will also be usng the following packages in this article.
# These will be fully namespaced, e.g. dplyr::select() throughout.

# library(dplyr)
# library(stringr)
# library(reactable)
# library(tidyr)
# library(gt)
# library(plotly)
```

Monitoring cardiovascular disease (CVD) indicators over time is
essential for understanding whether prevention activity is improving,
stagnating or declining. The {cvdprevent} package makes this process
reproducible and transparent by exposing time-period metadata, indicator
definitions and tidy tibbles that can be reshaped and visualised with
ease.

The example below walks through a complete workflow for producing
time-series view of blood pressure (BP) monitoring for a specific
Primary Care Network (PCN): St Helens Central PCN.

[TABLE]

## Step 1. Identify the geography

Before retrieving indicator values, we need to confirm which time
periods and geographies are available.

System levels differ in which periods they support, so we begin by
listing the periods associated with PCNs.

``` r
cvd_time_period_system_levels() |> 
  dplyr::filter(SystemLevelName == "PCN") |> 
  dplyr::select(dplyr::any_of(c("TimePeriodID", "TimePeriodName", "SystemLevelName"))) |> 
  gt::gt() |> 
  gt::tab_options(quarto.disable_processing = TRUE)
```

| TimePeriodID | TimePeriodName    | SystemLevelName |
|--------------|-------------------|-----------------|
| 1            | To March 2020     | PCN             |
| 2            | To March 2021     | PCN             |
| 3            | To September 2021 | PCN             |
| 4            | To March 2022     | PCN             |
| 5            | To June 2022      | PCN             |
| 6            | To September 2022 | PCN             |
| 7            | To December 2022  | PCN             |
| 8            | To March 2023     | PCN             |
| 9            | To June 2023      | PCN             |
| 10           | To September 2023 | PCN             |
| 15           | To December 2023  | PCN             |
| 17           | To March 2024     | PCN             |
| 18           | To June 2024      | PCN             |
| 20           | To September 2024 | PCN             |
| 22           | To December 2024  | PCN             |
| 24           | To March 2025     | PCN             |
| 26           | To June 2025      | PCN             |

From this table we can see that **TimePeriodID 26** is the most recent
reporting window, so we will use that for our geography lookup.

Next, we search for areas whose names contain “St Helens” within that
time period:

``` r
cvd_area_search(partial_area_name = "St Helens", time_period_id = 26) |> 
  dplyr::select(dplyr::any_of(c("AreaID", "AreaName", "SystemLevelID", "SystemLevelName"))) |> 
  gt::gt() |> 
  gt::tab_options(quarto.disable_processing = TRUE)
```

| AreaID | AreaName                | SystemLevelID | SystemLevelName |
|--------|-------------------------|---------------|-----------------|
| 5979   | Modality St Helens      | 5             | Practice        |
| 761    | St Helens Central PCN   | 4             | PCN             |
| 356    | St Helens North PCN     | 4             | PCN             |
| 6231   | St Helens Road Practice | 5             | Practice        |
| 1279   | St Helens South PCN     | 4             | PCN             |

This returns several matching areas. For this example we will use
**AreaID 761 for St Helens Central PCN**.

## Step 2. Identify the indicator

Each system level and time period exposes a set of indicators. We now
list all indicators available for PCNs in time period 26.

``` r
inds <-
  cvd_indicator_list(time_period_id = 26, system_level_id = 5) |>
  # select relevant columns to help
  dplyr::select(dplyr::any_of(c(
    "IndicatorID",
    "IndicatorCode",
    "IndicatorShortName",
    "IndicatorOrder"
  )))

inds |>
  # display in an interactive table
  reactable::reactable(searchable = TRUE, defaultPageSize = 5)
```

Using the searchable table, we identify that BP monitoring corresponds
to **IndicatorID 4**.

## Step 3. Retrieve the data

With the indicator and area identified, we can now pull the full
time-series dataset. This includes values for several metric categories
(overall, age groups, sex and ethnicity).

``` r
ind_data <- cvd_indicator_person_timeseries(indicator_id = 4, area_id = 761)
```

The returned tibble contains one row per metric category per time
period, making it ideal for both overall and subgroup trend analysis.

## Step 4. Visualise the overall performance

The raw data uses text-based time period names (e.g., “To March 2020”).
To plot these chronologically, we convert them to `yearmon` objects.

The following chart shows the overall BP monitoring performance
(“Persons” metric category) across all available periods.

``` r
ind_data |> 
    # get the overall performance metric
  dplyr::filter(MetricCategoryName == "Persons") |> 
  # wrangle data
  dplyr::mutate(
    # convert time period name to a yearmonth object
    TimePeriod = TimePeriodName |> stringr::str_remove_all(pattern = "To ") |> zoo::as.yearmon(),
    # prepare some labels
    lbl_value = scales::percent(Value, accuracy = 0.1, scale = 1),
    # create some text to include in the context box when you hover over the plot
    HoverText = glue::glue(
      "{AreaName}
      {TimePeriodName}
      {lbl_value}")
  ) |> 
  # plot as an interactive chart
  plotly::plot_ly(
    x = ~TimePeriod,
    y = ~Value,
    type = "scatter",
    mode = "lines+markers",
    hovertext = ~HoverText,
    hoverinfo = "text",
    marker = list(color = "#16a085"),
    line = list(color = "#16a085")
  ) |> 
  plotly::layout(
    # set the title to the indicator name
    title = inds |> dplyr::filter(IndicatorID == 4) |> dplyr::pull(IndicatorShortName),
    # clear the y-axis title as its unecessary
    yaxis = list(range = c(0, 100), title = "Indicator performance (percent)"),
    # set the x-axis range and title
    xaxis = list(title = ""),
    # set the font
    font = list(family = "sans-serif", size = 14),
    # adjust the margin to avoid the title getting clipped
    margin = list(t = 80),
    # change the colour of the hovertext box to add some contrast
    hoverlabel = list(bgcolor = "#ecf0f1")
  ) |> 
    # hide the mode bar
  plotly::config(displayModeBar = FALSE)
```

This gives a clear view of whether overall BP monitoring is improving or
declining over time.

## Step 5. Visualise metric-level performance

Often we want to understand which groups are driving changes in
performance. For example:

- Are younger or older age groups improving at different rates?

- Are there differences between men and women?

- Are there disparities between ethnic groups?

To support this, we define a helper function that produces a consistent
time-series chart for any metric category type.

``` r
# get the name of the indicator
ind_name <- inds |> dplyr::filter(IndicatorID == 4) |> dplyr::pull(IndicatorShortName)

#' Plot a related group of metrics on a time-series chart
#' 
#' @description
#' Returns a {plotly} scatter plot showing a time-series chart for the given df.
#'
#' @param df Tibble of data from the `cvd_indicator_person_timeseries()` function, filtered for the appropriate MetricCategoryTypeName, e.g. `MetricCategoryTypeName = "Age group"`
#'
#' @returns {plotly} object
#'
plot_metric_timeseries <- function(df) {
  plot <-
    df |> 
    # wrangle data
    dplyr::mutate(
      # convert time period name to a yearmonth object
      TimePeriod = TimePeriodName |> stringr::str_remove_all(pattern = "To ") |> zoo::as.yearmon(),
      # prepare some labels
      lbl_value = scales::percent(Value, accuracy = 0.1, scale = 1),
      # create some text to include in the context box when you hover over the plot
      HoverText = glue::glue(
        "{AreaName}
        {MetricCategoryTypeName}: {MetricCategoryName}
        {TimePeriodName}
        {lbl_value}")
    ) |> 
    # plot as an interactive chart
    plotly::plot_ly(
      x = ~TimePeriod,
      y = ~Value,
      color = ~MetricCategoryName,
      colors = viridisLite::mako(length(unique(df$MetricCategoryName)), begin = 0.1, end = 0.9),
      type = "scatter",
      mode = "lines+markers",
      hovertext = ~HoverText,
      hoverinfo = "text"
    ) |> 
    plotly::layout(
      # set the title to the indicator name
      title = ind_name,
      # clear the y-axis title as its unecessary
      yaxis = list(range = c(0, 100), title = "Indicator performance (percent)"),
      # set the x-axis range and title
      xaxis = list(title = ""),
      # set the font
      font = list(family = "sans-serif", size = 14),
      # adjust the margin to avoid the title getting clipped
      margin = list(t = 80),
      # change the colour of the hovertext box to add some contrast
      hoverlabel = list(bgcolor = "#ecf0f1")
    ) |> 
      # hide the mode bar
    plotly::config(displayModeBar = FALSE)

    return(plot)
}
```

We can now easily generate subgroup-specific time-series charts using a
tabset.

- Age-group
- Gender
- Ethnicity

Age-group trends help identify whether improvements are consistent
across the population or concentrated in specific cohorts.

``` r
ind_data |> 
  # get the overall performance metric and plot
  dplyr::filter(MetricCategoryTypeName == "Age group") |> 
  plot_metric_timeseries()
```

Examining performance by sex can highlight whether targeted
interventions may be needed.

``` r
ind_data |> 
  # get the overall performance metric and plot
  dplyr::filter(MetricCategoryTypeName == "Sex") |> 
  plot_metric_timeseries()
```

Ethnicity-level trends can reveal inequalities and support more
equitable service planning.

``` r
ind_data |> 
  # get the overall performance metric and plot
  dplyr::filter(MetricCategoryTypeName == "Ethnicity") |> 
  plot_metric_timeseries()
```

## Common pitfalls

Even with a tidy workflow, there are a few recurring issues that can
trip up users when working with time-series CVD indicators. The points
below highlight what to watch out for and how to avoid unnecessary
frustration.

### Mixing system levels and time periods

Indicators are not always available at every system level for every time
period. A common mistake is to:

- look up an indicator at one system level

- then attempt to retrieve values for a different level

Always check availability using:

- [`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)

- `cvd_indicator_list(time_period_id, system_level_id)`

If a query returns zero rows, mismatched levels are often the cause.

### Using the wrong AreaID

Area names are not unique, and many geographies share similar naming
patterns. For example, “St Helens” returns multiple PCNs as well as a
general practice. Selecting the wrong AreaID will silently return
valid - but incorrect - data.

To avoid this:

- always filter by **both** `AreaName` and `SystemLevelName`

- confirm the AreaID before pulling indicator values

### Forgetting that time periods are unevenly spaced

Time periods are not calendar quarters. They represent reporting windows
that may vary in length. If you plot them as plain character strings,
the x-axis will be mis-ordered or uneven.

Always convert to a valid time period, for example, by using
[`zoo::as.yearmon()`](https://rdrr.io/pkg/zoo/man/yearmon.html). This
ensures chronological ordering and correct spacing in plots.

### Filtering on the wrong metric category

Indicators often contain multiple metric categories (e.g. “Persons”,
“Sex”, “Age group”, “Ethnicity”).

If you forget to filter before plotting, you may end up with:

- dozens of overlapping lines

- duplicated categories

- misleading summaries

Check `MetricCategoryTypeName` and `MetricCategoryName` before
visualising.

### Assuming indicator definitions stay constant

Indicator definitions can change between time periods. If you compare
values across long spans of time, make sure the indicator:

- exists in all periods

- has consistent definitions

- uses the same denominator

The metadata returned by
[`cvd_indicator_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_list.md)
helps confirm this.

### Treating percentages as whole numbers

Values in the dataset are already expressed as percentages (0-100).

If you apply
[`scales::percent()`](https://scales.r-lib.org/reference/percent_format.html)
without `scale = 1`, you may accidentally multiply values by 100 again.

### Forgetting to handle missing categories

Some metric categories may not appear in all time periods (e.g., small
populations or suppressed values).

If you reshape data (e.g., with
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)),
missing categories can cause:

- dropped rows

- misaligned time series

- unexpected NA values

Check for missingness before reshaping or plotting.

### Over-interpreting short-term variation

CVD indicators can fluctuate between periods due to:

- small denominators

- seasonal effects

- data completeness

Look for sustained trends rather than single-period jumps.
