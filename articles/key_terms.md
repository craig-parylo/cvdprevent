# Key terms

``` r
library(cvdprevent)
```

## Terms

The following terms are used in the CVDPrevent API to describe key
concepts:

| Term         | Description                                       | Example                                                                                    |
|--------------|---------------------------------------------------|--------------------------------------------------------------------------------------------|
| System Level | Grouping of Areas into comparable levels          | England, Region, ICB, sub-ICB                                                              |
| Area         | Geographical locality for which data was recorded | Bedfordshire, Luton and Milton Keynes ICB                                                  |
| Time Period  | Time span in which data was recorded              | To June 2024                                                                               |
| Indicator    | High-level performance measure                    | CVDP001AF is short reference for “Prevalence of GP recorded atrial fibrillation” indicator |
| Metric       | Category breakdown of Indicator                   | CVDP001AF - Male, Aged 18-39                                                               |

## System Levels by Time Period

You can use
[`cvd_time_period_system_levels()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_time_period_system_levels.md)
to explore which system levels are reported in each time period. These
levels may vary over time due to changes in NHS geography - for example,
**Sustainability and Transformation Partnerships (STPs)** were later
replaced by **Integrated Care Systems (ICS / ICBs)**.[¹](#fn1)

The following code snippet creates a table showing which system levels
are available for each reporting time period. A ✔️ indicates that data
for that system level is available in the corresponding period.

``` r
cvd_time_period_system_levels() |>
  dplyr::select(dplyr::any_of(c(
    "TimePeriodID",
    "TimePeriodName",
    "SystemLevelName"
  ))) |>
  dplyr::mutate(value_show = "✔️") |>
  tidyr::pivot_wider(
    names_from = dplyr::any_of("SystemLevelName"),
    values_from = dplyr::any_of("value_show"),
    values_fill = ""
  ) |>
  gt::gt() |>
  gt::tab_options(quarto.disable_processing = TRUE) |>
  gt::cols_align(
    align = "center",
    columns = c(
      dplyr::everything(),
      -dplyr::any_of(c("TimePeriodName"))
    )
  )
```

| TimePeriodID | TimePeriodName      | England | STP | CCG | PCN | Practice | Region | ICB | Sub-ICB |
|--------------|---------------------|---------|-----|-----|-----|----------|--------|-----|---------|
| 1            | To March 2020       | ✔️      | ✔️  | ✔️  | ✔️  | ✔️       |        |     |         |
| 2            | To March 2021       | ✔️      | ✔️  | ✔️  | ✔️  | ✔️       |        |     |         |
| 3            | To September 2021   | ✔️      | ✔️  | ✔️  | ✔️  | ✔️       | ✔️     |     |         |
| 4            | To March 2022       | ✔️      | ✔️  | ✔️  | ✔️  | ✔️       | ✔️     |     |         |
| 5            | To June 2022        | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 6            | To September 2022   | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 7            | To December 2022    | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 8            | To March 2023       | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 9            | To June 2023        | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 10           | To September 2023   | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 11           | Jan 2022 - Dec 2022 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 12           | Apr 2022 - Mar 2023 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 13           | Oct 2022 - Sep 2023 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 14           | Jul 2022 - Jun 2023 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 15           | To December 2023    | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 16           | Jan 2023 - Dec 2023 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 17           | To March 2024       | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 18           | To June 2024        | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 19           | Apr 2023 - Mar 2024 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 20           | To September 2024   | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 21           | Jul 2023 - Jun 2024 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 22           | To December 2024    | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 23           | Oct 2023 - Sep 2024 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 24           | To March 2025       | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 25           | Jan 2024 - Dec 2024 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |
| 26           | To June 2025        | ✔️      |     |     | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 27           | Apr 2024 - Mar 2025 | ✔️      |     |     |     |          | ✔️     | ✔️  |         |

What this does:

- Selects relevant columns: Time period and system level names

- Adds a marker: A check-mark to indicate presence

- Reshapes the data: Converts long format to wide, with system levels as
  columns

- Displays the result: Uses {gt} to render a clean, readable table

## Listing areas

When working with areas, the first step is to identify the **time
period** you are interested in. This is important because:

- areas can change over time. For example, an area reported in *time
  period 1* may no longer exist in *time period 21* (such as `STP` in
  the above table).

- certain system levels are excluded from specific reports. For
  instance, the annual report covering **Jan 2022 - Dec 2022** (*time
  period id = 11*) does not include PCN, Practice or Sub-ICB level data.

To explore available areas, a recommended function is
[`cvd_area_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_list.md).
This requires you to specify both the **time period** and either a
`system_level_id` or a `parent_area_id`.

Using `system_level_id` will return all areas within the chosen
`time_period_id` that share the same system level. For example, to list
all NHS Regions (`system_level_id = 6`) available in *time period 22*:

``` r
cvd_area_list(time_period_id = 22, system_level_id = 6) |>
  dplyr::select(dplyr::any_of(c(
    "SystemLevelID",
    "SystemLevelName",
    "AreaName"
  ))) |>
  gt::gt() |>
  gt::tab_options(quarto.disable_processing = TRUE)
```

| SystemLevelID | SystemLevelName | AreaName                 |
|---------------|-----------------|--------------------------|
| 6             | Region          | East of England          |
| 6             | Region          | London                   |
| 6             | Region          | Midlands                 |
| 6             | Region          | North East and Yorkshire |
| 6             | Region          | North West               |
| 6             | Region          | South East               |
| 6             | Region          | South West               |

Alternatively, if you already know the area you’re interested in, you
can use
[`cvd_area_search()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_area_search.md).
This function searches by partial area name and returns all matches for
the specified time period.

For example, to find all areas containing the word “acorn” in *time
period 22*:

``` r
cvd_area_search(partial_area_name = "acorn", time_period_id = 22) |>
  dplyr::select(dplyr::any_of(c("AreaID", "AreaName"))) |>
  gt::gt() |>
  gt::tab_options(quarto.disable_processing = TRUE)
```

| AreaID | AreaName                          |
|--------|-----------------------------------|
| 4302   | Acorn Group Practice              |
| 3238   | Acorn Medical Centre              |
| 2444   | Acorn Medical Practice            |
| 2669   | Acorn Surgery                     |
| 4037   | The Acorn & Gaumont House Surgery |

## Listing indicators

To work with indicators, you first need to identify both the **time
period** and the **system level** of interest. This is important because
not all indicators are available at every system level. For example,
some indicators are not reported for PCN or Practice levels.

The following example demonstrates how to list indicators available for
each system level in *time period 26*.

``` r
# get details for system levels and time periods
syst_levels <- cvd_area_system_level_time_periods()

# get a summary of system level ids and names
df_syst_levels <-
  syst_levels |>
  dplyr::filter(TimePeriodID == 26) |>
  dplyr::select(dplyr::any_of(c("SystemLevelID", "SystemLevelName")))

# iterate over all system levels available at time period id 26 and gather the indicators
df_indicators_syst_level <-
  purrr::map2_df(
    .x = df_syst_levels$SystemLevelID,
    .y = df_syst_levels$SystemLevelName,
    .f = \(.x, .y) {
      cvd_indicator_list(time_period_id = 26, system_level_id = .x) |>
        dplyr::select(dplyr::any_of(c("IndicatorID", "IndicatorShortName"))) |>
        dplyr::mutate(SystemLevelName = .y)
    }
  )

# pivot the table to list system level
df_indicators_syst_level |>
  dplyr::mutate(value_show = "✔️") |>
  tidyr::pivot_wider(
    names_from = dplyr::any_of("SystemLevelName"),
    values_from = dplyr::any_of("value_show"),
    values_fill = ""
  ) |>
  gt::gt() |>
  gt::tab_options(quarto.disable_processing = TRUE) |>
  gt::cols_align(
    align = "center",
    columns = c(
      gt::everything(),
      -gt::any_of(c("IndicatorShortName"))
    )
  ) |>
  gt::tab_header(
    title = gt::md("Indicators for *Time Period 26* by system level")
  )
```

| Indicators for *Time Period 26* by system level |                                                                                    |         |     |          |        |     |         |
|-------------------------------------------------|------------------------------------------------------------------------------------|---------|-----|----------|--------|-----|---------|
| IndicatorID                                     | IndicatorShortName                                                                 | England | PCN | Practice | Region | ICB | Sub-ICB |
| 1                                               | AF: Prevalence (CVDP001AF)                                                         | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 2                                               | Hypertension: Treated to appropriate threshold (age \< 80) (CVDP002HYP)            | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 3                                               | Hypertension: Treated to appropriate threshold (age \>= 80) (CVDP003HYP)           | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 4                                               | Hypertension: BP monitoring (CVDP004HYP)                                           | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 7                                               | AF: Treated with anticoagulants (CVDP002AF)                                        | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 8                                               | CKD: Prevalence (CVDP001CKD)                                                       | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 9                                               | FH: Possible, probable and confirmed prevalence (CVDP002FH)                        | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 10                                              | FH: Genetically confirmed prevalence (CVDP003FH)                                   | ✔️      |     |          | ✔️     | ✔️  |         |
| 11                                              | Hypertension: Prevalence (CVDP001HYP)                                              | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 12                                              | CVD: Prevalence (CVDP001CVD)                                                       | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 13                                              | CKD: Uncoded - two low eGFRs with no recorded CKD (CVDP002CKD)                     | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 14                                              | Cholesterol: QRISK \>= 20% treated with LLT (CVDP003CHOL)                          | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 15                                              | CKD: High risk - one low eGFR with no recorded CKD (CVDP003CKD)                    | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 17                                              | CKD: Monitoring with ACR (CVDP004CKD)                                              | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 18                                              | FH: Cholesterol in at risk range for FH with no investigation for FH (CVDP004FH)   | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 19                                              | CKD: High risk treated with RAS antagonists (CVDP005CKD)                           | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 20                                              | Hypertension: High risk – one high BP with no recorded hypertension (CVDP005HYP)   | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 21                                              | Hypertension: Potential antihypertensive overtreatment (CVDP006HYP)                | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 22                                              | Cholesterol: QRISK \>= 10% treated with LLT (CVDP006CHOL)                          | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 23                                              | Cholesterol: CKD treated with LLT (CVDP010CHOL)                                    | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 24                                              | Smoking: Record of smoking status (CVDP001SMOK)                                    | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 25                                              | NDH: High risk – one HbA1c 42-48 mmol/mol, with no recorded NDH or DM (CVDP002NDH) | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 26                                              | Smoking: Current smokers offered support/treatment (CVDP002SMOK)                   | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 27                                              | DM: Uncoded - two high HbA1c with no recorded diabetes (CVDP003DM)                 | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 28                                              | DM: High risk - one high HbA1c with no recorded diabetes (CVDP005DM)               | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 29                                              | CKD: Monitoring with eGFR (CVDP006CKD)                                             | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 31                                              | CKD: ACR \< 70mg/mmol treated to appropriate BP threshold (CVDP007CKD)             | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 32                                              | Hypertension: Treated to appropriate threshold (all ages) (CVDP007HYP)             | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 33                                              | Cholesterol: Primary prevention of CVD treated with LLT (CVDP008CHOL)              | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 34                                              | Cholesterol: CVD treated with LLT (CVDP009CHOL)                                    | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 45                                              | BMI: Record of BMI status (CVDP001BMI)                                             | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 46                                              | HF: Prevalence (CVD001HF)                                                          | ✔️      |     |          | ✔️     | ✔️  | ✔️      |
| 47                                              | HF: Monitoring with eGFR (CVD002HF)                                                | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 48                                              | CHD: Treated to BP threshold (CVDP002CHD)                                          | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 49                                              | Stroke: Treatment to BP threshold (CVDP002STRK)                                    | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 50                                              | AF: Low risk AF with recorded CHA2DS2-VASc score (CVDP004AF)                       | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 51                                              | AF: Treated with anticoagulants - DOAC prioritised (CVDP005AF)                     | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 52                                              | Hypertension: Monitoring with ACR (CVDP009HYP)                                     | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 53                                              | Cholesterol: CVD cholesterol monitoring (CVDP011CHOL)                              | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |
| 54                                              | Cholesterol: CVD treated to cholesterol threshold (CVDP012CHOL)                    | ✔️      | ✔️  | ✔️       | ✔️     | ✔️  | ✔️      |

What this does:

- Retrieve all system levels and time periods

- Filter the system levels available in *time period 26*

- For each system level, collect the list of indicators

- Pivot the results into a table that clearly shows which indicators are
  available at each system level

The resulting table provides a quick overview of indicator coverage
across system levels, making it easy to spot where gaps exist.

## Listing metrics

Metrics provide **breakdowns of indicator performance** by demographic
categories such as sex, age group or ethnicity. They allow you to
explore how an indicator varies across different population subgroups.

The function
[`cvd_indicator_metric_list()`](https://craig-parylo.github.io/cvdprevent/reference/cvd_indicator_metric_list.md)
retrieves metrics for all indicators within a specified *time period*
and *system level*.

The example below shows how to list all metrics available for the
indicator **AF: Prevalence (CVDP01AF)** (Indicator ID = 1) in *time
period 26* at the *England* system level:

``` r
cvd_indicator_metric_list(time_period_id = 26, system_level_id = 1) |>
  dplyr::filter(IndicatorID == 1) |>
  dplyr::select(dplyr::any_of(c(
    "MetricCategoryTypeName",
    "Indicator",
    "MetricCategoryName",
    "CategoryAttribute",
    "MetricID"
  ))) |>
  dplyr::group_by(MetricCategoryTypeName) |>
  dplyr::arrange(MetricCategoryName, .by_group = TRUE) |>
  gt::gt(row_group_as_column = TRUE) |>
  gt::tab_options(quarto.disable_processing = TRUE) |>
  gt::tab_stubhead(label = "MetricCategoryTypeName") |>
  gt::tab_header(
    title = gt::md(
      "**Metrics** for *Indicator 1 (AF: Prevalence)* in *Time Period 26* and for the *England* system level"
    )
  )
```

| **Metrics** for *Indicator 1 (AF: Prevalence)* in *Time Period 26* and for the *England* system level |                    |                   |          |
|-------------------------------------------------------------------------------------------------------|--------------------|-------------------|----------|
| MetricCategoryTypeName                                                                                | MetricCategoryName | CategoryAttribute | MetricID |
| Age group                                                                                             | 18-39              | Male              | 177      |
|                                                                                                       | 18-39              | Female            | 180      |
|                                                                                                       | 18-39              | Persons           | 186      |
|                                                                                                       | 40-59              | Female            | 173      |
|                                                                                                       | 40-59              | Male              | 175      |
|                                                                                                       | 40-59              | Persons           | 187      |
|                                                                                                       | 60-79              | Female            | 176      |
|                                                                                                       | 60-79              | Male              | 179      |
|                                                                                                       | 60-79              | Persons           | 188      |
|                                                                                                       | 80+                | Male              | 174      |
|                                                                                                       | 80+                | Female            | 178      |
|                                                                                                       | 80+                | Persons           | 189      |
| Deprivation quintile                                                                                  | 1 - most deprived  | Persons           | 182      |
|                                                                                                       | 2                  | Persons           | 183      |
|                                                                                                       | 3                  | Persons           | 181      |
|                                                                                                       | 4                  | Persons           | 184      |
|                                                                                                       | 5 - least deprived | Persons           | 185      |
| Deprivation quintile - Age Standardised                                                               | 1 - most deprived  | Persons           | 863      |
|                                                                                                       | 2                  | Persons           | 864      |
|                                                                                                       | 3                  | Persons           | 865      |
|                                                                                                       | 4                  | Persons           | 866      |
|                                                                                                       | 5 - least deprived | Persons           | 867      |
| Sex                                                                                                   | Female             | Female            | 171      |
|                                                                                                       | Male               | Male              | 172      |
|                                                                                                       | Persons            | Persons           | 190      |
| Sex - Age Standardised                                                                                | Female             | Female            | 862      |
|                                                                                                       | Male               | Male              | 861      |
|                                                                                                       | Persons            | Persons           | 868      |

**Understanding Metric Categories**

*Common categories* include:

- Age group

- Deprivation quintile

- Sex

*Age-standardised metrics*: for some indicators, age-standardised
versions of these categories are also available.

*Sex disaggregation*: certain metrics are further broken down by sex.
For example, in the table above, the age group *18-39* is reported both
for all persons in that group (*CategoryAttribute = Person*) and
separately for *Male* and *Female* subgroups.

## Summary

Together, these terms form the foundation of the CVDPrevent API and
provide a consistent language for working with cardiovascular data. By
clearly defining system levels, areas, time periods, indicators and
metrics the API enables users to navigate complex datasets with
confidence, compare performance across different contexts and explore
meaningful demographic breakdowns. Understanding these concepts is an
essential first step before making use of the wider functionality
demonstrated in this vignette.

------------------------------------------------------------------------

1.  See
    <https://navigator.health.org.uk/theme/sustainability-and-transformation-plans-later-partnerships>
    for more details about the evolution of STPs
