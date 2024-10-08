% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvd_api_functions.R
\name{cvd_indicator_person_timeseries}
\alias{cvd_indicator_person_timeseries}
\title{Indicator persons time series by indicator}
\usage{
cvd_indicator_person_timeseries(indicator_id = 1, area_id = 1)
}
\arguments{
\item{indicator_id}{integer - the indicator to return data for (compulsory)}

\item{area_id}{integer - the area to return data for (compulsory)}
}
\value{
Tibble of metric performance for the specified indicator in the area
}
\description{
Returns data for the Inequalities Markers Time Series chart for the provided
indicator ID and area ID. \code{Data} contains information about the chosen
target value as well as an array \code{InequalityMarkers} which contains all the
time series data grouped into metric category types e.g. age group,
ethnicity, etc.
}
\details{
CVD Prevent API documentation:
\href{https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#\%2Findicator\%2FpersonsTimeSeriesByIndicator\%2F\%3Cindicator_ID\%3E}{Indicator person time series}
}
\examples{
# View the details of the time-series performance for indicator 'AF:
# treatment with anticoagulants' (ID 7) in Salford South East PCN (area ID
# 705), focussed just on the age group inequalities metrics:
cvd_indicator_person_timeseries(indicator_id = 7, area_id = 705) |>
  dplyr::filter(
    MetricCategoryTypeName == 'Age group',
    !is.na(Value)
  ) |>
  dplyr::select(MetricCategoryName, TimePeriodName, TimePeriodID, Value) |>
  tidyr::pivot_wider(
    names_from = MetricCategoryName,
    values_from = Value
  )
}
\seealso{
\code{\link[=cvd_indicator_list]{cvd_indicator_list()}}, \code{\link[=cvd_indicator_metric_list]{cvd_indicator_metric_list()}}, \code{\link[=cvd_indicator]{cvd_indicator()}},
\code{\link[=cvd_indicator_tags]{cvd_indicator_tags()}}, \code{\link[=cvd_indicator_details]{cvd_indicator_details()}}, \code{\link[=cvd_indicator_sibling]{cvd_indicator_sibling()}},
\code{\link[=cvd_indicator_child_data]{cvd_indicator_child_data()}}, \code{\link[=cvd_indicator_data]{cvd_indicator_data()}}, \code{\link[=cvd_indicator_metric_data]{cvd_indicator_metric_data()}},
\code{\link[=cvd_indicator_raw_data]{cvd_indicator_raw_data()}}, \code{\link[=cvd_indicator_nationalarea_metric_data]{cvd_indicator_nationalarea_metric_data()}},
\code{\link[=cvd_indicator_priority_groups]{cvd_indicator_priority_groups()}}, \code{\link[=cvd_indicator_pathway_group]{cvd_indicator_pathway_group()}}, #
\code{\link[=cvd_indicator_group]{cvd_indicator_group()}}, \code{\link[=cvd_indicator_metric_timeseries]{cvd_indicator_metric_timeseries()}},
\code{\link[=cvd_indicator_metric_systemlevel_comparison]{cvd_indicator_metric_systemlevel_comparison()}},
\code{\link[=cvd_indicator_metric_area_breakdown]{cvd_indicator_metric_area_breakdown()}}
}
