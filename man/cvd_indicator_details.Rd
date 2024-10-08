% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvd_api_functions.R
\name{cvd_indicator_details}
\alias{cvd_indicator_details}
\title{Indicator details}
\usage{
cvd_indicator_details(indicator_id = 1)
}
\arguments{
\item{indicator_id}{integer - the ID for the indicator (compulsory)}
}
\value{
Tibble of details for the specified indicator
}
\description{
Returns details of a single indicator
}
\details{
CVD Prevent API documentation:
\href{https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#\%2Findicator\%2F\%3Cindicator_ID\%3E\%2Fdetails}{Indicator details}
}
\examples{
cvd_indicator_details(indicator_id = 7) |>
  dplyr::select(IndicatorID, MetaDataTitle, MetaData) |>
  dplyr::slice_head(n=5)
}
\seealso{
\code{\link[=cvd_indicator_list]{cvd_indicator_list()}}, \code{\link[=cvd_indicator_metric_list]{cvd_indicator_metric_list()}}, \code{\link[=cvd_indicator]{cvd_indicator()}},
\code{\link[=cvd_indicator_tags]{cvd_indicator_tags()}}, \code{\link[=cvd_indicator_sibling]{cvd_indicator_sibling()}},
\code{\link[=cvd_indicator_child_data]{cvd_indicator_child_data()}}, \code{\link[=cvd_indicator_data]{cvd_indicator_data()}}, \code{\link[=cvd_indicator_metric_data]{cvd_indicator_metric_data()}},
\code{\link[=cvd_indicator_raw_data]{cvd_indicator_raw_data()}}, \code{\link[=cvd_indicator_nationalarea_metric_data]{cvd_indicator_nationalarea_metric_data()}},
\code{\link[=cvd_indicator_priority_groups]{cvd_indicator_priority_groups()}}, \code{\link[=cvd_indicator_pathway_group]{cvd_indicator_pathway_group()}}, #
\code{\link[=cvd_indicator_group]{cvd_indicator_group()}}, \code{\link[=cvd_indicator_metric_timeseries]{cvd_indicator_metric_timeseries()}},
\code{\link[=cvd_indicator_person_timeseries]{cvd_indicator_person_timeseries()}}, \code{\link[=cvd_indicator_metric_systemlevel_comparison]{cvd_indicator_metric_systemlevel_comparison()}},
\code{\link[=cvd_indicator_metric_area_breakdown]{cvd_indicator_metric_area_breakdown()}}
}
