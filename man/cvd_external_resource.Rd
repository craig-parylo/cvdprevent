% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvd_api_functions.R
\name{cvd_external_resource}
\alias{cvd_external_resource}
\title{External resource}
\usage{
cvd_external_resource()
}
\value{
Tibble of details for external resources
}
\description{
Returns a list of all external resources
}
\details{
CVD Prevent API documentation:
\href{https://bmchealthdocs.atlassian.net/wiki/spaces/CP/pages/317882369/CVDPREVENT+API+Documentation#\%2FexternalResource}{External resources}
}
\examples{
# Here we show the first five external resources:
cvd_external_resource() |>
  dplyr::filter(ExternalResourceID < 10) |>
  dplyr::select(ExternalResourceCategory, ExternalResourceSource, ExternalResourceTitle) |>
  dplyr::group_by(ExternalResourceCategory)
}
\seealso{
\code{\link[=cvd_data_availability]{cvd_data_availability()}}
}
