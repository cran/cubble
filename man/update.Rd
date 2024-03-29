% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cubble-update.R
\name{update_cubble}
\alias{update_cubble}
\alias{update_cubble.spatial_cubble_df}
\alias{update_cubble.temporal_cubble_df}
\title{Temporary update cubble if the sf class take precedent of cubble classes}
\usage{
update_cubble(data, key, index, coords, ...)

\method{update_cubble}{spatial_cubble_df}(data, key = NULL, index = NULL, coords = NULL, ...)

\method{update_cubble}{temporal_cubble_df}(
  data,
  key = NULL,
  index = NULL,
  coords = NULL,
  spatial = NULL,
  ...
)
}
\arguments{
\item{data, key, index, coords, spatial, ...}{skdflsjkd}
}
\description{
When the data is already a cubble object but need update on attributes
}
