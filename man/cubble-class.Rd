% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_cubble.R, R/cubble-print.R, R/cubble-s3.R
\name{as_cubble}
\alias{as_cubble}
\alias{as_cubble.list}
\alias{as_cubble.tbl_df}
\alias{as_cubble.rowwise_df}
\alias{tbl_sum.cubble_df}
\alias{is_cubble}
\alias{cubble}
\title{The constructor for the cubble class}
\usage{
as_cubble(data, key, index, coords, ...)

\method{as_cubble}{list}(data, key, index, coords, output = "all", ...)

\method{as_cubble}{tbl_df}(data, key, index, coords, ...)

\method{as_cubble}{rowwise_df}(data, key, index, coords, ...)

\method{tbl_sum}{cubble_df}(data)

is_cubble(data)

cubble(..., key, index, coords)
}
\arguments{
\item{data}{the object to be created or tested as cubble}

\item{key}{the spatial identifier}

\item{index}{the time identifier}

\item{coords}{the coordinates that characterise the spatial dimension}

\item{...}{a list object to create new cubble}

\item{output}{either "all" or "unmatch", whether to output all or a list of unmatched summary}
}
\value{
a cubble object

a cubble object

a TRUE/FALSE predicate

a cubble object
}
\description{
The constructor for the cubble class
}
\examples{
# Declaimer: to make the examples easier, here we first `climate_flat` into
# different classes and show how they can be casted into a cubble. This is to
# demonstrate if your data come in one of the classes, it can be directly cast
# into a cubble. By no mean you need to first transform your data into any of
# the following class and then cast it to cubble.

# If the data is in a tibble:
climate_flat \%>\%  as_cubble(key = id, index = date, coords = c(long, lat))

# If the spatial and temporal information are in two separate tables:
library(dplyr)
spatial <- climate_flat \%>\%  select(id:wmo_id) \%>\%  distinct()
temporal <- climate_flat \%>\%  select(id, date: tmin) \%>\%  filter(id != "ASN00009021")
as_cubble(data = list(spatial = spatial, temporal = temporal),
          key = id, index = date, coords = c(long, lat))

# If the data is already in a rowwise_df:
dt <- climate_flat \%>\%
  tidyr::nest(ts = date:tmin) \%>\%
  dplyr::rowwise()
dt \%>\%  as_cubble(key = id, index = date, coords = c(long, lat))

# If the data is already in a tsibble, only need to supply `coords`
dt <- climate_flat \%>\%  tsibble::as_tsibble(key = id, index = date)
dt \%>\%  as_cubble(coords = c(long, lat))

# If the data is in netcdf:
path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
raw <- ncdf4::nc_open(path)
dt <- as_cubble(raw, vars = c("q", "z"))
}
