## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE,
  message = FALSE
)

library(cubble)
library(dplyr)

## ----echo = FALSE-------------------------------------------------------------
syd_all <- climate_aus %>% slice_nearby(coord = c(151.207, -33.867), n = 5)
syd <- syd_all %>% select(id:wmo_id)

## -----------------------------------------------------------------------------
syd

## ----eval = FALSE-------------------------------------------------------------
#  raw <- syd %>%
#    rowwise() %>%
#    mutate(ts = list(rnoaa::meteo_pull_monitors(id,
#                                         date_min = "2020-01-01",
#                                         date_max = "2020-12-31",
#                                         var = c("PRCP", "TMAX", "TMIN")) %>%  select(-id)))
#  raw

## ----echo = FALSE-------------------------------------------------------------
raw <- syd_all %>% select(-c(long_ref, lat_ref, dist)) %>% rowwise()
raw

## -----------------------------------------------------------------------------
syd_climate <- raw %>%  
  as_cubble(key = id, index = date, coords = c(long, lat))

syd_climate

## ----echo = FALSE-------------------------------------------------------------
dt <- climate_flat %>%  
  tsibble::as_tsibble(key = id, index = date)

## -----------------------------------------------------------------------------
dt
dt %>%  as_cubble(coords = c(long, lat))

## -----------------------------------------------------------------------------
# a spatial sheet
cubble::stations

# a temporal sheet
cubble::climate

## -----------------------------------------------------------------------------
as_cubble(list(spatial = cubble::stations, temporal = cubble::climate),
          key = id, index = date, coords = c(long, lat))

## -----------------------------------------------------------------------------
path <- system.file("ncdf/era5-pressure.nc", package = "cubble")
raw <- ncdf4::nc_open(path)
raw

## -----------------------------------------------------------------------------
dt <- as_cubble(raw, vars = c("q", "z"))
dt

## -----------------------------------------------------------------------------
dt <- as_cubble(raw, vars = c("q", "z"),
                long_range = seq(-180, 180, 1),
                lat_rnage = seq(-90, -5, 1))
dt

## -----------------------------------------------------------------------------
# create a toy stars object
m <- array(1:60, dim = c(x= 5, y = 4, t = 3))
time = 1:3
library(units)
units(time) = as_units("days since 2015-01-01")
m_dim <- stars::st_dimensions(x =  seq(146, 162, 4), y = seq(-44, -41, 1), t = time)
st <- stars::st_as_stars(list(m = m, m2 = m), dimensions = m_dim)
st
as_cubble(st)

