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
latlon_ref <- tibble::tibble(id = c("sydney", "melbourne"),
                         lat = c(-33.8675, -37.8136),
                         long = c(151.2070, 144.9631))

master_list <- climate_aus %>%  
   as_tibble() %>%  
   select(id, lat, long, elev, name, wmo_id)

calc_dist <- function(df_ref, df, n = 5){
  long_ref <- df_ref$long
  lat_ref <- df_ref$lat
  
  df %>%  
    mutate(long_ref = long_ref,
           lat_ref=  lat_ref,
           dist = rnoaa::meteo_spherical_distance(lat, long,lat_ref, long_ref)) %>%  
    slice_min(dist, n = n) %>%  
    mutate(city = df_ref$id) %>%  
    select(-long_ref, -lat_ref)
}

stations <- do.call("rbind", purrr::map(1:nrow(latlon_ref), 
                             ~calc_dist(latlon_ref[.x, ], master_list)))

## -----------------------------------------------------------------------------
stations

## -----------------------------------------------------------------------------
raw <- stations %>%  
  rowwise() %>%  
  mutate(ts = list(rnoaa::meteo_pull_monitors(id, 
                                       date_min = "2020-01-01", 
                                       date_max = "2020-12-31",
                                       var = c("PRCP", "TMAX", "TMIN")) %>%  select(-id))) 
raw

## -----------------------------------------------------------------------------
sydmel_climate <- raw %>%  
  as_cubble(key = id, index = date, coords = c(long, lat))

sydmel_climate

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

