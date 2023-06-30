## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(cubble)
library(tsibble)
library(sf)

## ----echo = TRUE--------------------------------------------------------------
ts_nested <- make_cubble(
  spatial = stations, temporal = meteo_ts, coords = c(long, lat))
(ts_long <- face_temporal(ts_nested))
class(ts_long)

## ----echo = TRUE--------------------------------------------------------------
ts_long %>% has_gaps()

## ----echo = TRUE--------------------------------------------------------------
ts_long2 <- make_cubble(
  stations, meteo, 
  key = id, index = date, coords = c(long, lat)) %>% 
  face_temporal() %>% 
  make_temporal_tsibble() 
identical(ts_long2, ts_long)

## ----echo = TRUE--------------------------------------------------------------
(sf_nested <- make_cubble(
  spatial = stations_sf, temporal = meteo, 
  key = id, index = date))
class(sf_nested)

## ----echo =TRUE, message=FALSE------------------------------------------------
sf_nested %>% sf::st_transform(crs = "EPSG:3857")

## ----echo = TRUE--------------------------------------------------------------
(sf_nested2 <- make_cubble(
  stations, meteo, 
  key = id, index = date, coords = c(long, lat)) %>% 
  make_spatial_sf())

