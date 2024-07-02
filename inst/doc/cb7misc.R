## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(cubble)
library(dplyr)

## -----------------------------------------------------------------------------
climate_mel |> 
  face_temporal() |> 
  summarise(tmax_avg = mean(tmax, na.rm=TRUE))

## -----------------------------------------------------------------------------
climate_mel |> 
  rowwise() |> 
  mutate(tmax_avg = mean(ts$tmax, na.rm=TRUE))

