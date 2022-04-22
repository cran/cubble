## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE, 
  message = FALSE
)

## ----setup, echo = FALSE------------------------------------------------------
library(cubble)
library(dplyr)

## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("cluster-diagram/cubble-design.png")

## -----------------------------------------------------------------------------
climate_small <-  climate_flat %>%  
  filter(date %in% as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04"))) 

nested <- climate_small%>%  
  as_cubble(key = id, index = date, coords = c(long, lat))
nested
attributes(nested)

## -----------------------------------------------------------------------------
nested %>%  
  mutate(rain = sum(ts$prcp != 0, na.rm = TRUE))

## -----------------------------------------------------------------------------
climate_small %>%  
  tidyr::nest(c(date, prcp, tmax, tmin)) %>%  
  mutate(rain = purrr::map_dbl(data, ~sum(.x$prcp != 0, na.rm= TRUE)))

## -----------------------------------------------------------------------------
long <- nested %>%  face_temporal()
long
attributes(long)

## -----------------------------------------------------------------------------
key_vars(long)
key_data(long)
spatial(long)

