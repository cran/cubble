## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE, 
  warning = FALSE,
  out.width = "100%"
)

## ----setup, echo = FALSE------------------------------------------------------
library(dplyr)
library(cubble)
library(ggplot2)

## -----------------------------------------------------------------------------
tmax <- tmax_hist %>% 
  filter(between(as.numeric(stringr::str_sub(id, 7, 8)), 76, 90))

tmax

## -----------------------------------------------------------------------------
tmax <-tmax %>% 
    face_temporal() %>% 
    mutate(
        month = lubridate::month(yearmonth),
        group = as.factor(ifelse(lubridate::year(yearmonth) > 2015, "2016 ~ 2020","1971 ~ 1975"))) %>%
  group_by(month, group)%>%  
  summarise(tmax = mean(tmax, na.rm = TRUE))

tmax

## -----------------------------------------------------------------------------
tmax <- tmax %>%  face_spatial() %>%  filter(nrow(ts) == 24)
tmax

## -----------------------------------------------------------------------------
tmax <- tmax %>%  face_temporal() %>%  unfold(latitude, longitude)
tmax

