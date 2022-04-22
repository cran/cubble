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
  filter(between(stringr::str_sub(id, 7, 8), 76, 90))

tmax

## -----------------------------------------------------------------------------
tmax <- tmax %>% 
  face_temporal() %>% 
  group_by(
    month = lubridate::month(yearmonth),
    group = as.factor(ifelse(lubridate::year(yearmonth) > 2015, "2016 ~ 2020","1971 ~ 1975"))) %>%  
  summarise(tmax = mean(tmax, na.rm = TRUE))

tmax

## -----------------------------------------------------------------------------
tmax <- tmax %>%  face_spatial() %>%  filter(nrow(ts) == 24)
tmax

## -----------------------------------------------------------------------------
tmax <- tmax %>%  face_temporal() %>%  unfold(latitude, longitude)
tmax

## -----------------------------------------------------------------------------
nsw_vic <- ozmaps::abs_ste %>%  filter(NAME %in% c("Victoria"))

ggplot() + 
  geom_sf(data = nsw_vic, fill = "transparent", color = "grey", linetype = "dotted") + 
  geom_glyph(data = tmax, 
             aes(x_major = longitude, x_minor = month, 
                 y_major = latitude, y_minor = tmax,
                 group = interaction(id, group), color = group),
             width = 1, height = 0.4) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() + 
  coord_sf(xlim = c(141, 150)) + 
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")


