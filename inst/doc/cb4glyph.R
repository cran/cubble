## ----include = FALSE----------------------------------------------------------
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
library(tsibble)

## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("cluster-diagram/glyph-steps.png")

## -----------------------------------------------------------------------------
set.seed(12345)
(tmax <- climate_aus |> 
    rowwise() |> 
    filter(nrow(ts) == 366) |> 
    slice_sample(n = 80))

## -----------------------------------------------------------------------------
(tmax <- tmax |> 
  face_temporal() |> 
  group_by(month = tsibble::yearmonth(date)) |> 
  summarise(tmax = mean(tmax, na.rm = TRUE)))

## -----------------------------------------------------------------------------
(tmax <- tmax |> unfold(long, lat))

## -----------------------------------------------------------------------------
tmax |> 
  ggplot(aes(x_major = long, y_major = lat, 
             x_minor = month, y_minor = tmax))  + 
  geom_sf(data = ozmaps::abs_ste, 
          fill = "grey95", color = "white",
          inherit.aes = FALSE) + 
  geom_glyph_box(width = 1, height = 0.5) + 
  geom_glyph(width = 1,  height = 0.5) + 
  coord_sf(xlim = c(110, 155)) + 
  theme_void() + 
  theme(legend.position = "bottom") + 
  labs(x = "Longitude", y = "Latitude")

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  # script for diagram
#  library(tidyverse)
#  library(ggsvg)
#  library(patchwork)
#  nsw <- ozmaps::abs_ste |>
#    filter(NAME %in% c("New South Wales")) |>
#    sf::st_simplify(dTolerance = 4000)
#  
#  single <- climate_aus |> filter(id == "ASN00076031")
#  glyph_dt <- single |> face_temporal() |> unfold(long, lat)
#  p1 <- ggplot() +
#    geom_sf(data = nsw,fill = "transparent", linetype = "dotted")+
#    geom_point(data = single, aes(x = long, y = lat), color = "#443750") +
#    theme_bw() +
#    coord_sf(xlim = c(141, 143), ylim = c(-35, -33.5)) +
#    scale_x_continuous(breaks = seq(140, 143, 1)) +
#    scale_y_continuous(breaks = seq(-35, -33, 1)) +
#    ggtitle("(1)")
#  
#  p2 <- single |>
#    face_temporal() |>
#    ggplot(aes(x = date, y = tmax)) +
#    geom_line(color = "#443750") +
#    theme_bw() +
#    theme() +
#    ggtitle("(2)")
#  
#  glyph <- glyph_dt |>
#    ggplot(aes(x_major = long, x_minor = as.numeric(date),
#               y_major = lat, y_minor = tmax)) +
#    geom_glyph(width = 1, height = 0.3)
#  
#  p3 <- layer_data(glyph) |>
#    ggplot(aes(x = x, y = y)) +
#    geom_line(color = "#443750") +
#    theme_bw() +
#    theme(axis.line = element_line(color = "#840032"),
#          axis.text = element_text(color = "#840032", size = 10),
#    ) +
#    ggtitle("(3)") + xlab("long") + ylab("lat")
#  
#  p4 <- glyph_dt |>
#    ggplot(aes(x_major = long, x_minor = as.numeric(date),
#               y_major = lat, y_minor = tmax)) +
#    geom_sf(data = nsw, fill = "transparent",
#            linetype = "dotted", inherit.aes = FALSE) +
#    geom_glyph_box(width = 1, height = 0.3, color= "#840032", size = 1.2) +
#    geom_glyph(color = "#443750", width = 1, height = 0.3) +
#    geom_point(data = single, aes(x = long, y = lat),
#               color = "#443750", inherit.aes = FALSE) +
#    theme_bw() +
#    coord_sf(xlim = c(141, 143), ylim = c(-35, -33.5)) +
#    scale_x_continuous(breaks = seq(140, 143, 1)) +
#    scale_y_continuous(breaks = seq(-35, -33, 1)) +
#    ggtitle("(4)") + xlab("long") + ylab("lat")
#  
#  g2 <- (p1 | p2) / (p4 | p3) + plot_layout(guides='collect') &
#    theme(legend.position='none')
#  
#  ggsave(g2,
#         filename = here::here("vignettes/cluster-diagram/glyph-steps.png"),
#         height = 4)

