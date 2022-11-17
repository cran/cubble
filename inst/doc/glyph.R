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

## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("cluster-diagram/glyph-steps.png")

## ----echo = FALSE-------------------------------------------------------------
OUTPUT_FROM_GET_STARTED <- tmax_hist %>% 
  filter(between(as.numeric(stringr::str_sub(id, 7, 8)), 76, 90)) %>% 
  face_temporal() %>% 
  mutate(
    month = lubridate::month(yearmonth),
    group = as.factor(ifelse(lubridate::year(yearmonth) > 2015, "2016 ~ 2020","1971 ~ 1975"))) %>%  
  group_by(month, group) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE)) %>% 
  face_spatial() %>%  
  filter(nrow(ts) == 24) %>% 
  face_temporal() %>%  
  unfold(latitude, longitude)

## -----------------------------------------------------------------------------
vic_map <- ozmaps::abs_ste %>%  filter(NAME %in% c("Victoria"))
OUTPUT_FROM_GET_STARTED %>% 
  ggplot()  + 
  geom_sf(data = vic_map, 
          fill = "grey95", color = "white",
          inherit.aes = FALSE) + 
  geom_glyph(
    aes(x_major = longitude, y_major = latitude, 
        x_minor = month, y_minor = tmax, 
        group = interaction(id, group), color = group), 
    width = 0.8, height = 0.3) + 
  scale_color_brewer(palette = "Dark2") + 
  coord_sf(xlim = c(141, 150)) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Longitude", y = "Latitude")

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  # script for diagram
#  library(tidyverse)
#  library(ggsvg)
#  library(patchwork)
#  nsw <- ozmaps::abs_ste %>% filter(NAME %in% c("New South Wales")) %>% sf::st_simplify(dTolerance = 4000)
#  
#  single <- climate_aus %>% filter(id == "ASN00076031")
#  glyph_dt <- single %>% face_temporal() %>% unfold(long, lat)
#  p1 <- ggplot() +
#    geom_sf(data = nsw,fill = "transparent", linetype = "dotted")+
#    geom_point(data = single, aes(x = long, y = lat), color = "#443750") +
#    theme_bw() +
#    coord_sf(xlim = c(141, 143), ylim = c(-35, -33.5)) +
#    scale_x_continuous(breaks = seq(140, 143, 1)) +
#    scale_y_continuous(breaks = seq(-35, -33, 1)) +
#    ggtitle("(1)")
#  
#  p2 <- single %>%
#    face_temporal() %>%
#    ggplot(aes(x = date, y = tmax)) +
#    geom_line(color = "#443750") +
#    theme_bw() +
#    theme() +
#    ggtitle("(2)")
#  
#  glyph <- glyph_dt %>%
#    ggplot(aes(x_major = long, x_minor = as.numeric(date),
#               y_major = lat, y_minor = tmax)) +
#    geom_glyph(width = 1, height = 0.3)
#  
#  p3 <- layer_data(glyph) %>%
#    ggplot(aes(x = x, y = y)) +
#    geom_line(color = "#443750") +
#    theme_bw() +
#    theme(axis.line = element_line(color = "#840032"),
#          axis.text = element_text(color = "#840032", size = 10),
#    ) +
#    ggtitle("(3)") + xlab("long") + ylab("lat")
#  
#  p4 <- glyph_dt %>%
#    ggplot(aes(x_major = long, x_minor = as.numeric(date),
#               y_major = lat, y_minor = tmax)) +
#    geom_sf(data = nsw, fill = "transparent", linetype = "dotted", inherit.aes = FALSE) +
#    geom_glyph_box(width = 1, height = 0.3, color= "#840032", size = 1.2) +
#    geom_glyph(color = "#443750", width = 1, height = 0.3) +
#    geom_point(data = single, aes(x = long, y = lat), color = "#443750", inherit.aes = FALSE) +
#    theme_bw() +
#    coord_sf(xlim = c(141, 143), ylim = c(-35, -33.5)) +
#    scale_x_continuous(breaks = seq(140, 143, 1)) +
#    scale_y_continuous(breaks = seq(-35, -33, 1)) +
#    ggtitle("(4)") + xlab("long") + ylab("lat")
#  
#  g2 <- (p1 | p2) / (p4 | p3) + plot_layout(guides='collect') &
#    theme(legend.position='none')
#  
#  ggsave(g2, filename = here::here("vignettes/cluster-diagram/glyph-steps.png"), height = 4)

