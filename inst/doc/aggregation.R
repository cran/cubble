## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE, 
  message = FALSE,
  out.width = "100%"
)
library(dplyr)
library(cubble)
library(ggplot2)
library(sf)

## -----------------------------------------------------------------------------
state_map <- st_simplify(ozmaps::abs_ste, dTolerance = 4000)
ggplot() + 
  geom_sf(data = state_map, inherit.aes = FALSE,
          color = "grey80", alpha = 0.4, linetype = 3) +
  geom_point(data = prcp_aus , aes(x = long, y = lat)) + 
  theme_void()

## -----------------------------------------------------------------------------
prcp_aus

## ----echo = FALSE-------------------------------------------------------------
coords <- cbind(prcp_aus$long, prcp_aus$lat)
dist_raw <- geosphere::distm(coords, coords)

set.seed(123)
station_nested <- prcp_aus %>%  
  strip_rowwise() %>%  
  mutate(cluster = kmeans(dist_raw,centers = 20, nstart = 500)$cluster)

## -----------------------------------------------------------------------------
station_nested

## -----------------------------------------------------------------------------
cluster_nested <- station_nested %>%  switch_key(cluster)
cluster_nested %>%  head(5)

## -----------------------------------------------------------------------------
(cluster_nested <- cluster_nested %>%  get_centroid())

## -----------------------------------------------------------------------------
(cluster_long <- cluster_nested %>%  face_temporal(ts))

## ----echo = FALSE, out.height="110%"------------------------------------------
knitr::include_graphics("cluster-diagram/cluster-diagram.png")

## ----echo = FALSE-------------------------------------------------------------
cluster_long <- cluster_nested %>%  
  face_temporal() %>%  
  group_by(wk) %>%  
  summarise(prcp = mean(prcp, na.rm = TRUE)) %>%  
  unfold(cent_long, cent_lat)

state_map <- st_simplify(ozmaps::abs_ste, dTolerance = 4000)

ggplot_smooth <- cluster_long %>%  
  ggplot() +
  geom_smooth(aes(x = wk, y = prcp, group = cluster), span = 0.4) 

smoother <- layer_data(ggplot_smooth) %>%  
  left_join(cluster_long %>%  select(cluster, cent_long, cent_lat), by = c("group" = "cluster"))

ggplot(data = smoother, 
       aes(x_minor = x, y_minor = y, 
           x_major = cent_long, y_major = cent_lat)) + 
  geom_sf(data = state_map, inherit.aes = FALSE, 
          color = "grey80", alpha = 0.4, linetype = 3) + 
  geom_text(data = cluster_nested, 
            aes(x = cent_long, y = cent_lat, label = cluster), 
            inherit.aes = FALSE) +
  geom_glyph(height = 2, width = 4) + 
  theme_void()

## -----------------------------------------------------------------------------
ggplot() + 
  geom_sf(data = state_map, inherit.aes = FALSE,
          color = "grey80", alpha = 0.4, linetype = 3) +
  geom_point(data = station_nested, aes(x = long, y = lat), size = 0.5) +
  ggforce::geom_mark_hull(
    data = cluster_nested %>% tidyr::unnest(hull),
    expand = 0, radius = 0,
    aes(x = long, y = lat, group = cluster)) + 
  theme_void()

