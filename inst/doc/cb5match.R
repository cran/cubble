## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE,
  message = FALSE,
  out.width = "100%"
)
library(cubble)
library(dplyr)
library(ggplot2)
library(patchwork)

## ----echo = FALSE-------------------------------------------------------------
river <- cubble::river |>  mutate(type = "river") |> rename(id = station)

climate_vic <- climate_aus |>  
  # subset for Victoria stations
  filter(between(as.numeric(stringr::str_sub(id, 7, 8)), 76, 90)) |>  
  mutate(type = "climate")

vic_map <- ozmaps::abs_ste |>  filter(NAME == "Victoria")

ggplot() + 
  geom_sf(data = vic_map) + 
  geom_point(data = dplyr::bind_rows(river, climate_vic), 
             aes(x = long, y = lat, color = type)) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude") + 
  scale_color_brewer(palette = "Dark2")

## -----------------------------------------------------------------------------
(res_sp <- match_spatial(climate_vic, river, spatial_n_group = 10))

## -----------------------------------------------------------------------------
res_sp <- match_spatial(climate_vic, river, spatial_n_group = 10, return_cubble = TRUE)
str(res_sp, max.level = 0)
res_sp[[1]]
(res_sp <- res_sp[-c(5, 8)] |> bind_rows())

## -----------------------------------------------------------------------------
(res_tm <- res_sp |> 
  match_temporal(
    data_id = type, match_id = group,
    temporal_by = c("prcp" = "Water_course_level")))

## -----------------------------------------------------------------------------
res_tm <- res_sp |> 
  match_temporal(
    data_id = type, match_id = group,
    temporal_by = c("prcp" = "Water_course_level"),
    return_cubble = TRUE)
(res_tm <- res_tm |> bind_rows() |> filter(group %in% c(1, 7, 6, 9)))

## ----echo = FALSE-------------------------------------------------------------
res_tm_long <- res_tm |>  
  face_temporal() |>  
  unfold(group, type) |>  
  group_by(group, type) |> 
  mutate(matched = (matched - min(matched, na.rm = TRUE))/ 
           (max(matched, na.rm = TRUE) - min(matched, na.rm = TRUE))) 

vic_map <- ozmaps::abs_ste |> 
  filter(NAME == "Victoria")

p1 <-ggplot() + 
  geom_sf(data = vic_map, fill = "grey95", color = "white") + 
  geom_point(data = dplyr::bind_rows(river, climate_vic), 
             aes(x = long, y = lat, color = type), 
             alpha = 0.2, fill = 0.2) +
  geom_point(data = res_tm |> as_tibble(), 
             aes(x = long, y = lat, color = type)) +
  ggrepel::geom_label_repel(
    data = res_tm |>  filter(type == "climate") |> as_tibble(), 
    aes(x = long, y = lat, label = group)) +
  scale_color_brewer(palette = "Dark2")  + 
  theme_void() + 
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "Longitude", y = "Latitude") 

p2 <- res_tm_long |>  
  ggplot(aes(x = date, y = matched, group = type,color = type)) + 
  geom_line() + 
  facet_wrap(vars(group)) + 
  scale_color_brewer(palette = "Dark2", guide = "none") + 
  theme_bw() + 
  labs(x=  "date") + 
  scale_x_date(date_labels = "%b") + 
  labs(x = "Week", y = "Precipitation/ water level")

(p1 | p2) + 
  patchwork::plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom") 

