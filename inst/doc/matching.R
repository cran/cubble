## ---- include = FALSE---------------------------------------------------------
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

## -----------------------------------------------------------------------------
nsw_map <- ozmaps::abs_ste %>%  
  filter(NAME == "New South Wales") 

nsw <- prcp_aus %>%    
  # subset for New South Wales stations
  filter(between(stringr::str_sub(id, 7, 8), 46, 75)) %>% 
  mutate(automated = stringr::str_detect(name, "aws")) %>%  
  face_temporal(ts) %>% 
  filter(lubridate::month(date) == 1,
         lubridate::year(date) == 2020) %>%  
  face_spatial() %>%  
  filter(!any(is.na(ts$prcp)))
 
ggplot() +
  geom_sf(data = nsw_map, color = "grey", linetype = "dotted") +
  geom_point(data = nsw, aes(x = long, y = lat, color = automated)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude") + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("New Sourth Wales") + 
  coord_sf(xlim = c(141, 154))

## -----------------------------------------------------------------------------
auto <- nsw %>%  filter(automated)
non_auto <- nsw %>%  filter(!automated)

matched <- match_sites(auto, non_auto, temporal_matching = FALSE) 

## -----------------------------------------------------------------------------
matched 

## -----------------------------------------------------------------------------
ggplot() + 
  geom_sf(data = nsw_map) + 
  geom_point(data = matched, 
             aes(x = long, y = lat, color = automated)) + 
  ggrepel::geom_label_repel(
    data = matched %>%  filter(automated),
    aes(x = long, y = lat, label = group)) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("New South Wales") + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_sf(xlim = c(141, 154))

## ----echo = FALSE-------------------------------------------------------------
ts <- matched %>% 
  switch_key(group) %>%  
  face_temporal(ts) %>% 
  unfold(id, automated) 

ts %>% 
  ggplot(aes(x = date, y = prcp, color = automated, group = id)) +
  geom_line() +
  facet_wrap(vars(group)) +
  scale_color_brewer(palette = "Dark2") + 
  scale_x_date(date_labels = "%d") + 
  theme_bw()

## ----echo = FALSE-------------------------------------------------------------
river <- river %>%  mutate(type = "river")

vic <- prcp_aus %>%  
  # subset for Victoria stations
  filter(between(stringr::str_sub(id, 7, 8), 76, 90)) %>%  
  face_temporal() %>%  
  filter(lubridate::year(date) == 2020) %>%  
  face_spatial() %>%  
  mutate(type = "climate")

vic_map <- ozmaps::abs_ste %>%  filter(NAME == "Victoria")

ggplot() + 
  geom_sf(data = vic_map) + 
  geom_point(data = dplyr::bind_rows(river, vic), 
             aes(x = long, y = lat, color = type)) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude") + 
  scale_color_brewer(palette = "Dark2")

## ----echo = FALSE-------------------------------------------------------------
set.seed(123)
dt <- tibble(
  id = factor(c(rep("A", 31), rep("a", 31)), levels = c("A", "a")),
  date = rep(1:31, 2),
  val = c(
    c(rnorm(5), 10, rnorm(7), 5, rnorm(8), 7, rnorm(8)),
    c(
      rnorm(6, sd = 0.5), 7, rnorm(7, sd = 0.5), rnorm(6, sd = 0.5),
      4, rnorm(5, sd = 0.5), 6, rnorm(4, sd = 0.5)
    )
  )
) %>%  mutate(val = ifelse(val < 0, 0, val))

circle <- tibble(
  x = c(6, 14, 23, 7, 21, 27),
  y = c(10, 5, 7, 7, 4, 6),
  id = factor(c(rep("A", 3), rep("a", 3)), levels = c("A", "a")),
  xend = x + 5,
  match = factor(c("yes", "no", rep("yes", 2), "no", "yes"), levels = c("yes", "no"))
) 

errorbar <- bind_rows(
  circle %>%  filter(id == "A"),
  circle %>%  filter(id == "A") %>%  mutate(id = "a")
) %>% 
  mutate(id = factor(id, c("A", "a")))

ggplot() +
  geom_rect(data = errorbar, aes(xmin = x, xmax = xend, ymin = 0, ymax = 10), fill = "grey90") +
  geom_line(data = dt, aes(x = date, y = val, group = id), color = "black") +
  geom_vline(data = circle %>%  filter(id == "A"), 
             aes(xintercept = x), linetype = "longdash", color = "grey10", lwd = 0.2) +
  geom_point(data = circle, aes(x = x, y = y, color = match), size = 3) +
  facet_wrap(vars(id), nrow = 2) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  scale_x_continuous(breaks = seq(1, 31, 1)) +
  scale_color_brewer(palette = "Dark2") + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom") +
  labs(x = "Time", y = "Value")


## -----------------------------------------------------------------------------
res <- match_sites(river, vic,
                   temporal_by = c("Water_course_level" = "prcp"),
                   temporal_independent = "prcp",
                   temporal_n_highest = 30,
                   temporal_min_match = 15)

## -----------------------------------------------------------------------------
res

## -----------------------------------------------------------------------------
ggplot() + 
  geom_sf(data = vic_map) + 
  geom_point(data = res, 
             aes(x = long, y = lat, color = type)) + 
  ggrepel::geom_label_repel(data = res %>%  filter(type == "river"),
                            aes(x = long, y = lat, label = group)) + 
  scale_color_brewer(palette = "Dark2") + 
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "Longitude", y = "Latitude") + 
  ggtitle("Victoria") 

## -----------------------------------------------------------------------------
res_long <- res %>%  
  face_temporal(ts) %>%  
  unfold(group, type) %>%  
  rename(prcp = matched_var) %>%  
  mutate(prcp = (prcp - min(prcp, na.rm = TRUE))/ (max(prcp, na.rm = TRUE) - min(prcp, na.rm = TRUE))) 

res_long %>%  
  ggplot(aes(x = date, y = prcp, group = type,color = type)) + 
  geom_line() + 
  facet_wrap(vars(group)) + 
  scale_color_brewer(palette = "Dark2", guide = "none") + 
  theme_bw() + 
  labs(x=  "date") + 
  scale_x_date(date_labels = "%b") + 
  labs(x = "Week", y = "Precipitation/ water level")

