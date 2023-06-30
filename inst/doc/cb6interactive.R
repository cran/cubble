## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE,
  message = FALSE,
  fig.align = "center"
)

library(cubble)
library(dplyr)
library(ggplot2)
library(leaflet)
library(crosstalk)
library(plotly)

## ----echo = FALSE-------------------------------------------------------------
knitr::include_graphics("cluster-diagram/interactive.png")

## -----------------------------------------------------------------------------
set.seed(123)
climate_smaller <- climate_aus |> head(n = 30) 
(clean <- climate_smaller |>
  face_temporal() |>
  group_by(month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  summarise(
    tmax = mean(tmax, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    diff = mean(tmax - tmin, na.rm = TRUE)
    ) |>
    face_spatial() |>
    rowwise() |> 
    mutate(temp_diff_var = var(ts$diff, na.rm = TRUE))
)


## -----------------------------------------------------------------------------
nested <- clean %>% SharedData$new(~id, group = "cubble")
long <- clean |>
  face_temporal() |>
  unfold(temp_diff_var) |>
  arrange(temp_diff_var) %>% 
  SharedData$new(~id, group = "cubble")

## -----------------------------------------------------------------------------
domain <- clean$temp_diff_var
pal <- colorNumeric(
  colorspace::sequential_hcl(
    "Rocket",  n = 7, cmax = 90, rev = TRUE, c2 = 40, l2= 85, c1 = 20, l1 = 30),
  domain = domain)

map <- leaflet(nested, width = 300, height = 300) |>
  addTiles() |>
  addCircleMarkers(color = ~pal(domain), group = "a", radius = 0.1,
                   popup = ~name, fillOpacity = 1, opacity = 1)

## -----------------------------------------------------------------------------
ts_static <- long %>% 
  ggplot(aes(x = month, group = id,
         fill = temp_diff_var, color = temp_diff_var
         )) +
  geom_ribbon(aes(ymin = tmin, ymax = tmax), size = 0.1, alpha = 0.3) +
  # geom_point(aes(y = tmax), size = 0.1) +
  # geom_point(aes(y = tmin), size = 0.1) +
  colorspace::scale_fill_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
    c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
  colorspace::scale_colour_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
    c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
  labs(x = "Month", y = "Temperature") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "bottom"
    )

## -----------------------------------------------------------------------------
ts_interactive <- ggplotly(ts_static, width = 600, height = 300) %>% 
    highlight(on = "plotly_selected", opacityDim = 0.012)

## ----eval = FALSE-------------------------------------------------------------
#  bscols(map, ts_interactive, widths = c(4, 6))

## ----echo = FALSE, out.width="150%"-------------------------------------------
knitr::include_graphics("cluster-diagram/interactive-full.png")

## ----echo = FALSE, out.width="150%"-------------------------------------------
knitr::include_graphics("cluster-diagram/selection1.png")

## ----echo = FALSE, out.width="150%"-------------------------------------------
knitr::include_graphics("cluster-diagram/selection2.png")

## ----echo = FALSE, out.width="150%"-------------------------------------------
knitr::include_graphics("cluster-diagram/selection3.png")

