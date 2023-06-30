## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE, 
  message = FALSE
)

## ----setup, echo = FALSE------------------------------------------------------
library(cubble)
library(tsibble)
library(tibble)
library(ggplot2)
library(dplyr)
library(tsibble)
library(patchwork)

## ----echo = FALSE-------------------------------------------------------------
cb_nested <- climate_mel

## -----------------------------------------------------------------------------
cb_nested
class(cb_nested)

## ----echo = FALSE-------------------------------------------------------------
cb_long <- climate_mel %>% face_temporal()

## -----------------------------------------------------------------------------
cb_long
class(cb_long)

## -----------------------------------------------------------------------------
attributes(cb_nested)
attributes(cb_long)

