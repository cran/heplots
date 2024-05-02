## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.height=5,
  fig.width=5,
  # results='hide',
  # fig.keep='none',
  fig.path='fig/datasets-',
  echo=TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
set.seed(1071)
options(width=80, digits=5, continue="  ")
library(heplots)
library(candisc)
library(ggplot2)
library(dplyr)

## ----datasets-----------------------------------------------------------------
library(here)
library(dplyr)
library(tinytable)
#dsets <- read.csv(here::here("extra", "datasets.csv"))  # doesn't work in a vignette
dsets <- read.csv("https://raw.githubusercontent.com/friendly/heplots/master/extra/datasets.csv")
dsets <- dsets |> dplyr::select(-X) |> arrange(tolower(dataset))

# link dataset to pkgdown doc
refurl <- "http://friendly.github.io/heplots/reference/"

dsets <- dsets |>
  mutate(dataset = glue::glue("[{dataset}]({refurl}{dataset}.html)")) 

#tinytable::tt(dsets)
knitr::kable(dsets)


## ----concepts-----------------------------------------------------------------
concepts <- dsets |>
  select(dataset, tags) |>
  tidyr::separate_longer_delim(tags, delim = " ") |>
  arrange(tags, dataset) |>
  summarize(datasets = toString(dataset), .by = tags) |>
  rename(concept = tags)

#tinytable::tt(concepts)
knitr::kable(concepts)

