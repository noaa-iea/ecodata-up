if (!require(librarian)){
  # https://github.com/DesiQuintans/librarian/issues/21
  if (!require(remotes))
    install.packages("remotes")
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}

shelf(
  # shiny
  shiny, shinydashboard, shinydashboardPlus, shinyjs,
  # google login
  gargle,
  # utilities
  here, glue, fs, digest,
  # tidyverse
  lubridate, stringr, readr, dplyr, tidyr, tibble, purrr,
  # viz
  DT)

datasets <- list(
  name      = "SOE 2020 Contributors (copy 2020-04-24)",
  url_csv   = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/gviz/tq?tqx=out:csv&sheet=0",
  url_edit  = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/edit",
  local_csv = "data/datasets.csv")
