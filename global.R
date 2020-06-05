library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinymanager)
library(here)
library(lubridate)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(DT)
library(shinyjs)

users_sqlite <- here("data/users.sqlite")

datasets <- list(
  name      = "SOE 2020 Contributors (copy 2020-04-24)",
  url_csv   = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/gviz/tq?tqx=out:csv&sheet=0",
  url_edit  = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/edit",
  local_csv = "data/datasets.csv")

if (!file.exists(users_sqlite)){
  
  users <- tribble(
    ~user, ~password,        ~start,       ~expire, ~admin,          ~comment,
    "bb",         "",  "2019-04-15",  "2020-12-31",      F,    "user example",
    "mgr",     "pwd",  "2019-04-15",            NA,      T, "manager example") %>% 
    as.data.frame()
  
  create_db(
    credentials_data = users,
    sqlite_path = users_sqlite,
    passphrase = "supersecret")
}

if (!file.exists(datasets$local_csv)){
  read_csv(datasets$url_csv) %>% 
    write_csv(datasets$local_csv)
}
datasets <- read_csv(datasets$local_csv)

# get_datasets_local <- reactiveFileReader(
#   intervalMillis = 1000,  
#   NULL,
#   filePath = datasets$local_csv,
#   readFunc = read_csv)
