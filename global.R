# source: https://shiny.rstudio.com/gallery/file-upload.html

library(shiny)
library(here)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(kableExtra)
library(ggplot2)
#library(plyr)
library(shinyjs)
library(shinydashboard)
library(ecodata)
#library(shinyauthr) # remotes::install_github("paulc91/shinyauthr")
library(shinymanager) # remotes::install_github("datastorm-open/shinymanager")

datasets_csv <- "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/gviz/tq?tqx=out:csv&sheet=0"
#users_db     <- here("data/users.sqlite")
users_db     <- "/Users/bbest/github/iea-uploader/test_shinymanager/credentials.sqlite"

if (!file.exists(users_db)){
  
  users <- tribble(
    ~user     , ~password, ~start      , ~expire     , ~admin, ~comment         ,
    "user1"   , "pass1"  , "2019-04-15", NA          , F     , "user example"   ,
    "manager1", "pass1"  , NA          , "2020-12-31", T     , "manager example")
  # users <- tribble(
  #   ~user     , ~password, ~admin, ~comment         ,
  #   "user1"   , "pass1"  , F     , "user example"   ,
  #   "manager1", "pass1"  , T     , "manager example")
  
  shinymanager::create_db(
    credentials_data = users,
    sqlite_path      = users_db,
    passphrase       = "supersecret")
}

shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "Please login",
  "Username:" = "Username:",
  "Password:" = "Password:"
)

# datasets
datasets <- read_csv(datasets_csv)

