# source: https://shiny.rstudio.com/gallery/file-upload.html

library(shiny)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(plyr)
library(shinyauthr) # remotes::install_github("paulc91/shinyauthr")
library(shinyjs)

# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE,
  row.names = NULL
)
