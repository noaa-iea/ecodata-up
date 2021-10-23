# libraries ----
if (!require(librarian)){
  #remotes::install_github("DesiQuintans/librarian")
  install.packages("librarian")
  library(librarian)
}
shelf(
  AICcmodavg,
  colorRamps,
  cowplot,
  dplyr,
  devtools,
  DT,
  NOAA-EDAB/ecodata, # bbest/ecodata, # load_all(dir_ecodata_src) # install_local(dir_ecodata_src)
  fs,
  ggiraph,
  ggplot2,
  ggrepel,
  ggspatial, # plot aquaculture
  glue,
  grid,
  heatwaveR,
  here,
  janitor,
  kableExtra,
  knitr,
  magrittr,
  marmap, # plot aquaculture
  nlme,
  patchwork,
  purrr,
  raster,
  readr,
  readxl,
  rgdal,
  rmarkdown,
  rpart,
  sf,
  shiny, 
  shinyjs,
  stringr,
  tidyr,
  vegan)
select = dplyr::select
source(here("functions.R"))

options(
  shiny.maxRequestSize = 30*1024^2,
  readr.show_col_types = FALSE) # 30MB limit

# variables ----
admin_emails     <- c("ben@ecoquants.com", "kimberly.bastille@noaa.gov", "andrew.beet@noaa.gov")
#dir_ecodata_src  <- "/share/github/ecodata_uploader/ecodata"
dir_uploader     <- here()                                   # TODO: -> dir_up
dir_ecodata_src  <- "/share/github/ecodata_edab"             # TODO: -> dir_ecodata
dir_plotr        <- glue("{dir_ecodata_src}/chunk-scripts")  # TODO: -> dir_plotr

# load_all(dir_ecodata_src) # install_local(dir_ecodata_src)

# datasets <- list(
#   name      = "SOE 2020 Contributors (copy 2020-04-24)",
#   url_csv   = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/gviz/tq?tqx=out:csv&sheet=0",
#   url_edit  = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/edit",
#   local_csv = "data/datasets.csv")

ds_ls <- data(package = "ecodata") %>% .$results %>% .[,3]

not_null <- function(x) !is.null(x)

datasets_all <- tibble(
  dataset_id = ds_ls) %>% 
  mutate(
    data = map(dataset_id, function(dataset_id){
      d <- try(get(dataset_id), silent = T)
      if ("try-error" %in% class(d))
        return(NA)
      d}),
    data_steward = map(data, function(data){
      attr(data, "data_steward")}),
    tech_doc_url = map(data, function(data){
      attr(data, "tech-doc_url")}),
    data_files   = map(data, function(data){
      attr(data, "data_files") %>% 
        unlist()}),
    plot_scripts  = map(data, function(data){
      attr(data, "plot_script") %>% 
        unlist()}),
    has_data = map_lgl(data, function(data){
      if (length(data) == 1 && is.na(data[[1]]))
        return(F)
      T}),
    has_steward = map_lgl(data_steward, not_null),
    has_doc     = map_lgl(tech_doc_url, not_null),
    has_files   = map_lgl(data_files  , not_null),
    has_plots   = map_lgl(plot_scripts, not_null))

datasets_todo <- datasets_all %>% 
  select(dataset_id, starts_with("has")) %>% 
  rowwise() %>% 
  mutate(
    has_missing = any(!has_data, !has_steward, !has_doc, !has_files, !has_plots)) %>% 
  filter(has_missing) %>% 
  select(-has_missing)

datasets_done <- datasets_all %>% 
  anti_join(datasets_todo, by = "dataset_id") %>% 
  select(dataset_id, starts_with("has"))

dataset_stewards <- datasets_all %>% 
  select(dataset_id, data_steward) %>% 
  unnest(data_steward)

dataset_techdocurls <- datasets_all %>% 
  select(dataset_id, tech_doc_url) %>% 
  unnest(tech_doc_url)

dataset_datafiles <- datasets_all %>% 
  select(dataset_id, data_files) %>% 
  unnest(data_files) %>% 
  arrange(dataset_id, data_files)

dataset_plotscripts <- datasets_all %>% 
  select(dataset_id, plot_scripts) %>% 
  unnest(plot_scripts)

datasets_stewards <- datasets_all %>% 
  select(dataset_id, tech_doc_url, data_steward) %>% 
  unnest(data_steward) %>% 
  mutate(
    steward = str_replace(data_steward, "(.*)<(.*)>", "\\1") %>% str_trim(),
    email   = str_replace(data_steward, "(.*)<(.*)>", "\\2"))

datasets <- datasets_all %>%
  #filter(has_data, has_steward, has_doc, has_files, has_plots) %>% 
  filter(has_data, has_steward, has_files, has_plots) %>% 
  mutate(
    datafile1_ext = map_chr(data_files, 1) %>% fs::path_ext(),
    tech_doc_url  = map_chr(tech_doc_url, 1)) %>% 
  filter(
    datafile1_ext %in% c("csv", "xlsx"))
# datasets %>% select(dataset_id, datafile1_ext)
# table(datasets$datafile1_ext)
# csv    nc   Rda Rdata RData   rds  xlsx 
# 18     1     1     2     5     1     8

# admins already handled with: server.R get_datasets() using admin_emails
# datasets_admins <- expand_grid(
#   dataset_id = datasets$dataset_id, 
#   email      = admin_emails,
#   is_admin   = TRUE)
# 
# datasets_stewards <- datasets_stewards %>% 
#   full_join(
#     datasets_admins,
#     by = c("dataset_id",  "email")) %>% 
#   arrange(dataset_id, email) # %>% View()
  
# dataset_id = "ch_bay_sal"


# plotRs <- tibble(
#   file_plotr = list.files(dir_plotR, "\\.R$")) %>% 
#   mutate(
#     # txt            = map_chr(plotR, function(plotR){
#     #   readLines(path(dir_plotR, plotR)) %>% paste(collapse = "\n")}),
#     # dataset_id     = map(txt, function(txt){
#     #   str_match(txt, glue("ecodata::{datasets_all$dataset_id}"))[,1] %>% 
#     #     na.omit() %>% 
#     #     str_replace("ecodata::", "")}),
#     dataset_id     = map(file_plotr, function(txt){
#       
#       
#     a <-  # %>% 
#       
#         full_join(
#           tibble(
#             dataset_id = datasets_all$dataset_id,
#             src_datasets = T),
#           by = "dataset_id") %>% 
#         arrange(dataset_id, src_datasets, file_plotr) %>% 
#         write_csv(here("data/tmp_plotr_dataset_match.csv"))
#       
#       str_match(txt, glue("ecodata::{datasets_all$dataset_id}"))[,1] %>% 
#         na.omit() %>% 
#         str_replace("ecodata::", "")}),
#     title = map_chr(plotR, function(x){
#       x %>% 
#         str_replace(".Rmd", ":")
#     }),
#     n_datasets     = map_int(dataset_id, length),
#     dataset_ids    = map_chr(dataset_id, paste, collapse = ", "),
#     plotR_datasets = glue("- [ ] {plotR} (n={n_datasets}): {dataset_ids}"))
# # View(plotRs)
# 
# 
# datasets_plotR <- plotRs %>% filter(n_datasets == 1) %>% pull(dataset_ids) %>% unique() %>% sort()

# headers in plot?
# plotRs %>% filter(n_datasets == 1) %>% arrange(dataset_ids) %>% select(dataset_ids, plotR) %>% View()

# datasets missing plotR (see plotRs without dataset)
# datasets_not1plotR <- setdiff(datasets$dataset_id, datasets_plotR)
# glue("- [ ] {datasets_not1plotR}")
# - [ ] ESP_seasonal_oisst_anom
# - [ ] hp_density
# - [ ] seasonal_sst_anomaly_gridded

# plotRs without dataset -- typically missing ecodata:: prefix to dataset
# plotRs %>% 
#   filter(n_datasets == 0) %>% 
#   pull(plotR_datasets) %>% 
#   paste(collapse = "\n") %>% 
#   cat()
  

# plotRs %>% 
#   filter(n_datasets > 1) %>% 
#   pull(plotR_datasets) %>% 
#   paste(collapse = "\n") %>% 
#   cat()
  

# plotRs %>% 
#   filter(n_datasets == 1) %>%
#   select(dataset_id = dataset_ids, plotR) %>% 
#   group_by(dataset_id) %>% 
#   nest() %>% 
#   mutate(
#     n_plotR = map_int(data, nrow)) %>% 
#   unnest(data) %>% 
#   select(dataset_id, n_plotR, plotR) %>% 
#   mutate(
#     dataset_plotR = glue("- [ ] {dataset_id} (n={n_plotR}): {plotR}")) %>% 
#   pull(dataset_plotR) %>% 
#   paste(collapse = "\n") %>% 
#   cat()

# bottomtemp
# LTL_NE.Rmd-bottom-temp-glorys.R

# datasets_plotRs <- datasets %>% 
#   mutate(
#     plot_chunks = list.files(dir_chunks))