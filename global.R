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
  bbest/ecodata,
  fs,
  ggiraph,
  ggplot2,
  ggrepel,
  glue,
  grid,
  heatwaveR,
  here,
  kableExtra,
  knitr,
  magrittr,
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

options(shiny.maxRequestSize = 30*1024^2) # 30MB limit

# variables ----
admin_emails     <- c("ben@ecoquants.com", "kimberly.bastille@noaa.gov", "andrew.beet@noaa.gov")
#dir_ecodata_src  <- "/share/github/ecodata_uploader/ecodata"
dir_ecodata_src  <- "/share/github/ecodata_edab"
dir_uploader     <- here()

load_all(dir_ecodata_src)

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
    data_files = map(data, function(data){
      attr(data, "data_files") %>% 
        unlist()}),
    has_data = map_lgl(data, function(data){
      if (length(data) == 1 && is.na(data[[1]]))
        return(F)
      T}),
    has_steward = map_lgl(data_steward, not_null),
    has_doc     = map_lgl(tech_doc_url, not_null),
    has_files   = map_lgl(data_files  , not_null))

datasets_todo <- datasets_all %>% 
  select(dataset_id, starts_with("has")) %>% 
  rowwise() %>% 
  mutate(
    has_missing = any(!has_data, !has_steward, !has_doc, !has_files)) %>% 
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
  unnest(data_files)

datasets <- datasets_all %>%
  filter(has_data, has_steward, has_doc, has_files) %>% 
  mutate(
    tech_doc_url = map_chr(tech_doc_url, 1))

admin_emails <- c("benjamin.best@noaa.gov")
datasets_stewards <- datasets %>% 
  select(dataset_id, tech_doc_url, data_steward) %>% 
  unnest(data_steward) %>% 
  mutate(
    steward = str_replace(data_steward, "(.*)<(.*)>", "\\1") %>% str_trim(),
    email   = str_replace(data_steward, "(.*)<(.*)>", "\\2")) %>% 
  bind_rows(
    datasets
  )

dir_plotR <- glue("{dir_ecodata_src}/chunk-scripts")

plotRs <- tibble(
  plotR = list.files(dir_plotR, "\\.R$")) %>% 
  mutate(
    txt            = map_chr(plotR, function(plotR){
      readLines(path(dir_plotR, plotR)) %>% paste(collapse = "\n")}),
    dataset_id     = map(txt, function(txt){
      str_match(txt, glue("ecodata::{datasets_all$dataset_id}"))[,1] %>% 
        na.omit() %>% 
        str_replace("ecodata::", "")}),
    n_datasets     = map_int(dataset_id, length),
    dataset_ids    = map_chr(dataset_id, paste, collapse = ", "),
    plotR_datasets = glue("- [ ] {plotR} (n={n_datasets}): {dataset_ids}"))
# View(plotRs)

datasets_plotR <- plotRs %>% filter(n_datasets == 1) %>% pull(dataset_ids) %>% unique() %>% sort()

# headers in plot?
# plotRs %>% filter(n_datasets == 1) %>% arrange(dataset_ids) %>% select(dataset_ids, plotR) %>% View()

# datasets missing plotR (see plotRs without dataset)
datasets_not1plotR <- setdiff(datasets$dataset_id, datasets_plotR)
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