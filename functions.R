rm <- function(path){
  if (file.exists(path)) file_delete(path)}

chmod <- function(path, mode = "775"){
  if (file.exists(path)) system(glue("sudo chmod {mode} {path}"))}

git_branch_freshen <- function(git_branch, dir_ecodata_branch){
  
  # https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
  gh_pat <- readLines("/share/iea-uploader_github-personal-access-token_bbest.txt")
  
  git_branch_reset_cmds <- glue(
    "
      # set git user defaults
      git config --global user.email 'ben@ecoquants.com'
      git config --global user.name 'Ben Best'
      GITHUB_TOKEN={gh_pat}
      GITHUB_USER=bbest
      
      # get latest
      git fetch -p
      
      # delete local branch if exists
      if [ `git branch --list {git_branch}` ]
      then
        echo 'deleting local branch {git_branch}'
        git branch -D {git_branch}
      fi
      
      # delete remote branch if exists
      if [ `git branch -r | grep '{git_branch}' | wc -l` == '1' ]
      then
        echo 'deleting remote branch {git_branch}'
        git push origin --delete {git_branch}
      fi
      
      # TODO: check that actually deletes
      # git remote update origin --prune
      # git branch -a
      
      # branch and reset
      cd {dir_ecodata_branch}
      git fetch
      git pull
      git checkout -b {git_branch}
      git reset
      git checkout .
      git clean -f -d

      git branch --set-upstream-to=origin/{git_branch} {git_branch}
      git remote set-url origin https://bbest:{gh_pat}@github.com/bbest/ecodata.git
      git pull
      ")
  # git ls-remote --heads bbest@github.com:bbest/ecodata.git bdbest@gmail.com_slopewater | wc -l
  
  message(glue("before dir.exists() dir_ecodata_branch: {dir_ecodata_branch}"))
  if (dir.exists(dir_ecodata_branch)){
    git_cmds <- glue(
      "
        # get latest
        #cd {dir_ecodata_branch}
        #git checkout master
        #git pull
        #git fetch
          
        {git_branch_reset_cmds}")
  } else {
    git_cmds <- glue(
      "
        # freshen src
        cd {dir_ecodata_src}
        git config core.fileMode false
        git pull
        git fetch
        
        # copy with attributes recursively
        cp -ar {dir_ecodata_src} {dir_ecodata_branch}
        
        # turn off file chmod modifications
        cd {dir_ecodata_branch}
        git config core.fileMode false
        sudo chmod -R 775 {dir_ecodata_branch}

        {git_branch_reset_cmds}")
  }
  message(git_cmds)
  system(git_cmds)
}

sanitize_comments <- function(txt){
  txt <- txt %>% 
    str_replace_all("'", '"') %>% 
    str_replace_all(fixed('{'), "[") %>% 
    str_replace_all(fixed('}'), "]")
  txt
}


get_flds_type <- function(tbl_paths){
  
  has_csv <- any(fs::path_ext(tbl_paths$path) == "csv")
  has_xl  <- any(fs::path_ext(tbl_paths$path) %in% c("xls", "xlsx"))
  # TODO: handle if has both csv & xl
  
  if (has_csv){
    tbl_flds <- tbl_paths %>% 
      filter(fs::path_ext(path) == "csv") %>% 
      mutate(
        data = map(path, function(x) read_csv(x)),
        sheet = NA,
        fld_type = map(data, function(x){
          flds  <- names(x)
          types <- sapply(x, class)
          glue("{flds} <{types}>")
        })) %>% 
      select(-data, -path) %>% 
      unnest(fld_type)
  }
  
  if (has_xl){
    tbl_flds <- tbl_paths %>% 
      filter(fs::path_ext(path) %in% c("xls","xlsx")) %>% 
      mutate(
        # type  = "xl",
        sheet = map(path, function(x) excel_sheets(x))) %>% 
      unnest(sheet) %>% 
      mutate(
        data = map2(path, sheet, function(x, y) read_excel(x, y)),
        fld_type = map(data, function(x){
          flds  <- names(x)
          types <- sapply(x, class)
          glue("{flds} <{types}>")
        })) %>% 
      select(-data, -path) %>% 
      unnest(fld_type)
  }
  tbl_flds
}

get_Rfiles <- function(dataset){
  # plot_R     <- str_split(dataset$plot_chunks, ", ", simplify=T) %>% as.vector()
  plot_scripts <- dataset$plot_scripts %>% unlist()
  
  r_tbl <- tibble(
    Operation = "Load",
    R_path    = glue("data-raw/get_{dataset$dataset_id}.R")) %>% 
    bind_rows(
      tibble(
        Operation = "Plot",
        # R_path    = glue("chunk-scripts/{plot_R}"))) %>% 
        R_path    = glue("chunk-scripts/{plot_scripts}"))) %>% 
    mutate(
      R_url      = glue("https://github.com/NOAA-EDAB/ecodata/blob/master/{R_path}"),
      R_fname    = basename(R_path),
      `R script` = glue("<a href='{R_url}' target='_blank'>{R_fname}</a>")) %>% 
    select(Operation, `R script`)
  
  tagList(
    r_tbl %>% 
      kable(escape = F, table.attr = "class='kable'") %>% 
      HTML())
}

preview_tbl <- function(file1, is_files){
  if (fs::path_ext(file1) == "csv")
    return(preview_csv(file1, is_files))
  # otherwise assume Excel spreadsheet
  if (fs::path_ext(file1) %in% c("xls","xlsx"))
    return(preview_xl(file1, is_files))
  
  stop(glue("Sorry, the Uploader does not yet handle {fs::path_ext(file1)} file extensions (only csv, xls or xlsx)."))
}
  
preview_csv <- function(file1_csv, is_files){
  if (!file.exists(file1_csv)) return("")
  tagList(
    ifelse(is_files, "They", "It"), "should have header names and values like",
    ifelse(is_files, glue("this ({basename(file1_csv)}):"), "this:"), br(),
    read_csv(file1_csv) %>% head() %>% kable(table.attr = "class='kable'") %>% HTML())
}

preview_xl <- function(file1_xl, is_files){
  if (!file.exists(file1_xl)) return("")
  #browser()
  tagList(
    ifelse(is_files, "They", "It"), "should have sheets named:", 
    # paste(HTML(strong(readxl::excel_sheets(file1_xl))), collapse = ", "), br(),
    # sapply(, strong)
    # tags$ul(
    #   purrr::map(warns, function(.x) tags$li(.x)))
    tags$ul(
      purrr::map(readxl::excel_sheets(file1_xl), function(.x) tags$li(strong(.x)))), br(),
    "with header names and values like",
    ifelse(is_files, glue("this ({basename(file1_xl)}):"), "this:"), br(),
    read_excel(file1_xl) %>% head() %>% kable(table.attr = "class='kable'") %>% HTML())
}

plot_chunk_to_tab_panel <- function(
  plot_name, plot_scripts, 
  dataset, user, dir_ecodata_branch, rmd_grp, 
  fig_width_in, fig_height_in, fig_dpi){
  # plot_chunk = "LTL_MAB.Rmd-bottom-temp.R"
  
  plot_script = plot_scripts[[plot_name]]
  
  # plot_rgn <- ifelse(
  #   length(plot_chunks) > 1,
  #   str_replace(plot_chunk, "^(.*?)_(.*?)(\\.Rmd)-(.*)\\.R$", "\\2"),
  #   "plot")
  
  plot_pfx <- glue(
    "{dir_uploader}/www/figures/{user}/{dataset$dataset_id}_{plot_name}")
  plot_R    <- glue("{plot_pfx}.R")
  plot_log  <- glue("{plot_pfx}_Rlog.txt")
  plot_png  <- glue("{plot_pfx}.png")
  plot_img  <- glue("./figures/{user}/{basename(plot_png)}")
  dir_create(dirname(plot_pfx))
  chmod(dirname(plot_pfx), "775")
  
  message(glue("\nplot_R: {plot_R} -> {plot_png}\n\n"))
  
  walk(c(plot_R, plot_png, plot_log), rm)
  
  setup_R_files <- tibble(
    file = list.files(glue("{dir_ecodata_branch}/chunk-scripts"), glue("{rmd_grp}.*-setup.R")),
    nchar = nchar(file)) %>% 
    arrange(nchar) %>% 
    pull(file)
  setup_R <- paste(glue("source(here('chunk-scripts/{setup_R_files}'))"), collapse = "\n") # cat(setup_R)
  plot_code  <- glue("
    library(here)
    
    {setup_R}
    devtools::load_all('{dir_ecodata_branch}')
    source(here('chunk-scripts/{plot_script}'))
  
    ggsave('{plot_png}', width = {fig_width_in}, height = {fig_height_in}, dpi = {fig_dpi})") # cat(plot_code)
  message(c(glue("\nplot_code -> {plot_R}:\n"), plot_code, "\n\n"))
  writeLines(plot_code, plot_R)
  chmod(plot_R, "775")
  cmd <- glue("cd {dir_ecodata_branch}; Rscript {plot_R} 2>{plot_log}")
  res <- system(cmd, intern=T)
  walk(c(plot_png, plot_log), chmod, mode = "775")
  
  # catch error
  if (!is.null(attr(res, "status")) && attr(res, "status") == 1){
    log <- readLines(plot_log) %>% paste(collapse = "\n")
    
    stop(paste(
      "plotting dataset within ecodata R package:\n\n----\n", 
      plot_code, 
      "\n----\nHere is a log of messages from running above code:\n----\n", 
      log, 
      "\n----\nPlease try uploading fixed file(s)."))
  }
  stopifnot(file_exists(plot_png))
  
  tabPanel(
    plot_name, 
    img(
      src = plot_img,
      alt = glue("Plot output for {dataset$dataset_id} - {plot_name}")))
}