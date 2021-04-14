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

load_data <- function(dataset){
  
  values$load_success <- F
  
  dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
  
  files_dest <- glue("{values$dir_ecodata_branch}/data-raw/{file_input()$name}")
  
  file_move(file_input()$datapath, files_dest)
  walk(files_dest, chmod, mode = "775")
  
  message(glue('Sys.info()[["effective_user"]]: {Sys.info()[["effective_user"]]}'))
  
  load_R <- glue("{values$dir_ecodata_branch}/data-raw/get_{dataset$dataset_code}.R")
  load_log <- glue(
    "{dir_uploader}/www/figures/{input$g_email}/{dataset$dataset_code}_load_Rlog.txt")
  
  dir_create(dirname(load_log))
  chmod(dirname(load_log), "775")
  
  load_cmd <- glue("cd {values$dir_ecodata_branch}; Rscript {load_R} 2>{load_log}")
  
  #stop(paste("load_cmd DEBUG:\n\n----\n",  load_cmd))
  # cd /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater; Rscript /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater/data-raw/get_slopewater.R 2>/share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_load_Rlog.txt
  # cd /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater; Rscript /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater/data-raw/get_slopewater.R 2>/share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_load_Rlog.txt
  # browser()
  res <- system(load_cmd, intern = T)
  
  # values = list(
  #   dir_ecodata_branch="/share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater")
  # dataset = list(dataset_code)
  # load_R = glue("{values$dir_ecodata_branch}/data-raw/get_{dataset$dataset_code}.R")
  # 
  # ls -la /share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_load_Rlog.txt
  # 
  # load("/share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater/data/slopewater.rda")
  # tail(slopewater)
  # View(slopewater)
  # browser()
  
  # catch error
  if (!is.null(attr(res, "status")) && attr(res, "status") == 1){
    log <- readLines(load_log) %>% paste(collapse = "\n")
    stop(paste(
      "loading dataset within ecodata R package:\n\n----\n", 
      glue("source(data-raw/get_{dataset$dataset_code}.R)"), 
      "\n----\nHere is a log of messages from running above code:\n----\n", 
      log, 
      "\n----\nPlease try uploading fixed file(s)."))
  }
  
  load_all(values$dir_ecodata_branch)
  
  d <- get(dataset$dataset_code, pos = "package:ecodata")
  
  values$load_success <- T
  
  d
}

get_flds_type <- function(tbl_paths){
  tbl_paths %>% 
    mutate(
      data = map(path, function(x) read_csv(x)),
      flds_type = map(data, function(x){
        flds  <- names(x)
        types <- sapply(x, class)
        glue("{flds} <{types}>")
      })) %>% 
    select(-data, -path) %>% 
    unnest(flds_type)
}

get_Rfiles <- function(){
  plot_R <- str_split(dataset$plot_chunks, ", ", simplify=T) %>% as.vector()
  
  r_tbl <- tibble(
    Operation = "Load",
    R_path    = glue("data-raw/get_{dataset$dataset_code}.R")) %>% 
    bind_rows(
      tibble(
        Operation = "Plot",
        R_path    = glue("chunk-scripts/{plot_R}"))) %>% 
    mutate(
      R_url   = glue("https://github.com/NOAA-EDAB/ecodata/blob/master/{R_path}"),
      R_fname = basename(R_path),
      `R script` = glue("<a href='{R_url}' target='_blank'>{R_fname}</a>")) %>% 
    select(Operation, `R script`)
  
  tagList(
    r_tbl %>% 
      kable(escape = F, table.attr = "class='kable'") %>% 
      HTML())
}

preview_csv <- function(file1_csv, is_files){
  if (!file.exists(file1_csv)) return("")
  tagList(
    ifelse(is_files, "They", "It"), "should have header names and values like",
    ifelse(is_files, glue("this ({basename(file1_csv)}):"), "this:"), br(),
    read_csv(file1_csv) %>% head() %>% kable(table.attr = "class='kable'") %>% HTML())
}

plot_chunk_to_tab_panel <- function(plot_chunk){
  # plot_chunk = "LTL_MAB.Rmd-bottom-temp.R"
  plot_rgn <- ifelse(
    length(plot_chunks) > 1,
    str_replace(plot_chunk, "^(.*?)_(.*?)(\\.Rmd)-(.*)\\.R$", "\\2"),
    "plot")
  
  plot_pfx <- glue(
    "{dir_uploader}/www/figures/{input$g_email}/{dataset$dataset_code}_{plot_rgn}")
  plot_R    <- glue("{plot_pfx}.R")
  plot_log  <- glue("{plot_pfx}_Rlog.txt")
  plot_png  <- glue("{plot_pfx}.png")
  plot_img  <- glue("./figures/{input$g_email}/{basename(plot_png)}")
  dir_create(dirname(plot_pfx))
  chmod(dirname(plot_pfx), "775")
  
  message(glue("\nplot_R: {plot_R} -> {plot_png}\n\n"))
  
  walk(c(plot_R, plot_png, plot_log), rm)
  
  setup_R_files <- tibble(
    file = list.files(glue("{values$dir_ecodata_branch}/chunk-scripts"), glue("{rmd_grp}.*-setup.R")),
    nchar = nchar(file)) %>% 
    arrange(nchar) %>% 
    pull(file)
  setup_R <- paste(glue("source(here('chunk-scripts/{setup_R_files}'))"), collapse = "\n") # cat(setup_R)
  plot_code  <- glue("
          library(here)
          
          {setup_R}
          devtools::load_all('{values$dir_ecodata_branch}')
          source(here('chunk-scripts/{plot_chunk}'))
        
          ggsave('{plot_png}', width = {fig_width_in}, height = {fig_height_in}, dpi = {fig_dpi})") # cat(plot_code)
  message(c(glue("\nplot_code -> {plot_R}:\n"), plot_code, "\n\n"))
  writeLines(plot_code, plot_R)
  chmod(plot_R, "775")
  cmd <- glue("cd {values$dir_ecodata_branch}; Rscript {plot_R} 2>{plot_log}")
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
    plot_rgn, 
    img(
      src = plot_img,
      alt = glue("Plot output for {dataset$dataset_code}")))
}