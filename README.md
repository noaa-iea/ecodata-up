# uploader

Shiny app for dataset uploading, checking and visualization

## app

Main Shiny app: [shiny.ecoquants.com/ecodata-up](https://shiny.ecoquants.com/ecodata-up/)

## html

To see this html listing, visit [noaa-iea.github.io/ecodata-up](https://noaa-iea.github.io/ecodata-up).

These web pages (\*.html) are typically rendered from Rmarkdown (\*.Rmd):

<!-- Jekyll rendering: https://marineenergy.github.io/apps/ -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}

## debug

See files on admin@rstudio.iea-demo.us:

```
~/shiny-logs
~/github/iea-uploader/www/figures/{email}/{dataset}/*
- {dataset}_load_Rlog.txt
- {dataset}_plot.R
- {dataset}_plot_Rlog.txt
- {dataset}_plot.png
```



## setup

From admin@rstudio.iea-demo.us in Terminal:

```bash

# git clone
cd /share/github
git clone https://github.com/bbest/ecodata.git
git clone https://github.com/marinebon/iea-uploader.git

# turn off git tracking of chmod
cd /share/github/ecodata; git config core.fileMode false
cd /share/github/iea-uploader; git config core.fileMode false

# chmod so shiny app can edit
cd /share/github; sudo chmod -R 777 ecodata
cd /share/github/iea-uploader; sudo chmod -R 777 data; sudo chmod -R 777 www

# revert git changes
cd /share/github/ecodata; git reset; git checkout .
```

### default permissions

Setup default permissions to work for both shiny and admin users.

Set so group is writable for each user in Terminal manually:

- admin
- shiny

```bash
user=admin
user=shiny

sudo su - $user
printf "\numask 0002\n" >> .profile
```

Add shiny to staff group:

```bash
user=shiny
group=staff

sudo usermod -a -G $group $user; groups $user
```

Set default group for following paths:

- /share/github/ecodata_uploader
- /share/github/iea-uploader

```bash
group=staff
dir=/share/github/ecodata_uploader
dir=/share/github/iea-uploader/www/figures

sudo chgrp -R $group $dir
sudo chmod g+s $dir
sudo chmod -R 775 $dir

# test
tst=/share/github/ecodata_uploader/umask_test.txt
touch $tst; ls -l $tst; rm $tst
```

## initial dataset test

Initially, using:

- app code:
  - [Shiny - File Upload](https://shiny.rstudio.com/gallery/file-upload.html)
  
- dataset:
  - Chose first single dataset with `` fs::path_ext(`File Name`) == "csv" `` &  `` `New to this SOE?` == "No"` `` from  [SOE 2020 Contributors - Google Sheets](https://docs.google.com/spreadsheets/d/1p6DZNeVSNo1id1IwBHYuQrNmkjRDDULZrcJbox01xO4/edit#gid=0): `slopewater_proportions.csv`
  
- "slopewater" mentions in `ecodata`:
  * [data-raw/get_slopewater.R](https://github.com/NOAA-EDAB/ecodata/blob/ed64ae5d37bdc4b4b97f07b42a0d8f6b6dbf7c16/data-raw/get_slopewater.R): create dataset
  * [R/slopewater.R](https://github.com/NOAA-EDAB/ecodata/blob/ed64ae5d37bdc4b4b97f07b42a0d8f6b6dbf7c16/R/slopewater.R): document dataset
  * [other/Rmd/process_env.Rmd](https://github.com/NOAA-EDAB/ecodata/blob/ed64ae5d37bdc4b4b97f07b42a0d8f6b6dbf7c16/other/Rmd/process_env.Rmd#L300-L358): table, plot
  * [other/Rmd/process_raw.Rmd](https://github.com/NOAA-EDAB/ecodata/blob/ed64ae5d37bdc4b4b97f07b42a0d8f6b6dbf7c16/other/Rmd/process_raw.Rmd#L300-L323): table, contributor
  * [docs/LTL.Rmd](https://github.com/NOAA-EDAB/ecodata/blob/924f417238da1b24a8a372109da2981af7507b40/docs/LTL.Rmd#L967-L996): table, plot
  
  
  
- https://github.com/nanxstats/awesome-shiny-extensions
