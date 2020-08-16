library(here)

source(here('chunk-scripts/LTL.Rmd-setup.R'))
source(here('chunk-scripts/LTL.Rmd-GIS-setup.R'))
source(here('chunk-scripts/LTL.Rmd-MAB-bot-temp.R'))

ggsave('/share/github/iea-uploader/data/bdbest@gmail.com/bottom_temp/bottom_temp_MAB.png', width = 8.33333333333333, height = 5.55555555555556, dpi = 72)
