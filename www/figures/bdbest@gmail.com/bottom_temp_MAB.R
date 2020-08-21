library(here)

source(here('chunk-scripts/LTL.Rmd-setup.R'))
source(here('chunk-scripts/LTL.Rmd-GIS-setup.R'))
source(here('chunk-scripts/LTL.Rmd-MAB-bot-temp.R'))
        
ggsave('/share/github/iea-uploader/www/figures/bdbest@gmail.com/bottom_temp_MAB.png', width = 6, height = 4, dpi = 72)
