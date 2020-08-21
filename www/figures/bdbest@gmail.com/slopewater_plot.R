library(here)

source(here('chunk-scripts/LTL.Rmd-setup.R'))
source(here('chunk-scripts/LTL.Rmd-GIS-setup.R'))
source(here('chunk-scripts/LTL.Rmd-wsw-prop.R'))
        
ggsave('/share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_plot.png', width = 6, height = 4, dpi = 72)
