#################################################################
############### R and package version ###########################
#################################################################
> sessionInfo()
R version 4.3.0 (2023-04-21 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=Chinese (Simplified)_China.utf8  LC_CTYPE=Chinese (Simplified)_China.utf8    LC_MONETARY=Chinese (Simplified)_China.utf8
[4] LC_NUMERIC=C                                LC_TIME=Chinese (Simplified)_China.utf8    

time zone: Etc/GMT-8
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggrepel_0.9.5      ggpubr_0.6.0       ggthemes_5.0.0     stringr_1.5.1      reshape2_1.4.4     pheatmap_1.0.12    ggcorrplot_0.1.4.1
 [8] rstatix_0.7.2      GGally_2.2.0       ggplot2_3.4.4     

loaded via a namespace (and not attached):
 [1] utf8_1.2.4         generics_0.1.3     tidyr_1.3.0        stringi_1.8.3      magrittr_2.0.3     grid_4.3.0         RColorBrewer_1.1-3
 [8] plyr_1.8.9         backports_1.4.1    reshape_0.8.9      purrr_1.0.2        fansi_1.0.6        scales_1.3.0       textshaping_0.3.7 
[15] abind_1.4-5        cli_3.6.1          rlang_1.1.1        crayon_1.5.2       cowplot_1.1.3      munsell_0.5.0      withr_3.0.0       
[22] tools_4.3.0        ggsignif_0.6.4     dplyr_1.1.4        colorspace_2.1-0   ggstats_0.5.1      broom_1.0.5        vctrs_0.6.5       
[29] R6_2.5.1           lifecycle_1.0.4    car_3.1-2          ragg_1.2.7         pkgconfig_2.0.3    pillar_1.9.0       gtable_0.3.4      
[36] glue_1.6.2         Rcpp_1.0.11        systemfonts_1.0.5  tibble_3.2.1       tidyselect_1.2.0   rstudioapi_0.15.0  farver_2.1.1      
[43] carData_3.0-5      labeling_0.4.3     compiler_4.3.0  

#################################################################
############### Loading data and packages #######################
#################################################################

setwd("./")
library(ggrepel)
library(ggpubr)
library(dplyr)
library(ggthemes)
library(stringr)
library(ggpubr)
library(RColorBrewer)
library(tidyr)
library(reshape2)
library(pheatmap)
library(reshape2)
library(ggcorrplot)
library(rstatix)
library(GGally)
library(ggplot2)

