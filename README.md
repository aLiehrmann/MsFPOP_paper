## Foreword

This repository contains code to reproduce all figures from the paper "Ms.FPOP: 
An Exact and Fast Segmentation Algorithm With a Multiscale Penalty" (Liehrmann 
and Rigaill 2023) 

## Where can I learn more about Ms.FPOP ?

For further information on Ms.FPOP, we suggest referring to the 
[Vignette](https://aliehrmann.github.io/MsFPOP/index.html).

## Setup

You should make sure that you have installed the following packages : 
``` r
install.packages("remotes")
remotes::install_github("aLiehrmann/MsFPOP")
install.packages("parallel")
install.packages("ggrepel")
install.packages("ggplot2")
install.packages("stringr")
install.packages("data.table")
install.packages("ggpubr")
install.packages("fpopw")
install.packages("changepoint")
install.packages("latex2exp")
```

## Figures
  
This repository contains: 

(1) R scripts to compare the runtime of Ms.FPOP, Ms.PELT, FPOP, PELT.

    => To reproduce Figure 1, Figure 3, Figure S3 you should look 
    at runtime_cmp_gr.R and runtime_cmp_al.R in the R repository. 

(2) R script to compare the runtime of sampling strategies of future changepoint 
    candidates in Ms.FPOP.

    => To reproduce Figure S2, you should look at runtime_cmp_al.R in the R 
       repository.
    
(3) R script to calibrate the constants ($\gamma$ and $\beta$) of the multiscale
    penalty. 

    => To reproduce Figure 2 and Figure S1, you should look at 
       calibrateMsFPOP.R in the R repository.
    
(4) R script to compare Ms.FPOP (multiscale penalty) and FPOP (BIC penalty) on 
    simulated step-like profiles.

    => To reproduce Figure S5, you should look at step_simu.R in the R 
       repository.
    
(5) R script to compare Ms.FPOP (multiscale penalty) and FPOP (BIC penalty) on 
    simulated hat-like profiles.

    => To reproduce Figure 4 and Figure S4, you should look at hat_simu.R in 
       the R repository.

(6) R scripts to compare MOSUM, Ms.FPOP (multiscale penalty) and FPOP (BIC penalty)
    on an extanded range of simulations.
    
    => To reproduce Figure 5, Figures G. and Figures H., you should look at 
       m_simulated_PerfStat.Rmd and somesegsimu_figures.R in the R repository.
    
## References

[Liehrmann, A. and Rigaill, G. Ms.Fpop: An Exact and Fast Segmentation
Algorithm With a Multiscale Penalty (2023)]( 	
https://doi.org/10.48550/arXiv.2303.08723).

## SessionInfo

R version 3.6.3 (2020-02-29)

Platform: x86_64-pc-linux-gnu (64-bit)

Running under: Debian GNU/Linux 9 (stretch)

Matrix products: default

BLAS:   /usr/lib/openblas-base/libblas.so.3

LAPACK: /usr/lib/libopenblasp-r0.2.19.so

locale:

 [1] LC_CTYPE=fr_FR.UTF-8       LC_NUMERIC=C
               
 [3] LC_TIME=fr_FR.UTF-8        LC_COLLATE=fr_FR.UTF-8
     
 [5] LC_MONETARY=fr_FR.UTF-8    LC_MESSAGES=fr_FR.UTF-8
    
 [7] LC_PAPER=fr_FR.UTF-8       LC_NAME=C
                  
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
             
[11] LC_MEASUREMENT=fr_FR.UTF-8 LC_IDENTIFICATION=C
       

attached base packages:

[1] parallel  stats     graphics  grDevices utils     datasets  methods
  
[8] base
     

other attached packages:

 [1] latex2exp_0.9.6   changepoint_2.2.3 zoo_1.8-9         fpopw_1.1
         
 [5] ggpubr_0.4.0      data.table_1.14.2 stringr_1.4.0     ggrepel_0.9.1
     
 [9] ggplot2_3.3.6     MsFPOP_1.0
        

loaded via a namespace (and not attached):

 [1] Rcpp_1.0.9        lattice_0.20-40   tidyr_1.1.4       prettyunits_1.1.1
 
 [5] ps_1.6.0          rprojroot_2.0.2   utf8_1.2.2        R6_2.5.1
          
 [9] cellranger_1.1.0  backports_1.2.1   pillar_1.8.1      rlang_1.0.6
       
[13] curl_4.3.2        readxl_1.3.1      car_3.0-11        callr_3.7.0
      
[17] desc_1.4.0        devtools_2.4.2    foreign_0.8-75    munsell_0.5.0
    
[21] broom_0.7.9       compiler_3.6.3    pkgconfig_2.0.3   pkgbuild_1.2.0
   
[25] tidyselect_1.1.1  tibble_3.1.8      rio_0.5.27        fansi_1.0.3
      
[29] crayon_1.5.0      dplyr_1.0.7       withr_2.5.0       grid_3.6.3
       
[33] gtable_0.3.1      lifecycle_1.0.3   magrittr_2.0.3    scales_1.2.1
     
[37] zip_2.2.0         cli_3.4.1         stringi_1.7.6     cachem_1.0.6
     
[41] carData_3.0-4     ggsignif_0.6.3    fs_1.5.0          remotes_2.4.1
    
[45] testthat_3.1.0    ellipsis_0.3.2    generics_0.1.3    vctrs_0.5.0
      
[49] openxlsx_4.2.4    tools_3.6.3       forcats_0.5.1     glue_1.6.2
       
[53] purrr_0.3.4       hms_1.1.1         processx_3.5.2    abind_1.4-5
      
[57] pkgload_1.2.2     fastmap_1.1.0     colorspace_2.0-3  sessioninfo_1.1.1

[61] rstatix_0.7.0     memoise_2.0.0     haven_2.4.3       usethis_2.0.1
