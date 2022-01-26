[![DOI](https://zenodo.org/badge/324365720.svg)](https://zenodo.org/badge/latestdoi/324365720)

# twinR

This is the repository providing the code and the data associated with the paper
"**Mothers with higher twinning propensity have lower fertility in pre-industrial Europe**" by:

- Ian J. Rickard *(Department of Anthropology, Durham University, Durham, UK / Department of Evolutionary Genetics, Leibniz Institute for Zoo and Wild Research, Berlin, Germany)*
- Colin Vullioud *(Department of Evolutionary Genetics, Leibniz Institute for Zoo and Wild Research, Berlin, Germany)*
- François Rousset *(Institut des Sciences de l'Évolution (ISEM), Université de Montpellier, CNRS, EPHE, IRD, Montpellier, France)*
- Erik Postma *(University of Exeter, Cornwall Campus, UK)*
- Samuli Helle *(Department of Social Research, University of Turku, Turku, Finland)*
- Virpi Lummaa *(Department of Biology, University of Turku, Turku, Finland)*
- Ritva Kylli *(Department of History, University of Oulu, Oulu, Finland)*
- Jenni E. Pettay *(Department of Social Research, University of Turku, Turku, Finland)*
- Eivin Røskaft *(Department of Biology, Norwegian University of Science and Technology, Trondheim, Norway)*
- Gine R. Skjærvø *(Department of Biology, Norwegian University of Science and Technology, Trondheim, Norway)*
- Charlotte Störmer *(Institute for Philosophy, Justus Liebig University Gießen, Germany)*
- Eckart Voland *(Institute for Philosophy, Justus Liebig University Gießen, Germany)*
- Dominique Waldvogel *(Department of Evolutionary Biology and Environmental Studies, University of Zurich, Zurich, Switzerland)*
- Alexandre Courtiol *(Department of Evolutionary Genetics, Leibniz Institute for Zoo and Wild Research, Berlin, Germany)*

Please address any correspondence to Alexandre Courtiol: courtiol@izw-berlin.de


## Installation

To install this R package, make sure the first install the package **{remotes}** (if it is not already installed on your system), then simply type the following in your R Console:

```r
remotes::install_github("courtiol/twinR", dependencies = TRUE)
```

Then, load the installed package, as usual, and follow the instructions that will appear in the main help file of the package:

```r
library(twinR)
?twinR
```


## Documentation

Click on **Releases** to access the PDF manual for this package. 


## Exploration of the source code

Since comments embedded within source code disappear upon installation of R packages, we recommend you to directly explore the source of the R code here in GitHub.

The source files are located within the folder **"R"** shown above.


## R session info

Here are the versions of all the packages used directly or indirectly for this work:

```r
> sessionInfo()
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 11 (bullseye)

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/atlas/libblas.so.3.10.3
LAPACK: /usr/lib/x86_64-linux-gnu/atlas/liblapack.so.3.10.3

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] twinR_0.9999     tibble_3.1.6     doSNOW_1.0.19    snow_0.4-4       iterators_1.0.13 foreach_1.5.1    spaMM_3.9.40     testthat_3.1.1   cachem_1.0.6    
[10] memoise_2.0.1   

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.7          ggnewscale_0.4.5    lattice_0.20-45     tidyr_1.1.4         prettyunits_1.1.1   ps_1.6.0            assertthat_0.2.1    rprojroot_2.0.2    
 [9] utf8_1.2.2          slam_0.1-50         R6_2.5.1            pracma_2.3.6        ggplot2_3.3.5       pillar_1.6.4        rlang_0.4.12        minqa_1.2.4        
[17] rstudioapi_0.13     callr_3.7.0         nloptr_1.2.2.3      Matrix_1.4-0        desc_1.4.0          devtools_2.4.3      munsell_0.5.0       proxy_0.4-26       
[25] ROI_1.0-0           compiler_4.1.2      numDeriv_2016.8-1.1 pkgconfig_2.0.3     pkgbuild_1.3.1      tidyselect_1.1.1    codetools_0.2-18    fansi_0.5.0        
[33] crayon_1.4.2        dplyr_1.0.7         withr_2.4.3         MASS_7.3-54         grid_4.1.2          nlme_3.1-153        gtable_0.3.0        lifecycle_1.0.1    
[41] registry_0.5-1      DBI_1.1.2           magrittr_2.0.1      scales_1.1.1        cli_3.1.0           pbapply_1.5-0       fs_1.5.2            remotes_2.4.2      
[49] ellipsis_0.3.2      vctrs_0.3.8         generics_0.1.1      boot_1.3-28         cowplot_1.1.1       tools_4.1.2         glue_1.6.0          purrr_0.3.4        
[57] processx_3.5.2      pkgload_1.2.4       parallel_4.1.2      fastmap_1.1.0       colorspace_2.0-2    sessioninfo_1.2.2   usethis_2.1.5 
```

## Info for the maintainer (outdated: fitted models no longer stored due to size constraints)

In `fitted_models/` we only store the latest version of the fitted models (using git lfs).
If it takes more than what is allowed for storage, `mv` the folder using `fitted_models_backup`, commit the deletion, then run `java -jar ~/bfg.jar --delete-folders fitted_models` (see [bfg](https://rtyley.github.io/bfg-repo-cleaner/)), then `git reflog expire --expire=now --all && git gc --prune=now --aggressive`, then `git push`, then `mv` back the folder under its original name and commit its content again!

