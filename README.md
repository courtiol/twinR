[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6445293.svg)](https://zenodo.org/record/6445293)

# twinR

This is the repository providing the code and the data associated with the paper
"**Mothers with higher twinning propensity had lower fertility in pre-industrial Europe**" (Nature Communications, 2022) by:

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
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8      
 [8] LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] spaMM_3.11.14 twinR_1.0     cachem_1.0.6  memoise_2.0.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.8.3        pillar_1.7.0        compiler_4.1.2      tools_4.1.2         boot_1.3-28         lifecycle_1.0.1     tibble_3.1.6        gtable_0.3.0        nlme_3.1-153       
[10] lattice_0.20-45     pkgconfig_2.0.3     rlang_1.0.2         Matrix_1.4-1        registry_0.5-1      ROI_1.0-0           cli_3.2.0           DBI_1.1.2           parallel_4.1.2     
[19] fastmap_1.1.0       dplyr_1.0.8         generics_0.1.2      vctrs_0.4.0         grid_4.1.2          cowplot_1.1.1       tidyselect_1.1.2    glue_1.6.2          R6_2.5.1           
[28] fansi_1.0.3         pbapply_1.5-0       minqa_1.2.4         ggplot2_3.3.5       purrr_0.3.4         magrittr_2.0.3      scales_1.1.1        ellipsis_0.3.2      MASS_7.3-54        
[37] assertthat_0.2.1    colorspace_2.0-3    numDeriv_2016.8-1.1 utf8_1.2.2          proxy_0.4-26        munsell_0.5.0       slam_0.1-50         crayon_1.5.1   
```
