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


## Info for maintainer

In `fitted_models/` we only store the latest version of the fitted models (using git lfs).
If it takes more than what is allowed for storage, `mv` the folder using `fitted_models_backup`, commit the deletion, then run `java -jar ~/bfg.jar --delete-folders fitted_models` (see [bfg](https://rtyley.github.io/bfg-repo-cleaner/)), then `git reflog expire --expire=now --all && git gc --prune=now --aggressive`, then `git push`, then `mv` back the folder under its original name and commit its content again!

