# Comparison of multi-model runs

R package **mip**, version **0.149.3**

[![CRAN status](https://www.r-pkg.org/badges/version/mip)](https://cran.r-project.org/package=mip) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158586.svg)](https://doi.org/10.5281/zenodo.1158586) [![R build status](https://github.com/pik-piam/mip/workflows/check/badge.svg)](https://github.com/pik-piam/mip/actions) [![codecov](https://codecov.io/gh/pik-piam/mip/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mip) [![r-universe](https://pik-piam.r-universe.dev/badges/mip)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Package contains generic functions to produce comparison
    plots of multi-model runs.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mip")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("mif") # Model Intercomparison File Format (MIF)
```

## Questions / Problems

In case of questions / problems please contact David Klein <dklein@pik-potsdam.de>.

## Citation

To cite package **mip** in publications use:

Klein D, Dietrich J, Baumstark L, Humpenoeder F, Stevanovic M, Wirth S, Führlich P, Richters O, Rüter T (2024). _mip: Comparison of multi-model runs_. doi:10.5281/zenodo.1158586 <https://doi.org/10.5281/zenodo.1158586>, R package version 0.149.3, <https://github.com/pik-piam/mip>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mip: Comparison of multi-model runs},
  author = {David Klein and Jan Philipp Dietrich and Lavinia Baumstark and Florian Humpenoeder and Miodrag Stevanovic and Stephen Wirth and Pascal Führlich and Oliver Richters and Tonn Rüter},
  year = {2024},
  note = {R package version 0.149.3},
  url = {https://github.com/pik-piam/mip},
  doi = {10.5281/zenodo.1158586},
}
```
