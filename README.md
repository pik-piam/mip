# R mip package

## Purpose and Functionality

The mip package provides a visualization tools for model intercomparison results stored in quitte class format.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mip")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```
## Standards for the Model-Intercomparison-Plots (MIP) Library

The MIP-Library contains functions that have the following attributes:

*NAME:* The functions start with *mip*, followed by the type of plot (first letter capital) and afterwards some further description (e.g. mipLineHistorical.R, mipCrossbarDiscreteX.R)

*INPUT:* All functions should be able to read in *magpie* as well as *quitte* objects. Internally the class can be changed by as.quitte or as. magpie. Please check if the input data are in the right format (quitte or magpie) and give a meaningful error message if not

*OUTPUT:* The returned object of the functions is a ggplot-object that can be adjusted, printed, saved, etc. afterwards
The data shall show the *full dimensionality* of a read in [[Model_Intercomparison_File_Format_(mif)|mif-file]]. That means it has at least the dimensions: _Model;Scenario;Region;Variable;Unit_. If for a specific function more dimensions are needed, please check if it exists and give a meaningful error message if not

*ARGUMENTS:* please use standardized names for the data-arguments (*x* - default data, *x_hist* - historical data, *x_valid* - validation data) to facilitate the use of the interactive plotting function scenTool.R

As we would like to use these functions also for the validation of a single model output all functions should be able to deal with data that contain only *one scenario and one model*

Use function *plotstyle* for colour coding and styles

If possible, please add some *plot examples* to the manual

## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.


## Citation

[![DOI](https://zenodo.org/badge/103911845.svg)](https://zenodo.org/badge/latestdoi/103911845)

