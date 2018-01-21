# paramtests: Automated exploration of mixed-effects model parameters in R

This package facilitates explorations of mixed-effects models by automatically running all permutations of the various parameters. The functions currently only address linear mixed-effects models, although mixed logit models are in development. Note the dependencies required for each of the functions in the package, listed below in "Dependencies."

## Current functions
* `mrest` (Maximal Random Effects: Slope Testing): For linear mixed-effects models. This function runs all permutations of random effects structures and outputs resulting model fit statistics OR warnings produced. This function uses fixed dependent variables, predictors, and random intercepts.
* `omse` (Optimal Model Structure Exploration): For linear mixed-effects models. This function runs all permutations of fixed and random effects model structures and outputs resulting model fit statistics OR warnings produced. This function uses fixed dependent variables and random intercepts.

## Dependencies
`paramtests` functions require the following packages to run:
* `mrest`: `gtools`, `lme4`, `plyr`
* `omse`: `gtools`, `lme4`, `plyr`

## Installation
This package may be downloaded and installed or may be directly installed from R.

### Downloading and installing manually from R
1. Download ZIP of paramtests.
2. Unzip file and change folder name to `paramtests` (from `paramtests-master`).
3. Move folder to target location.
4. In R, change working directory to new parent folder for "paramtests": 
	```
	setwd("/parent-folder-here/")
	```
5. Install.
	```
	install("paramtests")
	```
6. Confirm installation.
	```
	?paramtests
	?omse
	?mrest
	```

### Installing from GitHub in R
1. Install (if necessary) and load `devtools` package.
	```
	install.packages("devtools")
	library(devtools)
	```
2. Use `install_github()` function to install directly.
	```
	install_github("a-paxton/paramtests",dependencies=TRUE)
	```
