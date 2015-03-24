#' Optimal Model Structure Exploration
#' 
#' This function allows the user to explore all permutations of a set of parameters for mixed-effects models.
#' @param dependent String. Name of dependent variable.
#' @param intercepts String. Vector of names of possible predictors.
#' @param predictors String. Vector of names of all random intercepts to be included.
#' @param dataFrame String. Name of data frame that includes all dependent, predictor, and intercept variables.
#' @param REMLfit String. Model fit ("TRUE" = REML fit; "FALSE" = maximum likelihood estimation) for lme4.
#' @return Data frame with 3 variables. "model": string vector listing parameters of each model run. 
#'  "fit": model fit statistic, given no warnings occurred while running. "warnings": warning(s) produced
#'  during attempt to run, separated by semicolon. "fit" and "warnings" revert to "n/a" if aforementioned
#'  conditions not met.
#' @keywords mixed-effect-models model-fit exploratory
#' @examples
#' mrest(mpg,carTypes,weatherConditions,carData,REMLfit)
#' mrest(energy,caffeineDetails,personalTraits,coffeeData,REMLfit)
#' @seealso \code{\link{mrest}} for exploring permutations of possible random slopes.

# create function
omse <- function(dependent,predictors,intercepts,dataFrame,REMLfit){
  
  # generate linear mixed-effects model shell
  modelName = "model = "
  modelShell = paste("lme4::lmer(",dependent," ~ ",sep="")
  dataShell = paste(", data = ",dataFrame,",REML = ",REMLfit,")",sep="")
  
  # generate random slopes structures for all possible combinations
  slopeEntries = c() # will later be used for main effects, too
  for (j in 1:length(predictors)){
    slopeCombinations = gtools::permutations(n=length(predictors),r=j,v=predictors,repeats.allowed=FALSE)
    for (k in 1:dim(slopeCombinations)[1]){
      slopeEntries = rbind(slopeEntries, paste(slopeCombinations[k,],collapse = " + "))
    }}
  
  # bind 
  interceptShell = c()
  for (j in 1:length(intercepts)){
    interceptShell = cbind(interceptShell,paste("(1 + ",slopeEntries," | ",intercepts[j],")",sep=""))
  }
  finalRandom = c()
  for (j in 1:length(slopeEntries)){
    finalRandom = c(finalRandom,paste(interceptShell[j,],collapse=" + "))
  }
  finalRandom = c(paste(" + ",finalRandom,sep=""))
  
  # catch convergence warnings and store them
  tryConverge <- function(mod){
    warnSet = NULL
    warn.handler <- function(warn){
      warnSet <<- warn
      invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(mod, error=function(e) e), warning=warn.handler),warning=warnSet)
  }
  
  # run all models
  mresVals = matrix("a",length(finalRandom)*length(slopeEntries),3)
  track = 0
  for (k in 1:length(slopeEntries)){
    for (i in 1:length(finalRandom)){
      
      # attempt convergence for with next random effects structure
      (eval(parse(text=paste("warnings = tryConverge(",modelShell,slopeEntries[k],finalRandom[i],dataShell,")",sep=""))))
      
      # if no warning is produced, run model again and save values
      if (length(warnings$warning) == 0){
        warnings$warning = "n/a"
        (eval(parse(text=paste("model = ",modelShell,slopeEntries[k],finalRandom[i],dataShell,sep=""))))
        aic = summary(model)$AIC[[1]]
      }
      
      # if there's a warning, save warning message
      if (length(warnings$warning) > 1){
        warnings$warning = paste(warnings$warning,collapse=";")
        aic = "n/a"
      }
      
      # store to matrix
      track = track + 1
      mresVals[track,] = c(paste(modelName,modelShell,slopeEntries[k],finalRandom[i],dataShell,sep=""),aic,warnings$warning) 
    }}
  mresVals = data.frame(mresVals)
  mresVals = plyr::rename(mresVals,c("X1"="model","X2"="fit","X3"="warnings"))
  
  return(mresVals)
}