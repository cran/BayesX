#####################################################################################
## Author: Daniel Sabanes Bove [daniel *.* sabanesbove *a*t* campus *.* lmu *.* de]
## Time-stamp: <[extractSamples.R] by DSB Mon 23/02/2009 22:15 (CET) on daniel@puc-home>
##
## Description:
## Extract samples and prediction info from BayesX results directory.
##
## History:
## 05/02/2009   file creation
## 06/02/2009   add na.string "." to import of predictions table
## 10/02/2009   add import of spatial estimates
## 23/02/2009   plug into BayesX package
#####################################################################################


## returns list of MCMC objects for all samples in BayesX results directory
extractSamples <- function(directoryWithBasename,
                           logfile= # log with the prior specifications, necessary for pspline
                                        # and MCMC option extraction
                           file.path(dirname(directoryWithBasename),
                                     "log.txt"))
{
    ## extracts
    resBasename <- basename(directoryWithBasename)
    resDirname <- dirname(directoryWithBasename)

    ## upgrade to full absolute path
    directoryWithBasename <- file.path(resDirname, resBasename)
    
    ## which files are to be processed?
    resFiles <- list.files(path=resDirname,
                           pattern=
                           paste(resBasename,
                                 "_.+_sample\\.raw",
                                 sep=""),
                           full.names=TRUE)

    ## Extract MCMC parameters from log file
    bayesxLog <- readLines(logfile)

    ## convenience function:
    getNumbers <- function(stringList,  # list with the strings before the numbers
                           stringVector) # look for the strings in this vector
    {
        lapply(stringList,
               function(string)
               as.numeric(sub(pattern=".+[[:blank:]]+([[:digit:]]+)$",
                              replacement="\\1",
                              x=
                              stringVector[grep(pattern=
                                                paste(".*",
                                                      string),
                                                x=stringVector)])))
    }

    numbers <- getNumbers(stringList=
                          list(Iterations="Number of iterations:",
                               BurnIn="Burn-in period:",
                               Thin="Thinning parameter:"),
                          stringVector=bayesxLog)

    ## setup return list
    ret <- list()

    ## more convenience functions:
    convert2Mcmc <- function(samples)   # just a shortcut
        coda::mcmc(data=samples,
                   start=numbers$BurnIn + 1,
                   end=numbers$Iterations,
                   thin=numbers$Thin)
    
    readData <- function(file)          # just a shortcut
        read.table(file,
                   header=TRUE,
                   row.names=1)

    getResFile <- function(sampleFile)  # just a shortcut
        sub(pattern="(.+)_sample\\.raw",
            replacement="\\1\\.res",
            x=sampleFile)
    
    readNamedSamples <-
        function(sampleFile, # .raw file
                 resFile=getResFile(sampleFile)) # corresponding .res file containing the varnames 
    {
        ## read unnamed data
        sampleData <- readData(sampleFile)
        ## and label correctly, if possible
        sampleNames <- readData(resFile)[, 1] # assumes that names are in first column!
        if(identical(ncol(sampleData),
                     length(sampleNames)))
        {
            colnames(sampleData) <- sampleNames
        }
        
        return(sampleData)
    }

    
    ## process nonlinear functions with rw or spatial priors
    rwInds <- grep(pattern=".+(rw|spatial)_sample\\.raw",
                   x=resFiles)  
    for(sampleFile in resFiles[rwInds])
    {
        ## read function samples from this file
        functionSamples <- readNamedSamples(sampleFile)

        ## and the variance samples from the corresponding file
        varianceSamples <- readData(sub(pattern="(.+)_sample\\.raw",
                                        replacement="\\1_variance_sample\\.raw",
                                        x=sampleFile))[, 1]
        
        ## coerce to MCMC objects and insert into list with correct name
        functionName <- sub(pattern=
                            paste(directoryWithBasename,
                                  "(.+)_(rw|spatial)_sample\\.raw",
                                  sep="_"),
                            replacement="\\1",
                            x=sampleFile)
        ret[[functionName]] <- list(functionSamples=convert2Mcmc(functionSamples),
                                    varianceSamples=convert2Mcmc(varianceSamples))
    }

    
    ## process nonlinear functions modelled as psplines
    psplineInds <- grep(pattern=".+pspline_sample\\.raw",
                       x=resFiles)  
    for(sampleFile in resFiles[psplineInds])
    {       
        ## read the variance samples from the corresponding file
        varianceSamples <- readData(sub(pattern="(.+)_sample\\.raw",
                                        replacement="\\1_variance_sample\\.raw",
                                        x=sampleFile))[, 1]

        ## get corresponding covariate values (or gridpoints if it was restricted)
        covValues <- readData(getResFile(sampleFile))[, 1]

        ## get name of function
        functionName <- sub(pattern=
                            paste(directoryWithBasename,
                                  "(.+)_pspline_sample\\.raw",
                                  sep="_"),
                            replacement="\\1",
                            x=sampleFile)
        
        ## extract pspline parameters from log file
        optionsPart <- bayesxLog[grep(pattern=
                                      paste("[[:blank:]]*OPTIONS FOR P-SPLINE TERM:",
                                            functionName),
                                      x=bayesxLog)
                                 + (1:10)] # look for numbers in the next ten lines
        
        optionsNumbers <- getNumbers(stringList=
                                     list(knots="Number of knots:",
                                          degree="Degree of Splines:"),
                                     stringVector=optionsPart)

        ## build design matrix of basis function values
        eps <- 0.001
        minx <- min(covValues) - eps
        maxx <- max(covValues) + eps
        
        step <- (maxx - minx) / (optionsNumbers$knots - 1)
        knots <- seq(from=minx - optionsNumbers$degree * step,
                     to=maxx + optionsNumbers$degree * step,
                     by=step)

        design <- splines::spline.des(knots=knots,
                                      x=covValues,
                                      ord=optionsNumbers$degree + 1)$design

        ## and generate function samples at covariate values from basis functions coefficients 
        coefSamples <- as.matrix(readData(sampleFile))

        functionSamples <- tcrossprod(coefSamples, design)
        colnames(functionSamples) <- covValues
        
        ## write function and variance samples into list
        ret[[functionName]] <- list(functionSamples=convert2Mcmc(functionSamples),
                                    varianceSamples=convert2Mcmc(varianceSamples))
    }

    
    ## process fixed effects
    fixedInds <- grep(pattern=".+FixedEffects[[:digit:]]+_sample\\.raw",
                      x=resFiles)
    if(length(fixedInds))
    {
        samplesMatrix <- matrix(nrow=
                                with(numbers,
                                     (Iterations - BurnIn) / Thin),
                                ncol=0)
        
        for(sampleFile in resFiles[fixedInds])
        {
            samplesMatrix <- cbind(samplesMatrix,
                                   as.matrix(readNamedSamples(sampleFile)))
        }
        ret$FixedEffects <- convert2Mcmc(samplesMatrix)
    }

    ## process deviance
    devianceInd <- grep(pattern=
                        paste(directoryWithBasename,
                              "deviance_sample\\.raw",
                              sep="_"),
                        x=resFiles)
    if(length(devianceInd))
    {
        ret$Deviance <- convert2Mcmc(head(readData(resFiles[devianceInd]),
                                          -2)) # discard last two lines with pD and DIC
    }    

    ## process prediction means
    ret$PredictMeans <- read.table(paste(directoryWithBasename,
                                         "predictmean.raw",
                                         sep="_"),
                                   header=TRUE,
                                   na.strings=c("NA", "."))
    
    ## finished!
    return(ret)
}
