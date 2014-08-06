## Dimensions of Simulation
DGP <- c("rNcopMargN", "rNcopMargT", "rTcopMargT")
EST <- c("CovClassic", "CovMcd", "CovMest", "CovMMest",
         "CovMve", "CovOgk", "CovSde", "CovSest")
SAMPLE <- c(60, 120, 240)
## Creating list objects for combinations of
## DGP and sample sizes
## initialising vector for data objects
datnames <- NULL
for(i in DGP){
  for(j in SAMPLE){
    objname <- paste(i, j, sep = "")
    datnames <- c(datnames, objname)
    cat(paste("Creating list object", objname, "\n"))
    assign(objname, lapply(eval(as.name(i)), function(x) x[1:j, ]))
  }
}
## Creating list objects with estimates of 
## location and dispersion for combinations of
## DGP, sample sizes and estimators
## initialising vector for list objects
objnames <- NULL
for(i in datnames){
  for(j in EST){
    objname <- paste(j, i, sep = "")
    objnames <- c(objnames, objname)
    cat(paste("Creating list object", objname, "\n"))
    assign(objname, lapply(eval(as.name(i)), Moments, method = j))
  }
}
