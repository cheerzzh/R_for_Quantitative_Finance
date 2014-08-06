## Function for minimum-variance portfolio
## Constraints: Fully invested, long-only
PortMinVar <- function(x){
  Dmat <- x
  k <- ncol(Dmat)
  dvec <- rep.int(0, k)
  a1 <- rep.int(1, k)
  b1 <- 1
  a2 <- diag(k)
  b2 <- rep.int(0, k)
  Amat <- t(rbind(a1, a2))
  bvec <- c(b1, b2)
  opt <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat,
                  bvec = bvec, meq = 1)
  return(opt$solution)
}
## Conduct optimisation
portnames <- NULL
idx <- 1:1000
for(i in objnames){
    objname <- paste("Port", i, sep = "")
    portnames <- c(portnames, objname)
    obj <- eval(as.name(i))
    weights <- lapply(obj, PortMinVar)
    assign(objname, sapply(idx, function(x)
                    sqrt(t(weights[[x]]) %*% obj[[x]] %*%
                         weights[[x]])))
}
## Caluculate median and IQR of portfolio risks
mednames <- NULL
iqrnames <- NULL
for(i in portnames){
  objname1 <- paste("Med", i, sep = "")
  objname2 <- paste("IQR", i, sep = "")
  mednames <- c(mednames, objname1)
  iqrnames <- c(iqrnames, objname2)
  assign(objname1, median(eval(as.name(i))))
  assign(objname2, IQR(eval(as.name(i))))
}

