library(xts)
library(xtsExtra)

#currency <- read.csv('INR266.csv',stringsAsFactors = FALSE)
index <- read.csv("MYR209_currency.csv",stringsAsFactors = FALSE)

data <- index
dates <- data$Date
rates <- data[,-1]
dates <- as.POSIXct(dates,format ="%m/%d/%Y")
t <- as.xts(rates, order.by = dates)
  # scale 
rate_name <- names(t)
span <- 0.1

tsoutliers <- function(x,plot=TRUE,span=0.1,name="",percentile = c(0.2,0.75),k=3,range = c(-1.1))
{
    # x <- as.ts(x)
    if(frequency(x)>1)
        resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
    else
    {
        tt <- 1:length(x)
        #resid <- residuals(loess(x ~ tt,span=span))

        loe <- loess(x~tt, span = span)
    pred <- predict(loe,tt,se=TRUE)
    resid <- loe$residuals
    }


    # can adjust the parameters to control the outlier 
    resid.q <- quantile(resid,prob= percentile)
    iqr <- diff(resid.q)
    limits <- resid.q + k*iqr*range
    score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
    indicator <- ifelse(score>0,1,0)
    indi <- cbind(indicator,x)[,1]

    if(plot)
    {
        custom.panel <- function(index,x,...) {
        default.panel(index,x,...)
        points(x=index(indi[indi == 1]),
          y=x[,1][index(indi[indi == 1])],cex=0.9,pch=19,
          col="blue")
          #abline(v=index(indi[indi == 1]),col="grey")
    }

      newlist <- list(score = score, resid = resid,indicator = indicator)
    plot.xts(x=cbind(x,fitted=loe$fitted,residual=loe$residuals),panel = custom.panel, screens = factor(1, 1), auto.legend = TRUE, main = paste("LOESS plot",rate_name[i],sep=" "))

        cat("Number of outliers for ",name," is ", sum(indicator),"\n")
        return(invisible(newlist))
    }
    else
        return(list(score,resid,indicator))
}

jpeg(file = " MYR209 currency LOESS plot %d.jpeg",quality=100,width = 1200, height = 800,units = 'px', pointsize = 12)

indi <- rep(0,nrow(t)) # initialize the indicator vector

for (i in 1:ncol(t))
{
  a <- tsoutliers(t[,i],name = rate_name[i],span=span, percentile = c(0.1,0.9),k=3,range=c(-1,1))
# if any col in one raw has 1, label this row as 1
indi <- indi | a$indicator
}
dev.off()

indi <- cbind(indi,t)[,1]
num <- sum(indi)
per <-sum(indi)/nrow(t)

cat("\ntotal number of outliers is ", num)
cat("\npercentage of outliers is ",per)
# percenatge of potential outlier

jpeg(file = "MYR209 currency.jpeg",quality=100,width = 1200,height = 800,units = 'px', pointsize = 12)

# try to add points to show outlier
# index(indi[indi == 1]) get outlier
custom.panel <- function(index,x,...) {
  default.panel(index,x,...)
  abline(v=index(indi[indi == 1]),col=rgb(1,0,0,0.2),lwd=0.7)
  usr <- par( "usr" )
  text( usr[ 2 ], usr[ 4 ], paste("number of outliers: ",num,"\n","ratio: ",format(per,digits=4)),  adj = c( 1, 1 ), col = "blue" )

}

plot.xts(t, screens = factor(1, 1), panel = custom.panel,auto.legend = TRUE, main = "MYR209 currency", xlab="day",ylab="%")
dev.off()

# notice: some period has massive increase/ decrease, my detect by hand

data <- cbind(indi,t)
data <- data.frame(date=index(data), coredata(data))
colnames(data)[2] <- "indicator" 
write.csv(data, file = "MYR209_currency_outlier.csv",row.names=TRUE)


ret <-  (t/lag(t,1) - 1)[-1,]
potential_day <- ret[index(indi[indi==1]),]
head(potential_day) # take a quick view

# plot all graph first
# coredata(potential_day[1,])
jpeg(file = " MYR209 currency check plot %d.jpeg",quality=100,width = 1200, height = 800,units = 'px', pointsize = 12)
par(mfrow=c(3,3))
for(i in 1 : nrow(potential_day))
{
  dat <- coredata(potential_day[i,])
  plot(c(dat),col=ifelse(c(dat)==0, "black", ifelse(c(dat)>0,"blue","red")),
      pch=16,axes=FALSE,xlab = "Tenor",ylab = "scale of change",main = index(potential_day[i,]))
  axis(2)
  axis(1, at=seq_along(c(dat)),labels=names(potential_day), las=2)
  box()
  abline(h=0,lty="dashed",col="chartreuse4")
  legend("topleft", pch = c(15, 15, 15),col = c("blue", "black","red"),legend = c(">0","=0","<0"))
}
dev.off()


library(rgl)
persp3d(x = seq(1, nrow(t), len = nrow(t)), y = seq(1, ncol(t), len = ncol(t)), t,
    xlab = "date", ylab = "tenor", zlab = "r", col = "lightblue",ltheta = 120, shade = 0.75, ticktype = "detailed")

x = seq(1, nrow(t), len = nrow(t))
y = seq(1, ncol(t), len = ncol(t))  
z = t

surf3D(x, y, z, colvar = z, colkey = FALSE, box = FALSE)
