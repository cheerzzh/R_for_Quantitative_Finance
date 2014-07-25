
library(akima)

# prepare a 3-column data frame for using interpolation
d <- data.frame(i=rep(1:nrow(t),ncol(t)),
                j=rep(1:ncol(t),each=nrow(t)),
                rate=as.vector(t))

# a procedure to convert specific date into 2 index for d
# 2010-6-9 X9M -> t[i,j] -> d[n,]

# for matching
dateIndex <- index(t)
tenorName <- names(t) 

# noew for CNY
targetList <- read.csv("outlier-case.csv",stringsAsFactors = FALSE)
# a data frame to store the possible outlier tenors on some days
# can read from a csv containing the dates and name of tneors
targetList <- data.frame(Date="2010-03-08",Tenor = "X6M")
targetList <- data.frame(lapply(targetList, as.character), stringsAsFactors=FALSE)
# for testing purpose
targetList <- rbind(targetList,c("2010-03-08","X9M"))
targetList <- rbind(targetList,c("2010-03-09","X6M"))
targetList <- rbind(targetList,c("2010-03-09","X9M"))
targetList <- rbind(targetList,c("2010-03-10","X6M"))
targetList <- rbind(targetList,c("2010-03-10","X9M"))
targetList <- rbind(targetList,c("2010-03-11","X6M"))
targetList <- rbind(targetList,c("2010-03-11","X9M"))
targetList <- rbind(targetList,c("2010-03-12","X6M"))
targetList <- rbind(targetList,c("2010-03-12","X9M"))

targetList <- rbind(targetList,c("2010-03-15","X6M"))
targetList <- rbind(targetList,c("2010-03-15","X9M"))
argetList <- rbind(targetList,c("2010-03-16","X6M"))
targetList <- rbind(targetList,c("2010-03-16","X9M"))
targetList <- rbind(targetList,c("2010-03-17","X6M"))
targetList <- rbind(targetList,c("2010-03-17","X9M"))
targetList <- rbind(targetList,c("2010-03-18","X6M"))
targetList <- rbind(targetList,c("2010-03-18","X9M"))
targetList <- rbind(targetList,c("2010-03-19","X6M"))
targetList <- rbind(targetList,c("2010-03-19","X9M"))

targetList <- rbind(targetList,c("2010-03-22","X6M"))
targetList <- rbind(targetList,c("2010-03-22","X9M"))

# loop for all rows in target list, convert into index i, j for temp
# initialize to ensure empty
index1_list <- c()
index2_list <- c()
remove <- rep(0,nrow(d))
original_value <- c()
t_new <- t # to replace interpolated value 

for (i in 1:nrow(targetList))
{
	targetDate <- as.POSIXct(targetList[i,1])
	index1 <- which(dateIndex == targetDate)
	index1_list <- c(index1_list,index1)

	targetTenor <- targetList[i,2]
	index2 <- which(tenorName == targetTenor)
	index2_list <- c(index2_list,index2)
	# t[index1,index2]
	remove <- remove | (d$i==index1 & d$j == index2)
	original_value <- c(original_value, coredata(t[index1,index2]))

}
# # remove the target tennor in target days
temp <- d[! remove,]
# original_value <- d[remove,3]

interpolated <- with(temp,interp(i, j,rate,xo=index1_list,yo=index2_list),linear=FALSE)
#interpolated[interpolated$x == index1 & interpolated$y==index2] 
# take diagnoal entry
interpolated_value <- diag(interpolated$z)
# output the result into a data frame
estimation_result <- targetList
estimation_result[,"Original value"] = original_value
estimation_result[,"Interpolated value"] = interpolated_value
estimation_result

# replace interpolated value
for(i in 1:nrow(targetList))
{
	date <- index1_list[i]
	tenor <- index2_list[i]
	t_new[date,tenor] = interpolated_value[i]
}


# =============================
# for illustration purpose
# plot the neighbour around the interpolated tenors
# replace old value with interpolated value 

par(mfrow=c(1,1))
plot.xts(t['2010-12::2011-4'], screens = factor(1, 1),auto.legend = TRUE, main = "original 2010-03 to 2010-04", xlab="day",ylab="%")
plot.xts(t_new['2010-12::2011-4'], screens = factor(1, 1),auto.legend = TRUE, main = "interpolated 2010-03-08", xlab="day",ylab="%")


# ====================


# may write out as a csv file
# merge into the original curve data set






