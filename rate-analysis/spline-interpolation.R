
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

# a data frame to store the possible outlier tenors on some days
# can read from a csv containing the dates and name of tneors
targetList <- data.frame(Date="2010-03-08",Tenor = "X6M")
targetList <- data.frame(lapply(targetList, as.character), stringsAsFactors=FALSE)
# for testing purpose
targetList <- rbind(targetList,c("2010-03-08","X9M"))
targetList <- rbind(targetList,c("2010-03-22","X6M"))
targetList <- rbind(targetList,c("2010-03-22","X9M"))

# loop for all rows in target list, convert into index i, j for temp
# initialize to ensure empty
index1_list <- c()
index2_list <- c()
remove <- rep(0,nrow(d))
original_value <- c()

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








