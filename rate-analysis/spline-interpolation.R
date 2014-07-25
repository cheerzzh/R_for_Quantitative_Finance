

# y
x <- 1:nrow(t)
y <- 1:ncol(t)
z <- t

rgl.spheres(x,z , y,0.5,color="red")
aspect3d("iso")

rgl.bbox()

# ===
ylim <- range(y)
ylen <- ylim[2] - ylim[1] + 1
colorlut <- terrain.colors(ylen) # height color lookup table
col <- colorlut[ y-ylim[1]+1 ] # assign colors to heights for each point
rgl.open()
rgl.surface(x, y, z, color=col, back="lines")
aspect3d("iso")


plot3d(x,y,z)



z[10,3] = NA
akima.bic <- bicubic.grid(x,y,z,c(1,10),c(1,2),0.1,0.1)
image(akima.bic)
contour(akima.bic, add=TRUE)

# should use unsapced data
# delete the outlier data point
# 

t[100,2]
t[446,9]

d <- data.frame(i=rep(1:nrow(t),ncol(t)),
                j=rep(1:ncol(t),each=nrow(t)),
                rate=as.vector(t))


d[d$i == 100 & d$j ==2,3] # to test whether the 
removeList <- d$i == 100 & d$j ==2
removeList <- removeList | (d$i==446 & d$j==9)
temp <- d[-removeList,]
# subset(d , i==100 & j ==2)
d[c(100,120,143,456),]
temp <- d[-c(100,120,143,456),]

# a procedure to convert specific date into 2 index for d
# 2010-6-9 X9M -> t[i,j] -> d[n,]

dateIndex <- index(t)
targetDate <- as.POSIXct("2009-06-02")
index1 <- which(dateIndex == target)
tenorName <- names(t)
targetTenor <- "X6M"
index2 <- which(tenorName == targetTenor)

t[index1,index2]

# remove the target tennor in a day

li <- with(temp,interp(i, j,rate,xo=100,yo=2))




