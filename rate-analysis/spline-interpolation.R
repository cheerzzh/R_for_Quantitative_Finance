

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
d <- data.frame(i=rep(1:nrow(t),ncol(t)),
                j=rep(1:ncol(t),each=nrow(t)),
                rate=as.vector(t))


d[d$i == 100 & d$j ==2,3]
removeList <- d$i == 100 & d$j ==2
temp <- d[-removeList,]
# subset(d , i==100 & j ==2)

li <- with(temp,interp(i, j,rate,xo=100,yo=2))
