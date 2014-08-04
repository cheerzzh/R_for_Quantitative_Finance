
# 移动平均线的计算方法就是求连续若干天的收盘价的算术平均。天数就是MA的参数
# 
# 1. 移动平均线是股价定型后产生的图形，反映较慢，只适用于日间交易。
# 2. 移动平均线不能反映股价在当日的变化及成交量的大小，不适用于日内交易。
# 3. 移动平均线是趋势性模型，如果股价未形成趋势，只是频繁波动，模型不适用。


# fetch data
library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)


#下载数据
download <-function(stock,from="2010-01-01"){
	df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  #下载数据
	names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
	write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) #保存到本地
	}
#本地读数据
read <-function(stock){  
	as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
}

stock <- "IBM"
download(stock,from="2010-01-01")
IBM <- read(stock)

# 查看数据类型
class(IBM)
head(IBM) # 7 columns

# visualize
chartSeries(IBM)
# add some statistics
chartSeries(IBM,TA = "addVo(); addSMA(); addEnvelope();addMACD(); addROC()")


# self-defined MA model
# 我们需要自定义均线指标：

# 日期时间序列为索引
# 收盘价做为价格指标
# 不考虑成交量及其他维度字段
# 取2010-01-01至2012-01-01，形成趋势的数据
# 画出价格曲线，5日均线，20日均线，60日是均线


ma <- function(cdata, mas = c(5,20,60))
{
	ldata <- cdata
	for(m in mas)
	{
		ldata <- merge(ldata,SMA(cdata,m))# column combine
	}
	ldata <- na.locf(ldata, fromLast = TRUE)
	names(ldata) <- c("Value",paste("ma",mas,sep=""))
	return(ldata)
}



# function to plot the MA curve
drawLine <- function(ldata, title = "Stock_MA", sData = min(index(ldata))
	,eDate = max(index(ldata)), out = FALSE)
{
	g <- ggplot(aes(x = Index, y = Value), data = fortify(ldata[,1], melt=TRUE))
	g <- g + geom_line()
	g <- g + geom_line(aes(colour = Series), data = fortify(ldata[,-1],melt=TRUE))
	g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
	g<-g+xlab("") + ylab("Price")+ggtitle(title)

	if(out) ggsave(g,file=paste(titie,".png",sep=""))
	else g
}	


# try on IBM 
cdata <- IBM[,"Close"]
ldata  <- ma(cdata, c(5,30,60))
title <- "Stock IBM"
sDate <-as.Date(min(index(ldata)))
eDate <- as.Date(max(index(ldata)))
drawLine(ldata,title,sDate,eDate)


