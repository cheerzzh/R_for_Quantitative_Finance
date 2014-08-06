library(FRAPO)
library(evir)
library(forecast)
## Data preparation 
data(StockIndexAdjD)
data(ESCBFX)
PDaily <- timeSeries(StockIndexAdjD,
                     charvec = rownames(StockIndexAdjD))
FXDaily <- timeSeries(ESCBFX, charvec = rownames(ESCBFX))
FXDaily <- window(FXDaily, start = start(FXDaily),
                  end = end(PDaily))
DDates <- time(FXDaily)
WedDays <- isWeekday(DDates, wday = 3)
FirstWed <- head(which(WedDays, arr.ind = TRUE), 1)
LastWed <- tail(which(WedDays, arr.ind = TRUE), 1)
AllWedDays <- timeSequence(from = DDates[FirstWed],
                           to = DDates[LastWed],
                           by = "week")
DumWed <- timeSeries(rep(1, length(AllWedDays)),
                     charvec = AllWedDays)
PWeekly <- interpNA(cbind(DumWed, PDaily),
                            method = "before")[AllWedDays, -1]
FXWeekly <- interpNA(cbind(DumWed, FXDaily),
                     method = "before")[AllWedDays, -1]
PEWeekly <- PWeekly
PEWeekly[, "SP500"] <- PWeekly[, "SP500"] / FXWeekly[, "USD"]
PEWeekly[, "N225"] <- PWeekly[, "N225"] / FXWeekly[, "JPY"]
PEWeekly[, "FTSE100"] <- PWeekly[, "FTSE100"] / FXWeekly[, "GBP"]
PEWeekly[, "HSI"] <- PWeekly[, "HSI"] / FXWeekly[, "HKD"]
REDWeekly <- (PEWeekly / lag(PEWeekly, k = 1) - 1)
PELWeekly <- log(PEWeekly)
## Defining moving window and indexes
epoints <- time(PELWeekly)[-c(1:259)]
size <- length(epoints)
spoints <- time(PELWeekly)[1:size]
idx <- 1:size
NAssets <- ncol(PEWeekly)
## Time series chart of euro-denominated indexes
plot(PEWeekly, main = "", xlab = "", col = "black")
