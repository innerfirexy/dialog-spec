# Analyze the resulting data of maptask_timeunit_uttlen.py
# Yang Xu
# 12/30/2016

library(data.table)
library(ggplot2)
library(lme4)
library(MASS)
library(pracma)

# read data
dt = fread('model-data/maptask_timeunit_survey.csv')

dt$uttId = as.integer(dt$uttId)

# aggregate by convId and uttId
setkey(dt, convId, uttId)
dt.agg = dt[!is.na(uttId), .(uttTime = sum(time)), by = .(convId, uttId)]

# plot
plot(density(dt.agg$uttTime))
plot(density(log(dt.agg$uttTime)))

# boxplot
p = ggplot(dt.agg, aes(x = 1, y = uttTime)) + geom_boxplot()
mean(dt.agg$uttTime) # 1.6
median(dt.agg$uttTime) # 1.036 sec --> this is the basis of why we choose to segment signals with 1 sec interval


####
# analyze even-time data
resfiles = Sys.glob(file.path(paste0(getwd(), '/model-data/even-time/*.txt')))
# examine how many rows have different V2 and V3 column
diffnum = 0
zeronum = 0
difffiles = c()
for (rf in resfiles) {
    dt.tmp = fread(rf)
    diffnum <<- diffnum + nrow(dt.tmp[V2 != V3 & V2!=0 & V3!=0,])
    zeronum <<- zeronum + nrow(dt.tmp[V2==0 | V3==0,])
    if (nrow(dt.tmp[V2 != V3,]) > 0) {
        difffiles <<- c(difffiles, rf)
    }
}
# diffnum == 0 !this is what we want
# zeronum == 3993



########################
# examine even-time data under the even-time-results folder
#
resfiles = Sys.glob(paste0(getwd(), '/model-data/even-time-results/*.txt'))

dt = data.table()
for (rf in resfiles) {
    dt.tmp = fread(rf)
    dt = rbindlist(list(dt, dt.tmp))
}
setnames(dt, 'entropy')

# boxplot
p = ggplot(dt[entropy<4.5,], aes(x=1, y=entropy)) + geom_boxplot()

summary(dt$entropy)
# Median = 2.51, 3rd Qu = 4.475
nrow(dt[entropy<4.5,]) / nrow(dt) # 75%

# density curve
p = ggplot(dt, aes(entropy)) + geom_density()
p1 = ggplot(dt[entropy<4.5,], aes(entropy)) + geom_density()
