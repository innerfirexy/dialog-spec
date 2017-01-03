# Analyze the resulting data of maptask_timeunit_uttlen.py
# Yang Xu
# 12/30/2016

library(data.table)
library(ggplot2)

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
median(dt.agg$uttTime) # 1.036 sec
