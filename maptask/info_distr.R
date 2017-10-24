# Examine the distribution of sentence information
# Yang Xu
# 6/6/2017

library(data.table)
library(ggplot2)

# read maptask data
dt.map = readRDS('map.dt.ent_swbd.rds')
setkey(dt.map, observation, who)

# density curve of information
p = ggplot(dt.map, aes(x=ent_swbd)) + geom_density()

p = ggplot(dt.map, aes(x=log(ent_swbd))) + geom_density()


# density curve of sentence length
