# Analyze model-data/move_label.txt
# Yang Xu
# 1/30/2017

library(data.table)
library(RMySQL)

# read data
dt.ml = fread('model-data/move_label.txt')
setnames(dt.ml, c('observation', 'who', 'text', 'moveType'))
setkey(dt.ml, observation, who, text)
