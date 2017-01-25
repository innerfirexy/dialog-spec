# Experiment with granger causality
# Yang Xu
# 1/25/2017

library(lmtest)
library(data.table)
library(ggplot2)
library(MASS)
library(pracma)


# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

# read data
dt = readRDS('map.dt.ent_swbd.rds')

###
# analysis
dt.gc = dt[, {
        ent_g = ent_swbd[who=='g']
        ent_f = ent_swbd[who=='f']
        comb.ts = ts(matrix(c(ent_g, ent_f), ncol=2))
        colnames(comb.ts) = c('g', 'f')
        m = grangertest(f ~ g, order = 1, data = comb.ts)
        .(Fscore = m$F[2])
    }, by = observation]

dt.gc = dt.gc[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ Fscore, dt.gc)
summary(m)
# n.s.
