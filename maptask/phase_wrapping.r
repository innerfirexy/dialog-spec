# Explore the phase wrapping phenomena
# Yang Xu
# 2/9/2017

library(data.table)
library(ggplot2)
library(lme4)
library(MASS)
library(pracma)
library(lmerTest)

# read maptask data
dt = readRDS('map.dt.ent_swbd.rds')
setkey(dt, observation, who)

# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

# compute phase
dt.allPS = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # phase shift at all freq components
        .(allPS = spec$phase[,1], freq = spec$freq)
    }, by = observation]
dt.allPS = dt.allPS[dt.dev[, .(observation, pathdev)], nomatch=0]


# distr plot of allPS
p = ggplot(dt.allPS, aes(x = allPS)) + geom_freqpoly()
p = ggplot(dt.allPS, aes(x = allPS)) + geom_histogram(binwidth=.1)

# distr plot of pathdev
p = ggplot(dt.allPS, aes(x = pathdev)) + geom_freqpoly()
p = ggplot(dt.allPS, aes(x = pathdev)) + geom_histogram(binwidth=10)


##
# use pathdev == 100 as the threshold
p1 = ggplot() + geom_freqpoly(data = dt.allPS[pathdev > 100,], aes(x = allPS), color = 'red') +
    geom_freqpoly(data = dt.allPS[pathdev <= 100,], aes(x = allPS), color = 'blue')
###
# better performance (blue) shows that there are more phase near 0
# worse performance (red) shows phase wrapping

# simple comparison
length(unique(dt.allPS[pathdev > 100, observation])) # 27
length(unique(dt.allPS[pathdev <= 100, observation])) # 88

dt.allPS[pathdev > 100, pf:='bad']
dt.allPS[pathdev <= 100, pf:='good']

m = lm(abs(allPS) ~ pf, dt.allPS)
summary(m)
# n.s.


###
# the correlation between phase shift and freq
m = lmer(abs(allPS) ~ freq + (1|observation), dt.allPS)
summary(m)
# freq        5.102e-01  8.605e-02 5.476e+03    5.93 3.22e-09 ***
# the absolute value of phase increase with freq
# If abs is removed, then the model is n.s.

# regression plot
p = ggplot(dt.allPS, aes(x = freq, y = abs(allPS))) +
    geom_point() + geom_smooth(method='lm')



####
# explore with peak phase
dt.peakPS = dt[, {
        y_a = ent_swbd[who=='f']
        y_b = ent_swbd[who=='g']
        len = min(length(y_a), length(y_b))
        y_a = y_a[1:len]
        y_b = y_b[1:len]
        comb.ts = ts(matrix(c(y_a, y_b), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F)
        # find peaks
        peaks_a = findpeaks(spec$spec[,1])[,2]
        peaks_b = findpeaks(spec$spec[,2])[,2]
        peakPS = spec$phase[,1][union(peaks_a, peaks_b)]
        # return
        .(peakPS = peakPS)
    }, by = observation]
dt.peakPS = dt.peakPS[dt.dev[, .(observation, pathdev)], nomatch=0]

# plot
p2 = ggplot() + geom_freqpoly(data = dt.peakPS[pathdev > 100,], aes(x = peakPS), color = 'red') +
    geom_freqpoly(data = dt.peakPS[pathdev <= 100,], aes(x = peakPS), color = 'blue')
##
# not observable clustering of phase
