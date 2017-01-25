# Examine the peak frequencies of giver vs. follower
# Yang Xu
# 1/25/2017

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

dt.spec = dt[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]


#######
# examine the freqency that has the max spec value
dt.maxfreq = dt.spec[, {
        maxfreq = freq[spec == max(spec)]
        .(maxfreq = maxfreq)
    }, by = .(observation, who)]

# compare maxfreq by giver vs follower
m = lm(maxfreq ~ who, dt.maxfreq)
summary(m)
# 4.162 4.32e-05 ***
# givers have higher max frequency than follower
mean(dt.maxfreq[who=='g', maxfreq]) # 0.357
mean(dt.maxfreq[who=='f', maxfreq]) # 0.282

# compare density
plot(density(dt.maxfreq[who=='g', maxfreq]))
plot(density(dt.maxfreq[who=='f', maxfreq]))
##
# the overlaping area between g and f, largely lies in the high freq area


#######
# examine the weighted mean frequencies
dt.wmfreq = dt.spec[, {
        wmfreq = weighted.mean(freq, spec)
        .(wmfreq = wmfreq)
    }, by = .(observation, who)]

m = lm(wmfreq ~ who, dt.wmfreq)
summary(m)
# 7.199 6.86e-12 ***
# givers have higher mean frequency than follower

# plot mean frequency
plot(density(dt.wmfreq[who=='g', wmfreq]))
plot(density(dt.wmfreq[who=='f', wmfreq]))

##
# does the mean frequency correlate with pathdev
dt.wmfreq2 = dt.wmfreq[, {
        wmfreq_g = wmfreq[who=='g']
        wmfreq_f = wmfreq[who=='f']
        wmfreq_dist = abs(wmfreq_g - wmfreq_f)
        wmfreq_mean = (wmfreq_g + wmfreq_f) / 2
        .(wmfreq_g = wmfreq_g, wmfreq_f = wmfreq_f, wmfreq_dist = wmfreq_dist, wmfreq_mean = wmfreq_mean)
    }, by = observation]
dt.wmfreq2 = dt.wmfreq2[dt.dev[, .(observation, pathdev)]]

m = lm(pathdev ~ wmfreq_g + wmfreq_f + wmfreq_dist + wmfreq_mean, dt.wmfreq2)
summary(m)
step = stepAIC(m)
step$anova
# Final Model:
# pathdev ~ wmfreq_g
m = lm(pathdev ~ wmfreq_g, dt.wmfreq2)
summary(m)
# insignificant


##########
# if we remove all the entropy outliers in data
# how does the density curve of the max peak freq change?
# (We expect it to lose some weight in high frequency area)
ent_swbd.mean = mean(dt$ent_swbd)
ent_swbd.sd = sd(dt$ent_swbd)

dt[,outlier:=F][ent_swbd > ent_swbd.mean + 2*ent_swbd.sd, outlier:=T]
dt.rmol = dt[,]
dt.rmol[outlier==T, {ent_swbd := ent_swbd.mean}]

dt.spec.rmol = dt.rmol[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]

dt.maxfreq.rmol = dt.spec.rmol[, {
        maxfreq = freq[spec == max(spec)]
        .(maxfreq = maxfreq)
    }, by = .(observation, who)]

plot(density(dt.maxfreq.rmol[who=='g', maxfreq]))
plot(density(dt.maxfreq.rmol[who=='f', maxfreq]))


###
# compare wmfreq.rmol vs wmfreq
dt.wmfreq.rmol = dt.spec.rmol[, {
        wmfreq = weighted.mean(freq, spec)
        .(wmfreq = wmfreq)
    }, by = .(observation, who)]

t.test(dt.wmfreq.rmol$wmfreq, dt.wmfreq$wmfreq) # n.s.
t.test(dt.wmfreq.rmol[who=='g']$wmfreq, dt.wmfreq[who=='g',]$wmfreq) # n.s.
t.test(dt.wmfreq.rmol[who=='f']$wmfreq, dt.wmfreq[who=='f']$wmfreq) # n.s.

# compare maxfreq.rmol vs maxfreq
t.test(dt.maxfreq.rmol$maxfreq, dt.maxfreq$maxfreq)
