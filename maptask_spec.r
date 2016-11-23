# Analyze the spectral density of maptask entropy series
# Yang Xu
# 11/20/2016

library(data.table)
library(ggplot2)
library(lme4)

##
# experiment with toy data
x = seq(0, 100, .01)
y = sin(5*pi * x) + sin(pi * x) + sin(.5*pi * x)

df = data.frame(time = x, value = y)
p = ggplot(df, aes(x = time, y = value)) + geom_point()

# raw periodogram
spec.pgram(y, taper=0, log='no')
# with smooth
spec.pgram(x, spans=9, taper = 0, log="no")


##########
# analyze map task data
#
dt = readRDS('map.dt.rds')

# use one observation as example
obsv = dt$observation[1]
ent_f = dt[observation == obsv & utterID<=100 & who == 'f', ent]
ent_g = dt[observation == obsv & utterID<=100 & who == 'g', ent]

specval_f = spec.pgram(ent_f, taper=0, log="no")
specval_g = spec.pgram(ent_g, taper=0, log="no")

# smoothed
specval_sm_f = spec.pgram(ent_f, spans=c(9,9), taper=0, log="no")
specval_sm_g = spec.pgram(ent_g, spans=c(9,9), taper=0, log="no")

##
# average overall the observations
setkey(dt, observation, who)

dt.ent_raw = dt[utterID<=100, {
        if (.N > 0) {
            specval = spec.pgram(ent, taper=0, log="no")
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]

dt.ent_smooth1 = dt[utterID<=100, {
        if (.N > 6) {
            specval = spec.pgram(ent, spans=c(3,3), taper=0, log="no")
            .(spec = specval$spec, freq = specval$freq)
        }
    }, by = .(observation, who)]


# aggregated plot
p1 = ggplot(dt.ent_smooth1[who == 'f',], aes(x = freq, y = spec)) +
    geom_line(aes(color=observation)) +
    guides(color=F)

p2 = ggplot(dt.ent_smooth1[who == 'g',], aes(x = freq, y = spec)) +
    geom_line(aes(color=observation)) +
    guides(color=F)


##
# compute the position of min and max for all observations
dt.ent_raw_minmax = dt.ent_raw[, {
        i_max = which(diff(sign(diff(spec)))<0) + 1
        freq_max = ifelse(length(i_max)>0, mean(freq[i_max]), NA)
        i_min = which(diff(sign(diff(spec)))>0) + 1
        freq_min = ifelse(length(i_min)>0, mean(freq[i_min]), NA)
        .(maxFreq = freq_max, minFreq = freq_min)
    }, by = .(observation, who)]

t.test(dt.ent_raw_minmax[who=='f', maxFreq], dt.ent_raw_minmax[who=='g', maxFreq]) # t = 1.134, p = 0.2579
t.test(dt.ent_raw_minmax[who=='f', minFreq], dt.ent_raw_minmax[who=='g', minFreq]) # t = 2.8547**

dt.ent_raw_minmax[, maxmindiff := abs(maxFreq - minFreq)]
t.test(dt.ent_raw_minmax[who=='f', maxmindiff], dt.ent_raw_minmax[who=='g', maxmindiff]) # t = 1.3961, p = 0.1639

# diff between f and g
dt.ent_raw_diff = dt.ent_raw_minmax[, {
        maxFreqF = maxFreq[which(who=='f')]
        minFreqF = minFreq[which(who=='f')]
        maxFreqG = maxFreq[which(who=='g')]
        minFreqG = minFreq[which(who=='g')]
        maxDiff = abs(maxFreqF - maxFreqG)
        minDiff = abs(minFreqF - minFreqG)
        .(maxFreqF=maxFreqF, minFreqF=minFreqF, maxFreqG=maxFreqG, minFreqG=minFreqG, maxDiff=maxDiff, minDiff=minDiff)
    }, by = observation]
# join with dt
setkey(dt, observation)
dt.ent_raw_diff = dt.ent_raw_diff[unique(dt[,.(observation, resultSize)]),]

# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

# join with dt.dev
dt.ent_raw_diff = dt.ent_raw_diff[dt.dev[, .(observation, pathdev)], nomatch=0]


# model
m1 = lm(pathdev ~ maxFreqF + minFreqF + maxFreqG + minFreqG + maxDiff + minDiff, dt.ent_raw_diff)
summary(m1)
# maxDiff: t = 2.196  p = 0.0302 *
# stepwise to select variables
library(MASS)
step = stepAIC(m1, direction='both')
step$anova
# maxDiff is left

# optimal model
m2 = lm(pathdev ~ maxDiff, dt.ent_raw_diff)
summary(m2)
# maxDiff: t = 2.213, p = 0.0289 *, Adjusted R-squared: 0.03307

m2_1 = lm(pathdev ~ log(maxDiff), dt.ent_raw_diff)
summary(m2_1) # p = 0.0891.

m2_2 = lm(log(pathdev) ~ maxDiff, dt.ent_raw_diff)
summary(m2_2) # n.s.

# regression plot
p = ggplot(dt.ent_raw_diff, aes(x = maxDiff, y = pathdev)) +
    geom_point() +
    geom_smooth(method = lm)
pdf('maxdiff_vs_pathdev.pdf', 5, 5)
plot(p)
dev.off()


###
# same analysis for smoothed spectral plot
dt.ent_smooth1_maxfreq = dt.ent_smooth1[, {
        i_max = which(diff(sign(diff(spec)))<0) + 1
        freq_max = ifelse(length(i_max)>0, mean(freq[i_max]), NA)
        .(maxFreq = freq_max)
    }, by = .(observation, who)]

dt.ent_smooth1_diff = dt.ent_smooth1_maxfreq[, {
        maxFreqF = maxFreq[which(who=='f')]
        maxFreqG = maxFreq[which(who=='g')]
        maxDiff = abs(maxFreqF - maxFreqG)
        .(maxFreqF=maxFreqF, maxFreqG=maxFreqG, maxDiff=maxDiff)
    }, by = observation]

dt.ent_smooth1_diff = dt.ent_smooth1_diff[dt.dev[, .(observation, pathdev)], nomatch=0]

# model
m3 = lm(pathdev ~ maxFreqF + maxFreqG + maxDiff, dt.ent_smooth1_diff)
summary(m3)
# maxDiff: t = 1.779   p = 0.0779 . marginal significant
step  =stepAIC(m3, direction='both')
step$anova
# maxDiff is left

m4 = lm(pathdev ~ maxDiff, dt.ent_smooth1_diff)
summary(m4)
# t = 1.919   p = 0.0575 .  Adjusted R-squared:  0.023


#########
# test
summary(lmer(spec ~ who + (1|observation), dt.ent_raw)) # t = -10.78 (whog)
t.test(dt.ent_raw[who=='f', spec], dt.ent_raw[who=='g', spec]) # t = 9.8647, f > g

summary(lmer(spec ~ who + (1|observation), dt.ent_smooth1)) # t = -24.22 (whog)
t.test(dt.ent_smooth1[who=='f', spec], dt.ent_smooth1[who=='g', spec]) # t = 18.91, f > g
