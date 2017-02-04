# Plot the avarage spectrum for MapTask
# Yang Xu
# 2/1/2017

library(data.table)
library(ggplot2)
library(MASS)
library(pracma)
library(lme4)


dt = readRDS('map.dt.ent_swbd.rds')
dt.spec = dt[, {
        spec = spectrum(ent_swbd, taper=0, log='no', plot=F, method='pgram')
        .(spec = spec$spec, freq = spec$freq)
    }, by = .(observation, who)]
freq.base = sort(unique(dt.avg$freq))

# interpolation
dt.intpl = dt.spec[, {
        x = freq
        y = spec
        approx = approx(x, y, xout = freq.base)
        x_out = freq.base[!is.na(approx$y)]
        y_out = approx$y[!is.na(approx$y)]
        .(spec = y_out, freq = x_out)
    }, by = .(observation, who)]

# compute average spec for each freq.base value
setkey(dt.intpl, freq)
dt.spec.base = dt.intpl[, {.(spec = mean(spec))}, by = freq]
spec.base = dt.spec.base$spec

# plot
p = ggplot(dt.spec.base, aes(x = freq, y = spec)) + geom_line()
pdf('plots/specbase_vs_freqbase_mean.pdf', 5, 5)
plot(p)
dev.off()

p = ggplot(dt.intpl, aes(x = freq, y = spec)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon') +
    stat_summary(fun.y = mean, geom = 'line')
pdf('plots/specbase_vs_freqbase.pdf', 5, 5)
plot(p)
dev.off()


####
# models
dt.spec2 = copy(dt.spec)
dt.spec2[, {seriesId := .GRP}, by = .(observation, who)]

m = lmer(spec ~ freq + (1|seriesId), dt.spec2)
summary(m)
# freq          314.13      19.81   15.86***

m2 = lm(spec ~ freq, dt.spec2)
summary(m)

p = ggplot(dt.spec2, aes(x=freq, y=spec)) +
    geom_smooth(method='lm')


#####
# white noise baseline
len.mean = floor(mean(dt[, .N, by = .(observation, who)]$N)) # 105
ent_swbd.mean = mean(dt$ent_swbd)
ent_swbd.sd = sd(dt$ent_swbd)

dt.rnorm = data.table()
for (i in 1:100) {
    set.seed(i)
    tmp = data.table(sid = i, s = rnorm(len.mean, mean = ent_swbd.mean, sd = ent_swbd.sd))
    dt.rnorm = rbindlist(list(dt.rnorm, tmp))
}

dt.rnorm.spec = dt.rnorm[, {
        spec = spectrum(s, taper=0, log='no', plot=F, method='pgram')
        .(spec = spec$spec, freq = spec$freq)
    }, by = sid]

# plot
p = ggplot(dt.rnorm.spec, aes(x=freq, y=spec)) +
    geom_smooth(method='lm')


#####
# plot actual spectrum and baseline together
dt.plot1 = dt.spec[, .(spec, freq)]
dt.plot1$Type = 'Actual data'
dt.plot2 = dt.rnorm.spec[, .(spec, freq)]
dt.plot2$Type = 'White noise'
dt.plot = rbindlist(list(dt.plot1, dt.plot2))

p = ggplot(dt.plot, aes(x=freq, y=spec, group=Type)) +
    geom_smooth(aes(color = Type, lty = Type), method='lm') +
    xlab('Frequency') + ylab('Power density') +
    theme_bw() +
    theme(legend.position = c(.75,.17))
pdf('plots/average_spectrum_MapTask.pdf', 3, 3)
plot(p)
dev.off()


############
# plot Map Task and DJD together

# read DJD data
dt.DJD = fread('../danish/fusaroli2015_exp/data/all_pairs_entropy.txt')
setnames(dt.DJD, c('pairId', 'who', 'ent'))
setkey(dt.DJD, pairId, who)

dt.DJD.spec = dt.DJD[, {
        spec = spectrum(ent, taper=0, log='no', plot=F, method='pgram', spans=c(7,7))
        .(spec = spec$spec, freq = spec$freq)
    }, by = .(pairId, who)]
