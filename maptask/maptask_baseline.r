# Compute the baseline for shuffling the time series for each dialogue speaker
# Yang Xu
# 1/23/2017

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

dt.shuffle = dt[, {
        .(ent = sample(ent_swbd))
    }, by = .(observation, who)]

dt.spec = dt.shuffle[, {
        specval = spectrum(ent, taper=0, log='no', plot=FALSE, method='pgram')
        .(spec = as.numeric(specval$spec), freq = specval$freq)
    }, by = .(observation, who)]
dt.pso = dt.spec[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO)
    }, by = observation]
dt.pso = dt.pso[dt.dev[, .(observation, pathdev)], nomatch=0]

# model
m = lm(pathdev ~ PSO, dt.pso)
summary(m)
###
## oh, dear!
# we got significant results!


########################
# solutions: filter out noise from spectrum plots
########################
###
# the function that gives the average spectrum by shuffling the serires for given times
avgSpectrum = function(series, ntime = 100) {
    # get the results of running ntime times shuffling spectrum analysis
    mt = matrix()
    for (i in 1:ntime) {
        series_sf = sample(series)
        spec = spectrum(series_sf, taper=0, log='no', plot=FALSE, method='pgram')
        if (i==1) {
            mt = matrix(as.numeric(spec$spec))
        } else {
            mt = cbind(mt, as.numeric(spec$spec))
        }
    }
    data.table(spec = apply(mt, 1, mean), freq = spec$freq)
}

# experiment
s1 = dt[observation=='q1ec1' & who=='g', ent_swbd]
s1.spec = spectrum(s1, taper=0, log='no', plot=F, method='pgram')
s1.avgspec = avgSpectrum(s1, ntime=100)

s1.plot = cbind(s1.avgspec, 'avg')
setnames(s1.plot, 'V2', 'type')
s1.plot = rbindlist(list(s1.plot, data.table(spec = s1.spec$spec, freq = s1.spec$freq, type='actual')))

p = ggplot(s1.plot, aes(x = freq, y = spec, group = type)) + geom_line(aes(color=type))

###
# the function that filters out the average spectrum
filterAvgSpec = function(serires, ntime=100) {
    spec.origin = spectrum(series, taper=0, log='no', plot=F, method='pgram')
    spec.avg = avgSpectrum(series, ntime=ntime)
    spec = spec.origin$spec
    # spec[spec.origin$spec < spec.avg$spec] = 0
    # spec[spec.origin$spec < spec.avg$spec] = spec.avg$spec[spec.origin$spec < spec.avg$spec]
    # spec[spec.origin$spec < spec.avg$spec] = (spec.avg$spec[spec.origin$spec < spec.avg$spec] + spec.origin$spec[spec.origin$spec < spec.avg$spec]) / 2
    spec[spec.origin$spec >= spec.avg$spec] = spec[spec.origin$spec >= spec.avg$spec] - spec.avg$spec[spec.origin$spec >= spec.avg$spec]
    data.table(spec = spec, freq = spec.origin$freq)
}

s1.filter = filterAvgSpec(s1, ntime=100)
p1 = ggplot(s1.filter, aes(x = freq, y = spec)) + geom_line()

s1.filter$type = 'filter'
s1.plot = rbindlist(list(s1.plot, s1.filter))
p2 = ggplot(s1.plot, aes(x = freq, y = spec, group = type)) + geom_line(aes(color=type))



#############################
# Compute the actual average spectrum from all speakers from the corpus
#############################
dt = readRDS('map.dt.ent_swbd.rds')
dt.avg = dt[, {
        spec = spectrum(ent_swbd, taper=0, log='no', plot=F, method='pgram')
        .(spec = spec$spec, freq = spec$freq)
    }, by = .(observation, who)]

freq.base = sort(unique(dt.avg$freq))
# interpolation
dt.avg2 = dt.avg[, {
        x = freq
        y = spec
        approx = approx(x, y, xout = freq.base)
        x_out = freq.base[!is.na(approx$y)]
        y_out = approx$y[!is.na(approx$y)]
        .(spec = y_out, freq = x_out)
    }, by = .(observation, who)]
# compute average spec for each freq.base value
setkey(dt.avg2, freq)
dt.spec.base = dt.avg2[, {.(spec = mean(spec))}, by = freq]
spec.base = dt.spec.base$spec
# plot
p = ggplot(dt.spec.base, aes(x = freq, y = spec)) + geom_line()
pdf('plots/specbase_vs_freqbase_mean.pdf', 5, 5)
plot(p)
dev.off()

p = ggplot(dt.avg2, aes(x = freq, y = spec)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon') +
    stat_summary(fun.y = mean, geom = 'line')
pdf('plots/specbase_vs_freqbase.pdf', 5, 5)
plot(p)
dev.off()

## write freq.base and spec.base to rds
saveRDS(freq.base, 'model-data/freq.base.rds')
saveRDS(spec.base, 'model-data/spec.base.rds')

# reread
freq.base = readRDS('model-data/freq.base.rds')
spec.base = readRDS('model-data/spec.base.rds')

# filter spec.base for each speakers in dt.avg2
setkey(dt.avg2, observation, who)
dt.avg2.filter = dt.avg2[, {
        match(freq, freq.base)
    }, by = .(observation, who)]




##########################
# see if subtracting the average series from the shuffled one can void significant correlation
##########################
dt = readRDS('map.dt.ent_swbd.rds')
dt.shuffle = dt[, {
        .(ent = sample(ent_swbd))
    }, by = .(observation, who)]

dt.filter = dt.shuffle[, {
        tmp = filterAvgSpec(ent, ntime=100)
        .(spec = tmp$spec, freq = tmp$freq)
    }, by = .(observation, who)] # takes a few seconds

dt.pso = dt.filter[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO)
    }, by = observation]
dt.pso = dt.pso[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ PSO, dt.pso)
summary(m)
## no longer significant
# ok, does this operation keep the significance of the actual data (unshuffled)


##############################
# see if subtracting the average shuffled series from the original series keep the significance
##############################
dt = readRDS('map.dt.ent_swbd.rds')
dt.filter = dt[, {
        tmp = filterAvgSpec(ent, ntime=100)
        .(spec = tmp$spec, freq = tmp$freq)
    }, by = .(observation, who)] # takes a few seconds

dt.pso = dt.filter[, {
        x_g = freq[who=='g']
        y_g = spec[who=='g']
        x_f = freq[who=='f']
        y_f = spec[who=='f']
        # linear interpolation
        x_out = sort(union(x_g, x_f))
        approx_g = approx(x_g, y_g, xout = x_out)
        approx_f = approx(x_f, y_f, xout = x_out)
        # find min ys and remove NAs
        x_out_g = x_out[which(!is.na(approx_g$y))]
        y_out_g = approx_g$y[which(!is.na(approx_g$y))]
        x_out_f = x_out[which(!is.na(approx_f$y))]
        y_out_f = approx_f$y[which(!is.na(approx_f$y))]
        y_min = pmin(approx_g$y, approx_f$y)
        x_min = x_out[which(!is.na(y_min))]
        y_min = y_min[which(!is.na(y_min))]
        # compute AUVs and PSO
        AUV_g = trapz(x_out_g, y_out_g)
        AUV_f = trapz(x_out_f, y_out_f)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_g + AUV_f)
        # return PSO
        .(PSO = PSO)
    }, by = observation]
dt.pso = dt.pso[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ PSO, dt.pso)
summary(m)
### still, insignificant...
# But if we use the third way to filter out average spectrum, then
# the correlation is significant again:
