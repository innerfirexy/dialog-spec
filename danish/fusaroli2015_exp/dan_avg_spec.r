# Plot the avarage spectrum for DJD
# Yang Xu
# 2/1/2017

library(data.table)
library(ggplot2)
library(MASS)
library(pracma)
library(lme4)
library(lmerTest)

# read data
dt = fread('data/all_pairs_entropy.txt')
setnames(dt, c('pairId', 'who', 'ent'))
setkey(dt, pairId, who)


##
# analysis
dt.spec = dt[, {
        spec = spectrum(ent, taper=0, log='no', plot=F, method='pgram', spans=c(7,7))
        .(spec = spec$spec, freq = spec$freq)
    }, by = .(pairId, who)]
freq.base = sort(unique(dt.spec$freq))

# interpolation
dt.intpl = dt.spec[, {
        x = freq
        y = spec
        approx = approx(x, y, xout = freq.base)
        x_out = freq.base[!is.na(approx$y)]
        y_out = approx$y[!is.na(approx$y)]
        .(spec = y_out, freq = x_out)
    }, by = .(pairId, who)]

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

# model
dt.intpl[, {seriesId := .GRP}, by = .(pairId, who)]
m = lmer(spec ~ freq + (1|seriesId), dt.intpl)
summary(m)



##########
# Do not use linear interpolation
dt.spec2 = copy(dt.spec)
dt.spec2[, {seriesId := .GRP}, by = .(pairId, who)]

m = lmer(spec ~ freq + (1|seriesId), dt.spec2)
summary(m)

m2 = lm(spec ~ freq, dt.spec2)
summary(m)


########
# what if using spans (smooth) in spectrum function
dt.spec3 = dt[, {
        spec = spectrum(ent, taper=0, log='no', plot=F, method='pgram', spans=c(7,7))
        .(spec = spec$spec, freq = spec$freq)
    }, by = .(pairId, who)]
dt.spec3[, {seriesId := .GRP}, by = .(pairId, who)]

m = lmer(spec ~ freq + (1|seriesId), dt.spec3)
summary(m)

m2 = lm(spec ~ freq, dt.spec3)
summary(m)

##
p = ggplot(dt.spec3, aes(x=freq, y=spec)) +
    geom_smooth(method='lm')
    # stat_summary(fun.data = mean_cl_boot, geom='ribbon') +
    # stat_summary(fun.y = mean, geom = 'line')


##
# does adding spans increase the correlation with task performance?
dt.pf = fread('data/PerformanceData.tsv')
setkey(dt.pf, Pair)

dt.spec3.pso = dt.spec3[, {
        x_A = freq[who=='A']
        y_A = spec[who=='A']
        x_B = freq[who=='B']
        y_B = spec[who=='B']
        # linear interpolation
        x_out = sort(union(x_A, x_B))
        approx_A = approx(x_A, y_A, xout = x_out)
        approx_B = approx(x_B, y_B, xout = x_out)
        # find min ys and remove NAs
        x_out_A = x_out[!is.na(approx_A$y)]
        y_out_A = approx_A$y[!is.na(approx_A$y)]
        x_out_B = x_out[!is.na(approx_B$y)]
        y_out_B = approx_B$y[!is.na(approx_B$y)]
        y_min = pmin(approx_A$y, approx_B$y)
        x_min = x_out[!is.na(y_min)]
        y_min = y_min[!is.na(y_min)]
        # compute AUVs and PSO
        AUV_A = trapz(x_out_A, y_out_A)
        AUV_B = trapz(x_out_B, y_out_B)
        AUV_min = trapz(x_min, y_min)
        PSO = AUV_min / (AUV_A + AUV_B)
        # return PSO
        .(PSO = PSO)
    }, by = pairId]
dt.spec3.pso = dt.spec3.pso[dt.pf, nomatch=0]

m = lm(CollectivePerformance ~ PSO, dt.spec3.pso)
summary(m)
# does not work!
# Hmmm, still works, but does not significantly increase performance



#########
# white noise baseline
len.mean = floor(mean(dt[, .N, by = .(pairId, who)]$N)) # 346
ent.mean = mean(dt$ent) # 2.42
ent.sd = sd(dt$ent) # .56

dt.rnorm = data.table()
for (i in 1:100) {
    set.seed(i)
    tmp = data.table(sid = i, s = rnorm(len.mean, mean = ent.mean, sd = ent.sd))
    dt.rnorm = rbindlist(list(dt.rnorm, tmp))
}

dt.rnorm.spec = dt.rnorm[, {
        spec = spectrum(s, taper=0, log='no', plot=F, method='pgram')
        .(spec = spec$spec, freq = spec$freq)
    }, by = sid]

# plot
p = ggplot(dt.rnorm.spec, aes(x=freq, y=spec)) +
    geom_smooth(method='lm')
    # stat_summary(fun.data = mean_cl_boot, geom='ribbon') +
    # stat_summary(fun.y = mean, geom = 'line')

# model
m = lmer(spec ~ freq + (1|sid), dt.rnorm.spec)
summary(m)


#####
# plot actual spectrum and white noise baseline together
dt.plot1 = dt.spec[, .(spec, freq)]
dt.plot1$Type = 'Actual data'
dt.plot2 = dt.rnorm.spec[, .(spec, freq)]
dt.plot2$Type = 'White noise'
dt.plot = rbindlist(list(dt.plot1, dt.plot2))

p = ggplot(dt.plot, aes(x=freq, y=spec, group=Type)) +
    geom_smooth(aes(color = Type, lty = Type), method='lm') +
    theme(legend.position = c(.8,.15)) +
    xlab('Frequency') + ylab('Power')
pdf('plots/average_spectrum_DJD.pdf', 4, 4)
plot(p)
dev.off()
