# PSO analysis of timeunit even-time experiment
# Yang Xu
# 1/11/2017

library(data.table)
library(ggplot2)
library(lme4)
library(MASS)
library(pracma)

#########################
# Power Spectral Overlap analysis
#########################

# read the dataset that contains pathdev info
dt.dev = fread('moves_and_deviation.csv')
setnames(dt.dev, "Observation", "observation")
setnames(dt.dev, 'path dev', 'pathdev')
setkey(dt.dev, observation)

##
# the function that reads result files of certain delta value
readResults = function (delta) {
    resfiles = Sys.glob(paste0(getwd(), '/model-data/even-time-results/delta', delta,'/*.txt'))
    dt = data.table()
    for (file in resfiles) {
        dt.tmp = fread(file)
        m = regexpr('q[a-z0-9]+\\.[g|f]', file)
        str = regmatches(file, m)
        names = unlist(strsplit(str, '\\.'))
        dt.tmp$observation = names[1]
        dt.tmp$who = names[2]
        dt = rbindlist(list(dt, dt.tmp))
    }
    setnames(dt, 'V1', 'entropy')
    setkey(dt, observation, who)
    dt
}

# construct data table
for (i in 1:20) {
    # read
    dt = readResults(i)

    # analysis
    dt.spec = dt[, {
            specval = spec.pgram(entropy, taper=0, log='no', plot=FALSE)
            .(spec = specval$spec, freq = specval$freq)
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
    # save dt
    saveRDS(dt.pso, paste0('model-data/dt.pso.delta', i, '.rds'))
}



# models
dt.report = data.table()
for (i in 1:20) {
    dt.pso = readRDS(paste0('model-data/dt.pso.delta', i, '.rds'))
    m = lm(pathdev ~ PSO, dt.pso)
    pval = summary(m)$coefficients[,4][2]
    dt.report = rbindlist(list(dt.report, list(i, pval)))
}

# delta=17 has the best performance
dt.pso = readRDS('model-data/dt.pso.delta17.rds')
p = ggplot(dt.pso, aes(x = PSO, y = pathdev)) +
    geom_point() +
    geom_smooth(method = lm)
pdf('pathdev_PSO_delta17.pdf', 5, 5)
plot(p)
dev.off()

# what if remove outliers of 2 SD away from mean, in terms of pathdev
pd.mean = mean(dt.pso$pathdev)
pd.sd = sd(dt.pso$pathdev)
dt.pso.s = dt.pso[pathdev < pd.mean + 2*pd.sd,]

m = lm(pathdev ~ PSO, dt.pso.s)
summary(m)
p = ggplot(dt.pso.s, aes(x = PSO, y = pathdev)) +
    geom_point() +
    geom_smooth(method = lm)
pdf('pathdev_PSO_delta17_outlierremoved.pdf', 5, 5)
plot(p)
dev.off()

########################
#### important info ####
# when delta (maptask_timeunit_eventime.py) set to be 16 sec, the model is significant
# when smaller values, the model is not


#######
# todo:
# - try different lm training method, e.g. cross-validate
# - using only first half of dialogue
# - other baselines, Reitter200
# - performance w/o outliers



#########################
# examine the effect of "delta" (1 to 20) on segment length and entropy distribution etc.
#########################

# number of segments (rows in "*.timed-units.seg.txt" files under even-time folder)
dt.delta_segnum = data.table()
for (i in 1:20) {
    segfiles = Sys.glob(paste0(getwd(), '/model-data/even-time/delta', i,'/*.txt'))
    for (sf in segfiles) {
        dt.tmp = fread(sf)
        dt.delta_segnum = rbindlist(list(dt, list(i, nrow(dt.tmp))))
    }
}
setnames(dt.delta_segnum, c('delta', 'segNum'))
# plot
p = ggplot(dt.delta_segnum, aes(x = delta, y = segNum)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width=.2) +
    stat_summary(fun.y = mean, geom = 'line')
pdf('plots/segNum_delta.pdf', 5, 5)
plot(p)
dev.off()


# compare different distributions
system.time({
        dt.distr <- data.table()
        for (i in 1:20) {
            dt = readResults(i)
            dt.distr = rbindlist(list(dt.distr, dt))
        }
    }) # last 130 sec
setnames(dt.distr, 'V1', 'entropy')
# save to rds
saveRDS(dt.distr, 'model-data/dt.entropy.delta_all.rds')

# plots
p = ggplot(dt.distr, aes(x = delta, y = entropy)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width=.2) +
    stat_summary(fun.y = mean, geom = 'line')
pdf('plots/entropy_delta.pdf', 5, 5)
plot(p)
dev.off()

p = ggplot(dt.distr, aes(x = entropy, group = delta)) +
    geom_density(aes(color = delta))



###############################
# remove outliers in entropy from dt
###############################
dt.delta17 = readResults(17)
summary(dt.delta17$entropy) # 3rd Qu = 2.559
# boxplot
p = ggplot(dt.delta17, aes(x=who, y = entropy)) + geom_boxplot(width=.2)
# mean and sd
ent.mean = mean(dt.delta17$entropy) # 2.049759
ent.sd = sd(dt.delta17$entropy) # 1.36094

# get rid of outliers
dt.delta17.s = dt.delta17[entropy < ent.mean + 2*ent.sd,]

# PSO analysis
dt.spec = dt.delta17.s[, {
        specval = spec.pgram(entropy, taper=0, log='no', plot=FALSE)
        .(spec = specval$spec, freq = specval$freq)
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
## important !! ##
# when outliers (< mean + 2mu) in entropy are removed, the models are no longer significant!
#

### explore the above on original sentence-level entropy
dt.sent = readRDS('map.dt.ent_swbd.rds')
summary(dt.sent$ent_swbd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.164   3.264  10.600  16.180  22.820 295.200
p = ggplot(dt.sent, aes(x=ent_swbd)) + geom_density()
# handle outliers
# replace outliers with mean values
ent.mean = mean(dt.sent$ent_swbd)
ent.sd = sd(dt.sent$ent_swbd)
dt.sent.s = dt.sent[,]
dt.sent.s[ent_swbd > ent.mean + 2*ent.sd, ent_swbd := ent.mean,]
# reanalysis pso
dt.spec = dt.sent.s[, {
        specval = spec.pgram(ent_swbd, taper=0, log='no', plot=FALSE)
        .(spec = specval$spec, freq = specval$freq)
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
#####
# when outliers are removed (ent_swbd < ent.mean + 2*ent.sd)
# the model is no longer significant!!!
# How many outliers point??
nrow(dt.sent[ent_swbd >= ent.mean + 2*ent.sd,])/nrow(dt.sent) # 0.04726038
# 4.7% of the total data points determine the model's performance?!
##
# w/o remove outliers,
# PSO           201.78      77.55   2.602   0.0105 *
# Adjusted R-squared:  0.04818



#################################
# experiment with spectrum function on map.dt.ent_swbd.rds
# using parameter method='ar' (autoregression)
#################################
dt.sent = readRDS('map.dt.ent_swbd.rds')
# replace outliers with mean values
ent.mean = mean(dt.sent$ent_swbd)
ent.sd = sd(dt.sent$ent_swbd)
dt.sent.s = dt.sent[,]
dt.sent.s[ent_swbd > ent.mean + 2*ent.sd, ent_swbd := ent.mean,]
# analysis
dt.spec = dt.sent[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='ar')
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
##
# 2.164  0.03258 *
# autoregression (method = 'ar') is not as good as periodogram (method ='pgram')



############################
# try out the coh () value returned by the spectrum function
############################
dt.sent = readRDS('map.dt.ent_swbd.rds')
# replace outliers with mean values
ent.mean = mean(dt.sent$ent_swbd)
ent.sd = sd(dt.sent$ent_swbd)
dt.sent.s = dt.sent[,]
dt.sent.s[ent_swbd > ent.mean + 2*ent.sd, ent_swbd := ent.mean,]

# shuffle baseline
dt.sent.sf = dt.sent[, {
        .(ent_swbd = sample(ent_swbd))
    }, by = .(observation, who)]

dt.coh = dt.sent.sf[, {
        y_g = ent_swbd[who=='g']
        y_f = ent_swbd[who=='f']
        len = min(length(y_g), length(y_f))
        y_g = y_g[1:len]
        y_f = y_f[1:len]
        comb.ts = ts(matrix(c(y_g, y_f), ncol=2))
        spec = spectrum(comb.ts, detrend=FALSE, taper=0, log='no', plot=F, method='pgram', spans = 13)
        .(coh = as.numeric(spec$coh))
    }, by = .(observation)]
dt.coh = dt.coh[dt.dev[, .(observation, pathdev)], nomatch=0]

# model
m = lm(pathdev ~ coh, dt.coh)
summary(m)

## summary
# the bigger 'spans' paremeter gives better performance
# even significant when entropy outliers are removed



#############################
# Does simply the number of entropy outliers correlate with pathdev??
#############################
dt.sent = readRDS('map.dt.ent_swbd.rds')
ent.mean = mean(dt.sent$ent_swbd)
ent.sd = sd(dt.sent$ent_swbd)

dt.outnum = dt.sent[, {
        .(outnum = length(.I[ent_swbd > ent.mean + 2*ent.sd]))
    }, by = .(observation)]
dt.outnum = dt.outnum[dt.dev[, .(observation, pathdev)], nomatch=0]

m = lm(pathdev ~ outnum, dt.outnum)
summary(m)
# It indeed is a significant factor
# -2.204   0.0295 *
# Adjusted R-squared:  0.03274

##
# what about entropy magnitude alone??
dt.sent = dt.sent[dt.dev[, .(observation, pathdev)], nomatch=0]
m = lm(pathdev ~ ent_swbd, dt.sent)
summary(m)
##
# hmmm, only marginally significant, which is "good" for us
# -1.69   0.0911 .
# Argument: how much information there is in our language, does not greatly
# correlate with task success
# Potentially --> the patterns of information flow matters more



##############################
# Analyze the dataset that contains zeros for other speakers
##############################
dt = fread('model-data/maptask_ent_swbd_withzeros.csv')
setnames(dt, c('observation', 'who', 'ent_swbd'))
setkey(dt, observation, who)

dt.spec = dt[, {
        specval = spectrum(ent_swbd, taper=0, log='no', plot=FALSE, method='pgram')
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

m = lm(pathdev ~ PSO, dt.pso)
summary(m)
####
# 2.400   0.0180 *
# Adjusted R-squared:  0.04008
# a little bit worse than w/o zeros dataset (the original one)
#
