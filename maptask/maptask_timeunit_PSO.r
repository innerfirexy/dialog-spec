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

# construct data table
for (i in 1:20) {
    resfiles = Sys.glob(paste0(getwd(), '/model-data/even-time-results/delta', i,'/*.txt'))
    dt = data.table()

    for (rf in resfiles) {
        dt.tmp = fread(rf)
        m = regexpr('q[a-z0-9]+\\.[g|f]', rf)
        str = regmatches(rf, m)
        names = unlist(strsplit(str, '\\.'))
        dt.tmp$observation = names[1]
        dt.tmp$who = names[2]
        dt = rbindlist(list(dt, dt.tmp))
    }
    setnames(dt, 'V1', 'entropy')
    setkey(dt, observation, who)

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
# 1. try different lm training method, e.g. cross-validate
# 2. using only first half of dialogue
# 3. other baselines, Reitter2007
