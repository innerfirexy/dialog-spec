# Play with the crqa package
# Yang Xu
# 2/9/2017

# paper: Coco and Dale 2014, Frontiers in Psychology

library(crqa)
library(data.table)
library(ggplot2)

#
PC_lo = .05
PC_hi = .25

PS = .05
PCC = .2
PSS = .2
PSC = .25

ts_len = 1000


###
# replicate Figure 2 in the paper
dt.ts = data.table()
dt.prob = data.table()
for (i in 1:20) {
    # low condition
    ts_low = simts(PC_lo, PS, PCC, PSS, PSC, ts_len)
    P_lowC = length(which(ts_low[1,]==1)) / length(ts_low[1,])
    P_lowS = length(which(ts_low[2,]==1)) / length(ts_low[2,])
    # high condition
    ts_high = simts(PC_hi, PS, PCC, PSS, PSC, ts_len)
    P_highC = length(which(ts_high[1,]==1)) / length(ts_high[1,])
    P_highS = length(which(ts_high[2,]==1)) / length(ts_high[2,])
    #
    ts_comb = rbindlist(list(
            cbind(data.table(t(ts_low)), rep('low', ncol(ts_low))),
            cbind(data.table(t(ts_high), rep('high', ncol(ts_high))))
        ))
    ts_comb$trial = i
    dt.ts = rbindlist(list(dt.ts, ts_comb))
    dt.prob = rbindlist(list(dt.prob, data.table(
            prob = c(P_lowC, P_lowS, P_highC, P_highS),
            group = c('lowC', 'lowS', 'highC', 'highS')
        )))
}
setnames(dt.ts, c('tsC', 'tsS', 'group', 'trial'))
setkey(dt.ts, group, trial)
# plot
p = ggplot(dt.prob, aes(x = group, y = prob)) +
    stat_summary(fun.data = mean_cl_boot, geom = 'errorbar')


###
# compute cross correlation and replicate Figure 3
dt.ccf = dt.ts[, {
        ccf = ccf(tsC, tsS, plot=F, type = 'correlation', lag.max = 5)
        ccfs = c()
        for (i in seq(-5, 5, 1)) {
            ccfs = c(ccfs, ccf[i]$acf[1])
        }
        .(ccf = ccfs, lag = seq(-5, 5, 1))
    }, by = .(group, trial)]
# plot
p = ggplot(dt.ccf, aes(x = lag, y = ccf, group = group)) +
    geom_smooth(method='loess', aes(color = group))


####
