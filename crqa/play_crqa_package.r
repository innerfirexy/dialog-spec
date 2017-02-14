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

ts_len = 2000


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
p = ggplot(dt.ccf, aes(x = lag, y = ccf)) +
    geom_smooth(method='loess', aes(color = group))


####
# replicate the cross-recurrence plots (CRPs) (Figure 4), and Figure 5
# high condition
tsC_hi = dt.ts[trial==1 & group == 'high', tsC]
tsS_hi = dt.ts[trial==1 & group == 'high', tsS]

res.hi = crqa(tsC_hi, tsS_hi, delay=1, embed=1, rescale=0, radius=0, normalize=0, mindiagline=2, minvertline=1)
crp.hi = res.hi$RP # a 2000 by 2000 matrix

dt.crp.hi = data.table(which(crp.hi == T, arr.ind = T))
setnames(dt.crp.hi, c('tsC', 'tsS'))

p.hi = ggplot(dt.crp.hi, aes(x = tsC, y = tsS)) + geom_point(size=.1)

pdf('crp_demo_hi.pdf', 10, 10)
plot(p.hi)
dev.off()

# low condition
tsC_lo = dt.ts[trial==1 & group == 'low', tsC]
tsS_lo = dt.ts[trial==1 & group == 'low', tsS]

res.lo = crqa(tsC_lo, tsS_lo, delay=1, embed=1, rescale=0, radius=0, normalize=0, mindiagline=2, minvertline=1)
crp.lo = res.lo$RP # a 2000 by 2000 matrix

dt.crp.lo = data.table(which(crp.lo == T, arr.ind = T))
setnames(dt.crp.lo, c('tsC', 'tsS'))

p.lo = ggplot(dt.crp.lo, aes(x = tsC, y = tsS)) + geom_point(size=.1)

pdf('crp_demo_lo.pdf', 10, 10)
plot(p.lo)
dev.off()


##
# Figure 5, diagonal-wise recurrence rate (RR)

# params for crqa
delay = 1; embed = 1; rescale = 1; radius = 0.001;
normalize = 0; mindiagline = 2; minvertline = 2;
tw = 0; whiteline = FALSE; recpt = FALSE; side = "both";
checkl = list(do = FALSE, thrshd = 3, datatype = "categorical", pad = TRUE)

dt.RR = dt.ts[, {
        maxlag = 5
        # res = calcphi(tsC, tsS, ws = maxlag, 0) # only calculate state 1
        # res = drpdfromts(tsC, tsS, ws=maxlag, datatype='categorical', radius=.001)
        res = crqa(tsC, tsS, delay, embed, rescale, radius, normalize, mindiagline, minvertline, tw, whiteline, recpt, side, checkl)
        .(RR = res$profile, lag = seq(-maxlag, maxlag, 1))
    }, by = .(group, trial)]

# plot
p = ggplot(dt.RR, aes(x = lag, y = RR)) +
    stat_smooth(aes(color = group), method='loess') +
    scale_x_continuous(breaks = seq(-5, 5, 1))


# summary(lm(RR ~ group, dt.RR))


####
# Try Coco's advice
ts1 = c(rep(2,8),1,1,rep(2,10))
ts2 = c(rep(3,9),1,1,rep(3,9))
res = drpdfromts(ts1, ts2, ws=5, datatype='categorical', radius=0)
#
# nice! It works!
# Looks like we need to code non-events to different elements for different series
res$profile
# 0.00000000 0.00000000 0.00000000 0.05555556 0.10526316 0.05000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
