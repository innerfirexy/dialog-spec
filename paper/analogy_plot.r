# plot the analogy figure for the acl2017 draft
# Yang Xu
# 2/1/2017

library(ggplot2)
library(data.table)

t = seq(0, 10, .1)
s1 = .5*sin(.8*t) + 1
s2 = .75*cos(t) + 1

dt.plot = rbindlist(list(
        data.table(time = t, entropy = s1, signal = 's1'),
        data.table(time = t, entropy = s2, signal = 's2')
    ))

# solid lines to be drawn
# cos(t) = 0 ==> t = pi
# .5*sin(.8*t)+1 = .75*cos(t)+1 ==> t =
t2 = seq(pi, 4.5, .1)
y1 = .5*sin(.8*t2) + 1
y2 = .75*cos(t2) + 1
dt.plot2 = rbindlist(list(
        data.table(time=t2, entropy=y1, signal='s1'),
        data.table(time=t2, entropy=y2, signal='s2')
    ))

dt.arrow1 = data.table(x1=2*pi, y1=1.5, x2=2*pi+1.5, y2=1.5)
dt.arrow2 = data.table(x1=2.5*pi/.8-1.5, y1=1.5, x2=2.5*pi/.8, y2=1.5)


p = ggplot(dt.plot, aes(x=time, y=entropy)) +
    geom_line(aes(color=signal), lty='dashed') +
    geom_line(data=dt.plot2, aes(x=time, y=entropy, color=signal), lty='solid', size=1) +
    # scale_y_continuous(limits=c(-1, 3)) +
    geom_vline(xintercept = 2.5*pi/.8, lty='dotted') +
    geom_vline(xintercept = 2*pi, lty='dotted') +
    geom_segment(data = dt.arrow1, aes(x=x2, y=y2, xend=x1, yend=y1), arrow=arrow(length = unit(0.02, "npc"))) +
    geom_segment(data = dt.arrow2, aes(x=x1, y=y1, xend=x2, yend=y2), arrow=arrow(length = unit(0.02, "npc"))) +
    theme_bw() +
    theme(legend.position=c(.1,.25),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
pdf('analogy.pdf', 5, 2.5)
plot(p)
dev.off()
