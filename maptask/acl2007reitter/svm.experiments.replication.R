
test <- function(model, data, training, testset='test', initial=99999999) {

    if (testset == 'training') {
        s2 <- subset(data, training & time < initial)
    } else {
        s2 <- subset(data, !training & time < initial)
    }
    ## take mean for each dialogue
    dia <- levels(as.factor(as.integer(s2$dialogue)))
    thedialnum <- as.integer(s2$dialogue)
    predicted = c()
    target = c()

    for(i in dia) {

        p <- predict(model, subset(s2, thedialnum==i))
        predicted <- c(predicted, mean(p))
        target <- c(target, mean(subset(s2, thedialnum==i)$score))

    }
    return (cor(predicted, target)^2)
}


## /* Leave-one-out crossvalidation */

crossval <- function(formula, data,testset='test', initial=99999999, folds=5, gamma=5, cv=NULL) {
    res <- c()
    if(length(cv)<1) {
        cv <- sample(0:(folds-1), length(levels(data$dial)), replace=TRUE)
    }

    ## /* cv is a vector as long as the list of available dialogues.  For each dialogue it
    ##    assigns a fold. */
    thedialnum <- as.integer(data$dialogue)

    for (i in 0:(folds-1)) {
        training <- (cv[thedialnum]!=i)   # & data$time<initial)
        s <- svm( eval(formula),  data=data, subset=training, kernel="radial", gamma=gamma)
        res <- c(res, test(s, data, training, testset, initial))
    }
    print(mean(res))
    return (mean(res))
}

library(e1071)

 d <- read.table("maptask.performance.bal.data", header=TRUE)
 x <- d
 x5 <- subset(d, time < 300)



# to <- best.tune(svm, score ~ word.reps * letter.reps * rule.reps * word.total *letter.total * time, data=d, ranges=list(gamma=2^(-1:6)), kernel="radial")  to <- best.tune(svm, score ~ word.reps * letter.reps * rule.reps * word.total *letter.total * time, data=d, ranges=list(gamma=2^(-1:6), degree=2:3, coef=-1:1), kernel="polynomial")

folds <- 10
commoncv1 <- sample(0:(folds-1), length(levels(x$dialogue)), replace=TRUE)
commoncv2 <- sample(0:(folds-1), length(levels(x5$dialogue)), replace=TRUE)


# ALL
system.time(all1 <- crossval(quote(score ~ word.reps * letter.reps * rule.reps * word.total * letter.total * time), x, 'test',  folds=10, cv=commoncv1))

# 0.1220015
# takes 112 sec


# from first five minutes
# all2 <- crossval(quote(score ~ word.reps * letter.reps * rule.reps * time), x5, 'test', 300, folds=10, cv=commoncv2)


#####
# include PSO feature
library(data.table)
dt.pso = readRDS('dt.pso.rds')

d.new = data.table(d)
setkey(d.new, dialogue)
d.new = d.new[dt.pso, nomatch=0]

x.new = d.new
x5.new <- subset(d.new, time < 300)

folds <- 10
commoncv1.new <- sample(0:(folds-1), length(levels(x.new$dialogue)), replace=TRUE)
commoncv2.new <- sample(0:(folds-1), length(levels(x5.new$dialogue)), replace=TRUE)

all1.new <- crossval(quote(score ~ word.reps * letter.reps * rule.reps * word.total * letter.total * time * PSO), x.new, 'test',  folds=10, cv=commoncv1.new)

# all1.new == 0.07386766


####
# ##
# combine with dt.rp with dt.pso
dt.rp = readRDS('dt.peakRP.rds')
dt.comb = dt.rp[dt.pso, nomatch=0]

d <- read.table("maptask.performance.bal.data", header=TRUE)
d.new = data.table(d)
setkey(d.new, dialogue)

dt.comb2 = d.new[dt.comb, nomatch=0]
# save
saveRDS(dt.comb2, 'dt.comb.rds')
