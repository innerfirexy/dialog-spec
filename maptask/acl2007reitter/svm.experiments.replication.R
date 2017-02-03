
test <- function(model, data, training, testset='test', initial=99999999) {

    if (testset == 'training') {
      s2 <- subset(data, training & time < initial)
    } else {
        s2 <- subset(data, !training & time < initial)
    }

    # take mean for each dialogue
    dia <- levels(as.factor(as.integer(s2$dialogue)))
    predicted = c()
    target = c()
    for(i in dia) {

      p <- predict(model, subset(s2, dialogue==i))
      predicted <- c(predicted, mean(p))
      target <- c(target, mean(subset(s2, dialogue==i)$score))

    }
    return (cor(predicted, target)^2)
}


crossval <- function(formula, data,testset='test', initial=99999999, folds=5, gamma=5, cv=NULL) {


      res <- c()
      if(length(cv)<1) {
        cv <- sample(0:(folds-1), length(levels(data$dialogue)), replace=TRUE)
      }


    for (i in 0:(folds-1)) {
        training <- (cv[data$dialogue]!=i)   # & data$time<initial)
        s <- svm( eval(formula),  data=data, subset=training, kernel="radial", gamma=gamma)
        res <- c(res, test(s, data, training, testset, initial))
    }
      print(mean(res))
      return (mean(res))
}



library(e1071)

d <- read.table("maptask.performance.bal.data", header=TRUE)

# append PSO columns
library(data.table)
dt.pso = readRDS('dt.pso.rds')



d$dialogue <- as.factor(d$dialogue)
x <- d
x5 <- subset(d, time < 300)

# to <- best.tune(svm, score ~ word.reps * letter.reps * rule.reps * word.total *letter.total * time, data=d, ranges=list(gamma=2^(-1:6)), kernel="radial")  to <- best.tune(svm, score ~ word.reps * letter.reps * rule.reps * word.total *letter.total * time, data=d, ranges=list(gamma=2^(-1:6), degree=2:3, coef=-1:1), kernel="polynomial")

folds <- 10
commoncv1 <- sample(0:(folds-1), length(levels(x$dialogue)), replace=TRUE)
commoncv2 <- sample(0:(folds-1), length(levels(x5$dialogue)), replace=TRUE)


# ALL
all1 <- crossval(quote(score ~ word.reps * letter.reps * rule.reps * word.total * letter.total * time), x, 'test',  folds=10, cv=commoncv1)

# from first five minutes
# all2 <- crossval(quote(score ~ word.reps * letter.reps * rule.reps * time), x5, 'test', 300, folds=10, cv=commoncv2)


##
# results
# w/o PSO: 0.1220015
