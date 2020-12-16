##GROUP 4 CODE SNIPPETS
rm(list=ls())

#lab 1 loess1
data(cars)
cars.lo <- loess(dist ~ speed, cars)
predict(cars.lo, data.frame(speed = seq(5, 30, 1)), se = TRUE)
cars.lo2 <- loess(dist ~ speed, cars, control = loess.control(surface = "direct"))
predict(cars.lo2, data.frame(speed = seq(5, 30, 1)), se = TRUE)


#lab 1 loess 2

require(graphics)

plot(cars, main = "lowess(cars)")
lines(lowess(cars), col = 2)
lines(lowess(cars, f = .2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)

#lab 1 loess 3
require(graphics)

with(cars, {
  plot(speed, dist)
  lines(supsmu(speed, dist))
  lines(supsmu(speed, dist, bass = 7), lty = 2)
})


#lab 1 splines 1
require(graphics)
data(cars)

attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)

lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df = 10), lty = 2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')
detach()

plot(residuals(cars.spl) ~ fitted(cars.spl))
abline(h = 0, col = "gray")

stopifnot(all.equal(cars$dist,
                    fitted(cars.spl) + residuals(cars.spl)))

nKnots <- Vectorize(.nknots.smspl) ; c.. <- adjustcolor("gray20",.5)
curve(nKnots, 1, 250, n=250)
abline(0,1, lty=2, col=c..); text(90,90,"y = x", col=c.., adj=-.25)
abline(h=100,lty=2); abline(v=200, lty=2)

n <- c(1:799, seq(800, 3490, by=10), seq(3500, 10000, by = 50))
plot(n, nKnots(n), type="l", main = "Vectorize(.nknots.smspl) (n)")
abline(0,1, lty=2, col=c..); text(180,180,"y = x", col=c..)
n0 <- c(50, 200, 800, 3200); c0 <- adjustcolor("blue3", .5)
lines(n0, nKnots(n0), type="h", col=c0)
axis(1, at=n0, line=-2, col.ticks=c0, col=NA, col.axis=c0)
axis(4, at=.nknots.smspl(10000), line=-.5, col=c..,col.axis=c.., las=1)

y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))
xx  <- seq(1, length(y18), len = 201)
(s2  <- smooth.spline(y18)) # GCV
(s02  <- smooth.spline(y18, spar = 0.2))
(s02. <- smooth.spline(y18, spar = 0.2, cv = NA))
plot(y18, main = deparse(s2$call), col.main = 2)
lines(s2, col = "gray"); lines(predict(s2, xx), col = 2)
lines(predict(s02, xx), col = 3); mtext(deparse(s02$call), col = 3)

(s2  <- smooth.spline(y18, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))
(s2m <- smooth.spline(y18, cv = TRUE, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))

#lab 2 splines 2
require(graphics)

op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = 0.1+c(3,3,3,1))
n <- 9
x <- 1:n
y <- rnorm(n)
plot(x, y, main = paste("spline[fun](.) through", n, "points"))
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)

y <- (x-6)^2
plot(x, y, main = "spline(.) -- 3 methods")
lines(spline(x, y, n = 201), col = 2)
lines(spline(x, y, n = 201, method = "natural"), col = 3)
lines(spline(x, y, n = 201, method = "periodic"), col = 4)
legend(6, 25, c("fmm","natural","periodic"), col = 2:4, lty = 1)

y <- sin((x-0.5)*pi)
f <- splinefun(x, y)
ls(envir = environment(f))
splinecoef <- get("z", envir = environment(f))
curve(f(x), 1, 10, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
curve(f(x, deriv = 1), 1, 10, col = 2, lwd = 1.5)
curve(f(x, deriv = 2), 1, 10, col = 2, lwd = 1.5, n = 401)
curve(f(x, deriv = 3), 1, 10, col = 2, lwd = 1.5, n = 401)
par(op)

.x <- splinecoef$x
u <- seq(3, 6, by = 0.25)
(ii <- findInterval(u, .x))
dx <- u - .x[ii]
f.u <- with(splinecoef,
            y[ii] + dx*(b[ii] + dx*(c[ii] + dx* d[ii])))
stopifnot(all.equal(f(u), f.u))

set.seed(1); x <- round(rnorm(30), 1); y <- sin(pi * x) + rnorm(30)/10
plot(x, y, main = "spline(x,y)  when x has ties")
lines(spline(x, y, n = 201), col = 2)
tx <- table(x); mx <- as.numeric(names(tx[tx > 1]))
ry <- matrix(unlist(tapply(y, match(x, mx), range, simplify = FALSE)),
             ncol = 2, byrow = TRUE)
segments(mx, ry[, 1], mx, ry[, 2], col = "blue", lwd = 2)

n <- 20
set.seed(11)
x. <- sort(runif(n)) ; y. <- cumsum(abs(rnorm(n)))
plot(x., y.)
curve(splinefun(x., y.)(x), add = TRUE, col = 2, n = 1001)
curve(splinefun(x., y., method = "monoH.FC")(x), add = TRUE, col = 3, n = 1001)
curve(splinefun(x., y., method = "hyman")   (x), add = TRUE, col = 4, n = 1001)
legend("topleft",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")
x. <- c(7.09, 8.09, 8.19, 8.7, 9.2, 10, 12, 15, 20)
f <- c(0, 2.76429e-5, 4.37498e-2, 0.169183, 0.469428, 0.943740,
       0.998636, 0.999919, 0.999994)
s0 <- splinefun(x., f)
s1 <- splinefun(x., f, method = "monoH.FC")
s2 <- splinefun(x., f, method = "hyman")
plot(x., f, ylim = c(-0.2, 1.2))
curve(s0(x), add = TRUE, col = 2, n = 1001) -> m0
curve(s1(x), add = TRUE, col = 3, n = 1001)
curve(s2(x), add = TRUE, col = 4, n = 1001)
legend("right",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")

xx <- m0$x
plot(xx, s1(xx) - s2(xx), type = "l",  col = 2, lwd = 2,
     main = "Difference   monoH.FC - hyman"); abline(h = 0, lty = 3)

x <- xx[xx < 10.2] ## full range: x <- xx .. does not show enough
ccol <- adjustcolor(2:4, 0.8)
matplot(x, cbind(s0(x, deriv = 2), s1(x, deriv = 2), s2(x, deriv = 2))^2,
        lwd = 2, col = ccol, type = "l", ylab = quote({{f*second}(x)}^2),
        main = expression({{f*second}(x)}^2 ~" for the three 'splines'"))
legend("topright",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       lwd = 2, col  =  ccol, lty = 1:3, bty = "n")
#lab 1 splines 3
data(cars)
attach(cars)
plot(speed, dist, main = "data(cars)  &  smoothing splines")
cars.spl <- smooth.spline(speed, dist)
(cars.spl)
lines(cars.spl, col = "blue")
lines(smooth.spline(speed, dist, df = 10), lty = 2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')


library(kknn)
data(ionosphere)
attach(ionosphere)
str(ionosphere)
plot(V4, V14, main = "data(ionosphere)  &  smoothing splines")
iono.spl <- smooth.spline(V4, V14)
(iono.spl)
lines(iono.spl, col = "blue")
lines(smooth.spline(V4, V14, df = 10), lty = 2, col = "red")

data(glass)
str(glass)
attach(glass)
plot(Ca, Fe, main = "data(ionosphere)  &  smoothing splines")
glass.spl <- smooth.spline(Ca, Fe)
(glass.spl)
lines(glass.spl, col = "blue")
lines(smooth.spline(Ca, Fe, df = 10), lty = 2, col = "red")

#lab 2 lda 1
library(kknn)
data(ionosphere)
i2<-ionosphere[,-1]
i2<-i2[,-1]
library(MASS)
ifit <- lda(class ~ ., data=i2, na.action="na.omit", CV=TRUE)
summary(ifit)

ctable <- table(i2$class, ifit$class)
diag(prop.table(ctable, 1))
sum(diag(prop.table(ctable)))

#lab1cv1
library(cvTools)
library(robustbase)
data(coleman)
call <- call("lmrob", formula = Y ~ .)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
tuning <- list(tuning.psi=seq(2., 6., 20))
cvFitsLmrob <- cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
aggregate(cvFitsLmrob, summary)
#lab1cv2
library(MASS)
data(mammals)

mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals, mammals.glm)$delta)

(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)

muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.esterr <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))

library(boot)
data(nodal)

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)

#lab1cv3
library("robustbase")
require(cvTools)
data("coleman")
set.seed(1234) 
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
ncv(cvFitLts50)
nfits(cvFitLts50)
cvNames(cvFitLts50)
cvNames(cvFitLts50) <- c("improved", "initial")
fits(cvFitLts50)
cvFitLts50
ncv(cvFitsLts)
nfits(cvFitsLts)
cvNames(cvFitsLts)
cvNames(cvFitsLts) <- c("improved", "initial")
fits(cvFitsLts)
fits(cvFitsLts) <- 1:2
cvFitsLts
#lab1cv4
set.seed(4321)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
aggregate(cvFitLts50, summary)
aggregate(cvFitsLts, summary)
tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
call <- call("lmrob", formula = Y ~ .)
cvFitsLmrob <- cvTuning(call, data = coleman,
                        y = coleman$Y, tuning = tuning, cost = rtmspe,
                        folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
aggregate(cvFitsLmrob, summary)

#lab1cv5
folds <- cvFolds(nrow(coleman), K = 5, R = 50)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
bwplot(cvFitLmrob)
bwplot(cvFits)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
bwplot(cvFitsLts)

#lab1cv6
fit <- lmrob(Y ~ ., data=coleman)
cvFit(fit, data = coleman, y = coleman$Y, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)
cvFit(lmrob, formula = Y ~ ., data = coleman, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)
call <- call("lmrob", formula = Y ~ .)
cvFit(call, data = coleman, y = coleman$Y, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

#lab1cv7
set.seed(2143)
cvFolds(20, K = 5, type = "random")
cvFolds(20, K = 5, type = "consecutive")
cvFolds(20, K = 5, type = "interleaved")
cvFolds(20, K = 5, R = 10)
#lab1cv8
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, K = 5, R = 10,
                  fit = "both", trim = 0.1, seed = 1234)
cvFitLts
cvReshape(cvFitLts)

#lab1cv9
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
#lab1cv10
tuning <- list(tuning.psi = c(3.443689, 4.685061))
cvTuning(lmrob, formula = Y ~ ., data = coleman, tuning = tuning,
         cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
         seed = 1234)
call <- call("lmrob", formula = Y ~ .)
cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning, args=list(6, densityplot.cv),
         cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
         seed = 1234)

#lab1cv11
set.seed(1234) 
call <- call("lmrob", formula = Y ~ .)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe,
       folds = folds, costArgs = list(trim = 0.1))

#lab1cv12
set.seed(1234)
folds <- cvFolds(nrow(coleman), K = 5, R = 50)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
densityplot(cvFitLmrob)
densityplot(cvFits)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
densityplot(cvFitsLts)

#lab1cv13
set.seed(1234) 
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
dotplot(cvFits)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
dotplot(cvFitsLts)

#lab1cv14
set.seed(1234)
folds <- cvFolds(nrow(coleman), K = 5, R = 50)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
plot(cvFitLmrob, method = "bw")
plot(cvFitLmrob, method = "density")
plot(cvFits, method = "bw")
plot(cvFits, method = "density")
plot(cvFits, method = "xy")
plot(cvFits, method = "dot")
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
plot(cvFitsLts, method = "bw")
plot(cvFitsLts, method = "density")
plot(cvFitsLts, method = "xy")
plot(cvFitsLts, method = "dot")

#lab1cv15
set.seed(1234)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLm <- lm(Y ~ ., data = coleman)
repCV(fitLm, cost = rtmspe, folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman)
repCV(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
repCV(fitLts, cost = rtmspe, folds = folds, trim = 0.1)
repCV(fitLts, cost = rtmspe, folds = folds,
      fit = "both", trim = 0.1)

#lab1cv16
set.seed(1234)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
summary(cvFitLts50)
summary(cvFitsLts)
tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
call <- call("lmrob", formula = Y ~ .)
cvFitsLmrob <- cvTuning(call, data = coleman,
                        y = coleman$Y, tuning = tuning, cost = rtmspe,
                        folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
summary(cvFitsLmrob)

#lab1cv17
set.seed(1234)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)
fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
xyplot(cvFits)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
xyplot(cvFitsLts)
tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
cvFitsLmrob <- cvTuning(fitLmrob$call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob

xyplot(cvFitsLmrob)

#lab1cv18

require(boot)

data(mammals, package="MASS")
mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals, mammals.glm)$delta)
(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)

muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.err <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)
