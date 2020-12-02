
# Type I censoring
library(Epi)
set.seed(1234)
birth.date <- sample(seq(0,4.25, by = 0.25), size = 10, replace = TRUE)
entry.age <- rep(0, 10) #sample(35:70, size = 10, replace = TRUE)
exit.date <- pmin(5, birth.date + rexp(n = 10, rate = 0.6))
fail <- exit.date != 5 & rbinom(n = 10, size = 1, prob = 0.7)
col <- 1L + as.integer(birth.date < 0)
pdf("07-lexis_en.pdf", width = 6, height = 4)
LL <- Lexis.diagram(birth.date = birth.date,
                    entry.age = entry.age,
                    exit.date = exit.date,
                    fail = fail,
                    date.grid = FALSE,
                    age.grid = FALSE,
                    date = c(0, 5.1),
                    age = c(0, 5),
                    pch.fail = c(1,4),
                    col.fail = c(1,1),
                    lab.int = 1,
                    alab = "duration time",
                    dlab = "calendar time", bty = "l",
                    panel.first = {
                      abline(h = 1.5, col = "gray90")
                      abline(v = 5, lty = 2);
                      })
segments(x0 = 0, x1 = 2, y0 = 1.5, y1 = 3.5, col = 2, lwd = 2)
points(x=2,y=3.5, pch = 4, col = 2, lwd =2 )
dev.off()
pdf("07-lexis_fr.pdf", width = 6, height = 4)
LL <- Lexis.diagram(birth.date = birth.date,
                    entry.age = entry.age,
                    exit.date = exit.date,
                    fail = fail,
                    date.grid = FALSE,
                    age.grid = FALSE,
                    date = c(0, 5.1),
                    age = c(0, 5),
                    pch.fail = c(1,4),
                    col.fail = c(1,1),
                    lab.int = 1,
                    alab = "durée",
                    dlab = "date d'initialisation", bty = "l",
                    panel.first = {
                      abline(h = 1.5, col = "gray90")
                      abline(v = 5, lty = 2);
                    })
segments(x0 = 0, x1 = 2, y0 = 1.5, y1 = 3.5, col = 2, lwd = 2)
points(x=2,y=3.5, pch = 4, col = 2, lwd =2 )
dev.off()


# 
# # Gompertz-Makeham
# qgompmake <- function(p, alpha, beta, lambda){
#   alpha/(beta*lambda) - 1/lambda*log(1-p) - lamW::lambertW0(alpha*exp(alpha/lambda)*(1-p)^(-beta/lambda)/lambda)/beta
# }
# pgompmake <- function(q, alpha, beta, lambda){
#   1-exp(-lambda*q-alpha/beta*(exp(beta*q)-1))
# }
# dgompmake <- function(x, alpha, beta, lambda, log = FALSE){
#   logd <- log(alpha*exp(beta*x)+lambda) + -lambda*x - alpha/beta*(exp(beta*x)-1)
#   if(log){ return(logd)} else{ return(exp(logd))}                                         
# }
# hgompmake <- function(x, alpha, beta, lambda){
#   alpha*exp(beta*x)+lambda
# }
# alpha <- 0.5
# beta <- 0.05
# lambda <- 1
# q <- qgompmake(p = u, alpha= alpha, beta=beta, lambda= lambda)
# plot(q, hgompmake(q, alpha, beta, lambda), type = "l")
# 
# dexpweib <- function(x, k, lambda, alpha, log = FALSE){
#   logd <- log(alpha)+log(k)-k*log(lambda)+(k-1)*log(x) + (alpha-1)*log(1-exp(-(x/lambda)^k))-(x/lambda)^k
#   if(log){return(logd)} else { return(exp(logd))}
# }
# pexpweib <- function(q, k, lambda, alpha){
#   (1-exp(-(q/lambda)^k))^alpha
# }
# 
# hexpweib <- function(x, k, lambda, alpha){
#   dexpweib(x, k, lambda, alpha)/(1-pexpweib(x, k, lambda, alpha))
# }
# k = 0.8; lambda = 0.8; alpha = 0.8
# 
# pchen <- function(q, alpha, beta){
#   1-(exp(1-exp(q^beta)))^alpha
# }
# dchen <- function(x, alpha, beta, log = FALSE){
#   lpdf <- log(alpha)+log(beta) +(alpha-1)*log(1-exp(1-exp(x^beta))) +(1+x^beta-exp(x^beta)) + (beta-1)*log(x)
#   if(log){return(lpdf)} else{exp(lpdf)}
# }
 hchen <- function(x, alpha, beta){
   alpha*beta*((1-exp(1-exp(x^beta)))^(alpha-1))*exp(1+x^beta-exp(x^beta))*x^(beta-1)/(1-(1-exp(1-exp(x^beta)))^alpha)
 }
 alpha <- 0.4; beta <- 2
# 
pdf("07-bathtub-hazard.pdf", width = 6, height = 4)
par(mar = c(2,4,1,1))
xseq <- seq(0, 1, length.out = 1001)
plot(xseq, ylab = "hazard function", 
     y = hchen(xseq, beta = beta, alpha = alpha), 
     type = "l", xaxt = "n", xlab = "", bty = "l")
mtext(side = 1, line = 0.5, text = "time")
dev.off()

pdf("07-bathtub-hazard-fr.pdf", width = 6, height = 4)
par(mar = c(2,4,1,1))
xseq <- seq(0, 1, length.out = 1001)
plot(xseq, ylab = "fonction de risque", 
     y = hchen(xseq, beta = beta, alpha = alpha), 
     type = "l", xaxt = "n", xlab = "", bty = "l")
mtext(side = 1, line = 0.5, text = "temps")
dev.off()

pdf("07-survival-hazard.pdf", width = 8, height = 4)
shape <- 1.5; scale <- 2
par(mfrow = c(1,2), mar = c(4,4,1,1))
u <- seq(0, 0.998, by = 0.001)
xpos <- qweibull(u, shape = shape, scale = scale, lower.tail = TRUE)
plot(y = 1-u, x = xpos, xlim = c(0, max(xpos)), xaxs = "i", bty = "l", type = "l", ylim = c(0,1), yaxs = "i", xlab = "time", ylab = "survivor function")
plot(y = dweibull(xpos, shape = shape, scale = scale)/pweibull(xpos, shape = shape, scale = scale, lower.tail = FALSE), x = xpos, xlim = c(0, max(xpos)), xaxs = "i", bty = "l", type = "l", yaxs = "i", xlab = "time", ylab = "hazard function")
dev.off()
pdf("07-survival-hazard-fr.pdf", width = 8, height = 4)
par(mfrow = c(1,2), mar = c(4,4,1,1))
plot(y = 1-u, x= xpos, xlim = c(0, max(xpos)), xaxs = "i", bty = "l", type = "l", ylim = c(0,1), yaxs = "i", xlab = "temps", ylab = "fonction de survie")
plot(y = dweibull(xpos, shape = shape, scale = scale)/pweibull(xpos, shape = shape, scale = scale, lower.tail = FALSE), x = xpos, xlim = c(0, max(xpos)), xaxs = "i", bty = "l", type = "l", yaxs = "i", xlab = "temps", ylab = "fonction de risque")
dev.off()
