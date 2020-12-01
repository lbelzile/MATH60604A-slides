dat <- data.frame(time = c(2,2,3,5,5,5,6,8,10,11), 
                  subject = c(8,9,3,2,4,10,7,5,6,1), 
                  censoring = c(1,1,1,0,0,1,1,1,1,0))
plot(x = dat$time, 
     y = 1:nrow(dat), 
     type = 'p', 
     pch = c(20,4)[dat$censoring + 1], 
     xlim = c(0,12),
     xlab = "tim (in days)",
     ylab = "subject id",
     bty = "l",
     xaxs = "i",
     yaxt = "n")
Axis(side = 2, labels = dat$subject, at = 1:nrow(dat))
for(o in 1:nrow(dat)){
 segments(x0 = 0, 
          x1 = dat$time[o],
          y0 = o, 
          y1 = o)
}


dat <- data.frame(time = c(2,2,3,5,5,5,6,8,10,11), 
                  subject = c(8,9,3,2,4,10,7,5,6,1), 
                  censoring = c(1,1,1,0,0,1,1,1,1,0))
plot(x = dat$time, 
     y = 1:nrow(dat), 
     type = 'p', 
     pch = c(20,4)[dat$censoring + 1], 
     xlim = c(0,12),
     xlab = "temps (en jour)",
     ylab = "identifiant",
     bty = "l",
     xaxs = "i",
     yaxt = "n")
Axis(side = 2, labels = dat$subject, at = 1:nrow(dat))
for(o in 1:nrow(dat)){
   segments(x0 = 0, 
            x1 = dat$time[o],
            y0 = o, 
            y1 = o)
}
