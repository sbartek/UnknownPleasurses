require(ggplot2)

delta <- 0.01

x.mu1 <- 0.25
x.s1 <- 0.05
x.mu2 <- 0.5
x.s2 <- 0.02
x.mu3 <- 0.75
x.s3 <- 0.05

y.mu2 <- sqrt(0.33^2-0.167^2)*0.3
y.s2 <- 0.2

saw <- function(x, y) {
  n <- length(x)
  xmin <- x[1]
  xmax <- x[n]

  
  f <- function(t) {
    if (t<=xmin) {
      return(y[1])
    } else {
        if (t>= xmax) {
          return(y[n])
        } else {
            k <- which(x>= t)[1]
            a <- (y[k] - y[k-1])/(x[k]-x[k-1])
            b <- y[k] - a*x[k]
            return(a*t+b)
          }
      }
  }
  return(function(s) { sapply(s, f)})
}



x <- c(0, 0.5, 1)
y <- c(0, 1, 0)
tent <- saw(x, y)
t <- seq(-0.2,1.2,0.1)
tent(t)

x <- c(0, 0.5, 1, 2)
y <- c(0, 1, 0, 1)
tent <- saw(x, y)

plot(t, tent(t))

pulse.gen <- function() {
  x1 <- rnorm(1, x.mu1, x.s1)
  x2 <- max(rnorm(1, x.mu2, x.s2), x1+delta)
  x3 <- max(rnorm(1, x.mu3, x.s3), x2+delta)
  y2 <- max(rnorm(1, y.mu2, y.s2), delta)
  return(saw(c(x1,x2,x3), c(0,y2,0)))
}

pulse2.gen <- function() {
  x1 <- rnorm(1, x.mu1, x.s1)
  x2 <- max(rnorm(1, x.mu2, x.s2), x1+delta)
  x3 <- max(rnorm(1, x.mu3, x.s3), x2+delta)
  y2 <- max(rnorm(1, y.mu2, y.s2), delta)
  tent <- saw(c(x1,x2,x3), c(0,y2,0))
  d <- x2-x1
  x.s1x <- d/12
  x.mu11 <- x1+d/3
  x.mu12 <- x1+2*d/3
  x11 <- rnorm(1, x.mu11, x.s1x)
  x12 <- rnorm(1, x.mu12, x.s1x)
  y12 <- tent(x12)
  y.mu1 <- (y12+tent(x11))/2
  y.s1 <- y.mu1/4
  y11 <- max(rnorm(1, y.mu2, y.s2), delta)
  d2 <- x3-x2
  x.s2x <- d2/12
  x.mu21 <- x2+d2/3
  x.mu22 <- x2+2*d2/3
  x21 <- rnorm(1, x.mu21, x.s2x)
  x22 <- rnorm(1, x.mu22, x.s2x)
  y21 <- tent(x21)
  y.mu22 <- (y21+tent(x22))/2
  y.s22 <- y.mu22/4
  y22 <- max(rnorm(1, y.mu22, y.s22), delta)

  return(saw(c(x1, x11, x12, x2, x21, x22, x3),
             c(0, y11, y12, y2, y21, y22, 0)))
}


whiteNoise.gen <- function(x, sigma) {
  y <- rnorm(length(x), 0, sigma)
  return(saw(x,y))
}

x <- seq(0,1,0.01)
Ns <- seq(0.2,2.9,0.1)
mx <- matrix(x)
for (N in Ns) {
  f.wn <- whiteNoise.gen(seq(0,1,0.02), 0.006)
  f.pulse <- pulse2.gen()
  y <- f.pulse(x)  + N + f.wn(x)
  mx <- cbind(mx,y)
}

gg <- ggplot(data=NULL, aes(x=x))
K <- dim(mx)[2]

for (k in 2:K) {
  gg <- gg+geom_line(aes(y=mx[,k]))
}

require(data.table)
colnames(mx) <- c("x", Ns)
dt <- data.table(mx)
require(reshape2)
dt2 <- melt(dt, id.vars="x")

ggplot(dt2, aes(x, value,
                by=factor(variable, levels = rev(levels(variable))))) +
  geom_area(position="identity", colour="white", fill="black" )+theme_bw()+
  theme(panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("") + ylab("") +
  geom_vline(xintercept=0, colour="black", size=1)+
  geom_vline(xintercept=1, colour="black", size=1)+
  geom_hline(yintercept=0, colour="black", size=1)


 
