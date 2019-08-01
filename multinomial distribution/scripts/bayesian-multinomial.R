rm(list = ls())
######################################################################################
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
######################################################################################
hpd.Chen <- function(x, alpha = 0.05)
{
  x    <- sort(x)
  n    <- length(x)
  namp <- (1 - alpha) * n
  amp  <- sapply(1:(n - namp), function(j)   diff(c(x[j], x[j + namp])))
  id   <- order(amp)[1]
  c(lower = x[id], upper = x[id + namp])
}
######################################################################################
moda <- function(dados)
{
  ht=hist(dados, plot = F)
  mcl <- which.max(ht$counts) # i 
  li <- ht$breaks[mcl] # li
  width <- diff(ht$breaks[mcl+0:1]) # h
  counts <- c(0,ht$counts,0)
  delta <- abs(diff(counts[1+mcl+(-1:1)])) # denominador
  moda <- li+width*delta[1]/sum(delta)
  cols <- rep(5, length(ht$counts))
  cols[mcl] <- 3
  return(moda=moda)
}
######################################################################################
rdirichlet <- function(n, alphas)
{
  k  <- length(alphas)
  Wi <- sapply(1:k, function(i) rgamma(n = n, shape = alphas[i], scale = 1))  
  Ti <- rowSums(Wi)
  Di <- Wi / Ti
  colnames(Di) <- paste0("p", 1:k)
  return(Di)
}
######################################################################################
ni          <- c(727, 583, 137)
alphas      <- c(1, 1, 1)
posterioris <- rdirichlet(n = 1000, alphas = alphas + ni)
posterioris <- cbind(posterioris, theta = posterioris[, 1] - posterioris[, 2])
medias      <- colMeans(posterioris)
desvios     <- apply(posterioris, 2, sd)
hpds        <- apply(posterioris, 2, hpd.Chen)
tab         <- cbind(paste0(FF(medias), " (", FF(desvios), ")"), paste0("(",FF(hpds[1, ]), ", ", FF(hpds[2,]), ")"))
######################################################################################
par   <- posterioris[, 4]
densi <- density(par)
Rx    <- range(densi$x)
Ry    <- range(densi$y)
pdf(file = 'theta-density.pdf', width = 9)
par(mar = c(3.2, 3.2, 1.0, 1.0), cex = 1.8)
hist(par, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', main = '', probability = T)
lines(densi, lwd = 2)
rug(par)
box()
axis(side = 1, at = seq(Rx[1], Rx[2], l = 5), labels = FF(seq(Rx[1], Rx[2], l = 5), 2))
axis(side = 2, at = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 2))
mtext(text = 'Densidade', side = 2, line = 2, cex = 1.8)
mtext(text = expression(theta),  side = 1, line = 2, cex = 1.8)
abline(v = mean(par), col = 'red', lwd = 2)
abline(v = median(par), col = 'blue', lwd = 2)
abline(v = moda(par), col = 'green', lwd = 2)
legend('topleft', legend = c('Média', 'Mediana', 'Moda'), col = c('red', 'blue', 'green'), bty = 'n', lwd = 2)
graphics.off()


