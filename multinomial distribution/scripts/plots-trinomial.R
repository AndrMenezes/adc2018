rm(list = ls())
FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
library(ggplot2)


## Multinomial(100; 0.5, 0.3, 0.2)
set.seed(1212)
M  <- 1000
n  <- 100
p  <- c(0.5, 0.3, 0.2)
X  <- rmultinom(M, n, p)
df <- data.frame(t(X))
p1 <- ggplot(df, aes(x = X1, y = X2)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradient2(low ="dodgerblue2", high ="firebrick2", mid = "white", midpoint = 0.005) +
  scale_x_continuous(limits = c(32, 66), breaks = seq(34, 64, l = 5), labels = FF(seq(34, 64, l = 5), 0), 
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(14, 50), breaks = seq(16, 48, l = 5), labels = FF(seq(16, 48, l = 5), 0),
                     expand = c(0, 0)) +
  labs(fill = "") +
  ggtitle("Multinomial(100; 0.5, 0.3, 0.2)") +
  geom_point() + theme_bw() +
  theme(text              = element_text(colour = "black", size = 16),
        legend.position   = "right",
        legend.key.width  = unit(0.5, "cm"),
        legend.key.height = unit(2.7, "cm"),
        plot.title        = element_text(hjust = 0.5),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.border      = element_blank())

mu    <- n * p
xbar  <- rowMeans(X)
C.teo <- diag(3)
C.teo[1,2] <- C.teo[2,1] <- - sqrt(p[1] * p[2] / ((1 - p[1]) * (1 - p[2])))
C.teo[1,3] <- C.teo[3,1] <- - sqrt(p[1] * p[3] / ((1 - p[1]) * (1 - p[3])))
C.teo[2,3] <- C.teo[3,2] <- - sqrt(p[2] * p[3] / ((1 - p[2]) * (1 - p[3])))
C.emp <- cor(t(X))




## Multinomial(100; 0.7, 0.2, 0.1)
set.seed(1212)
x  <- rmultinom(1000, 100, c(0.7, 0.2, 0.1))
df <- data.frame(t(x))
p2 <- ggplot(df, aes(x = X1, y = X2)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradient2(low ="dodgerblue2", high ="firebrick2", mid = "white", midpoint = 0.005) +
  geom_point() + theme_bw() +
  scale_x_continuous(limits = c(54, 86), breaks = seq(56, 84, l = 5), labels = FF(seq(56, 84, l = 5), 0), 
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(6, 34), breaks = seq(8, 32, l = 5), labels = FF(seq(8, 32, l = 5), 0),
                     expand = c(0, 0)) +
  labs(fill = "") +
  ggtitle("Multinomial(100; 0.7, 0.2, 0.1)") +
  theme(text              = element_text(colour = "black", size = 16),
        legend.position   = "right",
        legend.key.width  = unit(0.5, "cm"),
        legend.key.height = unit(2.7, "cm"),
        plot.title        = element_text(hjust = 0.5),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        panel.border      = element_blank())
setwd("C:\\Users\\User\\Dropbox\\5° Série\\Análise de Dados Categóricos\\Distribuição Multinomial")
ggsave(filename = "mult1.pdf", plot = p1, device = "pdf", width = 9, height = 7)
ggsave(filename = "mult2.pdf", plot = p2, device = "pdf", width = 9, height = 7)
