rm(list = ls())

library(VGAM)
library(xtable)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wesanderson)

wd1 <- "C:/Users/User/Dropbox/5° Série/Análise de Dados Categóricos/Regressão Multinomial/dados"
wd2 <- "C:/Users/User/Dropbox/5° Série/Análise de Dados Categóricos/Regressão Multinomial/latex"
FF  <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}

# Leitura dos dados -------------------------------------------------------
setwd(wd1)
dados <- read.table("dados_datafolha.csv", sep = ";", header = T, na.strings = ".")

dados$y3        <- factor(dados$y3, ordered = T)
dados$sexo      <- factor(dados$sexo)
dados$votou2010 <- factor(dados$votou2010)
dados$denuncia  <- factor(dados$denuncia)
dados$sexo      <- relevel(dados$sexo, ref = "2")
dados$votou2010 <- relevel(dados$votou2010, ref = "2")
dados$denuncia  <- relevel(dados$denuncia, ref = "2")


# Gráficos descritivos ----------------------------------------------------
dados2           <- dados %>% select(y3, sexo, votou2010, denuncia) %>% na.exclude()
dados2$y3        <- factor(dados2$y3, labels = c("Ótimo", "Bom", "Regular", "Ruim", "Péssimo"))
dados2$sexo      <- factor(dados2$sexo, levels = c(1, 2), labels = c("Masculino", "Feminino"))
dados2$denuncia  <- factor(dados2$denuncia, levels = c(1, 2), labels = c("Favorável denúncia", "Contra denúncia"))
dados2$votou2010 <- factor(dados2$votou2010, levels = c(1, 2, 3), labels = c("Aécio", "Dilma", "Não votou"))

# Plots usando o R base ---------------------------------------------------
setwd(wd2)

tab1  <- with(dados2, table(y3))
ptab1 <- as.numeric(prop.table(tab1))

Ry <- c(0, max(tab1) + 150)
pdf(file = "temer-base.pdf", width = 9)
par(mar = c(3.0, 3.0, 1.5, 0.5), cex = 1.8)
bp <- barplot(height      = tab1, 
              col         = gray.colors(5), 
              xaxt        = "n", 
              yaxt        = "n",
              ylim        = Ry, 
              border      = "black", 
              legend.text = names(tab1),
              args.legend = list(x = "topleft", bty = "n", cex = 0.6, inset = 0.01),
              xlab        = "",
              ylab        = "")
axis(side = 2, at = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 0))
text(x = bp, y = tab1, labels = paste0(FF( ptab1 * 100, 2), '%'), pos = 3, cex = 0.6)
box(bty = "l")
graphics.off()


#  ------------------------------------------------------------------------
tab1  <- with(dados2, table(y3, sexo))
ptab1 <- as.numeric(prop.table(tab1, margin = 2))

Ry <- c(0, max(tab1) + 100)
pdf(file = "sexo-base.pdf", width = 9)
par(mar = c(3.0, 3.0, 1.5, 0.5), cex = 1.8)
bp <- barplot(height      = tab1, 
              beside      = TRUE,
              col         = gray.colors(5), 
#             xaxt        = "n", 
              yaxt        = "n",
              ylim        = Ry, 
              border      = "black", 
              legend.text = rownames(tab1),
              args.legend = list(x = "topleft", bty = "n", cex = 0.6, inset = 0.01),
              xlab        = "",
              ylab        = "")
axis(side = 2, at = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 0))
text(x = bp, y = tab1, labels = paste0(FF( ptab1 * 100, 2), '%'), pos = 3, cex = 0.6)
box(bty = "l")
graphics.off()

#  ------------------------------------------------------------------------
tab1  <- with(dados2, table(y3, denuncia))
ptab1 <- as.numeric(prop.table(tab1, margin = 2))

Ry <- c(0, max(tab1) + 100)
pdf(file = "denuncia-base.pdf", width = 9)
par(mar = c(3.0, 3.0, 1.5, 0.5), cex = 1.8)
bp <- barplot(height      = tab1, 
              beside      = TRUE,
              col         = gray.colors(5), 
#             xaxt        = "n", 
              yaxt        = "n",
              ylim        = Ry, 
              border      = "black", 
              legend.text = rownames(tab1),
              args.legend = list(x = "topleft", bty = "n", cex = 0.6, inset = 0.01),
              xlab        = "",
              ylab        = "")
axis(side = 2, at = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 0))
text(x = bp, y = tab1, labels = paste0(FF( ptab1 * 100, 2), '%'), pos = 3, cex = 0.6)
box(bty = "l")
graphics.off()

#  ------------------------------------------------------------------------
tab1  <- with(dados2, table(y3, votou2010))
ptab1 <- as.numeric(prop.table(tab1, margin = 2))

Ry <- c(0, max(tab1) + 100)
pdf(file = "votou2010-base.pdf", width = 9)
par(mar = c(3.0, 3.0, 1.5, 0.5), cex = 1.8)
bp <- barplot(height      = tab1, 
              beside      = TRUE,
              col         = gray.colors(5), 
#             xaxt        = "n", 
              yaxt        = "n",
              ylim        = Ry, 
              border      = "black", 
              legend.text = rownames(tab1),
              args.legend = list(x = "topleft", bty = "n", cex = 0.6, inset = 0.01),
              xlab        = "",
              ylab        = "")
axis(side = 2, at = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 0))
text(x = bp, y = tab1, labels = paste0(FF( ptab1 * 100, 2), '%'), pos = 3, cex = 0.4)
box(bty = "l")
graphics.off()

# Plot com geom_text ------------------------------------------------------
df      <- as.data.frame(table(dados2$y3))
df$lab1 <- paste0(df$Freq, ' (', round(df$Freq / sum(df$Freq), 3) * 100, '%)')
df$lab2 <- paste0(FF( (df$Freq / sum(df$Freq)) * 100, 2), '%')
df$por  <- (df$Freq / sum(df$Freq)) * 100
ggplot(data = df, aes(Var1, Freq)) + 
  geom_bar(aes(fill = Var1), colour = "black", size = 0.4, stat = 'identity') + 
  geom_text(aes(Var1, Freq + 30, label = lab2)) +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequencia absoluta", fill = "") +
  theme(text             = element_text(family = "Palatino", colour = "black", size = 16),
        axis.title.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position  = "top")
ggsave("temer.pdf", device = "pdf", height = 6, width = 9)

#  ------------------------------------------------------------------------
df      <- as.data.frame(table(dados2$y3, dados2$sexo))
df$lab2 <- paste0(FF( (df$Freq / sum(df$Freq)) * 100, 2), '%')
df$por  <- (df$Freq / sum(df$Freq)) * 100

ggplot(data = df, aes(x = Var2, y = Freq, label = lab2, group = Var1)) + 
  geom_bar(aes(fill = Var1), stat = "identity", position = "dodge", colour = "black") + 
  geom_label(position = position_dodge(width = 0.9), 
             show.legend = F,  vjust = -0.35, inherit.aes = TRUE, color = "black", fill = NA) +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequência absoluta", fill = "") +
  theme(panel.grid.minor = element_blank(), 
        text = element_text(family = "Palatino", colour = "black", size = 16),
        legend.position = "top")
ggsave("sexo.pdf", device = "pdf", height = 6, width = 9)

#  ------------------------------------------------------------------------
df      <- as.data.frame(table(dados2$y3, dados2$votou2010))
df$lab2 <- paste0(FF( (df$Freq / sum(df$Freq)) * 100, 2), '%')
df$por  <- (df$Freq / sum(df$Freq)) * 100

ggplot(data = df, aes(x = Var2, y = Freq, label = lab2, group = Var1)) + 
  geom_bar(aes(fill = Var1), stat = "identity", position = "dodge", colour = "black") + 
  geom_label(position = position_dodge(width = 0.9), 
             show.legend = F,  vjust = -0.35, inherit.aes = TRUE, color = "black", fill = NA) +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequência absoluta", fill = "") +
  theme(panel.grid.minor = element_blank(), 
        text = element_text(family = "Palatino", colour = "black", size = 16),
        legend.position = "top")
ggsave("votou2010.pdf", device = "pdf", height = 6, width = 9)



#  ------------------------------------------------------------------------
df      <- as.data.frame(table(dados2$y3, dados2$denuncia))
df$lab2 <- paste0(FF( (df$Freq / sum(df$Freq)) * 100, 2), '%')
df$por  <- (df$Freq / sum(df$Freq)) * 100

ggplot(data = df, aes(x = Var2, y = Freq, label = lab2, group = Var1)) + 
  geom_bar(aes(fill = Var1), stat = "identity", position = "dodge", colour = "black") + 
  geom_label(position = position_dodge(width = 0.9), 
             show.legend = F,  vjust = -0.35, inherit.aes = TRUE, color = "black", fill = NA) +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequência absoluta", fill = "") +
  theme(panel.grid.minor = element_blank(), 
        text = element_text(family = "Palatino", colour = "black", size = 16),
        legend.position = "top")
ggsave("denuncia.pdf", device = "pdf", height = 6, width = 9)



# Plots sem geom_text -----------------------------------------------------
ggplot(data = dados2, aes(sexo)) + 
  geom_bar(aes(fill = y3), colour = "black", size = 0.4, position = "dodge") +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequência absoluta", fill = "") +
  theme(panel.grid.minor = element_blank(), 
        text = element_text(family = "Palatino", colour = "black", size = 16),
        legend.position = "top")
ggsave("sexo.pdf", device = "pdf", height = 6, width = 9)

ggplot(data = dados2, aes(denuncia)) + 
  geom_bar(aes(fill = y3), colour = "black", size = 0.4, position = "dodge") +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequência absoluta", fill = "") +
  theme(panel.grid.minor = element_blank(), 
        text = element_text(family = "Palatino", colour = "black", size = 16),
        legend.position = "top")
ggsave("denuncia.pdf", device = "pdf", height = 6, width = 9)

ggplot(data = dados2, aes(votou2010)) + 
  geom_bar(aes(fill = y3), colour = "black", size = 0.4, position = "dodge") +
  scale_fill_grey(start = 0.1, end = 0.96) +
  theme_bw() +
  labs(x = "", y = "Frequência absoluta", fill = "") +
  theme(panel.grid.minor = element_blank(), 
        text = element_text(family = "Palatino", colour = "black", size = 16),
        legend.position = "top")
ggsave("votou2010.pdf", device = "pdf", height = 6, width = 9)


# Modelos -----------------------------------------------------------------
mod1 <- vgam(formula = y3 ~ 1, family = cumulative(parallel = TRUE, reverse = FALSE), data = dados)
mod2 <- vgam(formula = y3 ~ sexo, family = cumulative(parallel = TRUE, reverse = FALSE), data = dados)
mod3 <- vgam(formula = y3 ~ sexo + denuncia, family = cumulative(parallel = TRUE, reverse = FALSE), data = dados)
mod4 <- vgam(formula = y3 ~ sexo + denuncia + votou2010, family = cumulative(parallel = TRUE, reverse = FALSE), data = dados)
mod5 <- vgam(formula = y3 ~ sexo * denuncia * votou2010, family = cumulative(parallel = TRUE, reverse = FALSE), data = dados)

## número de parâmetros
npar_m1 <- length(coef(mod1))
npar_m2 <- length(coef(mod2))
npar_m3 <- length(coef(mod3))
npar_m4 <- length(coef(mod4))
npar_m5 <- length(coef(mod5))

## graus de liberdade
gl_1 <- npar_m2 - npar_m1
gl_2 <- npar_m3 - npar_m2
gl_3 <- npar_m4 - npar_m3
gl_4 <- npar_m5 - npar_m4

## log-likelihoods
ll_m1 <- logLik(mod1)
ll_m2 <- logLik(mod2)
ll_m3 <- logLik(mod3)
ll_m4 <- logLik(mod4)
ll_m5 <- logLik(mod5)

## testes da razão de verossimlhança
trv_m2_vs_m1 <- 2 * (ll_m2 - ll_m1); pv_m2_vs_m1 <- 1 - pchisq(q = trv_m2_vs_m1, df = gl_1)
trv_m3_vs_m2 <- 2 * (ll_m3 - ll_m2); pv_m3_vs_m2 <- 1 - pchisq(q = trv_m3_vs_m2, df = gl_2)
trv_m4_vs_m3 <- 2 * (ll_m4 - ll_m3); pv_m4_vs_m3 <- 1 - pchisq(q = trv_m4_vs_m3, df = gl_3)
trv_m5_vs_m4 <- 2 * (ll_m5 - ll_m4); pv_m5_vs_m4 <- 1 - pchisq(q = trv_m5_vs_m4, df = gl_4)

## critérios de informação
AIC_m1 <- AIC(mod1); BIC_m1 <- BIC(mod1)
AIC_m2 <- AIC(mod2); BIC_m2 <- BIC(mod2)
AIC_m3 <- AIC(mod3); BIC_m3 <- BIC(mod3)
AIC_m4 <- AIC(mod4); BIC_m4 <- BIC(mod4)
AIC_m5 <- AIC(mod5); BIC_m5 <- BIC(mod5)

## tabela final
tab <- cbind(gl  = c(NA, gl_1, gl_2, gl_3, gl_4), 
             trv = c(NA, trv_m2_vs_m1, trv_m3_vs_m2, trv_m4_vs_m3, trv_m5_vs_m4),
             pv  = c(NA, pv_m2_vs_m1, pv_m3_vs_m2, pv_m4_vs_m3, pv_m5_vs_m4), 
             AIC = c(AIC_m1, AIC_m2, AIC_m3, AIC_m4, AIC_m5), 
             BIC = c(BIC_m1, BIC_m2, BIC_m3, BIC_m4, BIC_m5)             )
rownames(tab) <- c("Nulo", "$X_1$", "$X_2 \\mid X_1$", "$X_3 \\mid X_1, X_2$", 
                   "$X_1 \\ast X_2 \\ast X_3 \\mid X_1, X_2, X_3$")
colnames(tab) <- c("G.L.", "TRV", "valor-\\emph{p}", "AIC", "BIC")
print.xtable(xtable(tab, digits = c(0, 0, 4, 4, 4, 4)), sanitize.text.function = force)



# Estimando a proporção sem modelo ----------------------------------------
sexo      <- unique(dados$sexo)
denuncia  <- unique(dados$denuncia)[-2]
votou2010 <- unique(dados$votou2010)
grid_pred <- expand.grid(sexo = sexo, denuncia = denuncia, votou2010 = votou2010)

obs <- c()
for(j in 1:nrow(grid_pred))
{
  id       <- grid_pred[j, ]
  aux      <- dados %>% filter(sexo == id$sexo & denuncia == id$denuncia & votou2010 == id$votou2010) %>% select(sexo, denuncia, votou2010, y3)
  prop     <- data.frame(t(as.matrix(prop.table(table(aux$y3)))))
  names(prop) <- paste0("Pobs(Y = ", 1:5, ")")
  
  obs <- rbind(obs, cbind(id, prop))
}
obs

# Predição ----------------------------------------------------------------
preditos  <- predict(mod4, newdata = grid_pred, type = "response")
preditos  <- cbind(grid_pred, preditos)
names(preditos) <- c("sexo", "denuncia", "votou2010", "P(Y = 1)", "P(Y = 2)", "P(Y = 3)", "P(Y = 4)", "P(Y = 5)")

pred_obs <- merge(preditos, obs)
nome     <- c("Ótimo", "Bom", "Regular", "Ruim", "Péssimo")
rmse     <- c()
for(i in 1:5)
{
  id1 <- paste0("P(Y = ", i, ")")
  id2 <- paste0("Pobs(Y = ", i, ")")
  
  teo <- pred_obs[, id1]
  emp <- pred_obs[, id2]
  Rx  <- range(teo)
  Ry  <- range(emp)
  # mse <- sqrt(sum((teo - emp)^2))
  rmse[i] <- sqrt(sum((teo - emp)^2))
  pdf(paste0("ppplot-", i, ".pdf"))
  par(mar = c(3.2, 3.2, 1.0, 1.0), cex = 1.8)
  plot(x = teo, y = emp,  type = 'p', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xlim = Rx, ylim = Ry, pch = 3, col = 1, bty = 'l')
  box(); abline(0, 1, lwd = 1);
  abline(h = seq(Rx[1], Rx[2], l = 5), v = seq(Ry[1], Ry[2], l = 5), col = "lightgray", lty = "dotted")
  axis(1, seq(Rx[1], Rx[2], l = 5), FF(seq(Rx[1], Rx[2], l = 5), 2))
  axis(2, seq(Ry[1], Ry[2], l = 5), FF(seq(Ry[1], Ry[2], l = 5), 2))
  mtext("Probabilidades teóricas", side = 1, line = 2.0, cex = 1.8)
  mtext("Probabilidades empíricas", side = 2.1, line =2.2, cex = 1.8)
  mtext(paste0(id1, " - ", nome[i]), side = 3, cex = 1.8)
  # if(i %in% c(1, 2, 3)) legend("topleft", legend = paste0("RMSE = ", round(mse, 4)), inset = 0.001, bty = "n")
  # else legend("bottomright", legend = paste0("RMSE = ", round(mse, 4)), inset = 0.001, bty = "n")
  graphics.off() 
}
tab  <- pred_obs[, c(1, 2, 3, 9, 4, 10, 5, 11, 6, 12, 7, 13, 8)]
round(rmse, 4)                    
print.xtable(xtable(tab, digits = c(0, 0, 0, 0, rep(4, 10))), include.rownames = F, sanitize.text.function = force)



# Predição SAS ------------------------------------------------------------

## Probabilidade em cada categoria
setwd(wd1)
dados_prob <- read.table("prob.csv", sep = ",", header = T)[, -4]
head(dados_prob)

dados_prob <- dados_prob %>% gather(key = "y", value = "prob", -c(sexo, denuncia, votou2010)) 
aux1       <- dados_prob %>% filter(y %in% paste0("P_", 1:5)) 
aux2       <- dados_prob %>% filter(y %in% paste0("LCL_", 1:5)) %>% rename(lower = prob) %>% mutate(y = gsub("LCL", "P", y))
aux3       <- dados_prob %>% filter(y %in% paste0("UCL_", 1:5)) %>% rename(upper = prob) %>% mutate(y = gsub("UCL", "P", y))
dados_prob <- merge(merge(aux1, aux2), aux3)

dados_prob$y         <- factor(dados_prob$y, labels = c("Ótimo", "Bom", "Regular", "Ruim", "Péssimo"))
dados_prob$sexo      <- factor(dados_prob$sexo, levels = c(1, 2), labels = c("Masculino", "Feminino"))
dados_prob$denuncia  <- factor(dados_prob$denuncia, levels = c(1, 2), labels = c("Favorável denúncia", "Contra denúncia"))
dados_prob$votou2010 <- factor(dados_prob$votou2010, levels = c(1, 2, 3), labels = c("Aécio", "Dilma", "Não votou"))
dados_prob$interacao <- with(dados_prob, paste0(sexo, ",\n ", denuncia, ", \n ", votou2010))
head(dados_prob)

Ry <- range(0, dados_prob$lower, dados_prob$upper)
dados_prob %>% ggplot(aes(x = y, y = prob, color = y)) +
  facet_wrap(~ interacao) + 
  geom_point(size = 1.5, pch = 23) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 0.4) +
  scale_y_continuous(breaks = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 2), limits = Ry) +
  labs(x = "", y = "Probabilidade estimada", color = "") +
  theme(text               = element_text(family = "Palatino", colour = "black", size = 16),
        axis.text.x        = element_blank(), 
        axis.title.x       = element_blank(), 
        axis.ticks.x       = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position    = "top") -> p1
ggsave(filename = "prob.pdf", plot = p1, device = "pdf", width = 11, height = 7)


## Probabilidade acumulada
dados_acum <- read.table("acum.csv", sep = ",", header = T)[, -c(4:19, 24, 33, 34)]
head(dados_acum)

dados_acum <- dados_acum %>% gather(key = "y", value = "prob", -c(sexo, denuncia, votou2010)) 
aux1       <- dados_acum %>% filter(y %in% paste0("CP_", 1:4)) 
aux2       <- dados_acum %>% filter(y %in% paste0("CLCL_", 1:4)) %>% rename(lower = prob) %>% mutate(y = gsub("CLCL", "CP", y))
aux3       <- dados_acum %>% filter(y %in% paste0("CUCL_", 1:4)) %>% rename(upper = prob) %>% mutate(y = gsub("CUCL", "CP", y))
dados_acum <- merge(merge(aux1, aux2), aux3)

dados_acum$y         <- factor(dados_acum$y, labels = c("Ótimo", "Bom\\Ótimo", "Bom\\Ótimo\\Regular", "Bom\\Ótimo\\Regular\\Ruim"))
dados_acum$sexo      <- factor(dados_acum$sexo, labels = c("Masculino", "Feminino"))
dados_acum$denuncia  <- factor(dados_acum$denuncia, levels = c(1, 2), labels = c("Favorável denúncia", "Contra denúncia"))
dados_acum$votou2010 <- factor(dados_acum$votou2010, labels = c("Aécio", "Dilma", "Não votou"))
dados_acum$interacao <- with(dados_acum, paste0(sexo, ",\n ", denuncia, ", \n ", votou2010))
head(dados_acum)

Ry <- range(0, dados_acum$lower, dados_acum$upper)
dados_acum %>% ggplot(aes(x = y, y = prob, color = y)) +
  facet_wrap(~ interacao) + 
  geom_point(size = 1.5, pch = 23) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 0.4) +
  scale_y_continuous(breaks = seq(Ry[1], Ry[2], l = 5), labels = FF(seq(Ry[1], Ry[2], l = 5), 2), limits = Ry) +
  labs(x = "", y = "Probabilidade estimada", color = "") +
  theme(text               = element_text(family = "Palatino", colour = "black", size = 16),
        axis.text.x        = element_blank(), 
        axis.title.x       = element_blank(), 
        axis.ticks.x       = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        legend.position    = "top") -> p2
ggsave(filename = "acum.pdf", plot = p2, device = "pdf", width = 11, height = 7)

