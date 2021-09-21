##Model czystej autoregresji przestrzennej
model_pure_sar <- spautolm(spatial_data$accident_per_1000 ~ 1, listw = W1_list)
summary(model_pure_sar)
res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W1_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_pure_sar, brks,all.inside = TRUE)])
title(paste ("pure SAR"))

model_pure_sar <- spautolm(spatial_data$accident_per_1000 ~ 1, listw = W4_list)
summary(model_pure_sar)
res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W4_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_pure_sar, brks,all.inside = TRUE)])
title(paste ("pure SAR"))

model_pure_sar <- spautolm(spatial_data$accident_per_1000 ~ 1, listw = W6_list)
summary(model_pure_sar)
res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W6_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_pure_sar, brks,all.inside = TRUE)])
title(paste ("pure SAR"))

#GRAPHICAL EVALUATION
colors <- brewer.pal(6, "BuGn") 
brks <- classIntervals(res, n = 6, style = "quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_pure_sar, brks,all.inside = TRUE)])
title(paste ("W6"))

model_pure_sar <- spautolm(spatial_data$accident_per_1000 ~ 1, listw = W4_list)
summary(model_pure_sar)
res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W4_list)

model_pure_sar <- spautolm(spatial_data$accident_per_1000 ~ 1, listw = W6_list)
summary(model_pure_sar)
res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W6_list)

##
res_pure_sar <- model_pure_sar$fit$residuals
moran.test(res_pure_sar, W4_list)

##Model SAR]
#W1
model_sar <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W1_list)
summary(model_sar)
res_sar <- model_sar$residuals
moran.test(res_sar, listw = W1_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sar, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sar, brks,all.inside = TRUE)], axes = F)
title(paste ("SAR"))
lm.LMtests(res_sar, listw = W1_list, test = "LMerr")

#W4
model_sar <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W4_list)
summary(model_sar)
res_sar <- model_sar$residuals
moran.test(res_sar, listw = W4_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sar, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sar, brks,all.inside = TRUE)], axes = F)
title(paste ("SAR"))
#W6
model_sar <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W6_list)
summary(model_sar)
res_sar <- model_sar$residuals
moran.test(res_sar, listw = W6_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sar, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sar, brks,all.inside = TRUE)], axes = F)
title(paste ("SAR"))


###Model SLX
#W1
W <- listw2mat(W1_list)
X <- cbind(spatial_data$car_per_pop,spatial_data$motocicle_per_pop,spatial_data$young_per,spatial_data$density)
WX <- W %*% X
lag.car_per_pop <- WX [, 1]
lag.motocicle_per_pop <- WX [, 2]
lag.young_per <- WX [, 3]
lag.density <- WX [, 4]
model_slx <- lm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop+
                  spatial_data$young_per
                + spatial_data$density+lag.car_per_pop+lag.motocicle_per_pop+lag.young_per+lag.density)

summary(model_slx)
print(paste("AIC:",as.character(AIC(model_slx))))
lm.morantest(model_slx, listw = W1_list)
res_slx <- model_slx$residuals
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_slx, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_slx, brks,all.inside = TRUE)], axes = F)
title(paste ("SLX"))

#W4
W <- listw2mat(W4_list)
X <- cbind(spatial_data$car_per_pop,spatial_data$motocicle_per_pop,spatial_data$young_per,spatial_data$density)
WX <- W %*% X
lag.car_per_pop <- WX [, 1]
lag.motocicle_per_pop <- WX [, 2]
lag.young_per <- WX [, 3]
lag.density <- WX [, 4]
model_slx <- lm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop+
                  spatial_data$young_per
                + spatial_data$density+lag.car_per_pop+lag.motocicle_per_pop+lag.young_per+lag.density)

summary(model_slx)
print(paste("AIC:",as.character(AIC(model_slx))))
lm.morantest(model_slx, listw = W4_list)
res_slx <- model_slx$residuals
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_slx, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_slx, brks,all.inside = TRUE)], axes = F)
title(paste ("SLX"))
#W6
W <- listw2mat(W6_list)
X <- cbind(spatial_data$car_per_pop,spatial_data$motocicle_per_pop,spatial_data$young_per,spatial_data$density)
WX <- W %*% X
lag.car_per_pop <- WX [, 1]
lag.motocicle_per_pop <- WX [, 2]
lag.young_per <- WX [, 3]
lag.density <- WX [, 4]
model_slx <- lm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop+
                  spatial_data$young_per
                + spatial_data$density+lag.car_per_pop+lag.motocicle_per_pop+lag.young_per+lag.density)

summary(model_slx)
print(paste("AIC:",as.character(AIC(model_slx))))
lm.morantest(model_slx, listw = W6_list)
res_slx <- model_slx$residuals
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_slx, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_slx, brks,all.inside = TRUE)], axes = F)
title(paste ("SLX"))
###Model SEM
#W1
model_sem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W1_list)
summary(model_sem)
res_sem <- model_sem$residuals
moran.test(res_sem, listw = W1_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SEM"))

#W4
model_sem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W4_list)
summary(model_sem)
res_sem <- model_sem$residuals
moran.test(res_sem, listw = W4_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SEM"))

#W6
model_sem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W6_list)
summary(model_sem)
res_sem <- model_sem$residuals
moran.test(res_sem, listw = W6_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SEM"))

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#W1
model_sar <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W1_list)
model_sem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W1_list)
model_sdm <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W1_list, type = "Durbin")
model_sdem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                           spatial_data$young_per+ spatial_data$density, etype = "emixed", listw = W1_list)
lL0 <- logLik(model_sar)
lL1 <- logLik(model_sem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))


lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sdm)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

#W4
model_sar <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W4_list)
model_sem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W4_list)
model_sdm <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W4_list, type = "Durbin")
model_sdem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                           spatial_data$young_per+ spatial_data$density, etype = "emixed", listw = W4_list)
lL0 <- logLik(model_sar)
lL1 <- logLik(model_sem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))


lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sdm)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))
#W3
model_sar <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W6_list)
model_sem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W6_list)
model_sdm <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W6_list, type = "Durbin")
model_sdem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                           spatial_data$young_per+ spatial_data$density, etype = "emixed", listw = W6_list)
lL0 <- logLik(model_sar)
lL1 <- logLik(model_sem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))


lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sar)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdm)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sem)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))

lL0 <- logLik(model_sdm)
lL1 <- logLik(model_sdem)
LRa <- 2 * (lL1 - lL0)
print(LR.p.value <- pchisq(as.numeric(LRa), df = 2, lower.tail = FALSE))



###SARAR
#W1
model_sarar <- sacsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W1_list)
summary(model_sarar)
res_sarar <- model_sarar$residuals
moran.test(res_sarar, listw = W1_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SARAR"))
#W4
model_sarar <- sacsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W4_list)
summary(model_sarar)
res_sarar <- model_sarar$residuals
moran.test(res_sarar, listw = W4_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SARAR"))
#W6
model_sarar <- sacsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                          spatial_data$young_per+ spatial_data$density, listw = W6_list)
summary(model_sarar)
res_sarar <- model_sarar$residuals
moran.test(res_sarar, listw = W6_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SARAR"))
#SDM
#W1
model_sdm <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W1_list, type = "Durbin")
summary(model_sdm)
res_sdm <- model_sdm$residuals
moran.test(res_sdm, listw = W1_list)

colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SDM"))
#W4
model_sdm <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W4_list, type = "Durbin")
summary(model_sdm)
res_sdm <- model_sdm$residuals
moran.test(res_sdm, listw = W4_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SDM"))

#W6
model_sdm <- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W6_list, type = "Durbin")
summary(model_sdm)
res_sdm <- model_sdm$residuals
moran.test(res_sdm, listw = W6_list)

colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SDM"))

##
#W1
model_sdem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                           spatial_data$young_per+ spatial_data$density, etype = "emixed", listw = W1_list)
summary(model_sdem)
res_sdem <- model_sdem$residuals
moran.test(res_sdem, listw = W1_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SDEM"))
#W4
model_sdem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                           spatial_data$young_per+ spatial_data$density, etype = "emixed", listw = W4_list)
summary(model_sdem)
res_sdem <- model_sdem$residuals
moran.test(res_sdem, listw = W4_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SDEM"))
#W6
model_sdem <- errorsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                           spatial_data$young_per+ spatial_data$density, etype = "emixed", listw = W6_list)
summary(model_sdem)
res_sdem <- model_sdem$residuals
moran.test(res_sdem, listw = W6_list)
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res_sem, n=6, style="quantile")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res_sem, brks,all.inside = TRUE)], axes = F)
title(paste ("SDEM"))

model_sdm<- lagsarlm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                        spatial_data$young_per+ spatial_data$density, listw = W4_list, type = "Durbin")
summary(model_sdm)
#Efekty srednie, posrednie i calkowite
impacts.SDM <- impacts(SDM, listw = W4_list, zstats = TRUE, R = 200)
HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "direct")
HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "indirect")
HPDinterval.lagImpact(impacts.SDM, prob = 0.95, choice = "total")
summary(impacts.SDM)


###
W <- listw2mat(W4_list)
colnames(W) <- row.names(W)
N <- dim(W)[1]
rho_sdm <- model_sdm$rho
M_sdm <- solve(diag(N) - rho_sdm  * W)%*%(model_sdm$coefficients[5]*diag(N) 
                                          + model_sdm$coefficients[9]*W)%*%matrix(c(0,0,0,0,0,0,0,0,100, rep(0,N-9)),N,1)
# Wzrost PKB per capita o jednostkę (tysiąc Euro) w regionie ITC18

colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(M_sdm, n=6, style="jenks")
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(M_sdm, brks,all.inside = TRUE)], axes = F)
title(main = paste ("Wzrost wzrost gestosci zaludnienia o 100 os w regionie ITC18 (Alessandria)"), sub = paste("Model SDM"))
legend(x = "right", y = NULL, legend = leglabs(round(brks, 4), under = "poniżej", over = "powyżej"), fill = colors, bty = "n", x.intersp = .5, y.intersp = 1)

