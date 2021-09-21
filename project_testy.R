#LINEAR MODEL ESTIMATED VIA OLS
model_liniowy <- lm(spatial_data$accident_per_1000 ~ spatial_data$car_per_pop + spatial_data$motocicle_per_pop +
                      spatial_data$young_per+ spatial_data$density)
summary(model_liniowy)
lm.morantest(model_liniowy, listw = W6_list)
lm.LMtests(model_liniowy, listw = W6_list, test = "all")

#GRAPHICAL EVALUATION
res <- model_liniowy $residuals
colors <- brewer.pal(6, "Blues") 
brks <- classIntervals(res, 6 )
brks <- brks$brks
plot(spatial_data, col = colors[findInterval(res, brks,all.inside = TRUE)], axes = F)


#GLOBAL MORAN'S TEST FOR RESIDUALS
cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W1_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W2_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W3_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W4_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W5_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W6_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

cont1 <- poly2nb(spatial_data, queen = T)
lm.morantest(model_liniowy , W7_list, alternative = "greater")
moran.plot(res, W1_list, ylab = "Spatial lag of residuals: W*e", xlab = "Residuals: e", pch = 20, main = "Moran's plot", col = sgh_green)

#The same vor a variable (not necessarily for residuals)
moran(res, W1_list, length(W1_list$neighbours), length(W1_list$neighbours)) #Only I statistic and kurtosis of residuals
moran.test(res, W1_list) #Test statistic with p-value

#Geary's C test
geary.test(res, W1_list)
geary.test(res, W2_list)
geary.test(res, W3_list)
geary.test(res, W4_list)
geary.test(res, W5_list)
geary.test(res, W6_list)
geary.test(res, W7_list)
#Local Moran's tests

localmoran(res, W1_list, p.adjust.method = "bonferroni")
localmoran(res, W2_list, p.adjust.method = "bonferroni")
localmoran(res, W3_list, p.adjust.method = "bonferroni")
localmoran(res, W4_list, p.adjust.method = "bonferroni")
localmoran(res, W5_list, p.adjust.method = "bonferroni")
localmoran(res, W6_list, p.adjust.method = "bonferroni")
localmoran(res, W7_list, p.adjust.method = "bonferroni")

#Join count tests
joincount.test(as.factor(res > 0.1), listw = W1_list)
joincount.test(as.factor(res > 0.1), listw = W2_list)
joincount.test(as.factor(res > 0.1), listw = W3_list)
joincount.test(as.factor(res > 0.1), listw = W4_list)
joincount.test(as.factor(res > 0.1), listw = W5_list)
joincount.test(as.factor(res > 0.1), listw = W6_list)
joincount.test(as.factor(res > -0.1), listw = W7_list)


#Tests with specified alternative hypotheses
lm.LMtests(model_liniowy , listw = W1_list, test = "all")
