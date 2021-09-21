install.packages("geospacom")

#Additional packages
library(geospacom)

sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
N <- nrow(spatial_data)
centroids <- coordinates(spatial_data)
plot(spatial_data)
points(centroids, pch = 16, col = sgh_green)

#Way 1: Neighbourhood matrix with row normalisation
cont1 <- poly2nb(spatial_data, queen = T)
W1_list <- nb2listw(cont1, style = "W")
W1 <- listw2mat(W1_list)
plot.nb(cont1, centroids, col = sgh_green, pch = 16)
W1_list$weights
summary(rowSums(W1))

#Way 2: Neighbourhood matrix with row normalisation - 2-nd order neighbourhood relationship included (neighbour of my neighbour is my neighbour)
cont2 <- nblag(cont1, 2)
cont2acum <- nblag_cumul(cont2)
W2 <- nb2mat(cont2acum)
plot.nb(cont2acum, centroids, col = sgh_green, pch = 16)
W2_list <- mat2listw(W2, style = "W")
summary(rowSums(W2))

#Way 3: Neighbourhood matrix with row normalisation
cont3 <- poly2nb(spatial_data, queen = T)
W3_list <- nb2listw(cont3, style = "S")
W3 <- listw2mat(W3_list)
plot.nb(cont3, centroids, col = sgh_green, pch = 16)
W3_list$weights
summary(rowSums(W3))

#Way 4: Matrix of inverted distance
distance <- DistanceMatrix(spatial_data, "NUTS_ID_char", unit = 1000)
for (i in 1:nrow(distance)) {  # waga = 0 dla jednostek znajdyjących się dalej niż 200 km
  for (j in 1:ncol(distance)) {
    if (distance[i,j] > 200) {
      distance[i,j] <- 0
    }
  }
}
gamma <- 2
W4 <- 1 / (distance ^ gamma)

for (i in 1:nrow(W4)) {  # usunięcie Inf
  for (j in 1:ncol(W4)) {
    if (W4[i,j] == Inf) {
      W4[i,j] <- 0
    }
  }
}
N <- nrow(spatial_data)
W4 <- W4 / as.matrix(rowSums(W4)) %*% matrix(1, nrow = 1, ncol = N)  # normalizacja
summary(rowSums(W4))
W4_list <- mat2listw(W4, style="W")

#Way 5: neighbours in a predefined distance [km]
cont5 <- dnearneigh(centroids, 0.01, 76, row.names = spatial_data@data$kod, longlat = TRUE)
W5_list <- nb2listw(cont5, style = "W")
W5 <- listw2mat(W5_list)
summary(rowSums(W5))

#Way 6: k nearest neighbours
k_neigh_matrix <- knearneigh(centroids, k = 5, longlat = TRUE)
cont6 <- knn2nb(k_neigh_matrix)
W6_list <- nb2listw(cont6, style = "W")
W6 <- listw2mat(W6_list)
summary(rowSums(W6))

#Way 7:
# wydzielenie zmiennych z obiektu spatial_data
unemployment <- as.data.frame(spatial_data$unemployment, row.names = as.vector(spatial_data$NUTS_ID_char))
foreigners_pop <- as.data.frame(spatial_data$foreigners_pop, row.names = as.vector(spatial_data$NUTS_ID_char))
death_rate <- as.data.frame(spatial_data$death_rate, row.names = as.vector(spatial_data$NUTS_ID_char))

names(unemployment) <- c("unemployment")
names(foreigners_pop) <- c("foreigners_pop")
names(death_rate) <- c("death_rate")

# skalowanie 
unemployment_st <- lapply(unemployment, FUN = scale)
foreigners_pop_st <- lapply(foreigners_pop, FUN = scale)
death_rate_st <- lapply(death_rate, FUN = scale)

# ramka ze zmniennymi standaryzowanymi
zmienne_st_df <- as.data.frame(cbind(unemployment_st[[1]], foreigners_pop_st[[1]], death_rate_st[[1]]), row.names = as.vector(spatial_data$NUTS_ID_char))
names(zmienne_st_df) <- c("unemployment_st", "foreigners_pop_st", "death_rate_st")

# Wydaje mi się, że należy użyć odwrotności odległości euklidesowej, ponieważ rośnie ona kiedy regiony różnią się pod względem tych zmiennych, a maleje kiedy są podobne.

distance_3 <- DistanceMatrix(spatial_data, "NUTS_ID_char", unit = 1000) # macierz z nagłówkami

for (i in 1:nrow(distance_3)) {  
  for (j in 1:ncol(distance_3)) {
    distance_3[i,j] <- 1/sqrt((zmienne_st_df[i,1] - zmienne_st_df[j,1])^2 + (zmienne_st_df[i,2] - zmienne_st_df[j,2])^2 + (zmienne_st_df[i,3] - zmienne_st_df[j,3])^2)
    if (distance_3[i,j] == Inf) {
      distance_3[i,j] <- 0
    }
  }
}

W7_list <- mat2listw(distance_3, style="W")
W7 <- listw2mat(W7_list)
summary(rowSums(W7))

###
sgh_green <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
###Global moran test
#W1
moran.test(spatial_data$accident_per_1000, listw = W1_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W1_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#W2
moran.test(spatial_data$accident_per_1000, listw = W2_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W2_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#W3
moran.test(spatial_data$accident_per_1000, listw = W3_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W3_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#W4
moran.test(spatial_data$accident_per_1000, listw = W4_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W4_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#W5
moran.test(spatial_data$accident_per_1000, listw = W5_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W5_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#W6
moran.test(spatial_data$accident_per_1000, listw = W6_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W6_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

#W7
moran.test(spatial_data$accident_per_1000, listw = W7_list, alternative = "greater")
moran.plot(spatial_data$accident_per_1000, listw = W7_list, ylab="", xlab="", pch = 20, main = "Wykres Morana", col = sgh_green)

###Lokalne testy Morana
#W1
localmoran(spatial_data$accident_per_1000, W1_list, p.adjust.method = "bonferroni")

#W2
localmoran(spatial_data$accident_per_1000, W2_list, p.adjust.method = "bonferroni")

#W3
localmoran(spatial_data$accident_per_1000, W3_list, p.adjust.method = "bonferroni")

#W4
localmoran(spatial_data$accident_per_1000, W4_list, p.adjust.method = "bonferroni")

#W5
localmoran(spatial_data$accident_per_1000, W5_list, p.adjust.method = "bonferroni")

#W6
localmoran(spatial_data$accident_per_1000, W6_list, p.adjust.method = "bonferroni")

#W7
localmoran(spatial_data$accident_per_1000, W7_list, p.adjust.method = "bonferroni")


###Test Gearyego

#W1
geary.test(spatial_data$accident_per_1000, W1_list)

#W2
geary.test(spatial_data$accident_per_1000, W2_list)

#W3
geary.test(spatial_data$accident_per_1000, W3_list)

#W4
geary.test(spatial_data$accident_per_1000, W4_list)

#W5
geary.test(spatial_data$accident_per_1000, W5_list)

#W6
geary.test(spatial_data$accident_per_1000, W6_list)

#W7
geary.test(spatial_data$accident_per_1000, W7_list)

###Test joint-count
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W1_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W2_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W3_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W4_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W5_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W6_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1), listw = W7_list)

joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W1_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W2_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W3_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W4_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W5_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W6_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.2), listw = W7_list)

joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W1_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W2_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W3_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W4_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W5_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W6_list)
joincount.test(as.factor(spatial_data$accident_per_1000 > 1.5), listw = W7_list)


model_liniowy <- lm(spatial_data$accident_per_1000 ~ spatial_data$unemploymentap + spatial_data$age_dep_ratio)
res_lin <- model_liniowy$residuals

moran.plot(res_lin, W3_list, ylab="Opóźnienie przestrzenne reszt", xlab="Reszty", pch = 20, main = "Wykres Morana", col = sgh_green)
