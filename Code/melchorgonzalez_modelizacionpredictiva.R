abs (qt((1-0.95)/2,8))
setwd("C:/Users/franj/Desktop/Repositorios M?STER UOC/Est. Avanzada/A3_ModelizacionPredictiva/Code/")
setwd("/home/fran/Escritorio/M??ster UOC/Est. Avanzada/ModelizacionPredictiva/Code")
#1. Datos y Estad?stica descriptiva

##1.1 Lectura de datos
house_filepath <- "../Data/house.csv"
house <- read.csv(file=house_filepath, header=TRUE, sep=";", na.strings=c(""," ","NA"))
head(house)
str(house)

house$airport <- as.factor(house$airport)
house$waterbody <- as.factor(house$waterbody)
house$bus_ter <- as.factor(house$bus_ter)
house$Sold <- as.factor(house$Sold)

str(house)

if(!require(DataCombine)){
    install.packages("DataCombine")
    library(DataCombine)
}
##1.2 Descriptiva y visualizaci?n
colSums(is.na(house))
house = DropNA(house)

factors = unlist(lapply(house, is.factor))
which(factors, arr.ind = TRUE)

levels(house$airport)
levels(house$waterbody)
levels(house$bus_ter)
levels(house$Sold)


par(mfrow=c(2,2))

counts <- table(house$waterbody)
barplot(counts, main="Distribuci?n de tipos de fuente natural de agua dulce 
        que hay en la ciudad", xlab="N?mero de fuentes por cada categor?a", 
        col = rainbow (length(levels(house$waterbody))))

colorForPieCharts = rainbow(length(levels(house$airport)) + 
                              length(levels(house$bus_ter)) + 
                              length(levels(house$Sold)))

levels(house$airport)

mytableAirport <- table(house$airport)
pctAirport <- round(mytableAirport/sum(mytableAirport)*100)
lblsAirport <- paste(names(mytableAirport), "\n", pctAirport, sep="")
lblsAirport <- paste (lblsAirport, '%', sep="")
pie(mytableAirport, labels = lblsAirport, col=colorForPieCharts[1:2],
    main="Pie Chart of Airport\n")


levels(house$bus_ter)

mytableBus_ter <- table(house$bus_ter)
pctBus_ter <- round(mytableBus_ter/sum(mytableBus_ter)*100)
lblsBus_ter <- paste(names(mytableBus_ter), "\n", pctBus_ter, sep="")
lblsBus_ter <- paste (lblsBus_ter, '%', sep="")
pie(mytableBus_ter, labels = lblsBus_ter, col=colorForPieCharts[3:3],
    main="Pie Chart of bus_ter\n")

levels(house$Sold)

mytableSold <- table(house$Sold)
pctSold <- round(mytableSold/sum(mytableSold)*100)
lblsSold <- paste(names(mytableSold), "\n", pctSold, sep="")
lblsSold <- paste (lblsSold, '%', sep="")
pie(mytableSold, labels = lblsSold, col=colorForPieCharts[4:5],
    main="Pie Chart of Sold\n")

str(house)

numeric = unlist(lapply(house, is.numeric))
which(numeric, arr.ind = TRUE)


length(which(numeric, arr.ind = TRUE))

colorForHistograms = rainbow(length(which(numeric, arr.ind = TRUE)))

par(mfrow=c(7,2),mar=c(2,1,3,1))

hist(house$price, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[1],main="Precio de venta por parte del propietario",cex.main=0.8, cex.lab=0.8)

hist(house$resid_area, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[2],main="Proporci?n de ?rea residencial 
     en la ciudad",cex.main=0.8, cex.lab=0.8)

hist(house$air_qual, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[3],main="Calidad del aire del vecindario"
     ,cex.main=0.8, cex.lab=0.8)

hist(house$room_num, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[4],main="N?mero medio de habitaciones en casas 
     de esa localidad", cex.main=0.8, cex.lab=0.8)

hist(house$dist1, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[5],main="Distancia al centro de empleo 1",
     cex.main=0.8, cex.lab=0.8)

hist(house$dist2, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[6],main="Distancia al centro de empleo 2",
     cex.main=0.8, cex.lab=0.8)

hist(house$dist3, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[7],main="Distancia al centro de empleo 3", 
     cex.main=0.8, cex.lab=0.8)

hist(house$dist4, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[8],main="Distancia al centro de empleo 4", 
     cex.main=0.8, cex.lab=0.8)

hist(house$teachers, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[9],main="N?mero de maestros en el municipio", 
     cex.main=0.8, cex.lab=0.8)

hist(house$poor_prop, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[10],main="Proporci?n de poblaci?n pobre en la ciudad", 
     cex.main=0.8, cex.lab=0.8)

hist(house$n_hos_beds, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[11],main="N?mero de camas de hospital por habitantes", 
     cex.main=0.8, cex.lab=0.8)

hist(house$n_hot_rooms, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[12],main="N?mero de habitaciones de hotel por 
     habitantes", cex.main=0.8, cex.lab=0.8)

hist(house$rainfall, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[13],main="Precipitaci?n media anual", 
     cex.main=0.8, cex.lab=0.8)

hist(house$parks, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[14],main="Proporci?n de terrenos asignados como 
     parques y ?reas verdes en la ciudad", 
     cex.main=0.8, cex.lab=0.8)

# 2. Modelo de regresi?n lineal

## 2.1 Modelo de RLS
### 2.1.1 Calcular 
# Estimar por m?nimos cuadrados ordinarios dos modelos lineales que expliquen la variable price, uno en 
# funci?n de la variable teachers y otro en funci?n de la variable poor_prop.

get_cov_muestral<- function(x,y){
    mean_x = mean(x)
    mean_y = mean(y)
    sum = 0
    for (i in 1:length(x)){
        sum = sum + ((x[i] - mean_x)*(y[i] - mean_y))
    }
    return (sum/(length(x) - 1))
}

get_var_muestral <- function(x){
    mean_x = mean(x)
    sum = 0
    for (i in 1:length(x)){
        sum = sum + ((x[i]-mean_x)^2)
    }
    return (sum/(length(x) - 1))
}

get_b1 <- function(x,y){
    Sxy = get_cov_muestral(x,y)
    S2x = get_var_muestral(x)
    
    return (Sxy/S2x)
}

get_b0 <- function(x,y){
    mean_y = mean(y)
    b1 = get_b1(x,y)
    mean_x = mean(x)
    
    return(mean_y - (b1*mean_x))
}

x_pr = c(161,152,167,153,161,168,167,153,159,173)
y_pr = c(63,56,77,49,72,62,68,48,57,67)
sum(x_pr)
sum(y_pr)

get_var_muestral(x_pr)
get_cov_muestral(x_pr,y_pr)

get_b1(x_pr,y_pr)
get_b0(x_pr,y_pr)

get_cov_muestral(house$price,house$teachers)
get_cov_muestral(house$price,house$poor_prop)

get_b0(house$price,house$teachers)
get_b1(house$price,house$teachers)

get_b0(house$price,house$poor_prop)
get_b1(house$price,house$poor_prop)


##2.1.3
plot(house$price, house$teachers, main="Gráfico de dispersión XY",
     ylab="Profesores por mil habitantes", xlab="Precio", pch=19)

plot(house$price, house$poor_prop, main="Gráfico de dispersión XY",
     ylab="Proporción de población pobre", xlab="Precio", pch=19)



##2.2 Modelo de regresión lineal múltiple (regresores cuantitativos)
###2.2.1. Calcular
get_b <- function(X,y){
    first_term = solve (t(X) %*% X)
    sec_term = t(X) %*% y
    return (first_term %*% sec_term)
}

X = cbind(rep(1,length(house$price)),house$age,house$teachers,house$poor_prop)
y = house$price
B = get_b(X,y)
B
B0 = B[1]
B1 = B[2]
B2 = B[3]
B3 = B[4]
lm(y ~ X)

rlm_1 <- function(age,teachers,poor_prop){
    return (B0 + B1*age + B2*teachers + B3*poor_prop)
}

###2.2.3. Evaluar la bondad de ajuste a través del coeficiente de determinación ajustado

predict_rlm_1 <- function(age,teachers,poor_prop){
    y <- c()
    for (i in 1:length(age)){
        y[i] <- rlm_1(age[i],teachers[i],poor_prop[i])
    }
    return(y)
}
get_sct <- function(y){
    D = y - mean(y)
    return (t(D) %*% D)
}

get_scr <- function(y, y_predict){
    W = y_predict - mean(y)
    return (t(W) %*% W)
}

get_r_square <- function (y, y_predict){
    return (get_scr(y, y_predict) / get_sct(y))
}

y_predict_rlm1 = predict_rlm_1(house$age,house$teachers,house$poor_prop)
get_r_square(y, y_predict_rlm1)


X = cbind(rep(1,length(house$price)),house$age,house$teachers,house$poor_prop, house$room_num, house$n_hos_beds, 
          house$n_hot_rooms)
B = get_b(X,y)
B
B0 = B[1]
B1 = B[2]
B2 = B[3]
B3 = B[4]
B4 = B[5]
B5 = B[6]
B6 = B[7]

rlm_2 <- function(age,teachers,poor_prop,room_num,n_hos_beds,n_hot_rooms){
    return (B0 + B1*age + B2*teachers + B3*poor_prop + B4*room_num + B5*n_hos_beds + B6*n_hot_rooms)
}
predict_rlm_2 <- function(age,teachers,poor_prop,room_num,n_hos_beds,n_hot_rooms){
    y <- c()
    for (i in 1:length(age)){
        y[i] <- rlm_2(age[i],teachers[i],poor_prop[i],room_num[i],n_hos_beds[i],n_hot_rooms[i])
    }
    return(y)
}

y_predict_rlm2 = predict_rlm_2(house$age,house$teachers,house$poor_prop,house$room_num,house$n_hos_beds,house$n_hot_rooms)
get_r_square(y, y_predict_rlm2)

##2.3. Modelo de regresión lineal múltiple (regresores cuantitativos y cualitativos)
head(house$airport)
tail(house$airport)
levels(house$airport)[levels(house$airport)=="NO"] <- 0
levels(house$airport)[levels(house$airport)=="YES"] <- 1
head(house$airport)
tail(house$airport)
house$airport <- as.integer(as.character(house$airport))
head(house$airport)
tail(house$airport)


X = cbind(rep(1,length(house$price)),house$age,house$teachers,house$poor_prop, house$room_num, house$n_hos_beds, 
          house$n_hot_rooms,house$airport)
B = get_b(X,y)
B
B0 = B[1]
B1 = B[2]
B2 = B[3]
B3 = B[4]
B4 = B[5]
B5 = B[6]
B6 = B[7]
B7 = B[8]

rlm_3 <- function(age,teachers,poor_prop,room_num,n_hos_beds,n_hot_rooms,airport){
    return (B0 + B1*age + B2*teachers + B3*poor_prop + B4*room_num + B5*n_hos_beds + B6*n_hot_rooms + B7*airport)
}
predict_rlm_3 <- function(age,teachers,poor_prop,room_num,n_hos_beds,n_hot_rooms,airport){
    y <- c()
    for (i in 1:length(age)){
        y[i] <- rlm_3(age[i],teachers[i],poor_prop[i],room_num[i],n_hos_beds[i],n_hot_rooms[i],airport[i])
    }
    return(y)
}

y_predict_rlm3 = predict_rlm_3(house$age,house$teachers,house$poor_prop,house$room_num,
                               house$n_hos_beds,house$n_hot_rooms,house$airport)
y_predict_rlm3
get_r_square(y, y_predict_rlm3)


###2.3.3. Efectuar una predicción del precio de la vivienda.
predict_rlm_2(70,15,15,8,8,100)

###2.3.4. Efectuar una verificación visual de las suposiciones de modelización.
residuos_rlm3 = y - y_predict_rlm3

plot(y_predict_rlm3, residuos_rlm3, main="Gráfico de valores residuales",
     xlab="Valores estimados", ylab="Residuos", pch=19)

# 3. Modelo de regresión logística
head(house$Sold)
tail(house$Sold)
levels(house$Sold)[levels(house$Sold)=="0"] <- "Not"
levels(house$Sold)[levels(house$Sold)=="1"] <- "Yes"
head(house$Sold)
tail(house$Sold)

## 3.1. Regresores cuantitativos
###3.1.1. Calcular
glm_1 <- glm(formula = house$Sold ~ house$price + house$age + house$poor_prop, 
             family = binomial (link=logit))
summary(glm_1)

# el incremento en una unidad del precio de una vivienda disminuye un 14'88% la probabilidad de venta de la misma
# el incremento por cada año de antiguedad de la vivienda aumenta en un 0'96% la probabilidad de venta de la misma
# el incremento de la proporción de población pobre en el lugar donde está la vivienda, 
# disminuye en un 18'6% la probabilidad de venta de la misma

###3.1.2. Interpretar
exp(confint(glm_1))
# por cada unidad que aumente el precio de la vivienda, el odds de vender la vivienda es entre 0,83 y 0,9 veces menor
# por cada unidad que aumente la edad de la vivienda, el odds de vender la vivienda es entre 1.00 y 1.01 veces mayor
# por cada unidad que aumente la proporción de población pobre en la ubicación donde se encuentra la vivienda, 
# el odds de vender la vivienda es entre 0.78 y 0.87 veces mayor

exp(coefficients(glm_1))
# Si la edad aumenta en 5 unidades, el odds será 1.009^5 = 1.045 veces mayor

## 3.2. Regresores cualitativos
### 3.2.1. Calcular
head(house$airport)
glm_2 <-glm(formula = house$Sold ~ house$airport, family = binomial (link=logit))
summary(glm_2)
glm_2

### 3.2.2. Interpretar
exp(confint(glm_2))
# la venta de una vivienda en un lugar donde haya un aeropuerto es entre 1.49 y 3.09 veces más probable 
# que en un lugar donde no lo haya

## 3.3. Regresores cuantitativos y cualitativos
glm_3 <- glm(formula = house$Sold ~ house$price + house$age + house$poor_prop + house$airport, 
             family = binomial (link=logit))
summary(glm_3)
glm_3

### 3.3.1. Interpretar
exp(confint(glm_3))
# Juntamos las conclusiones de los apartados 3.2.2 y 3.1.2
# ¿Qué regresor tiene más impacto en la probabilidad de venta? -> la existencia de un aeropuerto cercano a la vivienda

### 3.3.2. Predicción de venta
dim(house)
data.frame(price=20,age=50,poor_prop=50,airport=1) 
house$price[1]
house$age[1]
house$poor_prop[1]
house$airport[1]




predict(glm_3, data.frame(price=20,age=50,poor_prop=50,airport=1), interval = "confidence", level = 0.95)




