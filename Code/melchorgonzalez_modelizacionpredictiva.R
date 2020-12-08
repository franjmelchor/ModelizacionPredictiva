abs (qt((1-0.95)/2,8))
setwd("C:/Users/franj/Desktop/Repositorios MÁSTER UOC/Est. Avanzada/A3_ModelizacionPredictiva/Code/")

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

##1.2 Descriptiva y visualización
colSums(is.na(house))

factors = unlist(lapply(house, is.factor))
which(factors, arr.ind = TRUE)

levels(house$airport)
levels(house$waterbody)
levels(house$bus_ter)
levels(house$Sold)


par(mfrow=c(2,2))

counts <- table(house$waterbody)
barplot(counts, main="Distribución de tipos de fuente natural de agua dulce 
        que hay en la ciudad", xlab="Número de fuentes por cada categoría", 
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
     col=colorForHistograms[2],main="Proporción de área residencial 
     en la ciudad",cex.main=0.8, cex.lab=0.8)

hist(house$air_qual, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[3],main="Calidad del aire del vecindario"
     ,cex.main=0.8, cex.lab=0.8)

hist(house$room_num, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[4],main="Número medio de habitaciones en casas 
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
     col=colorForHistograms[9],main="Número de maestros en el municipio", 
     cex.main=0.8, cex.lab=0.8)

hist(house$poor_prop, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[10],main="Proporción de población pobre en la ciudad", 
     cex.main=0.8, cex.lab=0.8)

hist(house$n_hos_beds, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[11],main="Número de camas de hospital por habitantes", 
     cex.main=0.8, cex.lab=0.8)

hist(house$n_hot_rooms, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[12],main="Número de habitaciones de hotel por 
     habitantes", cex.main=0.8, cex.lab=0.8)

hist(house$rainfall, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[13],main="Precipitación media anual", 
     cex.main=0.8, cex.lab=0.8)

hist(house$parks, breaks=sqrt(dim(house)[1]),
     col=colorForHistograms[14],main="Proporción de terrenos asignados como 
     parques y áreas verdes en la ciudad", 
     cex.main=0.8, cex.lab=0.8)


