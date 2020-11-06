#1.Loading Data of CPL_Batsmen
Bat_Raw_Data <- read.table("dataminer-CPL_Bat.csv", sep = ",", header = TRUE)
Bat_Raw_Data

#2.Cleaning Data i.e., selecting required columns
Bat_Data_cleaned <- Bat_Raw_Data[,-c(2,3,4,5,7,9,11,12,13)]
Bat_Data_cleaned

Bat_Data <- Bat_Data_cleaned
Bat_Data

#3.Descriptive Statistics

#3.1 finding min
min(Bat_Data$Runs)
min(Bat_Data$Average)
min(Bat_Data$Strike_Rate)
min(Bat_Data$Fours)
min(Bat_Data$Sixes)
min(Bat_Data$HF)

#3.2 finding max
max(Bat_Data$Runs)
max(Bat_Data$Average)
max(Bat_Data$Strike_Rate)
max(Bat_Data$Fours)
max(Bat_Data$Sixes)
max(Bat_Data$HF)

#3.3 finding mean
mean(Bat_Data$Runs)
mean(Bat_Data$Average)
mean(Bat_Data$Strike_Rate)
mean(Bat_Data$Fours)
mean(Bat_Data$Sixes)
mean(Bat_Data$HF)

#3.4 finding median
median(Bat_Data$Runs)
median(Bat_Data$Average)
median(Bat_Data$Strike_Rate)
median(Bat_Data$Fours)
median(Bat_Data$Sixes)
median(Bat_Data$HF)

#3.5 finding variance
var(Bat_Data$Runs)
var(Bat_Data$Average)
var(Bat_Data$Strike_Rate)
var(Bat_Data$Fours)
var(Bat_Data$Sixes)
var(Bat_Data$HF)

#3.6 finding standard deviation
sd(Bat_Data$Runs)
sd(Bat_Data$Average)
sd(Bat_Data$Strike_Rate)
sd(Bat_Data$Fours)
sd(Bat_Data$Sixes)
sd(Bat_Data$HF)

#3.7 finding IQR
IQR(Bat_Data$Runs)
IQR(Bat_Data$Average)
IQR(Bat_Data$Strike_Rate)
IQR(Bat_Data$Fours)
IQR(Bat_Data$Sixes)
IQR(Bat_Data$HF)

#4. Plots

hist(Bat_Data$Runs,xlab = "Runs Scored by batsman",col = "darkmagenta",border = "black")
hist(Bat_Data$Average,xlab = "Avearage of batsman",col = "#18A4D2",border = "black")
hist(Bat_Data$Strike_Rate,xlab = "Strike_Rate batsman",col = "#D24618",border = "black")
hist(Bat_Data$Four,xlab = "Fours Scored by batsman",col = "#18A4D2",border = "black")
hist(Bat_Data$Sixes,xlab = "Sixes Scored by batsman",col = "darkmagenta",border = "black")
hist(Bat_Data$HF,xlab = "Half_Centuries Scored by batsman",col = "#18A4D2",border = "darkmagenta")

#5. MATRIX PLOT b/w VARAIBLES 
pairs(Bat_Data[,2:7], pch=20, col="#FC4E07")
d <- Bat_Data[,-1]
head(d)

#5.1 finding Correlation matrix b/w varaiables
cor(d)

#5.2 finding Covariance Matrix b/w variables
d1 <- cov(d)
head(d1)



#6. finding Principal components by using prcomp() and princomp()

pca <- prcomp(t(d1), scale = TRUE)
pca$x
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var


pc <- princomp(d, cor = TRUE, scores = TRUE)
dim(d)
attributes(pc)
pc$loadings
pc$scores
pc$call
pc$sdev
pc$center
pc$scale
pc$n.obs
str(pc)




#6.1 Plotting Principal Components
plot(pc,col = "#18A4D2")
plot(pc,type="l",col = "#18A4D2")
biplot(pc)

#6.2 Summary of each component:
summary(pc)
#6.3 finding eigen vectors of variables and each component value contributed
d2 <- eigen(d1)$vectors
head(d2)

#7. Calculating Scores of each batsman using First Principal Component (as only 1st component has S.D. > 1) 
pc$scores[,1]
d5 <- Bat_Data
d5$scorre = pc$scores[,1]
d5
d5 <- d5[order(d5$scorre, decreasing = TRUE),]
d5


#7.1 ordering and printing batsmans accor. to their ranks
d5$Ranks <- 89 - rank(d5$scorre)
d5 <- d5[order(d5$Ranks),]
cbind(d5,d5$Ranks)
print(d5)

#7.2 ploting bargraph for points calculated for each player
poin <- c(d5$scorre)
barplot(poin, col = "#18A4D2")









#1.Loading Data of CPL_Bowlers
Bowl_Raw_Data <- read.table("dataminer-CPL_Bowl.csv", sep = ",", header = TRUE)
Bowl_Raw_Data

#2.Cleaning Data i.e., selecting required columns
Bowl_Data_cleaned <- Bowl_Raw_Data[,-c(2,3,4,5,6,7,12,13)]
Bowl_Data_cleaned

Bowl_Data <- Bowl_Data_cleaned
Bowl_Data

#3.Descriptive Statistics

#3.1 finding min
min(Bowl_Data$Wickets)
min(Bowl_Data$Average)
min(Bowl_Data$Strike_Rate)
min(Bowl_Data$Economy)


#3.2 finding max
max(Bowl_Data$Wickets)
max(Bowl_Data$Average)
max(Bowl_Data$Strike_Rate)
max(Bowl_Data$Economy)


#3.3 finding mean
mean(Bowl_Data$Wickets)
mean(Bowl_Data$Average)
mean(Bowl_Data$Strike_Rate)
mean(Bowl_Data$Economy)


#3.4 finding median
median(Bowl_Data$Wickets)
median(Bowl_Data$Average)
median(Bowl_Data$Strike_Rate)
median(Bowl_Data$Economy)

#3.5 finding variance
var(Bowl_Data$Wickets)
var(Bowl_Data$Average)
var(Bowl_Data$Strike_Rate)
var(Bowl_Data$Economy)


#3.6 finding standard deviation
sd(Bowl_Data$Wickets)
sd(Bowl_Data$Average)
sd(Bowl_Data$Strike_Rate)
sd(Bowl_Data$Economy)


#3.7 finding IQR
IQR(Bowl_Data$Wickets)
IQR(Bowl_Data$Average)
IQR(Bowl_Data$Strike_Rate)
IQR(Bowl_Data$Economy)


#4. Plots

hist(Bowl_Data$Wickets,xlab = "Wickets taken by bowler",col = "darkmagenta",border = "black")
hist(Bowl_Data$Average,xlab = "Avearage of Bowler",col = "#18A4D2",border = "black")
hist(Bowl_Data$Strike_Rate,xlab = "Strike_Rate of Bowler",col = "#18A4D2",border = "black")
hist(Bowl_Data$Economy,xlab = "Economy of Bowler",col = "darkmagenta",border = "black")


#5. MATRIX PLOT b/w VARAIBLES 
pairs(Bowl_Data[,2:5], pch=20, col="#FC4E07")
d_bowl <- Bowl_Data[,-1]
head(d_bowl)

#5.1 finding Correlation matrix b/w varaiables
cor(d_bowl)

#5.2 finding Covariance Matrix b/w variables
d1_bowl <- cov(d_bowl)
head(d1_bowl)



#6. finding Principal components by using prcomp() and princomp()

pca1 <- prcomp(t(d1_bowl), scale = TRUE)
pca1$x
plot(pca1$x[,1], pca1$x[,2])
pca1.var <- pca1$sdev^2
pca1.var


pc1 <- princomp(d_bowl, cor = TRUE, scores = TRUE)
dim(d_bowl)
attributes(pc1)
pc1$loadings
pc1$scores
pc1$call
pc1$sdev
pc1$center
pc1$scale
pc1$n.obs
str(pc1)




#6.1 Plotting Principal Components
plot(pc1,col = "#18A4D2")
plot(pc1,type="l",col = "#18A4D2",border = "black")
biplot(pc1)

#6.2 Summary of each component:
summary(pc1)
#6.3 finding eigen vectors of variables and each component value contributed
d2_bowl <- eigen(d1_bowl)$vectors
head(d2_bowl)

#7. Calculating Scores of each bowler using First Principal Component (as only 1st component has S.D. > 1) 
pc1$scores[,1]
d5_bowl <- Bowl_Data
d5_bowl$scorre = pc1$scores[,1]
d5_bowl
d5_bowl <- d5_bowl[order(d5_bowl$scorre, decreasing = TRUE),]
d5_bowl


#7.1 ordering and printing bowlers accor. to their ranks
d5_bowl$Ranks <- 57 - rank(d5_bowl$scorre)
d5_bowl <- d5_bowl[order(d5_bowl$Ranks),]
cbind(d5_bowl,d5_bowl$Ranks)
print(d5_bowl)

#7.2 ploting bargraph for points calculated for each player
poin_bowl <- c(d5_bowl$scorre)
barplot(poin_bowl, col = "#18A4D2")

