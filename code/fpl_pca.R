rm(list=ls())

#Verinin hazirlanip duzenlenmesi:

fpl <- read.csv("C:/Users/User/Desktop/FPL_Cluster/data/FPL.csv",header=TRUE, sep=",", quote="", dec =".")
fpl
colnames(fpl)=c("Name", "Team" ,"Position" ,"Cost","Creativity","Influence","Threat","Goals_conceded","Goals_scored","Assists","Own_goals","Yellow_cards","Red_cards","TSB","Minutes","Bonus","Points")
fpl


fpl$Name<-(sapply(fpl$Name, function(x) gsub("\"", "", x)))


fpl$Team<-(sapply(fpl$Team, function(x) gsub("\"", "", x)))


fpl$Position<-(sapply(fpl$Position, function(x) gsub("\"", "", x)))


fpl$Points<-(sapply(fpl$Points, function(x) gsub("\"", "", x)))
fpl

#Points degiskeni string olarak geciyordu, bu yuzden integera donusturdum.
fpl$Points<-as.integer(fpl$Points)


#Temel bilesenler analizini yapabilmek icin numerik olmayan sutunlari verisetinden cikardim. 
string_fpl<-subset(fpl, select=c(1,2,3))
fpl=subset(fpl, select=-c(1,2,3))
fpl

View(round(as.matrix(fpl)))

summary(fpl)


library("corrplot")
fpl_cor=cor((fpl), method = "pearson")
fpl_cor
eigen=eigen(fpl_cor)
eigen$values
eigen$vectors



#####PCA uygulanmasi
fpl.pca <- prcomp(fpl, center = TRUE, scale. = TRUE)
fpl.pca
summary(fpl.pca)
sqrt(eigen$values) #Korelasyon matrisinden elde edilen ozdegerlerin karekoku ile pca'nin standart sapmasi ayni.


#Standart sapmanin karesi ozdegerleri veriyor:
(fpl.pca$sdev)^2  



#Aciklayicilik ve screeplot:
library("factoextra")
fviz_eig(fpl.pca)
summary(fpl.pca)

#Ek olarak diger birkac grafik:
plot(fpl.pca)
screeplot(fpl.pca, type='lines',  ylim = c(0, 10))
biplot(fpl.pca)



library(caret)
library(e1071)
pca1 = preProcess(fpl, method = 'pca', pcaComp = 4)
pca2 = preProcess(fpl, method = 'pca', thresh = 0.81)

pca1
pca2




fviz_pca_var(fpl.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#000000", "#00008B", "#FF0000", "#4169E1"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(fpl.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#000000", "#00008B", "#FF0000", "#4169E1"),
             repel = FALSE     # Avoid text overlapping
)


fpl.pca$rotation[,1:4]
predict(fpl.pca) #Skor degerleri
predict(fpl.pca)[1:10,1:4] #1.,2. 3. ve 4. temel bilesenlerin ilk 10 skor degeri


