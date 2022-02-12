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


#Kumelemeyi daha net yapabilmek icin numerik olmayan sutunlari verisetinden cikardim.
string_fpl<-subset(fpl, select=c(1,2,3))
fpl=subset(fpl, select=-c(1,2,3))
fpl


View(round(as.matrix(fpl)))


boxplot(fpl)
summary(fpl)



fpl_scale=scale(fpl, center = TRUE, scale = TRUE) #verileri standartlastirdim.
boxplot(fpl_scale)


head(fpl_scale)
dim(fpl_scale)


library(cluster)
library(factoextra)
library(fossil)
library(clustertend)
library(fpc)
library(clValid)


##### K-means

fviz_nbclust(fpl_scale, kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow method")



fviz_nbclust(fpl_scale,kmeans,method = "silhouette") 



#Kitapta genel olarak nboot=500 olmasini onermis. Bundan dolayi nboot=500 olarak aldim. Grafik cizildikten sonra iterasyon ile ilgili bir uyari aldigim icin iter.max=200 ekledim.
fviz_nbclust(fpl_scale, kmeans, nstart = 25,iter.max = 200, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


#k=5 icin k-means
set.seed(123)
fpl_km_data_5=kmeans(fpl_scale, 5, nstart=25, iter.max = 200) 
print(fpl_km_data_5)
table(fpl_km_data_5$cluster)

print(fpl_km_data_5$totss)

fviz_cluster(fpl_km_data_5, data = fpl_scale,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = FALSE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


##Dunn index kontrol k=5 icin
fpl_km_stats_5<-cluster.stats(dist(fpl_scale), fpl_km_data_5$cluster)
fpl_km_stats_5$dunn


#k=6 icin k-means
set.seed(123)
fpl_km_data_6=kmeans(fpl_scale, 6, nstart=25, iter.max = 200) 
print(fpl_km_data_6)
table(fpl_km_data_6$cluster)

print(fpl_km_data_6$totss)

fviz_cluster(fpl_km_data_6, data = fpl_scale,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = FALSE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)




##Dunn index kontrol k=6 icin
fpl_km_stats_6<-cluster.stats(dist(fpl_scale), fpl_km_data_6$cluster)
fpl_km_stats_6$dunn


#Verilerin hangi kumede bulundugunu veriseti uzerinden incelememi saglayan bu bolumu pdf'de en son sayfada kullanacagim.
fpl_cluster_cbind<- cbind(fpl, cluster=fpl_km_data_6$cluster)
fpl_cluster_cbind #Orjinal veriyi (standartlastirilmamis veriyi) kumeler ile incelemek icin
View(round(as.matrix(fpl_cluster_cbind)))





