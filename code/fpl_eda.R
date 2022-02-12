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


#Oncelikle numerik olmayan sutunlari verisetinden daha rahat tanimlayici istatistik yapabilmek icin cikardim. Bu sutunlara daha sonra tanimlayici istatistikleri uygulayacagim.
string_fpl<-subset(fpl, select=c(1,2,3))
fpl=subset(fpl, select=-c(1,2,3))
fpl

dim(fpl)
View(round(as.matrix(fpl)))

#####Tanimlayici istatistikler

summary(fpl)

sapply(fpl,mean)
sapply(fpl,sd)
sapply(fpl,median)
sapply(fpl,quantile)



boxplot(fpl)


#Verilerin histogrami
par(mfrow=c(4,4))
hist(fpl$Cost, main="Cost", xlab="Cost", border="black", col="blue",las=1, breaks=10)
hist(fpl$Creativity, main="Creativity", xlab="Creativity", border="black", col="blue",las=1, breaks=10)
hist(fpl$Influence, main="Influence", xlab="Influence", border="black", col="blue",las=1, breaks=10)
hist(fpl$Threat, main="Threat", xlab="Threat", border="black", col="blue",las=1, breaks=10)
hist(fpl$Goals_conceded, main="Goals_conceded", xlab="Goals_conceded", border="black", col="blue",las=1, breaks=10)
hist(fpl$Goals_scored, main="Goals_scored", xlab="Goals_scored", border="black", col="blue",las=1, breaks=10)
hist(fpl$Assists, main="Assists", xlab="Assists", border="black", col="blue",las=1, breaks=10)
hist(fpl$Own_goals, main="Own_goals", xlab="Own_goals", border="black", col="blue",las=1, breaks=10)
hist(fpl$Yellow_cards, main="Yellow_cards", xlab="Yellow_cards", border="black", col="blue",las=1, breaks=10)
hist(fpl$Red_cards, main="Red_cards", xlab="Red_cards", border="black", col="blue",las=1, breaks=10)
hist(fpl$TSB, main="TSB", xlab="TSB", border="black", col="blue",las=1, breaks=10)
hist(fpl$Minutes, main="Minutes", xlab="Minutes", border="black", col="blue",las=1, breaks=10)
hist(fpl$Bonus, main="Bonus", xlab="Bonus", border="black", col="blue",las=1, breaks=10)
hist(fpl$Points, main="Points", xlab="Points", border="black", col="blue",las=1, breaks=10)




#Faktor tipi verilerin ayrica histogrami
par(mfrow=c(1,3))
barplot(table(string_fpl$Name), ylim = c(0, 500), xlim=c(0,6), main="Name", xlab="Name", border="black", col="blue")
barplot(table(string_fpl$Team), ylim = c(0, 500), xlim=c(0,6), main="Team", xlab="Team", border="black", col="blue")
barplot(table(string_fpl$Position), ylim = c(0, 500), xlim=c(0,6), main="Position", xlab="Position", border="black", col="blue")



