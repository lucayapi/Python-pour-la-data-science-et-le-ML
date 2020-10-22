
#importation des donnees
setwd('D:/Kaggle competition')
train_data <- read.table("train-data.csv",sep=",",dec=".",header=T)
test_data=read.table("test-data.csv",sep=",",dec=".",header=T)
head(train_data)
head(test_data)

#librarie utilise
library(skimr)
library(stringr)
library(visdat)
library(gridExtra)
library(ggplot2)
library(GGally)
library(lubridate)
library(glmnet)
library(randomForest)
library(xgboost)
########################### Exploration des donnees ###################################
##################### Analyse de la forme ########################################
#dimension des tableaux de donnees
dim(train_data)
dim(test_data)

#format des colonnes
sapply(train_data, class)


#reformatage des colonnes

#VARIABLE MILIAGE

essai_1 <- str_replace_all(train_data$Mileage,pattern="km/kg",replacement="")
essai_2=str_replace_all(test_data$Mileage,pattern="km/kg",replacement="")
train_data$Mileage = as.numeric(str_replace_all(essai_1,pattern="kmpl",replacement=""))
test_data$Mileage = as.numeric(str_replace_all(essai_2,pattern="kmpl",replacement=""))

#VARIABLE ENGINE
train_data$Engine = as.numeric(str_replace_all(train_data$Engine,pattern="CC",replacement=""))
test_data$Engine = as.numeric(str_replace_all(test_data$Engine,pattern="CC",replacement=""))

#VARIABLE POWER
train_data$Power = as.numeric(str_replace_all(train_data$Power,pattern="bhp",replacement=""))
test_data$Power = as.numeric(str_replace_all(test_data$Power,pattern="bhp",replacement=""))

#VARIABE NEW_PRICE
train_data$New_Price = as.numeric(str_replace_all(train_data$New_Price,pattern="Lakh",replacement=""))
test_data$New_Price = as.numeric(str_replace_all(test_data$New_Price,pattern="Lakh",replacement=""))

#kilometers_Driven
train_data$Kilometers_Driven<-as.numeric(train_data$Kilometers_Driven)
test_data$Kilometers_Driven<-as.numeric(test_data$Kilometers_Driven)

#VARAIBLE SEATS
train_data$Seats<-as.integer(train_data$Seats)
test_data$Seats<-as.integer(test_data$Seats)

#VARIABLE YEAR
train_data$Year<-as.factor(train_data$Year)
test_data$Year<-as.factor(test_data$Year)

#verification du reformatage
sapply(train_data, class)

#observation globale du jeu de donnee
skim(train_data)
skim(test_data)


#Valeurs manquantes
#visualisation graphique
p=vis_miss(train_data)
q=vis_miss(test_data)
grid.arrange(p, q,ncol=2, nrow = 1)

#pourecentage de valeurs manquantes par variables
#fonction pour le calcul des pourecentages 
Pourc_Val=function(data){
tab_val_manq <-rep(0,dim(data)[2])
n=dim(data)[1]
for (i in 1:dim(data)[2]) {tab_val_manq[i]=(sum(is.na(data[,i]))/n)*100}
tab_val_manq_1<-cbind(variable=colnames(data),prop_val_manq=tab_val_manq)
return (tab_val_manq_1)}

tab1=Pourc_Val(train_data)
tab1
tab2=Pourc_Val(test_data)
tab2

################################# Analyse du fond ####################################
#######distribution de la variable cible (Price)########
# Histogramme +estimation de la densite
ggplot(train_data, aes(x=Price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth = 4)+
  geom_density(alpha=.2, fill="#FF6666") 

summary(train_data$Price) # resume statistique
sd(train_data$Price) #ecart-type

#Transformation logarithmique
ggplot(train_data, aes(x=log(Price)))+ 
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth =0.1)+
  geom_density(alpha=.2, fill="#FF6666") 

########## etude variables qualitatives ###################
###year###
#diagramme en barre
a=ggplot(train_data, aes(x=Year))+ geom_bar()
length(levels(train_data$Year)) #nombre de modalites
sort(table(train_data$Year),decreasing = T)[1:3] #top 3 des modalites

#impact sur le prix (boxplot)
p=ggplot(train_data, aes(x=Year,y=Price))+ geom_boxplot(aes(group=Year))

###Fuel_Type###
#diagramme en barre
b=ggplot(train_data, aes(x=Fuel_Type))+ geom_bar()
length(levels(train_data$Fuel_Type)) #nombre de modalites
sort(table(train_data$Fuel_Type),decreasing = T)[1:3] #top 3 des modalites

#impact sur le prix (boxplot)
q=ggplot(train_data, aes(x=Fuel_Type,y=Price))+ geom_boxplot(aes(group=Fuel_Type))

#creation d'une nouvelle variable Age 
train_data=cbind(train_data,Age=year(today())-as.numeric(as.character(train_data$Year)))
test_data=cbind(test_data,Age=year(today())-as.numeric(as.character(test_data$Year)))


###Transmission##
#diagramme en barre
c=ggplot(train_data, aes(x=Transmission))+ geom_bar()
levels(train_data$Transmission) #nombre de modalites
sort(table(train_data$Transmission),decreasing = T)[1:3] #top 3 des modalites

#impact sur le prix
r=ggplot(train_data, aes(x=Transmission,y=Price))+ geom_boxplot(aes(group=Transmission))


###Owner_Type###
#diagramme en barre
d=ggplot(train_data, aes(x=Owner_Type))+ geom_bar()
levels(train_data$Owner_Type)  #nombre de modalites
sort(table(train_data$Owner_Type),decreasing = T)[1:3] #top 3 des modalites

#impact sur le prix (boxplot)
s=ggplot(train_data, aes(x=Owner_Type,y=Price))+ geom_boxplot(aes(group=Owner_Type))

###Location##
#diagramme en barre
e=ggplot(train_data, aes(x=Location))+ geom_bar()
length(levels(train_data$Location)) #nombre de modalites
sort(table(train_data$Location),decreasing = T)[1:3] #top 3 des modalites

#impact sur le prix (boxplot)
t=ggplot(train_data, aes(x=Location,y=Price))+ geom_boxplot(aes(group=Location))

###Name###
#diagramme en barre
f=ggplot(train_data, aes(x=Name))+ geom_bar()
length(levels(dataset$Name)) #nombre de modalites

#creation de nouvelle variable marque
#fonction
extract_marque=function(data){
  marque <- rep(0,nrow(data))
  for (i in 1:dim(data)[1]){
    marque[i]<-str_split(data$Name[i]," ")[[1]][1]}
    marque[marque=="ISUZU"]='Isuzu'
    marque=as.factor(marque)
    return(marque)}

#diagramme en barrre 
g=ggplot(train_data, aes(x=marque))+ geom_bar()

train_data=cbind(train_data,marque=extract_marque(train_data))
test_data=cbind(test_data,marque=extract_marque(test_data))


length(levels(train_data$marque)) #nombre de modalites
sort(table(train_data$marque),decreasing = T)[1:3] #top 3 des modalites

#impact sur le prix (boxplot)
u=ggplot(train_data, aes(x=marque,y=Price))+ geom_boxplot(aes(group=marque))


#recapitulatif diagramme en barre
grid.arrange(a,b,c,d,e,f,ncol=2, nrow = 3)

#recapatitulation - impact sur Price
grid.arrange(p,q,r,s,t,u,ncol=2, nrow = 3)



#test de l'anova
reg.aov=lm(Price~marque,data=train_data)
tab=anova(reg.aov)
tab$`Pr(>F)`<0.05 #on rejette Ho (il ixiste au moins une modalitite dont la moyenne des prix est signicativement differents des autres)
var_qualitaive=colnames(train_data)[c(3,4,6,7,8,16)]
test_anova=cbind(var_qualitaive,p_value=rep(0,6),Statut=rep(0,6))
i=1
for (var in c(3,4,6,7,8,16)){
  reg.aov=lm(train_data[,"Price"]~train_data[,var])
  tab=anova(reg.aov)
  test_anova[i,2]=tab$`Pr(>F)`[1]
  if ((tab$`Pr(>F)`<0.05)[1]){
    test_anova[i,3]='Ho rejettee'}
  else {test_anova[i,3]='Ho accepte'}
  i=i+1}

test_anova

########## etude des variables quantitatives ###################
#seats
a=ggplot(train_data, aes(x=as.factor(Seats)))+ geom_bar()
length(levels(as.factor(train_data$Seats))) #nombre de modalites
sort(table(train_data$Seats),decreasing = T)[1:3] #top 3 des modalites

#resume statistique des autres variables quantitatives
summary(train_data$Kilometers_Driven)
sd(train_data$Kilometers_Driven, na.rm = T)

summary(train_data$Mileage)
sd(train_data$Mileage, na.rm = T)

summary(train_data$Engine)
sd(train_data$Engine, na.rm = T)

summary(train_data$Power)
sd(train_data$Power, na.rm = T)

summary(train_data$New_Price)
sd(train_data$New_Price, na.rm = T)

summary(train_data$Age)
sd(train_data$Age, na.rm = T)


#impact des autres variables quantitatives  sur le prix (matrice de correlation ) + distribution des variables
ggpairs(train_data[,c(5,9,10,11,13,14,15)])

#impact de la variable Seats
ggplot(train_data, aes(x=as.factor(Seats),y=Price))+ geom_boxplot(aes(group=as.factor(Seats)))

#zoom pour la valeur eloigne des autre de la variable Kilometers_Driven
View(train_data[train_data$Kilometers_Driven>1000000,])
train_data[-which(train_data$Kilometers_Driven>1000000),]

#suppression des observations avec Seats=0
train_data=train_data[-which(train_data$Seats==0),]
######################## Preparation des donnees ##############################
## Gestion valeur Manquante

#REMPLACEMENT DES VALEURS MANQUANTES AVEC LA MEDIANE
train_data$Mileage[which(is.na(train_data$Mileage))] = median(train_data$Mileage,na.rm=T)
train_data$Engine[which(is.na(train_data$Engine))] = median(train_data$Engine,na.rm=T)
train_data$Power[which(is.na(train_data$Power))] = median(train_data$Power,na.rm=T)
t <- table(train_data$Seats)
train_data$Seats[which(is.na(train_data$Seats))] = names(which.max(t))
train_data$Seats=as.numeric(train_data$Seats)

test_data$Mileage[which(is.na(test_data$Mileage))] = median(test_data$Mileage,na.rm=T)
test_data$Engine[which(is.na(test_data$Engine))] = median(test_data$Engine,na.rm=T)
test_data$Power[which(is.na(test_data$Power))] = median(test_data$Power,na.rm=T)
t <- table(test_data$Seats)
test_data$Seats[which(is.na(test_data$Seats))] = names(which.max(t))

#verification
p=vis_miss(train_data)
q=vis_miss(test_data)
grid.arrange(p, q,ncol=2, nrow = 1)



#suprresion de la variable New_Price, X , name, Year
train_data=train_data[,-c(1,2,4,13)]
test_data=test_data[,-c(1,2,4,13)]


#verification
vis_miss(train_data)
vis_miss(test_data)



#modication de la variable Fuel-Type et marque (regrouper les variables de faible modalite)

var=as.character(train_data$Fuel_Type)
var[var=='LPG' | var=='Electric']=rep('other',length(var[var=='Electric' | var=='LP']))
train_data$Fuel_Type=as.factor(var)

var=as.character(train_data$marque)
other=names(sort(table(var)))[1:5]
var[var %in% other]=rep('other',length(var[var %in% other]))
train_data$marque=as.factor(var)



#division en train set et valid set
set.seed(123)
ind=sample(2,nrow(train_data),replace=T,prob = c(0.8,0.2))
train=train_data[ind==1,]
valid=train_data[ind==2,]


cible_train=log10(train$Price)
X_train=model.matrix(Price~., train)

cible_valid=log10(valid$Price)
X_valid=model.matrix(Price~., valid)

variables=colnames(X_valid)[colnames(X_valid) %in% colnames(X_train)]

X_train=X_train[,variables]
X_valid=X_valid[,variables]


#modele1
fit=lm(log10(Price)~Age+marque+Power,data=train)
a=summary(fit)
print(paste("R2 score :",a$r.squared))
print(paste("MSE(train):", mean(resid(fit)^2)))
print(paste("MSE(Valid):", mean((predict(fit,newdata=as.data.frame(valid))-cible_valid)^2)))

## modele Lasso
#choix du meilleur lambda par validation croise
cv.model.lasso=cv.glmnet(X_train[,-1],cible_train)
plot(cv.model.lasso)
best.lambda=cv.model.lasso$lambda.min

model.lasso=glmnet(X_train[,-1],cible_train, lambda=best.lambda) #lasso par defaut alpha=1
coef(model.lasso)

#resultat
rss=sum((predict(model.lasso,X_train[,-1])-cible_train)^2)
tss=sum((cible_train-mean(cible_train))^2)
rsq = 1 - (rss/tss)

print(paste("R2 score :",rsq))
print(paste("MSE(train):",mean((predict(model.lasso, X_train[,-1])-cible_train)^2)))
print(paste("MSE(Valid):", mean((predict(model.lasso,X_valid[,-1])-cible_valid)^2)))


#Random Forest
regressor = randomForest(x = X_train,y =cible_train, ntree =500, mtry = 5, nPerm = 4, nodesize = 2)

#resultats
rss=sum((predict(regressor,X_train)-cible_train)^2)
tss=sum((cible_train-mean(cible_train))^2)
rsq = 1 - (rss/tss)

print(paste("R2 score :",rsq))
print(paste("MSE(train):",mean((predict(regressor,X_train)-cible_train)^2)))
print(paste("MSE(Valid):", mean((predict(regressor,X_valid)-cible_valid)^2)))


#Gradient Boosting
cv.model.xgb <- train(
  X_train[,-1],
  cible_train,
  tuneGrid = expand.grid(
    nrounds=500,
    max_depth=c(3,4,5),
    eta=c(1e-1, 1e-2, 1e-3),
    gamma=0,
    min_child_weight=1,
    colsample_bytree=1,
    subsample=1
  ),
  trControl = trainControl(
    method = "cv",
    verboseIter = TRUE
  ),
  method = "xgbTree",
  verbose = TRUE
)


rss=sum((predict(cv.model.xgb,X_train[,-1])-cible_train)^2)
tss=sum((cible_train-mean(cible_train))^2)
rsq = 1 - (rss/tss)

print(paste("R2 score :",rsq))
print(paste("MSE(train):",mean((predict(cv.model.xgb,X_train[,-1])-cible_train)^2)))
print(paste("MSE(Valid):", mean((predict(cv.model.xgb,X_valid[,-1])-cible_valid)^2)))


##########Predictition finale ############################################
#suppression des modalites contenues dans le modele mais pas  dansles donnees de test
test_data=test_data[-which(test_data$marque %in% c('OpelCorsa','Bentley','Hindustan')),]
test_data$marque=droplevels(test_data$marque)

#prediction de du log10 des prix voitures d'occasion
predict=predict(fit,newdata =test_data)

#prediction des vraies valeurs des prix
prediction=cbind(Id_voiture=1:nrow(test_data),prediction=10^predict)

