load.libraries <- c('ggplot2','dplyr','readxl','rpart','rpart.plot','moments','grid','gridExtra','reshape2','FactoMineR','factoextra','caret','rpart','rpart.plot','caret','randomForest')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

set.seed(1234)
#reading data
hp <- read_xls("AmesHousing_data.xls")

#Root-Mean-Squared-Error (RMSE) between the logarithm of the predicted value and the logarithm of the observed sales price. 
#(Taking logs means that errors in predicting expensive houses and cheap houses will affect the result equally.)
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

#EDA of data
var_data<-function(var=""){
  counter <<- counter + 1; 
  par(mfrow=c(2,3))
  plot(0,col="white",axes=FALSE,ann=FALSE)
  title(main = paste(colnames(hp)[counter],":",typeof(var)))
  mtext(paste("na:",length(which(is.na(var))),"values: ", var[1],var[2],collapse = "  "), side=3)
  
  if(typeof(var)!="character"){
    hist(var,main=paste("number of distinct values:",length(unique(var))))
    if(length(which(is.na(var)))==0){
      plot(density(var),main="")
      title(main = paste("skewness: ",round(skewness(var),2), "kurtosis: ",round(kurtosis(var),2)))}
    title(main=paste("number of outliers:",length(boxplot(var)$out)))
    plot(var,hp$SalePrice, main=paste("corealtion dependent:",round(cor(var,hp$SalePrice),2)))
  }
  else
  {
    barplot(prop.table(table(var)),main=paste("number of distinct values:",length(unique(var))))
    
    #plot(hp %>% group_by(paste("`",noquote(colnames(hp)[i]),"`", sep = "")) %>% summarise(av.saleprice = mean(SalePrice)),main="avg.sales price")
    boxplot(hp$SalePrice~var)
  }
}

#eda of all the variables
pdf("raw_data2.pdf")
counter<<-0
sapply(hp,var_data)
dev.off()

#This data consists of data of 2,930 houses
nrow(hp)

#order is just an increment variable and its not required
hp$Order<-NULL

#PID of the house is a unique identifier
length(unique(hp$PID))==nrow(hp)

#LOT Frontage
#Linear feet of street connected to property
#16% of the observations are not available for Lot Frontage and 6% are outliers
#predicting the missing values based on the aggregate of Neighborhood and "house style". 
#Houses in the same vicinty and the same hpuse style tend to have same Linear feet of street connected to property
length(which(is.na(hp$`Lot Frontage`)))/nrow(hp)
length(boxplot(hp$`Lot Frontage`)$out)/nrow(hp)
table(hp$`Lot Frontage`)
test <- aggregate(`Lot Frontage` ~ Neighborhood+`House Style`, data=hp ,median)
hp %>% left_join(test, by = c("Neighborhood","House Style")) -> hp
hp$`Lot Frontage.x`<-NULL
rm(test)

#Lot Area
#outliers exist so winsorizing top 2.5% of the data
#in this though the % of outliers still remains the same, the effect of extreme observations would reduce
lim <- quantile(hp$`Lot Area`, probs=c(1-0.025))
hp$`Lot Area`[ hp$`Lot Area` > lim] <- lim

#Street
#has only 12 observations with Grvl type of street
#so removing the variable
table(hp$Street)
hp$Street<-NULL

#Alley
#93% of observations have NA (unidentified alley)
#so removing the variable
hp$Alley<-NULL

#Utilities
#99.9% of the data has All pub as an utility
#removing utilities
table(hp$Utilities)
hp$Utilities<-NULL

#COndition2
#98% of the houses have proximity as 'Norm'
#this cannot be very useful in predictions
#remving the variable
table(hp$`Condition 2`)/nrow(hp)
hp$`Condition 2`<-NULL

#roof Mtl
#98% of the roof material is CompShg
#removing the variable
table(hp$`Roof Matl`)/nrow(hp)
hp$`Roof Matl`<-NULL

#Exterior covering 2
#creating a new variable depedning on type of second material used
hp$extcovering<-ifelse(hp$`Exterior 1st`==hp$`Exterior 2nd`,"same",hp$`Exterior 2nd`)
table(hp$extcovering)

#Mas Vnr Type and Mas Vnr Area 
##Mas Vnr: a single non-structural external layer of  building of structures from individual units, typically made of brick, stone or manufactured stone.
##Type of Masonry veneer is not defined in the data for 1752 observations and the corresponding Masonry veneer Area for 99.6% observations is also zero   
##there are 23 missing observations of these variables in the data
##therefore assigning these 23 observations to 'None' type
table(hp$`Mas Vnr Type`)
hp %>% filter(`Mas Vnr Type`=='None') %>% group_by(`Mas Vnr Area`) %>% summarise(freq=n())
hp[which(is.na(hp$`Mas Vnr Type`)),]$`Mas Vnr Type`<-'None'
hp[which(is.na(hp$`Mas Vnr Area`)),]$`Mas Vnr Area`<-0
lim <- quantile(hp$`Mas Vnr Area`, probs=c(1-0.025))
hp$`Mas Vnr Area`[ hp$`Mas Vnr Area` > lim] <- lim

#BSmt 2
#87.9% of the data doesn't have basement2 SF mentioned
#removing these two variables
table(hp$`BsmtFin SF 2`)/nrow(hp)
hp$`BsmtFin SF 2`<-NULL
hp$`BsmtFin Type 2`<-NULL

#Bmst Unf SF has 56 outliers
#winsorizing data - this doesn;t remove the outliers but decreases the effect of extreme values
#removing the one observation which has Bsmt un sf as null
hp<-hp[-which(is.na(hp$`Bsmt Unf SF`)),]
lim <- quantile(hp$`Bsmt Unf SF`, probs=c(1-0.025))
hp$`Bsmt Unf SF`[ hp$`Bsmt Unf SF` > lim] <- lim

#Total Bsmt SF has 123 outliers
#winsorizing data - this doesn't remove the outliers but decreases the effect of extreme values
lim <- quantile(hp$`Total Bsmt SF`, probs=c(1-0.025))
hp$`Total Bsmt SF`[ hp$`Total Bsmt SF` > lim] <- lim

#heating
#98% of data is GAsA
#removing the variable
table(hp$Heating)/nrow(hp)
hp$Heating<-NULL

#lowqual finnished sqft
#98.6% have zero values 
table(hp$`Low Qual Fin SF`)/nrow(hp)
hp$`Low Qual Fin SF`<-NULL

#GR Liv Area has 74 outliers
#winsorizing data - this doesn't remove the outliers but decreases the effect of extreme values
lim <- quantile(hp$`Gr Liv Area`, probs=c(1-0.025))
hp$`Gr Liv Area`[ hp$`Gr Liv Area` > lim] <- lim

#Garage Yr Blt <- Year garage was built
#There are 159 observations where Garage Yr blt is missing
#one of the years has been recorded as '2207'.
#predicting year in which garage was built has 104 possibilities and data can be binned to 7 sets of 20 years each
#using decision tree to predict the missing values and variables used to predict the "binned garange year built" are"variables related to house construction" "variables realted to house sold" 
#for observation where "binned garange year built" is already defined, 87% of them could are correctly predicted 
# so, using this decision tree to identify missing values of "binned garange year built"
table(hp$`Garage Yr Blt`)
length(unique(hp$`Garage Yr Blt`))
hp$Garageyearbin<-ifelse(is.na(hp$`Garage Yr Blt`),'missing',ifelse(hp$`Garage Yr Blt`<=1900, '1900 and before', ifelse(hp$`Garage Yr Blt`>=2000, '2000 and after', ifelse((hp$`Garage Yr Blt`-1900)/20< 1, '1901-1919', ifelse((hp$`Garage Yr Blt`-1900)/20< 2, '1920-1939',ifelse((hp$`Garage Yr Blt`-1900)/20< 3, '1940-1959',ifelse((hp$`Garage Yr Blt`-1900)/20< 4, '1960-1979','1980-1999')))))))
tree<-rpart(Garageyearbin~`Year Built`+`Year Remod/Add`+`Yr Sold`+`Sale Condition`+`Sale Type`,data=hp[-which(hp$Garageyearbin=='missing'),],method = "class")
tree$variable.importance
hp_train<-data.frame(actual=hp[-which(hp$Garageyearbin=='missing'),]$Garageyearbin,predict_year=predict(tree,hp[-which(hp$Garageyearbin=='missing'),], type="class"))
hp_train$match<-ifelse(hp_train$actual==hp_train$predict_year,1,0)
table(hp_train$match)/nrow(hp_train)
test<-hp[which(hp$Garageyearbin=='missing'),]
test$Garageyearbin<-predict(tree,test, type="class")
hp<-rbind(hp[-which(hp$Garageyearbin=='missing'),],test)

hp$`Garage Yr Blt`<-NULL
rm(tree)
rm(test)
rm(hp_train)

#creating a new variable year sold and year built to calculate the age
hp$houseage<-hp$`Yr Sold`-hp$`Year Built`
var_data(hp$houseage)
#house has been remodeled or not
hp$remodelornot<-ifelse(hp$`Year Remod/Add`-hp$`Year Built`==0,"no remodel","remodel")
#number of years since remodel
hp$houseageafterremodel<-hp$`Yr Sold`-hp$`Year Remod/Add`
var_data(hp$houseageafterremodel)
#removing the year variables
hp<-hp[,!names(hp) %in% c("Year Remod/Add","Year Built","Yr Sold")]


#Different types of porch and there sq.ft of houses
#across all the different porch variables majority of the houses have no porch size mentioned (>80% for Enclosed Porch, 3ssn Porch and Screen Porch)
#cor between all the 4 porch variables with the 'salesprice' is less than 30%
#creating a new variable 
#depending on the different type of proch available because houses with different type of porch have different type of average price
#totalporchsize (combination of all types of porch). it is skewed so winsorizing the data
#in this though the % of outliers still remains the same, the effect of extreme observations would reduce
round(table(hp$`Enclosed Porch`)/nrow(hp)*100,2)
round(table(hp$`Open Porch SF`)/nrow(hp)*100,2)
round(table(hp$`3Ssn Porch`)/nrow(hp)*100,2)
round(table(hp$`Screen Porch`)/nrow(hp)*100,2)
plot(hp$`Enclosed Porch`,hp$SalePrice)
plot(hp$`Open Porch SF`,hp$SalePrice)
plot(hp$`3Ssn Porch`,hp$SalePrice)
plot(hp$`Screen Porch`,hp$SalePrice)
cor(hp$`Enclosed Porch`,hp$SalePrice)
cor(hp$`Open Porch SF`,hp$SalePrice)
cor(hp$`3Ssn Porch`,hp$SalePrice)

cor(hp$`Screen Porch`,hp$SalePrice)

hp$enclosedporch_flag<-ifelse(hp$`Enclosed Porch`==0,"","enclosed")      
hp$openporch_flag<-ifelse(hp$`Open Porch SF`==0,"","open")      
hp$ssnporchflag <-ifelse(hp$`3Ssn Porch`==0,"","ssn")      
hp$screenporchflag<-ifelse(hp$`Screen Porch`==0,"","screen")
hp$porchtype<-trimws(paste(hp$enclosedporch_flag,hp$openporch_flag,hp$ssnporchflag, hp$screenporchflag))
hp[which(hp$porchtype==""),]$porchtype="no porch"
hp %>% group_by(porchtype) %>% summarise(avg.houseprice = mean(SalePrice)) %>% arrange(avg.houseprice)



hp$porchsize<-hp$`Enclosed Porch`+hp$`Open Porch SF`+hp$`3Ssn Porch`+hp$`Screen Porch`

hist(hp$porchsize)
lim <- quantile(hp$porchsize, probs=c(1-0.025))
hp$porchsize[ hp$porchsize > lim] <- lim

hp<-hp[,!names(hp) %in% c("Enclosed Porch","enclosedporch_flag","openporch_flag","Open Porch SF","ssnporchflag","3Ssn Porch","screenporchflag","Screen Porch")]

#PoolQC and Pool Area are not available for 99% of the houses
table(hp$`Pool QC`)
table(hp$`Pool Area`)
hp$pool_present<-ifelse(hp$`Pool Area`==0,"no pool","pool")
hp<-hp[,!names(hp) %in% c("Pool QC","Pool Area")]

#Fence is NA for 2358 observations and avg.price of houses with different type of fences is not very different
table(hp$Fence)
hp$fence_present<-ifelse(hp$Fence=="NA","no fence","fence")
hp$Fence<-NULL
table(hp$fence_present)

#MiscFeature and MiscValue are not available for 2824 observations
table(hp$`Misc Feature`)
hp$`Misc Feature`<-NULL
hp$`Misc Val`<-NULL


# MO Sold
# Though most number of houses have been sold in 6th month, prices of the houses don't vary with the month in which its sold
boxplot(hp$SalePrice ~ hp$`Mo Sold`)
hp$`Mo Sold`<-NULL


#other missing values
#removing observations where other missing values are present leads to loss of less tan 0.01% of data ( 9 observations)
1-nrow(na.omit(hp))/nrow(hp)
hp<-na.omit(hp)

#test and training
hp_random <- runif(nrow(hp)) 
hp <- hp[order(hp_random), ]
hptrain_num <- createDataPartition(y=hp$SalePrice, p=0.80, list=FALSE)  
hp <- hp %>% mutate_if(is.character, as.factor)
hp_train<-hp[hptrain_num,]
hp_test<-hp[-hptrain_num,]
#rm(hptrain_num)


RMSE_baseline_train <- RMSE(mean(hp_train$SalePrice),hp_train$SalePrice)
RMSE_baseline_test <- RMSE(mean(hp_test$SalePrice),hp_test$SalePrice)

hp_train<-hp_train2
hp_test<-hp_test2

###decision tree

tree <- rpart(SalePrice ~ ., data = hp_train[,!colnames(hp_train) %in% c("PID","Kitchen Qual","Utilities","Exterior 1st","Mas Vnr Type","extcovering")])
tree <- rpart(SalePrice ~ ., data = hp_train[,!colnames(hp_train) %in% c("PID")])


rpart.plot(tree)
tree$variable.importance
summary(tree)

hp_train$pred_decisiontree <- predict(tree, hp_train) 
hp_test$pred_decisiontree <- predict(tree, hp_test) 

length(round(table(predict(tree, hp_train))))

RMSE_tree_train <- RMSE(hp_train$pred_decisiontree,hp_train$SalePrice)
RMSE_tree_test <- RMSE(hp_test$pred_decisiontree,hp_test$SalePrice)

##random forest
train_rf <- data.frame(hp_train[,!colnames(hp_train) %in% c("PID")])
rf <- randomForest(SalePrice ~ ., data = train_rf)

# How many trees are needed to reach the minimum error estimate?
which.min(rf$mse)


imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

test_rf <- data.frame(hp_test[,!colnames(hp_test) %in% c("PID")])
table(sapply(test_rf,class))
length(round(table(predict(rf,test_rf))))


hp_train$pred_randomForest <- predict(rf, train_rf) 
hp_test$pred_randomForest <- predict(rf, test_rf) 

RMSE_randomforest_train <- RMSE(hp_train$pred_randomForest,hp_train$SalePrice)
RMSE_randomforest_test <- RMSE(hp_test$pred_randomForest,hp_test$SalePrice)

#rf.cv <- rf.crossValidation(rf, train_rf, p=0.10, n=99)

#linear
#Defining the function for checking linear relationship between dependent and all independent variables
linearrelationship <- function(dataset = ""){
  n <- dim(dataset)[2]-1
  for(i in 1:n){
    scatter.smooth(dataset[,i],dataset$SalePrice, xlab = colnames(dataset[i]))
    title(main = paste(colnames(dataset[i]),"vs","SalePrice", "correlation", round(cor(dataset[,i],dataset$SalePrice),2)))
  }
}

#Checking for normality in the data set
Column_classes <- sapply(names(hp),function(x){class(hp[[x]])})
numeric_columns <-names(Column_classes[Column_classes != "factor"])
#determining skew of each numric variable

skew <- sapply(numeric_columns,function(x){skewness(hp[[x]], na.rm = T)})
# Let us determine a threshold skewness and transform all variables above the treshold.
skew1 <- skew[skew > 1]
hp2<-hp
# transform excessively skewed features in the training data set with log(x + 1)
for(x in names(skew1)) {
  hp2[[x]] <- log(hp2[[x]] + 1)
}


#Correlation plots
heat <- hp2[,numeric_columns]
options(repr.plot.width=8, repr.plot.height=6)

qplot(x=Var1, y=Var2, data=melt(cor(heat, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.4))

#partition of the dataset
hp_train_linear<-hp2[hptrain_num,]
hp_test_linear<-hp2[-hptrain_num,]


#testing the linear regression model on the training dataset

trainmodel<-lm(SalePrice ~ `Lot Area`+ `Overall Qual`+houseage + houseageafterremodel + 
                 pool_present + fence_present + `Gr Liv Area` + Neighborhood + `Total Bsmt SF` + 
                 `Overall Cond` + `BsmtFin SF 1`+`Garage Cars`,data = hp_train_linear)

summary(trainmodel)


##Testing the accuracy of the model
hp_train$pred_lr <- predict(trainmodel, hp_train_linear) 
hp_test$pred_lr <- predict(trainmodel, hp_test_linear) 


RMSE <- function(x,y){
  a <- sqrt(sum((x-y)^2)/length(y))
  return(a)
}

RMSE_lr_train <- RMSE(hp_train$pred_lr,log(hp_train$SalePrice))
RMSE_lr_test <- RMSE(hp_test$pred_lr,log(hp_test$SalePrice))

cor(hp_train$pred_lr,log(hp_train$SalePrice))
cor(hp_test$pred_lr,log(hp_test$SalePrice))


total_train<-data.frame(lr=predict(trainmodel, hp_train_linear),decision=log(predict(tree, hp_train)), randomf=log(predict(rf, train_rf)) )
total_train$final<-(total_train$lr+total_train$decision+total_train$randomf)/3
total_train$final<-(total_train$lr+total_train$randomf)/2

total_test<-data.frame(lr=predict(trainmodel, hp_test_linear),decision=log(predict(tree, hp_test)), randomf=log(predict(rf, test_rf)) )
total_test$final<-(total_test$lr+total_test$decision+total_test$randomf)/3
total_test$final<-(total_test$lr+total_test$randomf)/2

RMSE_total_train<-RMSE(total_train$final,log(hp_train$SalePrice))
RMSE_total_test<-RMSE(total_test$final,log(hp_test$SalePrice))

#next
#EDA
#do cross validation in all the models
#linear regression. (checking for skewness, linearity assumptions, do varaible selection, checking model assumptions, change model if required)
#lasso regression (understnad why it's required)
#do cart(grow the tree and prune back)
#randomforest(grow tree and prune back)
#gradient descent
#XGboost  
#PCA and above steps
#cluster and do above steps
#compare which model performs better


