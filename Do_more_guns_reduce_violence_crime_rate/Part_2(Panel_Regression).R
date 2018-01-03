load.libraries <- c('ggplot2','dplyr','grid','ggpubr','car','moments','treemap','psych','plm','lmtest')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)


#reading data
guns <- read.csv("guns.csv")
guns$stateid <- as.character(guns$stateid)
guns$shall <- as.character(guns$shall)
#Assumptions of linear regression

normalplot <- function(dataset=""){
  n <- dim(dataset)[2]
  for(i in 1:n){
    plot(density(c(dataset[,i])), main = "")
    title(main = paste(colnames(dataset)[i],"skewness: ",round(skewness(dataset[i]),2), "kurtosis: ",round(kurtosis(dataset[i]),2)))
  }
}
par(mfrow=c(4,2))
normalplot(guns[,!colnames(guns) %in% c("shall","stateid","year")])
par(mfrow=c(1,1))


#Transforming the variables
## Log transformation for vio, mur, rob, incar_rate and density

guns1 <- guns

guns1$vio<-log(guns1$vio)
guns1$incarc_rate<-log(guns1$incarc_rate)
guns1$density<-log(guns1$density) 


#Pairwise Correlation - post data transformation
## pw1064 and pb1064 are highly correlated (-0.98)
pairs.panels(guns1[,!colnames(guns1) %in% c("shall","stateid","year")])


#Polled regression
guns2<-pdata.frame(guns, index = c("stateid","year"))

#violence
#pooled
vio_polled<-plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+shall+pw1064,data=guns2, model="pooling")
summary(vio_polled)

#combined significance test
Hnull <- c("pb1064=0","pw1064=0")
linearHypothesis(vio_polled,Hnull)

#plots for heteroscedasticity
res <- residuals(vio_polled3)
yhat <- fitted(vio_polled3)
plot(log(guns2$incarc_rate),res, xlab="incarceration rate", ylab="residuals") +abline(h =0)

#remove pw1064
vio_polled3<-plm(log(vio)~log(incarc_rate)+pm1029+pop+avginc+log(density)+shall,data=guns2, model="pooling")
coeftest(vio_polled3, vcov=vcovHC(vio_polled3,type="HC0",cluster="group"))

#fixed effects
vio_fixed1<-plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+shall+pw1064
                ,data=guns2, 
                model="within")
summary(vio_fixed1)

Hnull <- c("avginc=0")
linearHypothesis(vio_fixed1,Hnull)

vio_fixed2<-plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pop+log(density)+shall+pw1064
                ,data=guns2, 
                model="within")
summary(vio_fixed2)


#fixed effects with time

vio_fixed3 <- plm(log(vio)~log(incarc_rate)+pb1064+pm1029+pop+avginc+log(density)+shall+pw1064+factor(year)-1, data=guns2,
                  model="within")
summary(vio_fixed3)


Hnull <- c("pb1064=0","pop=0","avginc=0","pw1064=0")
linearHypothesis(vio_fixed3,Hnull)

vio_fixed4 <- plm(log(vio)~log(incarc_rate)+pm1029+log(density)+shall+factor(year)-1, data=guns2,
                  model="within")
summary(vio_fixed4)



