library(faraway)
data(fat)
plot(neck~knee,fat)
plot(chest~thigh,fat)
plot(hip~wrist,fat) #hay colinealidad
names(fat)
cfat<-fat[,9:18]

prfat<- prcomp(cfat)
dim(prfat$rot)# es la U
dim(prfat$x) #es la Z , componenetes principales estamos en R10
names(prfat)
dim(fat)
summary(prfat)
round(prfat$rot[,1],2) #componente principal uno PC1 
#loadings me dice como influye cada variable en PC1
#esto puede estar afectado por problemas de escala.

prfat<- prcomp(cfat, scale = TRUE) #lo escalamos
summary(prfat)
round(prfat$rot[,1],2) 
round(prfat$rot[,2],2) 

require(MASS)
robfat<-cov.rob(cfat)
md<-mahalanobis(cfat,center=robfat$center,cov=robfat$cov)
n<-nrow(cfat); p<-ncol(cfat)
plot(qchisq(1:n/(n+1),p), sort(md),)
abline(0,1)

lmoda <- lm(fat$brozek~.,data=fat)
summary(lmoda)
lmodpcr <- lm(fat$brozek~prfatc$x[,1:2])
summary(lmodpcr) #todavia estoy usando las 10 variables pues las uso para calcular los PC

lmodr<-lm(fat$brozek~scale(abdom) + I(scale(ankle)- scale(abdom)),data = cfat)
sumary(lmodr)#tengo con solo 2 variables explicado el 68% de la varianza de la de 10 que expicaba el 76%
#otro ejemplo ----
data(meatspec)
trainmeat<-meatspec[1:172,]
testmeal<-meatspec[173:215,]
modlm <- lm(fat~.,trainmeat)
summary(modlm)$r.squared
rmse <- function(x,y)sqrt(mean(x-y)^2)
