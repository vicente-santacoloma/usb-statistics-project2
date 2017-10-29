Examination=swiss[,3]
Agriculture=swiss[,2]
Fertility=swiss[,1]
Education=swiss[,4]
Catholic=swiss[,5]
Infant.Mortality=swiss[,6]
extra=read.table("extra",header=T)

pf <- function(a,ex,ed,c,im)
{
  pf=66.91518 - 0.17211 * a - 0.25801 * ex - 0.87094 * ed + 0.10412 * c + 1.07705 * im
}

for (i in 1:4){
Fe1[i]= pf(extra[,2][i],extra[,3][i],extra[,4][i],extra[,5][i],extra[,6][i])
}

pf2 <- function(a,ed,c,im)
{
pf2 = 62.1013 -0.1546 * a -0.9803 * ed +0.1247 * c +1.0784 *im
}

for (i in 1:4){
Fe2[i]= pf2(extra[,2][i],extra[,4][i],extra[,5][i],extra[,6][i])
}

pf3 <- function(Ede,Ce,Ime)
{
pf3 = 48.67707 -  0.75925 * Ede + 0.09607 * Ce + 1.29615 *Ime
}

for (i in 1:4){
Fe[i]= pf3(extra[,4][i],extra[,5][i],extra[,6][i])
}

par(mfrow=c(1,3))

modelo1 = lm(Fertility~Agriculture + Examination +Education+ Catholic+ Infant.Mortality)
temp1<-predict(modelo1,extra[,c(2,3,4,5,6)],interval='prediction',level=0.99)
temp2<-predict(modelo1,extra[,c(2,3,4,5,6)],interval='confidence',level=0.99)
Fe1=rep(1,4)
for (i in 1:4){
Fe1[i]= pfe(extra[,4][i],extra[,5][i],extra[,6][i])
}
mod1 = data.frame(Fertility=Fe1)
matplot(mod1$Fertility, cbind(temp1,temp2[,-1]), lty=c(1,2,2,3,3), type="l",ylab="Predicted Fertility" )

modelo2 = lm(Fertility~Agriculture+Education+ Catholic+ Infant.Mortality)
temp1<-predict(modelo2,extra[,c(2,4,5,6)],interval='prediction',level=0.99)
temp2<-predict(modelo2,extra[,c(2,4,5,6)],interval='confidence',level=0.99)
Fe2=rep(1,4)
for (i in 1:4){
Fe2[i]= pfe(extra[,4][i],extra[,5][i],extra[,6][i])
}
mod2 = data.frame(Fertility=Fe2)
matplot(mod2$Fertility, cbind(temp1,temp2[,-1]), lty=c(1,2,2,3,3), type="l",ylab="Predicted Fertility" )

modelo3 = lm(Fertility~Education+ Catholic+ Infant.Mortality)
temp1<-predict(modelo3,extra[,c(4,5,6)],interval='prediction',level=0.99)
temp2<-predict(modelo3,extra[,c(4,5,6)],interval='confidence',level=0.99)
Fe3=rep(1,4)
for (i in 1:4){
Fe3[i]= pfe(extra[,4][i],extra[,5][i],extra[,6][i])
}
mod3 = data.frame(Fertility=Fe3)
matplot(mod3$Fertility, cbind(temp1,temp2[,-1]), lty=c(1,2,2,3,3), type="l",ylab="Predicted Fertility" )
