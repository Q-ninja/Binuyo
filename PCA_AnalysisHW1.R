autodata1=read.csv("C:/Users/jamiu/Desktop/Auto.csv",header=TRUE,sep = ',',na.strings = "?")
autodata1=na.omit(autodata1)
autodata1=subset(autodata1, select=-c(year,origin,name))
nrow(autodata1); attach(autodata1);
cyl=cylinders; dis=displacement; hor=horsepower; wei = weight ; acc=acceleration #Lines: 1-5 11s
mean(cyl);sd(cyl);mean(dis);sd(dis);mean(hor);sd(hor);mean(wei);sd(wei);mean(acc);sd(acc) #4s
hist(mpg, col = "yellow"); hist(cyl, col = "brown"); hist(dis,col = "red");hist(hor,col="green");
hist(wei,col = "red"); hist(acc,col = "blue") #Lines: 7-8 17s
plot(cyl, mpg);plot(dis, mpg);plot(hor, mpg);plot(wei, mpg);plot(acc, mpg) #22s
autodata4 = apply(autodata1, 2, scale); cor(cyl,mpg); cor(dis, mpg); cor(hor, mpg); cor(wei, mpg);
cor(acc, mpg) #Lines: 10-11 5s
autodata2=subset(autodata4, select = -c(mpg)); autodata2cov=cov(autodata2); #4s
autodata2cor=cor(autodata2)
autodata2eigen=eigen(autodata2cor)
eigenverify=autodata2eigen$values[1]+autodata2eigen$values[2]+autodata2eigen$values[3]+autodata2eigen$values[4]+autodata2eigen$values[5]
L1=autodata2eigen$values[1];L2=autodata2eigen$values[2];L3=autodata2eigen$values[3];L4=autodata2eigen$values[4];L5=autodata2eigen$values[5] #Lines: 13-18 6s
R1=L1/5;R2=(L1+L2)/5;R3=(L1+L2+L3)/5;R4=(L1+L2+L3+L4)/5;R5=(L1+L2+L3+L4+L5)/5 #2s
autodata3=autodata1[order(mpg),]; autodata3=data.frame(autodata3)
medianmpg=median(autodata3$mpg);LOWmpg=subset(autodata3,autodata3$mpg<medianmpg);HIGHmpg=subset(autodata3,autodata3$mpg>medianmpg) #Lines: 19-22 7s
par(mfrow=c(1,2)); hist(LOWmpg$cylinders, col = "red"); hist(HIGHmpg$cylinders, col = "red")
par(mfrow=c(1,2)); hist(LOWmpg$displacement, col="green") ; hist(HIGHmpg$displacement,col="green")
par(mfrow=c(1,2)); hist(LOWmpg$horsepower, col = "red"); hist(HIGHmpg$horsepower, col = "red")
par(mfrow=c(1,2)); hist(LOWmpg$weight, col = "blue"); hist(HIGHmpg$weight, col = "blue")
par(mfrow=c(1,2)); hist(LOWmpg$acceleration, col = "yellow"); hist(HIGHmpg$acceleration, col ="yellow") #Lines: 24-29 32s
apply(LOWmpg, 2, mean)
apply(LOWmpg, 2, sd)
apply(HIGHmpg, 2, mean)
apply(HIGHmpg, 2, sd) #Lines: 30-33 11s

DISCR = function(f) {
result = (abs(mean(HIGHmpg[,f])-mean(LOWmpg[,f])))/(((sd(LOWmpg[,f]) + sd(HIGHmpg[,f])))/(sqrt(nrow(HIGHmpg))))
return(result)   
}

DISCR('cylinders')
DISCR('displacement')
DISCR('horsepower')
DISCR('weight')
DISCR('acceleration') #Lines: 35-45 17s

x = function(f) {
result = LOWmpg[,f]
return(result)
 }

y = function(f) {
result = HIGHmpg[,f]
return(result)
   }

 t = function(f) {
result = t.test(x(f), y(f))
 return(result)
 }
t.test(x('cylinders'), y('cylinders'))
t.test(x('displacement'), y('displacement'))
t.test(x('horsepower'), y('horsepower'))
t.test(x('weight'), y('weight'))
t.test(x('acceleration'), y('acceleration')) #Lines: