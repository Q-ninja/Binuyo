courier=read.csv("C:/Users/jamiu/Desktop/Math 6350-01 Stat. Learning/fonts/COURIER.csv", header=TRUE, sep=',', na.strings = "?")
calibri=read.csv("C:/Users/jamiu/Desktop/Math 6350-01 Stat. Learning/fonts/CALIBRI.csv",header=TRUE, sep=',', na.strings = "?")
times=read.csv("C:/Users/jamiu/Desktop/Math 6350-01 Stat. Learning/fonts/TIMES.csv", header=TRUE, sep=',', na.strings = "?")

courier =na.omit(courier)
calibri =na.omit(calibri)
times =na.omit(times)

courier=subset(courier, select=-c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
rw1=courier[1:12229,]
CL1=subset(rw1,rw1$strength==0.4 & rw1$italic==0)

calibri=subset(calibri, select=-c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
rw2=calibri[1:19068,]
CL2=subset(rw2,rw2$strength==0.4 & rw2$italic==0)

times=subset(times, select=-c(fontVariant, m_label, orientation, m_top, m_left, originalH, originalW, h, w))
rw3=times[1:12730,]
CL3=subset(rw3,rw3$strength==0.4 & rw3$italic==0)


n1=dim(CL1); n2=dim(CL2); n3=dim(CL3)
n1;n2;n3
DATA= rbind(CL1,CL2,CL3)
DATA= subset(DATA, select=-c(strength, italic,font ))

#PART 0
meandata=apply(DATA, 2, mean)
sddata=apply(DATA, 2, sd)

SDATA= apply(DATA, 2, scale)

hist(meandata, col = "yellow", xlab = "Mean")
hist(sddata, col = "brown", xlab = "Standard Deviations")

plot(meandata,sddata, col="red", pch=16, xlab = "Mean", ylab = "Standard Deviation")

#PART 1

#1.1

cor_SDATA=cor((SDATA))

#1.2
eigencor=eigen(cor_SDATA)
eigencor$values
eigencor$vectors
#1.3
plot(1:400, eigencor$values,  col="red", pch=16, xlab = "Features j", ylab = "Eigen Values")
#1.4

datalist = list()
for (i in 1:400)
{
   if (i==1)
   {
      Ri= (eigencor$values[1])
      print(Ri)
   }
   else 
   {
      Ri=(Ri + eigencor$values[i])
      print(Ri)
   }
   datalist[i] = Ri
   i=i+1
}
big_data = do.call(rbind, datalist)
big_data = big_data/400

#1.5
plot(1:400, big_data, col="red", pch=16, xlab = "Features j", ylab = "Rj")

pcDATA = prcomp(DATA,  scale=TRUE)
summary(pcDATA)
print(pcDATA)

#1.7
pcSDATA = prcomp(SDATA, scale=TRUE)
summary(pcSDATA)
print(pcSDATA)

#1.8
phi=eigencor$vectors[,1:3]
phi=-phi
colnames(phi)=c("PC1", "PC2", "PC3")
PC1=as.matrix(SDATA) %*% phi[,1]
PC2=as.matrix(SDATA) %*% phi[,2]
PC3=as.matrix(SDATA) %*% phi[,3]
PC=data.frame(data1=row.names(DATA), PC1, PC2, PC3)
head(PC)

scores1=pcSDATA$x[,1]
scores2=pcSDATA$x[,2]
scores3=pcSDATA$x[,3]

scores=pcSDATA$x

library(ggplot2)
# create data frame with scores
scores = as.data.frame(pcSDATA$x)

CL11= scores[1:4262,]
CL12=scores[4263:9030,]
CL13=scores[9031:13835,]

# plot of observations

#1.9

plot(CL11$PC1,CL11$PC2,xlim=range(c(CL11$PC1,CL12$PC1,CL13$PC1)),ylim=range(c(CL11$PC2,CL12$PC2,CL13$PC2)),
     col="red", pch=16, cex=0.5, xlab = "Principal Component 1", ylab="Principal Component 2", 
     main='2 Dimensional Scatter Plots', col.axis='red')
points(CL12$PC1,CL12$PC2,col="blue", pch=16, cex=0.5)
points(CL13$PC1,CL13$PC2,col="green", pch=16, cex=0.5)
legend(x=22, y=19, legend=c('CL1', 'CL2', 'CL3'), col=c('red', 'blue', 'green'), pch=c(16,16, 16))

#1.10a
library(scatterplot3d)
s3d=scatterplot3d(CL11$PC1,CL11$PC2,CL11$PC2,color="red",pch=16, box=TRUE,xlab="PC1",ylab="PC2",zlab="PC3", main = "3 Dimenstional Scatter Plots for the 3 scores")
s3d$points3d(CL12$PC1,CL12$PC2,CL12$PC2,col="blue",pch=16)
s3d$points3d(CL13$PC1,CL13$PC2,CL13$PC2,col="green", pch=16) 
legend(s3d$xyz.convert(30, 5, 5), col= c("red","blue", "green"),pch=16, legend = c("CL1", "CL2", "CL3"), cex = 0.8)

?plot3d


#1.10ba-1
s3da=scatterplot3d(CL11$PC1,CL11$PC2,CL11$PC2,color="red",pch=16, box=TRUE,xlab="PC1",ylab="PC2",zlab="PC3", main = "3 Dimenstional Scatter Plots for CL1 and CL2 scores")
s3da$points3d(CL12$PC1,CL12$PC2,CL12$PC2,col="blue",pch=16)
legend(s3d$xyz.convert(30, 5, 5), col= c("red","blue"),pch=16, legend = c("CL1", "CL2"), cex = 0.8)


#1.10ba-2
s3db=scatterplot3d(CL11$PC1,CL11$PC2,CL11$PC2,color="red",pch=16, box=TRUE,xlab="PC1",ylab="PC2",zlab="PC3", main = "3 Dimenstional Scatter Plots for the CL1 and CL3 scores")
s3db$points3d(CL13$PC1,CL13$PC2,CL13$PC2,col="green",pch=16) 
legend(s3d$xyz.convert(30, 5, 5), col= c("red","green"),pch=16, legend = c("CL1", "CL3"), cex = 0.8)


#1.10ba-3
s3d=scatterplot3d(CL12$PC1,CL12$PC2,CL12$PC2,color="blue",pch=16, box=TRUE,xlab="PC1",ylab="PC2",zlab="PC3", main = "3 Dimenstional Scatter Plots for the CL2 and CL3 scores")
s3d$points3d(CL13$PC1,CL13$PC2,CL13$PC2,col="green", pch=16)
legend(s3d$xyz.convert(30, 5, 5), col= c("blue", "green"),pch=16, legend = c("CL2", "CL3"), cex = 0.8)



#2.1 k=15
library(class)
ran = sample(1:nrow(SDATA), 0.8 * nrow(SDATA))
SDATA_train <- SDATA[ran,] 
SDATA_test <- SDATA[-ran,] 
SDATA_train_labels=SDATA[ran,2]
SDATA_test_labels=SDATA[-ran,2]

kn15 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=15)  #k=15
ACC15 = 100 * sum(SDATA_test_labels == kn15)/NROW(SDATA_test_labels)
Correct15=percent(ACC15)
print(Correct15)

#2.2
kn5 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=5)
ACC5= 100 * sum(SDATA_test_labels == kn5)/NROW(SDATA_test_labels)

kn10 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=10)
ACC10= 100 * sum(SDATA_test_labels == kn10)/NROW(SDATA_test_labels)

kn15 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=15)
ACC15= 100 * sum(SDATA_test_labels == kn15)/NROW(SDATA_test_labels)

kn20 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=20)
ACC20= 100 * sum(SDATA_test_labels == kn20)/NROW(SDATA_test_labels)

kn30 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=30)
ACC30= 100 * sum(SDATA_test_labels == kn30)/NROW(SDATA_test_labels)

kn40 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=40)
ACC40= 100 * sum(SDATA_test_labels == kn40)/NROW(SDATA_test_labels)

kn50 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=50)
ACC50= 100 * sum(SDATA_test_labels == kn50)/NROW(SDATA_test_labels)

kn100 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=100)
ACC100= 100 * sum(SDATA_test_labels == kn100)/NROW(SDATA_test_labels)

kn200 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=200)
ACC200= 100 * sum(SDATA_test_labels == kn200)/NROW(SDATA_test_labels)

a =list (c("K5", (ACC5)),c("K10", (ACC10)),c("K15", (ACC15)),c("K20", (ACC20)),c("K30", (ACC30)),
         c("K40", (ACC40)), c("K50", (ACC50)), c("K100", (ACC100)), c("K200", (ACC200)))
accuracy1 = data.frame(a)  
kvalues = c(5,10,15, 20, 30, 40, 50 ,100,200) 
accuracy= c((ACC5),(ACC10),(ACC15), (ACC20), (ACC30), (ACC40), (ACC50) ,(ACC100),(ACC200)) 

df= list(Kvalues=c(5,10,15, 20, 30, 40, 50 ,100,200), accuracy=c((ACC5), (ACC10),(ACC15), (ACC20), (ACC30), (ACC40), (ACC50),(ACC100),(ACC200)))
df=as.data.frame(df)

ggplot(data=df, aes(x=kvalues, y=accuracy)) + geom_line(linetype="dashed", color="blue", size=1.2) + geom_point(color="red", size=3)
print(accuracy1)   

plot(kvalues, accuracy, col="red", pch=16, cex=0.5, xlab = "K Values", ylab="Percentage per (K)", 
     main='Correct Classification', col.axis='red')

#2.3 for range [5,15]

kn6 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=6)
ACC6= 100 * sum(SDATA_test_labels == kn6)/NROW(SDATA_test_labels)

kn7 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=7)
ACC7= 100 * sum(SDATA_test_labels == kn7)/NROW(SDATA_test_labels)

kn8 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=8)
ACC8= 100 * sum(SDATA_test_labels == kn8)/NROW(SDATA_test_labels)

kn9 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=9)
ACC9= 100 * sum(SDATA_test_labels == kn9)/NROW(SDATA_test_labels)

kn11 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=11)
ACC11= 100 * sum(SDATA_test_labels == kn11)/NROW(SDATA_test_labels)

kn12 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=12)
ACC12= 100 * sum(SDATA_test_labels == kn12)/NROW(SDATA_test_labels)

kn13 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=13)
ACC13= 100 * sum(SDATA_test_labels == kn13)/NROW(SDATA_test_labels)

kn14 = knn(SDATA_train,SDATA_test,cl=SDATA_train_labels, k=14)
ACC14= 100 * sum(SDATA_test_labels == kn14)/NROW(SDATA_test_labels)


b =list (c("K5", (ACC5)),c("K6", (ACC6)),c("K7", (ACC7)),c("K8", (ACC8)),c("K9", (ACC9)),
         c("K10", (ACC10)), c("K11", (ACC11)), c("K12", (ACC12)), c("K13", (ACC13)), c("K14", (ACC14)), 
         c("K15", (ACC15)))
accuracyb = data.frame(b)


kvalues2 = c(5,6,7,8, 9, 10, 11 ,12,13, 14, 15) 
accuracy2= c((ACC5),(ACC6),(ACC7), (ACC8), (ACC9), (ACC10), (ACC11),(ACC12),(ACC13), (ACC14), (ACC15)) 

df2= list(Kvalues2=c(5,6,7,8, 9, 10, 11 ,12,13, 14, 15), accuracy2=c((ACC5),(ACC6),(ACC7), (ACC8), (ACC9), (ACC10),
                                                                     (ACC11) ,(ACC12),(ACC13), (ACC14), (ACC15)))
df2=as.data.frame(df2)

plot(kvalues2, accuracy2, col="red", pch=16, cex=0.5, xlab = "K Values", ylab="Percentage per (K)", 
     main='Correct Classification', col.axis='red')

print(accuracyb)


#2.4 confusion matrix for best k
confusionMatrix(kn7 ,SDATA_test_labels)
