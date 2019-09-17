rawdata2=read.table ("C:/Users/jamiu/Desktop/Math 6357 Linear Models & Design Experiment/Prof. Fu Class/CH01PR28.txt")
rawdata2$X3=rawdata2$V2-mean(rawdata2$V2)
rawdata2$Y3=rawdata2$V1-mean(rawdata2$V1)
rawdata2$XY=rawdata2$X3*rawdata2$Y3
rawdata2$X4=rawdata2$X3^2
rawdata2$Y4=rawdata2$Y3^2
meanX=mean(rawdata2$V2)
meanY=mean(rawdata2$V1)
sumXY=sum(rawdata2$XY)
sumX4=sum(rawdata2$X4)
sumY4=sum(rawdata2$Y4)
b1=(sumXY)/sumX4
b0=meanY-(b1*meanX)
plot(rawdata2$V2,rawdata2$V1,xlab="Percentage in County with Atleast a High-School Diploma (X)",ylab = "Crime Rate (reported per 100,000 Residents) (Y)", pch=19)
abline(b0,b1,pch=16)