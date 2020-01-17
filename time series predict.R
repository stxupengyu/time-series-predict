#初始化
rm(list=ls())
setwd("C:/Users/Lenovo/Code/R/time series")
#导入数据 划分训练集测试集
gdp=ts(read.csv("data.csv",head=F),start=2000,freq=12)
gdp0=ts(gdp,start=2000,end=c(2006,12),freq=12)
gdp1=ts(gdp[85:96],start=c(2007,1),end=c(2007,12),freq=12)
#第一种方法 Holt-Winters指数平滑法
gdp.hw=HoltWinters(gdp0,seasonal = "multi")
#使用该方法分解时间序列
plot(gdp.hw$fitted,type="o",cex.axis=1.5,cex.lab=1.5)
#时间序列模型在训练集上的效果
plot(gdp.hw,type="o",cex.axis=1.5,cex.lab=1.5,cex=1,main="In Train Data")
legend("topleft",c("real","predict"),lty=1,col=1:2)
#时间序列模型在测试集上的效果
gdp.for=predict(gdp.hw,n.ahead=12*1)
ts.plot(gdp,gdp.for,type="l",lty=1:2,col=1:2,main="In Test Data")
legend(x="topleft",c("real","predict"),lty=c(1,2),col=1:2)
#预测未来20年的数据
gdp.hw=HoltWinters(gdp,seasonal = "multi")
gdp.for=predict(gdp.hw,n.ahead=12*20)
ts.plot(gdp,gdp.for,type="l",lty=1:2,col=1:2,main="In Test Data")
#导出预测的数据
write.csv(gdp.for,file = 'predict1.csv')



#第二种方法 ARIMA model
library("astsa")
#画出log后的图像以及对应的自相关系数
gdp.log=log(gdp)
par(mfrow=c(2,1))
plot(gdp.log,type="o")
acf(gdp.log)
par(mfrow=c(1,1))
#在对上述数据进行差分后，画原始图，自相关系数，偏自相关系数图gdp.diff=diff(gdp.log)
gdp.diff=diff(gdp.log)
par(mfrow=c(3,1),cex.axis=1.5,cex.lab=1.5)
plot(gdp.diff,type="o")
acf(gdp.diff)
pacf(gdp.diff)
par(mfrow=c(1,1))

#使用ARIMA模型进行预测
ma.fit=sarima(gdp.log,0,1,1,details=F)
ma.fit
gdp.for=sarima.for(gdp.log,12,0,1,1)
gdp.for=sarima.for(gdp.log,20*12,0,1,1)
#将预测数据写入csv文件
write.csv(gdp.for,file = 'predict2.csv')

#第三种方法 SARIMA Model
gdp.insample=window(gdp,start=c(2000,1))
#先预测一年的数据，效果不错
for1=sarima.for(gdp.insample,12,0,1,1,1,1,0,12)
#画图
ts.plot(gdp,for1$pred,gpars=list(lty=c(1,2)),col=1:2)
legend(x="topleft",c("real","predict"),lty=c(1,2),col=1:2)
#预测未来20年的数据
for2=sarima.for(gdp.insample,12*20,0,1,1,1,1,0,12)
#画图
ts.plot(gdp,for2$pred,gpars=list(lty=c(1,2)),col=1:2)
legend(x="topleft",c("real","predict"),lty=c(1,2),col=1:2)
#将预测结果写入csv文件
write.csv(for2$pred,file = 'predict3.csv')
