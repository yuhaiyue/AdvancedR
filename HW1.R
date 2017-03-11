##Advance R Class Test1

#1.测试：计算在y向量中数字1连续出现2次的位置。
#法一：
y<- c(1,0,0,1,1,1,0,1,1)
output<-c()
for (i in 1:(length(y)-1)) {
        if(y[i]==1 & y[i+1]==1)
                output = c(output,i)
}
output

which(y==1 )
##法二：
a<-lapply(1:(length(y)-1), function(i){if(y[i]==1 & y[i+1]==1) output = c(output,i)})

#第一道题目继续更新- 
#测试：编写一个函数findNum(y,x,l)计算在y向量中数字x连续出现l次的位置。

y<- c(1,0,0,1,1,1,0,1,1)
output<-c()
findNum = function(y,x,l){
        output = c()
        for (i in 1:(length(y)-l+1)) {
                if(all(y[i:(i+l-1)]==x))
                output = c(output,i)
        }
        return(output)
}
findNum(y,1,2)


#2.测试：计算每天最低气温和最高气温的差值
raw <- read.delim("data/weather.txt",check.names = F, na.strings = ".")
a<-melt(raw, id=c("year","month","element"));
b<-dcast(a,year+month+variable~element,value.var = "value")
b$diff<-b$tmax-b$tmin


#3.测试：计算每个航空公司(UniqueCarrier)每个月到达延误时间(ArrDelay)的十分位(quantile)的均值。
library(hflights)
str(hflights)

data<-data.frame(UniqueCarrier=hflights$UniqueCarrier,Month=hflights$Month,Day=hflights$DayofMonth,ArrDelay=hflights$ArrDelay)
group1<-group_by(data,UniqueCarrier,Month)
quantile.permonth<-summarise(group1, q1=quantile(ArrDelay,prob=0.1,na.rm=T),q2=quantile(ArrDelay,prob=0.2,na.rm=T),q3=quantile(ArrDelay,prob=0.3,na.rm=T),q4=quantile(ArrDelay,prob=0.4,na.rm=T),q5=quantile(ArrDelay,prob=0.5,na.rm=T),q6=quantile(ArrDelay,prob=0.6,na.rm=T),q7=quantile(ArrDelay,prob=0.7,na.rm=T),q8=quantile(ArrDelay,prob=0.8,na.rm=T),q9=quantile(ArrDelay,prob=0.9,na.rm=T))
group2<-group_by(quantile.permonth,UniqueCarrier)
mean_of_quantile<-summarise(group2,mean(q1),mean(q2),mean(q3),mean(q4),mean(q5),mean(q6),mean(q7),mean(q8),mean(q9))




