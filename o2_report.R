#设置工作空间载入包
setwd("d:/data_project/o2o")
library(ggplot2)
library(dplyr)
library(InformationValue)
library(parallel)
library(ROCR)
#数据导入和查看
train<-read.table(file = "ccf_offline_stage1_train.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE,na.strings = "null")
test<-read.table(file="ccf_offline_stage1_test_revised.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE,na.strings = "null")
str(train)#查看数据结构
str(test)
train<-select(train,user=User_id,shop=Merchant_id,coupon=Coupon_id,discount=Discount_rate,distance=Distance,date_r=Date_received,date_u=Date)
test<-select(test,user=User_id,shop=Merchant_id,coupon=Coupon_id,discount=Discount_rate,distance=Distance,date_r=Date_received)
head(train)#显示训练集前六行
summary(train)#总览数据概况

#统计优惠券使用情况
#没领券直接消费
sum(is.na(train$coupon))/nrow(train) 
#领券，消费
sum(!is.na(train$coupon)&!is.na(train$date_u))/nrow(train)
#领券，没消费
sum(!is.na(train$coupon)&is.na(train$date_u))/nrow(train)
#用券消费占所有消费比例
sum(!is.na(train$coupon)&!is.na(train$date_u))/sum(!is.na(train$date_u))
#优惠券被使用的概率
sum(!is.na(train$coupon)&!is.na(train$date_u))/sum(!is.na(train$coupon))


#特征工程
#提取有优惠券信息的子集
train_1<-filter(train,!is.na(train$discount))

#增加优惠券是否15天内使用特征
#计算优惠券的使用时间
train_1$date_r<-as.Date(as.character(train_1$date_r),"%Y%m%d")#转换领券日期格式
train_1$date_u<-as.Date(as.character(train_1$date_u),"%Y%m%d")#转换消费日期格式
train_1$time<-difftime(train_1$date_u,train_1$date_r,units="days")
train_1$time<-as.integer(as.character(train_1$time))
#查看使用的优惠券的领取天数分布
ggplot(train_1,aes(x=train_1$time))+
  geom_bar()+
  labs(x="使用日和领券日的时间间隔（天）")

#定义函数，判断优惠券是否在15天内使用,0表示没使用，1表示使用
use<-function(x)
  if (is.na(x)|x>15) {return(0)}else 
    return(1)
#增加特征
train_1$use<-sapply(train_1$time,use)
train_1<-select(train_1,user,shop,coupon,use,discount,distance,date_r,date_u)#删除time列
table(train_1$use)#看看正负样本比例


#增加打折特征
#查看打折率的内容
unique(train_1$discount)
#增加优惠券打折方式，折扣率，使用门槛特征
discounttype<-function(x)#打折方式函数，无打折为NA,满减为1,直接打折为0
  if (is.na(x)) {return(NA)}else 
    if (grepl(":",x)) {return(1)}else
      return(0)

discountrate<-function(x)#打折率函数
  if (is.na(x)) {return(1)}else 
    if (grepl(":",x)) 
    {a<-strsplit(x,":")
    return(1-as.numeric(unlist(a)[2])/as.numeric(unlist(a)[1]))}else 
      return(as.numeric(x))

man<-function(x)#满多少函数
  if (grepl(":",x))
  {return(as.numeric(unlist(strsplit(x,":"))[1]))
  }else return(0)

#增加三列特征
cl <- makeCluster(6)
train_1$type<-parSapply(cl,train_1$discount,discounttype)
train_1$rate<-parSapply(cl,train_1$discount,discountrate)
train_1$man<-parSapply(cl,train_1$discount,man)
train_1<-train_1[,-5]
head(train_1)

#1优惠券类型的影响
ggplot(train_1,aes(x=factor(train_1$type),fill=factor(train_1$use)))+#对比两种优惠券的使用率
  geom_bar(position = "dodge")+
  geom_text(stat = "count",aes(label=..count..),
            position = position_dodge(width = 1),vjust=-0.5)+
  labs(x="优惠券类别",fill="是否使用")
prop.table(table(train_1$type,train_1$use),1)#计算两类优惠券使用率
IV(X=factor(train_1$type),Y=train_1$use)#计算信息价值

#2优惠券折扣率影响
#对比两种优惠券的折扣率
ggplot(train_1,aes(x=factor(train_1$type),y=train_1$rate))+
  geom_boxplot()+
  labs(x="优惠券类别",y="折扣")
#对比不同折扣的使用率
rate_cut<-cut(train_1$rate,breaks=c(0,0.5,0.7,0.8,0.9,1),labels = c("<0.5","0.5-0.7","0.7-0.8","0.8-0.9",">0.9"))
ggplot(train_1,aes(x=rate_cut,fill=factor(train_1$use)))+
  geom_bar(position = "dodge")+
  labs(x="打折率",fill="是否使用")
prop.table(table(train_1$use,rate_cut),2)#计算使用率
IV(X=factor(train_1$rate),Y=train_1$use)#计算信息价值

#3使用门槛的影响
#对比不同门槛的使用率
ggplot(train_1,aes(x=factor(train_1$man),fill=factor(train_1$use)))+
  geom_bar(position = "dodge")+
  labs(x="使用门槛",fill="是否使用")
prop.table(table(train_1$use,train_1$man),2)#计算使用率
IV(X=factor(train_1$man),Y=train_1$use)#计算信息价值


#距离特征
#对比不同距离对使用率的影响
ggplot(train_1,aes(x=train_1$distance,fill=factor(train_1$use)))+
  geom_bar(position = "dodge")+
  labs(x="距离",fill="是否使用")
IV(X=factor(train_1$distance),Y=train_1$use)#计算信息价值
prop.table(table(train_1$use,train_1$distance),2)#计算不同距离优惠券使用率

#计算距离缺失比例
mean(is.na(train_1$distance))
#距离缺失值回归插补
a<-which(is.na(train_1$distance))
train_1_1<-train_1[-a,]
train_1_2<-train_1[a,]
model<-lm(distance~use,data = train_1_1)
train_1_2$distance<-predict(model,train_1_2)
train_1<-rbind(train_1_1,train_1_2)


#优惠券领取日星期名的特征
#增加领券的星期名
train_1$week_r<-format(train_1$date_r,format = "%a")
head(train_1)
#对比不同星期名的使用率
ggplot(train_1,aes(x=train_1$week_r,fill=factor(use)))+
  geom_bar(position = "dodge")+
  labs(x="",fill="是否使用")
prop.table(table(train_1$use,train_1$week_r),2)#计算使用概率
IV(X=factor(train_1$week_r),Y=train_1$use)#计算信息价值
#生成星期的独热编码
#a<-as.data.frame(model.matrix(~week_r-1,train_1))
#train_1<-bind_cols(train_1,a)


#优惠券出现次数特征
#计算优惠券的领取总次数
coupon_f<-as.data.frame(table(train_1$coupon))
coupon_f$Var1<-as.integer(as.character(coupon_f$Var1))
#计算日均领取次数
day_1<-difftime(max(train_1$date_r),min(train_1$date_r),units="days")
day_1<-as.integer(day_1)
coupon_f$Freq<-coupon_f$Freq/(day_1+1)
#每张优惠券的日均领取次数赋值
#构建赋值函数
coupon_cishu<-function(x)
{return(coupon_f$Freq[which(coupon_f$Var1==x)])}
#赋值
train_1$coupon_cishu<-sapply(train_1$coupon,coupon_cishu)
head(train_1)
#作图对比不同领取次数优惠券的使用率
cut_1<-cut(train_1$coupon_cishu,breaks = c(0,5,10,50,100,1000),labels =c("<5","5-10","10-50","50-100",">100"))
ggplot(train_1,aes(x=cut_1,fill=factor(train_1$use)))+
  geom_bar(position = "fill")+
  labs(x="优惠券日均领取次数",y="比例",fill="是否使用")
#计算信息价值
IV(X=factor(train_1$coupon_cishu),Y=train_1$use)


#商店名出现次数特征
#计算不同商店的总出现次数
shop_f<-as.data.frame(table(train_1$shop))
shop_f$Var1<-as.integer(as.character(shop_f$Var1))
#计算商店的日均出现次数
shop_f$Freq<-shop_f$Freq/(day_1+1)
#每家商店的日均出现次数赋值
#构建赋值函数
shop_cishu<-function(x)
{return(shop_f$Freq[which(shop_f$Var1==x)])}
#赋值
train_1$shop_cishu<-sapply(train_1$shop,shop_cishu)
head(train_1)

#作图对比商店出现次数对使用率的影响
cut_2<-cut(train_1$shop_cishu,breaks = c(0,10,50,100,200,300,1000),labels =c("<10","10-50","50-100","100-200","200-300",">100"))
ggplot(train_1,aes(x=cut_2,fill=factor(train_1$use)))+
  geom_bar(position = "fill")+
  labs(x="商店日均次数",y="比例",fill="是否使用")
#计算信息价值
IV(X=factor(train_1$shop_cishu),Y=train_1$use)


#建立模型
#训练集1月1日-5月15日，测试集为5月16日-6月15日
day_1<-as.Date("2016-05-15")
day_2<-as.Date("2016-06-15")
train_data<-filter(train_1,train_1$date_r<=day_1)
test_data<-filter(train_1,train_1$date_r>day_1 & train_1$date_r<=day_2)
str(train_data)
str(test_data)
a<-test_data$use
test_data<-test_data[,-4]

#逻辑回归
model_log<-glm(use~type+week_r+rate+man+distance+coupon_cishu+shop_cishu,family=binomial(),
               data=train_data)
#预测
predict<-predict(model_log,type = "response",newdata = test_data)
#计算AUC
pred<-prediction(predict,a)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=T)
performance(pred, "auc")

#去掉week_r,再次逻辑回归
model_log<-glm(use~type+week_r+rate+man+distance+coupon_cishu+shop_cishu,family=binomial(),
               data=train_data)
#预测
predict<-predict(model_log,type = "response",newdata = test_data)
#计算AUC
pred<-prediction(predict,a)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=T)
performance(pred, "auc")


#测试集特征构建
#打折类型，打折率，使用门槛
test$type<-parSapply(cl,test$discount,discounttype)
test$rate<-parSapply(cl,test$discount,discountrate)
test$man<-parSapply(cl,test$discount,man)

#优惠券日均领取数量
coupon_f_1<-as.data.frame(table(test$coupon))
coupon_f_1$Var1<-as.integer(as.character(coupon_f_1$Var1))
#计算日均领取次数
coupon_f_1$Freq<-coupon_f_1$Freq/31
#每张优惠券的日均领取次数赋值
coupon_cishu<-function(x)
{return(coupon_f_1$Freq[which(coupon_f_1$Var1==x)])}
test$coupon_cishu<-sapply(test$coupon,coupon_cishu)

#商店名出现次数
shop_f_1<-as.data.frame(table(test$shop))
shop_f_1$Var1<-as.integer(as.character(shop_f_1$Var1))
#商店名日均出现次数赋值
shop_f_1$Freq<-shop_f_1$Freq/31
shop_cishu<-function(x)
{return(shop_f_1$Freq[which(shop_f_1$Var1==x)])}
test$shop_cishu<-sapply(test$shop,shop_cishu)

#距离插补
summary(test$distance)
test[is.na(test$distance),]$distance<-2.3

#训练最终模型并预测
model_log<-glm(use~type+rate+man+distance+coupon_cishu+shop_cishu,family=binomial(),
               data=train_1)
predict<-predict(model_log,type = "response",newdata = test)
#导出预测结果
submission<-select(test,user,coupon,date_r)
submission<-cbind(submission,predict)
write.csv(submission, file = 'd:/data_project/o2o/predict_Solution.csv',row.names = F)

