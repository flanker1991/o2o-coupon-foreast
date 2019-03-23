#���ù����ռ������
setwd("d:/data_project/o2o")
library(ggplot2)
library(dplyr)
library(InformationValue)
library(parallel)
library(ROCR)
#���ݵ���Ͳ鿴
train<-read.table(file = "ccf_offline_stage1_train.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE,na.strings = "null")
test<-read.table(file="ccf_offline_stage1_test_revised.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE,na.strings = "null")
str(train)#�鿴���ݽṹ
str(test)
train<-select(train,user=User_id,shop=Merchant_id,coupon=Coupon_id,discount=Discount_rate,distance=Distance,date_r=Date_received,date_u=Date)
test<-select(test,user=User_id,shop=Merchant_id,coupon=Coupon_id,discount=Discount_rate,distance=Distance,date_r=Date_received)
head(train)#��ʾѵ����ǰ����
summary(train)#�������ݸſ�

#ͳ���Ż�ȯʹ�����
#û��ȯֱ������
sum(is.na(train$coupon))/nrow(train) 
#��ȯ������
sum(!is.na(train$coupon)&!is.na(train$date_u))/nrow(train)
#��ȯ��û����
sum(!is.na(train$coupon)&is.na(train$date_u))/nrow(train)
#��ȯ����ռ�������ѱ���
sum(!is.na(train$coupon)&!is.na(train$date_u))/sum(!is.na(train$date_u))
#�Ż�ȯ��ʹ�õĸ���
sum(!is.na(train$coupon)&!is.na(train$date_u))/sum(!is.na(train$coupon))


#��������
#��ȡ���Ż�ȯ��Ϣ���Ӽ�
train_1<-filter(train,!is.na(train$discount))

#�����Ż�ȯ�Ƿ�15����ʹ������
#�����Ż�ȯ��ʹ��ʱ��
train_1$date_r<-as.Date(as.character(train_1$date_r),"%Y%m%d")#ת����ȯ���ڸ�ʽ
train_1$date_u<-as.Date(as.character(train_1$date_u),"%Y%m%d")#ת���������ڸ�ʽ
train_1$time<-difftime(train_1$date_u,train_1$date_r,units="days")
train_1$time<-as.integer(as.character(train_1$time))
#�鿴ʹ�õ��Ż�ȯ����ȡ�����ֲ�
ggplot(train_1,aes(x=train_1$time))+
  geom_bar()+
  labs(x="ʹ���պ���ȯ�յ�ʱ�������죩")

#���庯�����ж��Ż�ȯ�Ƿ���15����ʹ��,0��ʾûʹ�ã�1��ʾʹ��
use<-function(x)
  if (is.na(x)|x>15) {return(0)}else 
    return(1)
#��������
train_1$use<-sapply(train_1$time,use)
train_1<-select(train_1,user,shop,coupon,use,discount,distance,date_r,date_u)#ɾ��time��
table(train_1$use)#����������������


#���Ӵ�������
#�鿴�����ʵ�����
unique(train_1$discount)
#�����Ż�ȯ���۷�ʽ���ۿ��ʣ�ʹ���ż�����
discounttype<-function(x)#���۷�ʽ�������޴���ΪNA,����Ϊ1,ֱ�Ӵ���Ϊ0
  if (is.na(x)) {return(NA)}else 
    if (grepl(":",x)) {return(1)}else
      return(0)

discountrate<-function(x)#�����ʺ���
  if (is.na(x)) {return(1)}else 
    if (grepl(":",x)) 
    {a<-strsplit(x,":")
    return(1-as.numeric(unlist(a)[2])/as.numeric(unlist(a)[1]))}else 
      return(as.numeric(x))

man<-function(x)#�����ٺ���
  if (grepl(":",x))
  {return(as.numeric(unlist(strsplit(x,":"))[1]))
  }else return(0)

#������������
cl <- makeCluster(6)
train_1$type<-parSapply(cl,train_1$discount,discounttype)
train_1$rate<-parSapply(cl,train_1$discount,discountrate)
train_1$man<-parSapply(cl,train_1$discount,man)
train_1<-train_1[,-5]
head(train_1)

#1�Ż�ȯ���͵�Ӱ��
ggplot(train_1,aes(x=factor(train_1$type),fill=factor(train_1$use)))+#�Ա������Ż�ȯ��ʹ����
  geom_bar(position = "dodge")+
  geom_text(stat = "count",aes(label=..count..),
            position = position_dodge(width = 1),vjust=-0.5)+
  labs(x="�Ż�ȯ���",fill="�Ƿ�ʹ��")
prop.table(table(train_1$type,train_1$use),1)#���������Ż�ȯʹ����
IV(X=factor(train_1$type),Y=train_1$use)#������Ϣ��ֵ

#2�Ż�ȯ�ۿ���Ӱ��
#�Ա������Ż�ȯ���ۿ���
ggplot(train_1,aes(x=factor(train_1$type),y=train_1$rate))+
  geom_boxplot()+
  labs(x="�Ż�ȯ���",y="�ۿ�")
#�ԱȲ�ͬ�ۿ۵�ʹ����
rate_cut<-cut(train_1$rate,breaks=c(0,0.5,0.7,0.8,0.9,1),labels = c("<0.5","0.5-0.7","0.7-0.8","0.8-0.9",">0.9"))
ggplot(train_1,aes(x=rate_cut,fill=factor(train_1$use)))+
  geom_bar(position = "dodge")+
  labs(x="������",fill="�Ƿ�ʹ��")
prop.table(table(train_1$use,rate_cut),2)#����ʹ����
IV(X=factor(train_1$rate),Y=train_1$use)#������Ϣ��ֵ

#3ʹ���ż���Ӱ��
#�ԱȲ�ͬ�ż���ʹ����
ggplot(train_1,aes(x=factor(train_1$man),fill=factor(train_1$use)))+
  geom_bar(position = "dodge")+
  labs(x="ʹ���ż�",fill="�Ƿ�ʹ��")
prop.table(table(train_1$use,train_1$man),2)#����ʹ����
IV(X=factor(train_1$man),Y=train_1$use)#������Ϣ��ֵ


#��������
#�ԱȲ�ͬ�����ʹ���ʵ�Ӱ��
ggplot(train_1,aes(x=train_1$distance,fill=factor(train_1$use)))+
  geom_bar(position = "dodge")+
  labs(x="����",fill="�Ƿ�ʹ��")
IV(X=factor(train_1$distance),Y=train_1$use)#������Ϣ��ֵ
prop.table(table(train_1$use,train_1$distance),2)#���㲻ͬ�����Ż�ȯʹ����

#�������ȱʧ����
mean(is.na(train_1$distance))
#����ȱʧֵ�ع�岹
a<-which(is.na(train_1$distance))
train_1_1<-train_1[-a,]
train_1_2<-train_1[a,]
model<-lm(distance~use,data = train_1_1)
train_1_2$distance<-predict(model,train_1_2)
train_1<-rbind(train_1_1,train_1_2)


#�Ż�ȯ��ȡ��������������
#������ȯ��������
train_1$week_r<-format(train_1$date_r,format = "%a")
head(train_1)
#�ԱȲ�ͬ��������ʹ����
ggplot(train_1,aes(x=train_1$week_r,fill=factor(use)))+
  geom_bar(position = "dodge")+
  labs(x="",fill="�Ƿ�ʹ��")
prop.table(table(train_1$use,train_1$week_r),2)#����ʹ�ø���
IV(X=factor(train_1$week_r),Y=train_1$use)#������Ϣ��ֵ
#�������ڵĶ��ȱ���
#a<-as.data.frame(model.matrix(~week_r-1,train_1))
#train_1<-bind_cols(train_1,a)


#�Ż�ȯ���ִ�������
#�����Ż�ȯ����ȡ�ܴ���
coupon_f<-as.data.frame(table(train_1$coupon))
coupon_f$Var1<-as.integer(as.character(coupon_f$Var1))
#�����վ���ȡ����
day_1<-difftime(max(train_1$date_r),min(train_1$date_r),units="days")
day_1<-as.integer(day_1)
coupon_f$Freq<-coupon_f$Freq/(day_1+1)
#ÿ���Ż�ȯ���վ���ȡ������ֵ
#������ֵ����
coupon_cishu<-function(x)
{return(coupon_f$Freq[which(coupon_f$Var1==x)])}
#��ֵ
train_1$coupon_cishu<-sapply(train_1$coupon,coupon_cishu)
head(train_1)
#��ͼ�ԱȲ�ͬ��ȡ�����Ż�ȯ��ʹ����
cut_1<-cut(train_1$coupon_cishu,breaks = c(0,5,10,50,100,1000),labels =c("<5","5-10","10-50","50-100",">100"))
ggplot(train_1,aes(x=cut_1,fill=factor(train_1$use)))+
  geom_bar(position = "fill")+
  labs(x="�Ż�ȯ�վ���ȡ����",y="����",fill="�Ƿ�ʹ��")
#������Ϣ��ֵ
IV(X=factor(train_1$coupon_cishu),Y=train_1$use)


#�̵������ִ�������
#���㲻ͬ�̵���ܳ��ִ���
shop_f<-as.data.frame(table(train_1$shop))
shop_f$Var1<-as.integer(as.character(shop_f$Var1))
#�����̵���վ����ִ���
shop_f$Freq<-shop_f$Freq/(day_1+1)
#ÿ���̵���վ����ִ�����ֵ
#������ֵ����
shop_cishu<-function(x)
{return(shop_f$Freq[which(shop_f$Var1==x)])}
#��ֵ
train_1$shop_cishu<-sapply(train_1$shop,shop_cishu)
head(train_1)

#��ͼ�Ա��̵���ִ�����ʹ���ʵ�Ӱ��
cut_2<-cut(train_1$shop_cishu,breaks = c(0,10,50,100,200,300,1000),labels =c("<10","10-50","50-100","100-200","200-300",">100"))
ggplot(train_1,aes(x=cut_2,fill=factor(train_1$use)))+
  geom_bar(position = "fill")+
  labs(x="�̵��վ�����",y="����",fill="�Ƿ�ʹ��")
#������Ϣ��ֵ
IV(X=factor(train_1$shop_cishu),Y=train_1$use)


#����ģ��
#ѵ����1��1��-5��15�գ����Լ�Ϊ5��16��-6��15��
day_1<-as.Date("2016-05-15")
day_2<-as.Date("2016-06-15")
train_data<-filter(train_1,train_1$date_r<=day_1)
test_data<-filter(train_1,train_1$date_r>day_1 & train_1$date_r<=day_2)
str(train_data)
str(test_data)
a<-test_data$use
test_data<-test_data[,-4]

#�߼��ع�
model_log<-glm(use~type+week_r+rate+man+distance+coupon_cishu+shop_cishu,family=binomial(),
               data=train_data)
#Ԥ��
predict<-predict(model_log,type = "response",newdata = test_data)
#����AUC
pred<-prediction(predict,a)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=T)
performance(pred, "auc")

#ȥ��week_r,�ٴ��߼��ع�
model_log<-glm(use~type+week_r+rate+man+distance+coupon_cishu+shop_cishu,family=binomial(),
               data=train_data)
#Ԥ��
predict<-predict(model_log,type = "response",newdata = test_data)
#����AUC
pred<-prediction(predict,a)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=T)
performance(pred, "auc")


#���Լ���������
#�������ͣ������ʣ�ʹ���ż�
test$type<-parSapply(cl,test$discount,discounttype)
test$rate<-parSapply(cl,test$discount,discountrate)
test$man<-parSapply(cl,test$discount,man)

#�Ż�ȯ�վ���ȡ����
coupon_f_1<-as.data.frame(table(test$coupon))
coupon_f_1$Var1<-as.integer(as.character(coupon_f_1$Var1))
#�����վ���ȡ����
coupon_f_1$Freq<-coupon_f_1$Freq/31
#ÿ���Ż�ȯ���վ���ȡ������ֵ
coupon_cishu<-function(x)
{return(coupon_f_1$Freq[which(coupon_f_1$Var1==x)])}
test$coupon_cishu<-sapply(test$coupon,coupon_cishu)

#�̵������ִ���
shop_f_1<-as.data.frame(table(test$shop))
shop_f_1$Var1<-as.integer(as.character(shop_f_1$Var1))
#�̵����վ����ִ�����ֵ
shop_f_1$Freq<-shop_f_1$Freq/31
shop_cishu<-function(x)
{return(shop_f_1$Freq[which(shop_f_1$Var1==x)])}
test$shop_cishu<-sapply(test$shop,shop_cishu)

#����岹
summary(test$distance)
test[is.na(test$distance),]$distance<-2.3

#ѵ������ģ�Ͳ�Ԥ��
model_log<-glm(use~type+rate+man+distance+coupon_cishu+shop_cishu,family=binomial(),
               data=train_1)
predict<-predict(model_log,type = "response",newdata = test)
#����Ԥ����
submission<-select(test,user,coupon,date_r)
submission<-cbind(submission,predict)
write.csv(submission, file = 'd:/data_project/o2o/predict_Solution.csv',row.names = F)
