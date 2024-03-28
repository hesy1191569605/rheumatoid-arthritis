#####################calculate the difference#############################################----
setwd("C:/Users/11915/Desktop/RA/913/FIG5/")
data<-read.csv("cln-data1.csv")
data<-data[which(data$Drugs=="MTXLEF"&data$Responder!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]

Y<-data[which(data$Responder=="Y"),-c(1:18)]
N<-data[which(data$Responder=="N"),-c(1:18)]

Y_median<- apply(Y,2,median,na.rm=T)
N_median<- apply(N,2,median,na.rm=T)
FC_Y_N<-Y_median/N_median

P_Y_N<-rep(NA,ncol(Y))
for(i in 1:ncol(Y))try({
  P_Y_N[i] = t.test(as.numeric(Y[,i]),as.numeric(N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
})
TEST<-cbind(Y_median,N_median,FC_Y_N,P_Y_N)
write.csv(TEST,"TEST_MTXLEF_YN.csv")
data<-read.csv("cln-data1.csv")
data<-data[which(data$Drugs=="MTXHCQ"&data$Responder!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]

Y<-data[which(data$Responder=="Y"),-c(1:18)]
N<-data[which(data$Responder=="N"),-c(1:18)]

Y_median<- apply(Y,2,median,na.rm=T)
N_median<- apply(N,2,median,na.rm=T)
FC_Y_N<-Y_median/N_median

P_Y_N<-rep(NA,ncol(Y))
for(i in 1:ncol(Y))try({
  P_Y_N[i] = t.test(as.numeric(Y[,i]),as.numeric(N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
})
TEST<-cbind(Y_median,N_median,FC_Y_N,P_Y_N)
write.csv(TEST,"TEST_MTXHCQ_YN.csv")
data<-read.csv("TEST_MTXHCQ_YN.csv")
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$P_Y_N!="NA")&data$FC_Y_N!="NA"),]
data$color_pre[(data$P_Y_N> 0.05|data$P_Y_N=="NA")|(data$FC_Y_N > 1& data$FC_Y_N < 1)] <- "no"
data$color_pre[(data$P_Y_N<0.05)&(data$FC_Y_N>1)] <- "up"
data$color_pre[(data$P_Y_N<0.05)&(data$FC_Y_N< 1)]  <- "down"
table(data$color_pre)
log2(1.25)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_Y_N),y=-log10(data$P_Y_N),color=data$color_pre))+
  geom_point(aes(color=data$color_pre),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
  geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  labs(x="Log2(Y/N)",y="-Log10(P_Value)")+
  scale_y_continuous(expand = c(0,0),limits = c(0,2.5))+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("MTX_HCQ(Y_N).pdf", width = 3.2, height = 3)




setwd("C:/Users/11915/Desktop/RA/913/")
data<-read.csv("cln-data1.csv")
data<-data[which(data$Drugs=="MTXLEF"&data$Responder!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]

Y<-data[which(data$Responder=="Y"),-c(1:18)]
N<-data[which(data$Responder=="N"),-c(1:18)]

Y_median<- apply(Y,2,median,na.rm=T)
N_median<- apply(N,2,median,na.rm=T)
FC_Y_N<-Y_median/N_median

P_Y_N<-rep(NA,ncol(Y))
for(i in 1:ncol(Y))try({
  P_Y_N[i] = t.test(as.numeric(Y[,i]),as.numeric(N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
})
TEST<-cbind(Y_median,N_median,FC_Y_N,P_Y_N)
write.csv(TEST,"TEST_MTXLEF_YN.csv")

#####################boxplot###########----
data<-read.csv("TEST_MTXLEF_YN.csv")
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$P_Y_N!="NA")&data$FC_Y_N!="NA"),]
data$color_pre[(data$P_Y_N> 0.05|data$P_Y_N=="NA")|(data$FC_Y_N > 1& data$FC_Y_N < 1)] <- "no"
data$color_pre[(data$P_Y_N<0.05)&(data$FC_Y_N>1)] <- "up"
data$color_pre[(data$P_Y_N<0.05)&(data$FC_Y_N< 1)]  <- "down"
table(data$color_pre)
log2(1.25)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_Y_N),y=-log10(data$P_Y_N),color=data$color_pre))+
  geom_point(aes(color=data$color_pre),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
  geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  labs(x="Log2(Y/N)",y="-Log10(P_Value)")+
  scale_y_continuous(expand = c(0,0),limits = c(0,2.5))+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("MTX_LEF(Y_N).pdf", width = 3.2, height = 3)#

#####################pathway#############################################----
setwd("C:/Users/11915/Desktop/RA/913/FIG5/")
data<-read.csv("pathway.csv")
library(ggplot2)
ggplot(data,aes(x =data$Term,y =data$Count,fill=-log10(PValue)))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  #scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(data$Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,1,2,3)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
data<-read.csv("pathway.csv")
library(ggplot2)
ggplot(data,aes(x =data$Term,y =data$Count,fill=-log10(PValue)))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  #scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(data$Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,1,2,3)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  

ggsave("pathway.pdf", width = 3, height = 4)

####################lasso contribution calculation#######
setwd("C:/Users/11915/Desktop/RA/913/")
library(carData);library(car);library(corrplot);library(leaps);library(Matrix);library(glmnet);library(lattice)
library(ggplot2);library(caret);library(pROC);library(scales)

rm(list = ls())
data=read.csv("cln-dataKNN1.csv", header = T,row.names = NULL)


d=data[which(data$Drugs.HM=="MTXHCQ"&data$Responder.HM!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","Responder.HM")]
dat1=d
# 全部变量中心化到-1至1水平
dat1[,3:ncol(dat1)]=lapply(dat1[,3:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Responder.HM=ifelse(dat1$Responder.HM=="Y",1,ifelse(dat1$Responder.HM=="N",0,NA))
dat1<-dat1[,-c(1)]
a<-as.integer(runif(50,min=1,max=123456789))
coef <- data.frame(rep("0",ncol(dat1)))
coef<-data.frame(coef)

i=1
repeat{set.seed(a[i])
  x <- as.matrix(dat1[,-1])
  y <- as.factor(dat1[, 1])
  fit.cv <- cv.glmnet(x, y, family="binomial", alpha=1,nfolds = 10,type.measure ="mse")
  plot(fit.cv)
  lasso.coef<-  data.frame(coef(fit.cv, s ="lambda.1se", exact=TRUE)[1:ncol(dat1)])
  coef <- cbind(coef,lasso.coef)
  i=i+1
  if(i>50){auc <- auc[-1,]
  break}}
coef[,1]<-colnames(dat1)
write.csv(coef,"coef_lm_lasso_1se_MTXHCQ.csv",row.names = F)




rm(list = ls())
data=read.csv("cln-dataKNN1.csv", header = F,row.names = NULL)
colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXHCQ=t(read.csv("FIG4/gene_mtxhcq.csv",header = F))

d=data[which(data$`Drugs-HM`=="MTXHCQ"&data$`Responder-HM`!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"gene","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
dat1=d
# 全部变量中心化到-1至1水平
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$`Responder-HM`=ifelse(dat1$`Responder-HM`=="Y",1,ifelse(dat1$`Responder-HM`=="N",0,NA))
dat1<-dat1[,-c(1:2)]
a<-as.integer(runif(50,min=1,max=123456789))
coef <- data.frame(rep("0",ncol(dat1)))
coef<-data.frame(coef)
i=1
repeat{set.seed(a[i])
  x <- as.matrix(dat1[,-1])
  y <- as.factor(dat1[, 1])
  fit.cv <- cv.glmnet(x, y, family="binomial", alpha=1,nfolds = 10,type.measure ="mse")
  plot(fit.cv)
  lasso.coef<-  data.frame(coef(fit.cv, s ="lambda.1se", exact=TRUE)[1:ncol(dat1)])
  coef <- cbind(coef,lasso.coef)
  i=i+1
  if(i>50){auc <- auc[-1,]
  break}}
coef[,1]<-colnames(dat1)
write.csv(coef,"coef_lm_lasso_1se_mtxhcq.csv",row.names = F)


####################Plotting the average lasso contributions in a scatter plot#######
setwd("C:/Users/11915/Desktop/RA/913/FIG5/")
data<-read.csv("lasso_MTXHCQ.csv")
data$LASSO
ggplot(data, aes( x =rep..0...ncol.dat1..,y=LASSO)) +
  geom_hline(yintercept = 0, color = "black", size = 1,linetype=2) + # 添加y=0的辅助线
  geom_hline(yintercept = -1, color = "grey40", size = 1,linetype=2) + # 添加y=0的辅助线
  geom_hline(yintercept = 1, color = "grey40", size = 1,linetype=2) + # 添加y=0的辅助线
  geom_bar(fill ="black", stat = "identity", width = 0.1) + # 注意将width宽度设小
  geom_point(aes(fill = LASSO,size =abs(LASSO)),shape=21) +          # 将点的size设置大一些比较好看
  scale_x_discrete(limits=data$rep..0...ncol.dat1..)+
  scale_size_continuous(range=c(3,8))+
  scale_fill_gradient2(low="#737AAC",mid="white",high = "#AF322F")+
  coord_flip()+
  labs(x = "", y = "Coefficients", colour = "", linetype = "", fill = "")+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("MTX_HCQ coefficient.pdf", width = 5, height = 8)#


####################Drug prediction###############################
setwd("C:/Users/11915/Desktop/RA/")
library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
library(scales);library(pROC);library(lifecycle);library(purrr)

rm(list = ls())
data=read.csv("cln-dataKNN1.csv", header = F,row.names = NULL)
colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")

d=data[which(data$`Drugs-HM`=="MTXLEF"&data$`Responder-HM`!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
d=d[!is.na(d$`DDAS28-HM`),]%>%arrange(gene)
#n=data.frame(n= nrow(d)-colSums(is.na(d)));n;range(n)

dat1=d[,-12]

# 全部变量中心化到-1至1水平
dat1[,5:ncol(dat1)]=lapply(dat1[,5:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
sapply(dat1[,5:ncol(dat1)],range)


for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$`Responder-HM`,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}

dat1$`Responder-HM`=ifelse(dat1$`Responder-HM`=="Y",1,ifelse(dat1$`Responder-HM`=="N",0,NA))
dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))

Y=dat1[dat1$`Responder-HM`==1,]
N=dat1[dat1$`Responder-HM`==0,]

result=NULL
list_train=list()
list_test=list()
coef_all=NULL
i=1
for(i in 1:100){
  set.seed(i)
  train=rbind(Y[sample(1:nrow(Y),10,replace = F),],N[sample(1:nrow(N),17,replace = F),])
  test=dat1[!c(dat1$gene%in%train$gene),]
  y=train$`Responder-HM`
  y_test <- test$`Responder-HM`
  x <- cbind(train[,c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")])
  x_test <- cbind(test[,c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")])
  
  cvfit=cv.glmnet(as.matrix(x), y, nfolds = 10,family="binomial",alpha=0)
  ridge <- glmnet(as.matrix(x),y, family="binomial", lambda=cvfit$lambda.min, alpha=0)
  coef=data.frame(ID=colnames(x),i=rep(i,ncol(x)),coef=coef(ridge)@x[2:(ncol(x)+1)])
  train_y <- predict(ridge,as.matrix(x), type="response")
  df=data.frame(y,train_y=as.numeric(train_y))
  train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
  test_y <- predict(ridge,as.matrix(x_test), type="response")
  df1=data.frame(y_test,test_y=as.numeric(test_y))
  test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
  result=rbind(result, data.frame(i,train_auc, test_auc))
  coef_all=rbind(coef_all,coef)
  list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
  list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
}
write.csv(result,"rescaled 100 times ridge_MTXLEF(p-top5).csv",row.names = F)
write.csv(coef_all,"rescaled coef 100 times ridge_MTXLEF(p-top5).csv",row.names = F)
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)
plot_ROC(list_train,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(049,124,183,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.82-1.00", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.95", size=4)
ggsave("rescaled train 100 times ridge_MTXLEF(p-top5).pdf", width = 3.2, height = 3)

plot_ROC(list_test,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.68-1.00", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.88", size=4)
# ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled test 100 times ridge_MTXLEF(p-top5).pdf", width = 3.2, height = 3)





data=read.csv("cln-dataKNN1.csv", header = F,row.names = NULL)
colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))


MTXHCQ=c("GGT1","RPL27A")

d=data[which(data$`Drugs-HM`=="MTXHCQ"&data$`Responder-HM`!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"gene","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
d=d[!is.na(d$`DDAS28-HM`),]%>%arrange(gene)
#n=data.frame(n= nrow(d)-colSums(is.na(d)));n;range(n)

dat1=d[,-10]

# 全部变量中心化到-1至1水平
dat1[,5:ncol(dat1)]=lapply(dat1[,5:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
sapply(dat1[,5:ncol(dat1)],range)


for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$`Responder-HM`,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}

dat1$`Responder-HM`=ifelse(dat1$`Responder-HM`=="Y",1,ifelse(dat1$`Responder-HM`=="N",0,NA))
dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))

Y=dat1[dat1$`Responder-HM`==1,]
N=dat1[dat1$`Responder-HM`==0,]

result=NULL
list_train=list()
list_test=list()
coef_all=NULL
i=1
for(i in 1:100){
  set.seed(i)
  train=rbind(Y[sample(1:nrow(Y),7,replace = F),],N[sample(1:nrow(N),14,replace = F),])
  test=dat1[!c(dat1$gene%in%train$gene),]
  y=train$`Responder-HM`
  y_test <- test$`Responder-HM`
  x <- cbind(train[,c("GGT1","RPL27A")])
  x_test <- cbind(test[,c("GGT1","RPL27A")])
  
  cvfit=cv.glmnet(as.matrix(x), y, nfolds = 10,family="binomial",alpha=0)
  ridge <- glmnet(as.matrix(x),y, family="binomial", lambda=cvfit$lambda.min, alpha=0)
  coef=data.frame(ID=colnames(x),i=rep(i,ncol(x)),coef=coef(ridge)@x[2:(ncol(x)+1)])
  train_y <- predict(ridge,as.matrix(x), type="response")
  df=data.frame(y,train_y=as.numeric(train_y))
  train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
  test_y <- predict(ridge,as.matrix(x_test), type="response")
  df1=data.frame(y_test,test_y=as.numeric(test_y))
  test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
  result=rbind(result, data.frame(i,train_auc, test_auc))
  coef_all=rbind(coef_all,coef)
  list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
  list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
}
write.csv(result,"rescaled 100 times ridge_MTXHCQ(p-top2).csv",row.names = F)
write.csv(coef_all,"rescaled coef 100 times ridge_MTXHCQ(p-top2).csv",row.names = F)
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)
plot_ROC(list_train,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(049,124,183,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.73-1.00", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.87", size=4)
ggsave("rescaled train 100 times ridge_MTXHCQ(p-top2).pdf", width = 3.2, height = 3)

plot_ROC(list_test,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.53-0.95", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.81", size=4)
# ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled test 100 times ridge_MTXHCQ(p-top2).pdf", width = 3.2, height = 3)




