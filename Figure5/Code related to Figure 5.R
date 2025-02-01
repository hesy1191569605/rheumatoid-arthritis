########################calculate the difference#############################################----
data<-data[which(data$Drugs=="MTXLEF"&data$Responder!="NA"&data$CCP=="CCP+"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]
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
write.csv(TEST,"drug respense/TEST_MTXLEF_CCP+_YN.csv")
data<-data[which(data$Drugs=="MTXHCQ"&data$Responder!="NA"&data$CCP=="CCP+"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]

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
write.csv(TEST,"drug respense/TEST_MTXHCQ_CCP+_YN.csv")
data<-read.csv("drug respense//TEST_MTXLEF_CCP+_YN.csv")
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
ggsave("drug respense/MTX_LEF_CCP+(Y_N).pdf", width = 3.2, height = 3)

data<-read.csv("drug respense//TEST_MTXHCQ_CCP+_YN.csv")
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$P_Y_N!="NA")&data$FC_Y_N!="NA"),]
data$color_pre[(data$P_Y_N> 0.05|data$P_Y_N=="NA")|(data$FC_Y_N > 1& data$FC_Y_N < 1)] <- "no"
data$color_pre[(data$P_Y_N<0.05)&(data$FC_Y_N>1)] <- "up"
data$color_pre[(data$P_Y_N<0.05)&(data$FC_Y_N< 1)] <- "down"
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
  #scale_y_continuous(expand = c(0,0),limits = c(0,2.5))+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("drug respense/MTX_HCQ_CCP+(Y_N).pdf", width = 3.2, height = 3)




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
{data<-read.csv("cln-data1.csv")
  data<-data[which(data$Drugs=="MTXHCQ"&data$Responder!="NA"&data$CCP=="CCP+"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]
  #data<-data[which(data$Drugs=="MTXHCQ"&data$Responder!="NA"&data$CCP=="CCP-"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]
  data_F<-data[which(data$Gender=="Female"),]
  data_M<-data[which(data$Gender=="Male"),]
  Y<-data[which(data$Responder=="Y"),-c(1:18)]
  N<-data[which(data$Responder=="N"),-c(1:18)]
  YF<-data_F[which(data_F$Responder=="Y"),-c(1:18)]
  NF<-data_F[which(data_F$Responder=="N"),-c(1:18)]
  YM<-data_M[which(data_M$Responder=="Y"),-c(1:18)]
  NM<-data_M[which(data_M$Responder=="N"),-c(1:18)]
  Y_median<- apply(Y,2,median,na.rm=T)
  N_median<- apply(N,2,median,na.rm=T)
  YF_median<- apply(YF,2,median,na.rm=T)
  NF_median<- apply(NF,2,median,na.rm=T)
  YM_median<- apply(YM,2,median,na.rm=T)
  NM_median<- apply(NM,2,median,na.rm=T)
  FC_Y_N<-Y_median/N_median
  FC_F_Y_N<-YF_median/NF_median 
  FC_M_Y_N<-YM_median/NM_median
  P_Y_N<-rep(NA,ncol(Y))
  P_F_Y_N<-rep(NA,ncol(Y))
  P_M_Y_N<-rep(NA,ncol(Y))
  
  for(i in 1:ncol(Y))try({
    P_Y_N[i] = t.test(as.numeric(Y[,i]),as.numeric(N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
    P_F_Y_N[i] = t.test(as.numeric(YF[,i]),as.numeric(NF[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
    P_M_Y_N[i] = t.test(as.numeric(YM[,i]),as.numeric(NM[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
  })
  TEST<-cbind(Y_median,N_median,YF_median,NF_median,YM_median,NM_median,
              FC_Y_N,FC_F_Y_N,FC_M_Y_N,
              P_Y_N,P_F_Y_N,P_M_Y_N)
  write.csv(TEST,"drug respense/TEST_MTXHCQ_CCP+_YN.csv")
}



{data<-read.csv("cln-data1.csv")
  data<-data[which(data$Drugs=="MTXLEF"&data$Responder!="NA"&data$CCP=="CCP+"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]
  data_F<-data[which(data$Gender=="Female"),]
  data_M<-data[which(data$Gender=="Male"),]
  Y<-data[which(data$Responder=="Y"),-c(1:18)]
  N<-data[which(data$Responder=="N"),-c(1:18)]
  YF<-data_F[which(data_F$Responder=="Y"),-c(1:18)]
  NF<-data_F[which(data_F$Responder=="N"),-c(1:18)]
  YM<-data_M[which(data_M$Responder=="Y"),-c(1:18)]
  NM<-data_M[which(data_M$Responder=="N"),-c(1:18)]
  Y_median<- apply(Y,2,median,na.rm=T)
  N_median<- apply(N,2,median,na.rm=T)
  YF_median<- apply(YF,2,median,na.rm=T)
  NF_median<- apply(NF,2,median,na.rm=T)
  YM_median<- apply(YM,2,median,na.rm=T)
  NM_median<- apply(NM,2,median,na.rm=T)
  FC_Y_N<-Y_median/N_median
  FC_F_Y_N<-YF_median/NF_median 
  FC_M_Y_N<-YM_median/NM_median
  P_Y_N<-rep(NA,ncol(Y))
  P_F_Y_N<-rep(NA,ncol(Y))
  P_M_Y_N<-rep(NA,ncol(Y))
  
  for(i in 1:ncol(Y))try({
    P_Y_N[i] = t.test(as.numeric(Y[,i]),as.numeric(N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
    P_F_Y_N[i] = t.test(as.numeric(YF[,i]),as.numeric(NF[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
    P_M_Y_N[i] = t.test(as.numeric(YM[,i]),as.numeric(NM[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value
  })
  TEST<-cbind(Y_median,N_median,YF_median,NF_median,YM_median,NM_median,
              FC_Y_N,FC_F_Y_N,FC_M_Y_N,
              P_Y_N,P_F_Y_N,P_M_Y_N)
  write.csv(TEST,"drug respense/TEST_MTXLEF_CCP+_YN.csv")
}
########################Fig5A:scatter plot(MTX+LEF)################
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

########################Fig5B:pathway(MTX+LEF)################

library(ggplot2)
ggplot(MTXLEF,aes(x =Term,y =Count,fill=-log10(PValue)))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  #scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(Term))+
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
########################Fig5C:pathway(MTX+LEF)################
library(ggplot2)
ggplot(MTXLEF,aes(x =Term,y =Count,fill=-log10(PValue)))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  #scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,1,2,3)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  

########################Fig5D:scatter plot(MTX+HCQ)################
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
ggsave("MTX_LEF(Y_N).pdf", width = 3.2, height = 3)#
########################Fig5E:pathway(MTX+HCQ)################

library(ggplot2)
ggplot(MTXHCQ,aes(x =Term,y =Count,fill=-log10(PValue)))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  #scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(Term))+
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
########################Fig5F:pathway(MTX+HCQ)################
library(ggplot2)
ggplot(MTXHCQ,aes(x =Term,y =Count,fill=-log10(PValue)))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  #scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,1,2,3)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  

########################Fig5J:Lasso(MTX+LEF)################
colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=t(read.csv("gene_mtxlef.csv",header = F))

d=data[which(data$`Drugs-HM`=="MTXLEF"&data$`Responder-HM`!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
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
write.csv(coef,"coef_lm_lasso_1se_mtxLEF2.csv",row.names = F)
data<-read.csv("lasso_MTXLEF.csv")
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
ggsave("MTX_LEF coefficient.pdf", width = 5, height = 8)#
########################Fig5H:Lasso(MTX+HCQ)################
colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXHCQ=t(read.csv("gene_mtxhcq.csv",header = F))

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
write.csv(coef,"coef_lm_lasso_1se_mtxhcq2.csv",row.names = F)
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
########################Fig5I+SFig5A:ROC(MTX+LEF)################
library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
library(scales);library(pROC);library(lifecycle);library(purrr)
data=read.csv("RA_DATAKNN1.csv", header = F,row.names = NULL)
colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")

d=data[which(data$`Drugs-HM`=="MTXLEF"&data$`Responder-HM`!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
d=d[!is.na(d$`DDAS28-HM`),]%>%arrange(gene)
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
  Y1<-Y[sample(1:nrow(Y),7,replace = F),]
  Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
  N1<-N[sample(1:nrow(N),7,replace = F),]
  N2<-N[!c(N$Sample%in%N1$Sample),]
  N2<-N2[sample(1:nrow(N2),7,replace = F),]
  train=rbind(Y1,N1)
  test=rbind(Y2,N2)
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



result=read.csv("rescaled 100 times ridge_MTXLEF(p-top5).csv")
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)
re=reshape2::melt(result,id.vars=1)
ggplot(re,aes(value, fill=variable))+
  geom_density(alpha=.5)+
  geom_vline(xintercept = c(median(result$train_auc), median(result$test_auc)), linetype=2, color=c("#5D669F","#AF322F"))+
  scale_fill_manual(values = c("#317CB7","#DC6D57"),name="", label=c("Train","Test"))+
  theme_minimal()+
  labs(x="AUROC",y="Density")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        legend.position = "bottom")
# ggsave("density_plot 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled density_plot 100 times ridge_MTXLEF(p-top5).pdf",  width = 3, height = 3.4)

########################Fig5J+SFig5A:ROC(MTX+HCQ)################
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
  Y1<-Y[sample(1:nrow(Y),7,replace = F),]
  Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
  N1<-N[sample(1:nrow(N),7,replace = F),]
  N2<-N[!c(N$Sample%in%N1$Sample),]
  N2<-N2[sample(1:nrow(N2),7,replace = F),]
  train=rbind(Y1,N1)
  test=rbind(Y2,N2)
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



result=read.csv("rescaled 100 times ridge_MTXHCQ(p-top2).csv")
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)
re=reshape2::melt(result,id.vars=1)
ggplot(re,aes(value, fill=variable))+
  geom_density(alpha=.5)+
  geom_vline(xintercept = c(median(result$train_auc), median(result$test_auc)), linetype=2, color=c("#5D669F","#AF322F"))+
  scale_fill_manual(values = c("#5D669F","#AF322F"),name="", label=c("Train","Test"))+
  theme_minimal()+
  labs(x="AUROC",y="Density")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        legend.position = "bottom")
# ggsave("density_plot 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled density_plot 100 times ridge_MTXHCQ(p-top2).pdf",  width = 3, height = 3.4)
########################Fig5K:Elisa########################
coefficients <-coef[which(coef$data1..data.ID.!="int"),2]
intercept <- coef[which(coef$data1..data.ID.=="int"),2]
{setwd("H:/分析/RA/最后一版/913/")
  data=read.csv("cln-dataKNN1.csv", header = F,row.names = NULL)
  colnames(data)=data[1,];data=data[-1,];data[,c(8:14,16:ncol(data))] = lapply(data[,c(8:14,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  
  
  #MTXLEF=c("CRP.1","LGALS3BP","APOA4","SERPINC1","CYCS","IGLV3-10","C2", "IGHV4-61","MYH9","GPX3")
  #MTXLEF=c("CRP.1","LGALS3BP","APOA4","SERPINC1","CYCS")
  MTXLEF=c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")
  
  d=data[which(data$`Drugs-HM`=="MTXLEF"&data$`Responder-HM`!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
  d=d[!is.na(d$`DDAS28-HM`),]%>%arrange(gene)
  d=d[,-12]
  
  # 全部变量中心化到-1至1水平
  d[,5:ncol(d)]=lapply(d[,5:ncol(d)], function(x){rescale(x, to =c(-1,1))})
  d$`Responder-HM`=ifelse(d$`Responder-HM`=="Y",1,ifelse(d$`Responder-HM`=="N",0,NA))
  features =d[,c(14,13,12,11,10)]
  true_labels <- d$`Responder-HM`
  linear_predictor <- intercept + as.matrix(features) %*% coefficients
  
  # 应用逻辑函数转换为概率
  predicted_probabilities <- 1 / (1 + exp(-linear_predictor))
  roc_curve <- roc(true_labels, predicted_probabilities)
  auc_value <- auc(roc_curve)
  print(paste("AUC:", auc_value))
  
  # 绘制ROC曲线
  plot(roc_curve, main = "ROC Curve", col = "blue")
  
  # 找到最佳cut-off值
  best_cutoff <- coords(roc_curve, "best", ret = "threshold")
  print(paste("Best Cut-Off:", best_cutoff))
  
  # 绘制ROC曲线并标记最佳cut-off值
  plot(roc_curve, main = "ROC Curve", col = "blue")
  abline(v = best_cutoff, col = "red", lty = 2)
  text(best_cutoff, 0.5, paste("Best Cut-Off: ", round(best_cutoff, 2)), pos = 4, col = "red")
  
}



# 提取特征和标签
# 假设特征在new_data中列名为X1, X2, X3
# 假设标签在new_data中列名为"label"
dat1[,4:ncol(dat1)]=lapply(dat1[,4:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
features <- dat1[, c(4:5)]
true_labels <- dat1$RESPONSE
true_labels[which(true_labels=="Y")]<-1
true_labels[which(true_labels=="N")]<-0
# 计算线性预测值
linear_predictor <- intercept + as.matrix(features) %*% coefficients

# 应用逻辑函数转换为概率
predicted_probabilities <- 1 / (1 + exp(-linear_predictor))

# 计算ROC曲线
roc_curve <- roc(true_labels, predicted_probabilities)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
a<-data.frame(roc_curve[["original.response"]],roc_curve[["predictor"]])
a$predictor<-"N"

a$predictor[which(a$roc_curve...predictor...>0.4553769)]<-"Y"
table(a[,c(1,3)])

pdf("mtx_hcq_elisa_roc.pdf",width = 4, height =4)
plot(roc_curve, main = "ROC Curve", col = "#675496" )
dev.off()
library(caret)
optimal_coords <- coords(roc_curve, "best", best.method="youden", ret="threshold")
threshold <- optimal_coords$threshold
predicted_labels <- ifelse(a$roc_curve...predictor... >= threshold, 1, 0)
true_labels<-a$roc_curve...original.response...
# 计算混淆矩阵
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(true_labels))

# 打印混淆矩阵
print(conf_matrix)

# 获取混淆矩阵中的元素
{tp <- conf_matrix$table[2, 2]
  tn <- conf_matrix$table[1, 1]
  fp <- conf_matrix$table[1, 2]
  fn <- conf_matrix$table[2, 1]
  
  # 计算各项性能指标
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  library(cvms)
  library(tibble)
  # 打印性能指标
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall (Sensitivity):", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
  mytable <- table(true_labels,predicted_labels )
  mytibble <- as_tibble(mytable)
}
pdf("mtx_hcq_elisa_cvms.pdf",width = 4, height =4)
plot_confusion_matrix(mytibble,
                      target_col = "true_labels",
                      prediction_col = "predicted_labels",
                      
                      counts_col = "n")

dev.off()

dat1[,4:ncol(dat1)]=lapply(dat1[,4:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
features <- dat1[, c(4:8)]
true_labels <- dat1$RESPONSE
true_labels[which(true_labels=="Y")]<-1
true_labels[which(true_labels=="N")]<-0
# 计算线性预测值
linear_predictor <- intercept + as.matrix(features) %*% coefficients

# 应用逻辑函数转换为概率
predicted_probabilities <- 1 / (1 + exp(-linear_predictor))

# 计算ROC曲线
roc_curve <- roc(true_labels, predicted_probabilities)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
a<-data.frame(roc_curve[["original.response"]],roc_curve[["predictor"]])
a$predictor<-"N"

a$predictor[which(a$roc_curve...predictor...>0.4553769)]<-"Y"
table(a[,c(1,3)])
p=
  pdf("mtx_lef_elisa_roc.pdf",width = 4, height =4)
plot(roc_curve, main = "ROC Curve", col = "#675496" )
dev.off()
library(caret)
optimal_coords <- coords(roc_curve, "best", best.method="youden", ret="threshold")
threshold <- optimal_coords$threshold
predicted_labels <- ifelse(a$roc_curve...predictor... >= threshold, 1, 0)
true_labels<-a$roc_curve...original.response...
# 计算混淆矩阵
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(true_labels))

# 打印混淆矩阵
print(conf_matrix)

# 获取混淆矩阵中的元素
{tp <- conf_matrix$table[2, 2]
  tn <- conf_matrix$table[1, 1]
  fp <- conf_matrix$table[1, 2]
  fn <- conf_matrix$table[2, 1]
  
  # 计算各项性能指标
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  library(cvms)
  library(tibble)
  # 打印性能指标
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall (Sensitivity):", recall, "\n")
  cat("F1 Score:", f1_score, "\n")
  mytable <- table(true_labels,predicted_labels )
  mytibble <- as_tibble(mytable)
}
pdf("mtx_lef_elisa_cvms.pdf",width = 4, height =4)
plot_confusion_matrix(mytibble,
                      target_col = "true_labels",
                      prediction_col = "predicted_labels",
                      
                      counts_col = "n")

dev.off()




########################SFig5B:Feture select########
setwd("H:/分析/RA/返修第二版/feature select/")
MTXLEF<-read.csv("RF MTXLEF.csv")
MTXHCQ<-read.csv("RF MTXHCQ.csv")

ggplot(data=MTXLEF,aes(x=GENE,y=MeanDecreaseGini.2,fill=MeanDecreaseGini.2))+
  geom_bar(aes(fill=MeanDecreaseGini.2),position = "dodge",stat="identity",width = 0.7)+
  scale_x_discrete(limits=unique(MTXLEF$GENE))+
  #geom_hline(yintercept = 1, color = "grey40", size = 1,linetype=2) +
  scale_fill_gradient2(low="#737AAC",mid="white",high = "#AF322F")+
  labs(x = "", y = "Coefficients", colour = "", linetype = "", fill = "")+
  theme(axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        #panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("rf_coef_mtxlef.pdf",height = 4,width = 7)  
ggplot(data=MTXHCQ,aes(x=GENE,y=MeanDecreaseGini.1,fill=MeanDecreaseGini.1))+
  geom_bar(aes(fill=MeanDecreaseGini.1),position = "dodge",stat="identity",width = 0.7)+
  scale_x_discrete(limits=unique(MTXHCQ$GENE))+
  #geom_hline(yintercept = 1, color = "grey40", size = 1,linetype=2) +
  scale_fill_gradient2(low="#737AAC",mid="white",high = "#AF322F")+
  labs(x = "", y = "Coefficients", colour = "", linetype = "", fill = "")+
  theme(axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        #panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("rf_coef_mtxhcq.pdf",height = 4,width = 7)  
MTXLEF<-read.csv("feature select XGB mtx+lef.csv")[c(1:10),]
MTXHCQ<-read.csv("feature select XGB mtx+hcq.csv")[c(1:10),]

ggplot(data=MTXLEF,aes(x=Feature,y=Gain,fill=Gain))+
  geom_bar(aes(fill=Gain),position = "dodge",stat="identity",width = 0.7)+
  scale_x_discrete(limits=unique(MTXLEF$Feature))+
  #geom_hline(yintercept = 1, color = "grey40", size = 1,linetype=2) +
  scale_fill_gradient2(low="#AF322F",mid="white",high = "#737AAC")+
  labs(x = "", y = "Coefficients", colour = "", linetype = "", fill = "")+
  theme(axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        #panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("XGB_coef_mtxlef.pdf",height = 4,width = 7)  
ggplot(data=MTXHCQ,aes(x=Feature,y=Gain,fill=Gain))+
  geom_bar(aes(fill=Gain),position = "dodge",stat="identity",width = 0.7)+
  scale_x_discrete(limits=unique(MTXHCQ$Feature))+
  #geom_hline(yintercept = 1, color = "grey40", size = 1,linetype=2) +
  scale_fill_gradient2(low="#AF322F",mid="white",high = "#737AAC")+
  labs(x = "", y = "Coefficients", colour = "", linetype = "", fill = "")+
  theme(axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        #panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("XGB_coef_mtxhcq.pdf",height = 4,width = 7)  

########################SFig5C:RF and XGBoost#####################
setwd("H:/分析/RA/返修第二版/")
library(scales);library(randomForest);
library(reshape2);library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
library(scales);library(pROC);library(lifecycle);library(purrr);library(pROC);library(ggplot2);library(gtable)

data <- read.csv("RA_DATAKNN1.csv")
data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=read.csv("gene_mtxlef.csv",header = F)[-1,2]
data$Drugs.Response
d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
dat1=d
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,
                           ifelse(dat1$Drugs.Response=="No Response",0,NA))
dat1<-dat1[,-c(1:7)]
a<-as.integer(runif(50,min=1,max=123456789))
coef <- data.frame(rep("0",ncol(dat1)-1))
coef<-data.frame(coef)
i=1
repeat{set.seed(i)
  rf=randomForest(as.factor(Drugs.Response)~.,data =dat1,ntree=500 )
  lasso.coef<-importance(x=rf)
  coef <- cbind(coef,lasso.coef)
  i=i+1
  if(i>50){
    break}}
coef[,1]<-colnames(dat1)[-1]
coef<-coef[,-1]
coef1<-rowMeans(coef)
coef2<-data.frame(row.names(coef),coef1)
write.csv(coef,"coef_rf_mtxLEF.csv",row.names = T)







data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=c("VAT1","CBR1","ECI2","XRCC5")

d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
dat1=d
# 全部变量中心化到-1至1水平
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))



for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}

dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))

Y=dat1[dat1$Drugs.Response==1,]
N=dat1[dat1$Drugs.Response==0,]

result=NULL
list_train=list()
list_test=list()
coef_all=NULL
i=1
for(i in 1:100){
  set.seed(i)
  Y1<-Y[sample(1:nrow(Y),10,replace = F),]
  Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
  N1<-N[sample(1:nrow(N),10,replace = F),]
  N2<-N[!c(N$Sample%in%N1$Sample),]
  N2<-N2[sample(1:nrow(N2),9,replace = F),]
  train=rbind(Y1,N1)
  test=rbind(Y2,N2)
  y=train$Drugs.Response
  y_test <- test$Drugs.Response
  x <- cbind(train[,c("VAT1","CBR1","ECI2","XRCC5")])
  x_test <- cbind(test[,c("VAT1","CBR1","ECI2","XRCC5")])
  
  rf=randomForest(x=x,y=y,ntree=500 )
  train_y <-predict(rf,as.matrix(x), type="response")
  train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
  test_y <- predict(rf,as.matrix(x_test), type="response")
  df1=data.frame(y_test,test_y=as.numeric(test_y))
  test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
  result=rbind(result, data.frame(i,train_auc, test_auc))
  list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
  list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
}
write.csv(result,"rescaled 100 times rf_MTXLEF(p-top5)_sample2.csv",row.names = F)
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
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.96", size=4)
ggsave("rescaled train 100 times ridge_MTXLEF(p-top5).pdf", width = 3.2, height = 3)

plot_ROC(list_test,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.47-0.88", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.71", size=4)
# ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled test 100 times rf_MTXLEF(p-top5)___1.pdf", width = 3.2, height = 3)

setwd("H:/分析/RA/返修第二版/")
library(scales);library(randomForest);
library(reshape2);library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
library(scales);library(pROC);library(lifecycle);library(purrr);library(pROC);library(ggplot2);library(gtable)
library(caret);library(e1071)
data <- read.csv("RA_DATAKNN1.csv")
data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXHCQ=c("GGT1","RPL27A")

d=data[which(data$Drugs=="MTX+HCQ"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
dat1=d
# 全部变量中心化到-1至1水平
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))



for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}

dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))

Y=dat1[dat1$Drugs.Response==1,]
N=dat1[dat1$Drugs.Response==0,]

result=NULL
list_train=list()
list_test=list()
coef_all=NULL
i=1
for(i in 1:100){
  set.seed(i)
  Y1<-Y[sample(1:nrow(Y),7,replace = F),]
  Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
  N1<-N[sample(1:nrow(N),7,replace = F),]
  N2<-N[!c(N$Sample%in%N1$Sample),]
  N2<-N2[sample(1:nrow(N2),7,replace = F),]
  train=rbind(Y1,N1)
  test=rbind(Y2,N2)
  y=train$Drugs.Response
  y_test <- test$Drugs.Response
  x <- cbind(train[,c("GGT1","RPL27A")])
  x_test <- cbind(test[,c("GGT1","RPL27A")])
  
  rf=randomForest(x=x,y=y,ntree=500 )
  train_y <-predict(rf,as.matrix(x), type="response")
  train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
  test_y <- predict(rf,as.matrix(x_test), type="response")
  df1=data.frame(y_test,test_y=as.numeric(test_y))
  test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
  result=rbind(result, data.frame(i,train_auc, test_auc))
  list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
  list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
}
write.csv(result,"rescaled 100 times rf_MTXHCQ(p-top2)_sample2.csv",row.names = F)
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)

plot_ROC(list_test,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.41-0.98", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.79", size=4)
# ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled test 100 times rf_MTXHCQ(p-top5)___1.pdf", width = 3.2, height = 3)
{setwd("H:/分析/RA/返修第二版/")
  library(scales);library(randomForest);
  library(reshape2);library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
  library(scales);library(pROC);library(lifecycle);library(purrr);library(pROC);library(ggplot2);library(gtable)
  library(dplyr);library(data.table);library(xgboost) ;library(Matrix);library(caTools);library(caret) # 调参和计算模型评价参数使用
  library(pROC);library(ggplot2) ;library(ggpubr);library(ggprism) ;library(caTools);library(scales);library(Ckmeans.1d.dp)
  library(plyr);library(dplyr)
  data <- read.csv("RA_DATAKNN1.csv")
  data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  MTXLEF=read.csv("gene_mtxlef.csv",header = F)[-1,2]
  data$Drugs.Response
  d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
  dat1=d
  dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
  dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,
                             ifelse(dat1$Drugs.Response=="No Response",0,NA))
  dat1<-dat1[,-c(1:7)]
  a<-as.integer(runif(50,min=1,max=123456789))
  
  newtrain<-dat1
  head(newtrain)
  str(newtrain)
  sparse_matrix = sparse.model.matrix(Drugs.Response~ ., data = newtrain)[,-1]
  head(sparse_matrix)
  output_vector = as.numeric(newtrain$Drugs.Response)
  # 建模（不调参）
  xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
  
  # 特征重要性排序
  imp = xgb.importance(colnames(sparse_matrix),xgb)
  
  
  
  data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  MTXLEF=c("VAT1","APOD","PSMA6","SLC25A13")
  
  d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
  dat1=d
  # 全部变量中心化到-1至1水平
  dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
  dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))
  
  
  
  for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}
  
  dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))
  
  Y=dat1[dat1$Drugs.Response==1,]
  N=dat1[dat1$Drugs.Response==0,]
  
  result=NULL
  list_train=list()
  list_test=list()
  coef_all=NULL
  i=1
  for(i in 1:100){
    set.seed(i)
    Y1<-Y[sample(1:nrow(Y),9,replace = F),]
    Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
    N1<-N[sample(1:nrow(N),9,replace = F),]
    N2<-N[!c(N$Sample%in%N1$Sample),]
    N2<-N2[sample(1:nrow(N2),10,replace = F),]
    train=rbind(Y1,N2)
    test=rbind(Y2,N1)
    y=train$Drugs.Response
    y_test <- test$Drugs.Response
    x <- cbind(train[,c("VAT1")])
    x_test <- cbind(test[,c("VAT1")])
    dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
    dtest <- xgb.DMatrix(data = as.matrix(x_test))
    sparse_matrix = sparse.model.matrix(as.factor(Drugs.Response)~.,data = train[,-c(1:8)])[,-1]
    sparse_matrix_test = sparse.model.matrix(as.factor(Drugs.Response)~.,data = test[,-c(1:8)])[,-1]
    head(sparse_matrix)
    output_vector = as.numeric(train$Drugs.Response)
    # 建模（不调参）
    xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
    #caret.Control <- trainControl(method = "adaptive_cv",number = 10,repeats=5,verboseIter = FALSE,returnData = FALSE,selectionFunction="best",returnResamp = "final",search = "grid",seeds=set.seed(123))
    #caret.grid <- expand.grid(nrounds = c(100,200,500),max_depth = c(3,5,6),eta = c(0.01, 0.1, 0.3),gamma = c(0,0.1,0.5),colsample_bytree = 0.7,min_child_weight = c(1,3),subsample =0.7)
    #xgb_model_caret <- train(as.factor(Drugs.Response)~.,data = train[,-c(1:8)],method = "xgbTree",trControl = caret.Control,tuneGrid = caret.grid, verbose=FALSE,verbosity=0)
    #params.caret.bst <- list(objective = "binary:logistic",eval_metric = "logloss",max_depth =5,eta = 0.1)
    #xgb_model_caret.bst <- xgboost(data=dtrain,params = params.caret.bst,nrounds =100)
    print(i)
    train_y <-predict(xgb, sparse_matrix,type="response")
    df=data.frame(y,train_y=as.numeric(train_y))
    train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
    test_y <- predict(xgb,sparse_matrix_test, type="response")
    df1=data.frame(y_test,test_y=as.numeric(test_y))
    test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
    result=rbind(result, data.frame(i,train_auc, test_auc))
    list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
    list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
  }
  range(result$train_auc);range(result$test_auc)
  median(result$train_auc);median(result$test_auc)
  
  plot_ROC(list_test,show.AUC = F,show.points = F,
           show.eta=FALSE,#不显示截点
           show.sens=FALSE #不显示各种率
  )+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
    theme(axis.text = element_text(color="black", size=13),
          axis.title = element_text(color="black", size=13),
          panel.grid = element_blank())+
    annotate("text",x=0.65,y=0.2, label="AUROC range: 0.41-0.93", size=4)+
    annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.70", size=4)
  # ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
  ggsave("rescaled test 100 times XGB_MTXLEF(p-top1)__NEW.pdf", width = 3.2, height = 3)
}
setwd("H:/分析/RA/返修第二版/")
library(scales);library(randomForest);
library(reshape2);library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
library(scales);library(pROC);library(lifecycle);library(purrr);library(pROC);library(ggplot2);library(gtable)
library(dplyr);library(data.table);library(xgboost) ;library(Matrix);library(caTools);library(caret) # 调参和计算模型评价参数使用
library(pROC);library(ggplot2) ;library(ggpubr);library(ggprism) ;library(caTools);library(scales);library(Ckmeans.1d.dp)
library(plyr);library(dplyr)
data <- read.csv("RA_DATAKNN1.csv")
data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=read.csv("gene_mtxlef.csv",header = F)[-1,2]
data$Drugs.Response
d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"gene","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
dat1=d
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,
                           ifelse(dat1$Drugs.Response=="No Response",0,NA))
dat1<-dat1[,-c(1:7)]
a<-as.integer(runif(50,min=1,max=123456789))

newtrain<-dat1
head(newtrain)
str(newtrain)
sparse_matrix = sparse.model.matrix(Drugs.Response~ ., data = newtrain)[,-1]
head(sparse_matrix)
output_vector = as.numeric(newtrain$Drugs.Response)
# 建模（不调参）
xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
              eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")

# 特征重要性排序
imp = xgb.importance(colnames(sparse_matrix),xgb)



data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")

d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
dat1=d
# 全部变量中心化到-1至1水平
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))



for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}

dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))

Y=dat1[dat1$Drugs.Response==1,]
N=dat1[dat1$Drugs.Response==0,]

result=NULL
list_train=list()
list_test=list()
coef_all=NULL
i=1
for(i in 1:100){
  set.seed(i)
  Y1<-Y[sample(1:nrow(Y),9,replace = F),]
  Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
  N1<-N[sample(1:nrow(N),9,replace = F),]
  N2<-N[!c(N$Sample%in%N1$Sample),]
  N2<-N2[sample(1:nrow(N2),10,replace = F),]
  train=rbind(Y1,N2)
  test=rbind(Y2,N1)
  y=train$Drugs.Response
  y_test <- test$Drugs.Response
  x <- cbind(train[,c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")])
  x_test <- cbind(test[,c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")])
  dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
  dtest <- xgb.DMatrix(data = as.matrix(x_test))
  sparse_matrix = sparse.model.matrix(as.factor(Drugs.Response)~.,data = train[,-c(1:8)])[,-1]
  sparse_matrix_test = sparse.model.matrix(as.factor(Drugs.Response)~.,data = test[,-c(1:8)])[,-1]
  head(sparse_matrix)
  output_vector = as.numeric(train$Drugs.Response)
  # 建模（不调参）
  xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
  #caret.Control <- trainControl(method = "adaptive_cv",number = 10,repeats=5,verboseIter = FALSE,returnData = FALSE,selectionFunction="best",returnResamp = "final",search = "grid",seeds=set.seed(123))
  #caret.grid <- expand.grid(nrounds = c(100,200,500),max_depth = c(3,5,6),eta = c(0.01, 0.1, 0.3),gamma = c(0,0.1,0.5),colsample_bytree = 0.7,min_child_weight = c(1,3),subsample =0.7)
  #xgb_model_caret <- train(as.factor(Drugs.Response)~.,data = train[,-c(1:8)],method = "xgbTree",trControl = caret.Control,tuneGrid = caret.grid, verbose=FALSE,verbosity=0)
  #params.caret.bst <- list(objective = "binary:logistic",eval_metric = "logloss",max_depth =5,eta = 0.1)
  #xgb_model_caret.bst <- xgboost(data=dtrain,params = params.caret.bst,nrounds =100)
  print(i)
  train_y <-predict(xgb, sparse_matrix,type="response")
  df=data.frame(y,train_y=as.numeric(train_y))
  train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
  test_y <- predict(xgb,sparse_matrix_test, type="response")
  df1=data.frame(y_test,test_y=as.numeric(test_y))
  test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
  result=rbind(result, data.frame(i,train_auc, test_auc))
  list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
  list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
}
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)

plot_ROC(list_test,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.52-0.95", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.78", size=4)
# ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled test 100 times XBG_MTXLEF(p-top5)-OLD.pdf", width = 3.2, height = 3)
{setwd("H:/分析/RA/返修第二版/")
  library(scales);library(randomForest);
  library(reshape2);library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
  library(scales);library(pROC);library(lifecycle);library(purrr);library(pROC);library(ggplot2);library(gtable)
  library(dplyr);library(data.table);library(xgboost) ;library(Matrix);library(caTools);library(caret) # 调参和计算模型评价参数使用
  library(pROC);library(ggplot2) ;library(ggpubr);library(ggprism) ;library(caTools);library(scales);library(Ckmeans.1d.dp)
  library(plyr);library(dplyr)
  data <- read.csv("RA_DATAKNN1.csv")
  data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  MTXHCQ=read.csv("gene_mtxhcq.csv",header = F)[-1,2]
  data$Drugs.Response
  d=data[which(data$Drugs=="MTX+HCQ"&data$Drugs.Response!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"gene","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
  dat1=d
  dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
  dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,
                             ifelse(dat1$Drugs.Response=="No Response",0,NA))
  dat1<-dat1[,-c(1:7)]
  a<-as.integer(runif(50,min=1,max=123456789))
  
  newtrain<-dat1
  head(newtrain)
  str(newtrain)
  sparse_matrix = sparse.model.matrix(Drugs.Response~ ., data = newtrain)[,-1]
  head(sparse_matrix)
  output_vector = as.numeric(newtrain$Drugs.Response)
  # 建模（不调参）
  xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
  
  # 特征重要性排序
  imp = xgb.importance(colnames(sparse_matrix),xgb)
  
  
  
  data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  MTXHCQ=c("GGT1","RPL27A")
  
  d=data[which(data$Drugs=="MTX+HCQ"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
  dat1=d
  # 全部变量中心化到-1至1水平
  dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
  dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))
  
  
  
  for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}
  
  dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))
  
  Y=dat1[dat1$Drugs.Response==1,]
  N=dat1[dat1$Drugs.Response==0,]
  
  result=NULL
  list_train=list()
  list_test=list()
  coef_all=NULL
  i=1
  for(i in 1:100){
    set.seed(i)
    Y1<-Y[sample(1:nrow(Y),7,replace = F),]
    Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
    N1<-N[sample(1:nrow(N),7,replace = F),]
    N2<-N[!c(N$Sample%in%N1$Sample),]
    N2<-N2[sample(1:nrow(N2),7,replace = F),]
    train=rbind(Y1,N2)
    test=rbind(Y2,N1)
    y=train$Drugs.Response
    y_test <- test$Drugs.Response
    x <- cbind(train[,c("GGT1")])
    x_test <- cbind(test[,c("GGT1")])
    dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
    dtest <- xgb.DMatrix(data = as.matrix(x_test))
    sparse_matrix = sparse.model.matrix(as.factor(Drugs.Response)~.,data = train[,-c(1:8)])[,-1]
    sparse_matrix_test = sparse.model.matrix(as.factor(Drugs.Response)~.,data = test[,-c(1:8)])[,-1]
    head(sparse_matrix)
    output_vector = as.numeric(train$Drugs.Response)
    # 建模（不调参）
    xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
    #caret.Control <- trainControl(method = "adaptive_cv",number = 10,repeats=5,verboseIter = FALSE,returnData = FALSE,selectionFunction="best",returnResamp = "final",search = "grid",seeds=set.seed(123))
    #caret.grid <- expand.grid(nrounds = c(100,200,500),max_depth = c(3,5,6),eta = c(0.01, 0.1, 0.3),gamma = c(0,0.1,0.5),colsample_bytree = 0.7,min_child_weight = c(1,3),subsample =0.7)
    #xgb_model_caret <- train(as.factor(Drugs.Response)~.,data = train[,-c(1:8)],method = "xgbTree",trControl = caret.Control,tuneGrid = caret.grid, verbose=FALSE,verbosity=0)
    #params.caret.bst <- list(objective = "binary:logistic",eval_metric = "logloss",max_depth =5,eta = 0.1)
    #xgb_model_caret.bst <- xgboost(data=dtrain,params = params.caret.bst,nrounds =100)
    print(i)
    train_y <-predict(xgb, sparse_matrix,type="response")
    df=data.frame(y,train_y=as.numeric(train_y))
    train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
    test_y <- predict(xgb,sparse_matrix_test, type="response")
    df1=data.frame(y_test,test_y=as.numeric(test_y))
    test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
    result=rbind(result, data.frame(i,train_auc, test_auc))
    list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
    list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
  }
  range(result$train_auc);range(result$test_auc)
  median(result$train_auc);median(result$test_auc)
  
  plot_ROC(list_test,show.AUC = F,show.points = F,
           show.eta=FALSE,#不显示截点
           show.sens=FALSE #不显示各种率
  )+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
    theme(axis.text = element_text(color="black", size=13),
          axis.title = element_text(color="black", size=13),
          panel.grid = element_blank())+
    annotate("text",x=0.65,y=0.2, label="AUROC range: 0.45-0.97", size=4)+
    annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.75", size=4)
  # ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
  ggsave("rescaled test 100 times XGB_MTXHCQ(p-top1)__NEW.pdf", width = 3.2, height = 3)
}
{setwd("H:/分析/RA/返修第二版/")
  library(scales);library(randomForest);
  library(reshape2);library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
  library(scales);library(pROC);library(lifecycle);library(purrr);library(pROC);library(ggplot2);library(gtable)
  library(dplyr);library(data.table);library(xgboost) ;library(Matrix);library(caTools);library(caret) # 调参和计算模型评价参数使用
  library(pROC);library(ggplot2) ;library(ggpubr);library(ggprism) ;library(caTools);library(scales);library(Ckmeans.1d.dp)
  library(plyr);library(dplyr)
  data <- read.csv("RA_DATAKNN1.csv")
  data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  MTXHCQ=read.csv("gene_mtxhcq.csv",header = F)[-1,2]
  data$Drugs.Response
  d=data[which(data$Drugs=="MTX+HCQ"&data$Drugs.Response!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"gene","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
  dat1=d
  dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
  dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,
                             ifelse(dat1$Drugs.Response=="No Response",0,NA))
  dat1<-dat1[,-c(1:7)]
  a<-as.integer(runif(50,min=1,max=123456789))
  
  newtrain<-dat1
  head(newtrain)
  str(newtrain)
  sparse_matrix = sparse.model.matrix(Drugs.Response~ ., data = newtrain)[,-1]
  head(sparse_matrix)
  output_vector = as.numeric(newtrain$Drugs.Response)
  # 建模（不调参）
  xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
  
  # 特征重要性排序
  imp = xgb.importance(colnames(sparse_matrix),xgb)
  
  
  
  data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
  table(is.na(data[,16:ncol(data)]))
  MTXHCQ=c("GGT1","RPL27A")
  
  d=data[which(data$Drugs=="MTX+HCQ"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXHCQ,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","Gender","SJC","TJC","CRP.1")]
  dat1=d
  # 全部变量中心化到-1至1水平
  dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
  dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))
  
  
  
  for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}
  
  dat1$Gender=ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))
  
  Y=dat1[dat1$Drugs.Response==1,]
  N=dat1[dat1$Drugs.Response==0,]
  
  result=NULL
  list_train=list()
  list_test=list()
  coef_all=NULL
  i=1
  for(i in 1:100){
    set.seed(i)
    Y1<-Y[sample(1:nrow(Y),7,replace = F),]
    Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
    N1<-N[sample(1:nrow(N),7,replace = F),]
    N2<-N[!c(N$Sample%in%N1$Sample),]
    N2<-N2[sample(1:nrow(N2),7,replace = F),]
    train=rbind(Y1,N2)
    test=rbind(Y2,N1)
    y=train$Drugs.Response
    y_test <- test$Drugs.Response
    x <- cbind(train[,c("GGT1","RPL27A")])
    x_test <- cbind(test[,c("GGT1","RPL27A")])
    dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
    dtest <- xgb.DMatrix(data = as.matrix(x_test))
    sparse_matrix = sparse.model.matrix(as.factor(Drugs.Response)~.,data = train[,-c(1:8)])[,-1]
    sparse_matrix_test = sparse.model.matrix(as.factor(Drugs.Response)~.,data = test[,-c(1:8)])[,-1]
    head(sparse_matrix)
    output_vector = as.numeric(train$Drugs.Response)
    # 建模（不调参）
    xgb = xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")
    #caret.Control <- trainControl(method = "adaptive_cv",number = 10,repeats=5,verboseIter = FALSE,returnData = FALSE,selectionFunction="best",returnResamp = "final",search = "grid",seeds=set.seed(123))
    #caret.grid <- expand.grid(nrounds = c(100,200,500),max_depth = c(3,5,6),eta = c(0.01, 0.1, 0.3),gamma = c(0,0.1,0.5),colsample_bytree = 0.7,min_child_weight = c(1,3),subsample =0.7)
    #xgb_model_caret <- train(as.factor(Drugs.Response)~.,data = train[,-c(1:8)],method = "xgbTree",trControl = caret.Control,tuneGrid = caret.grid, verbose=FALSE,verbosity=0)
    #params.caret.bst <- list(objective = "binary:logistic",eval_metric = "logloss",max_depth =5,eta = 0.1)
    #xgb_model_caret.bst <- xgboost(data=dtrain,params = params.caret.bst,nrounds =100)
    print(i)
    train_y <-predict(xgb, sparse_matrix,type="response")
    df=data.frame(y,train_y=as.numeric(train_y))
    train_auc=multipleROC(y~train_y,data=df,plot =F)[["auc"]]
    test_y <- predict(xgb,sparse_matrix_test, type="response")
    df1=data.frame(y_test,test_y=as.numeric(test_y))
    test_auc=multipleROC(y_test~test_y,data=df1,plot =F)[["auc"]]
    result=rbind(result, data.frame(i,train_auc, test_auc))
    list_train[[i]]=multipleROC(y~train_y,data=df,plot =F)
    list_test[[i]]=multipleROC(y_test~test_y,data=df1,plot =F)
  }
  range(result$train_auc);range(result$test_auc)
  median(result$train_auc);median(result$test_auc)
  
  plot_ROC(list_test,show.AUC = F,show.points = F,
           show.eta=FALSE,#不显示截点
           show.sens=FALSE #不显示各种率
  )+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
    theme(axis.text = element_text(color="black", size=13),
          axis.title = element_text(color="black", size=13),
          panel.grid = element_blank())+
    annotate("text",x=0.65,y=0.2, label="AUROC range: 0.45-0.98", size=4)+
    annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.76", size=4)
  # ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
  ggsave("rescaled test 100 times XGB_MTXHCQ(p-top2)__OLD.pdf", width = 3.2, height = 3)
}
########################SFig5D:boxplot#####################
library(ggplot2);library(dplyr);library(reshape2);library(ggplot2);library(ggpubr)
data<-read.csv("drug respense/pathway—MTX+HCQ2.csv")
data$PValue2
data %>%
  mutate(X.1= factor(X.1, levels = c("All","Female"))) %>%
  ggplot(aes(y =Term,x = X.1,size=Count,fill=PValue2))+
  geom_point(aes(size=Count,fill=PValue2),color='black',shape = 21)+
  scale_fill_gradient2(low="#3C82B9",mid = "white",high = "red")+
  scale_y_discrete(limits=rev(unique(data$Term)),position = "left")+
  scale_size_continuous(range=c(4,13))+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        axis.line=element_line(linetype=1,color="black",size=0.75),
        panel.border = element_rect(fill=NA,color="black", size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black", hjust = 1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom") 
ggsave("drug respense/pathway_MTX_HCQ_CCP+(Y_N).pdf", width = 2.7, height = 9)





########################sFig5E:clinical factor################
library(dplyr);library(glmnet);library(ggpubr);library(DMwR2);library(mice);
library(scales);library(pROC);library(lifecycle);library(purrr)
setwd("H:/分析/RA/返修第二版/")
data=read.csv("RA_DATAKNN1.csv")
data[,c(5,8:13,16:ncol(data))] = lapply(data[,c(5,8:13,16:ncol(data))], as.numeric)
table(is.na(data[,16:ncol(data)]))
MTXLEF=c("CBR1","LGALS3BP","MYH9","COL1A1","ECI2")

d=data[which(data$Drugs=="MTX+LEF"&data$Drugs.Response!=""&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),colnames(data)%in%c(MTXLEF,"Sample","DAS28.CRP","DDAS28.CRP","Drugs.Response","Age","VAS","SJC","TJC","CRP.1")]
dat1=d
# 全部变量中心化到-1至1水平
dat1[,10:ncol(dat1)]=lapply(dat1[,10:ncol(dat1)], function(x){rescale(x, to =c(-1,1))})
dat1$Drugs.Response=ifelse(dat1$Drugs.Response=="Response",1,ifelse(dat1$Drugs.Response=="No Response",0,NA))



for(i in 10:ncol(dat1)){p=t.test(dat1[,i]~dat1$Drugs.Response,alternative = c("two.sided"),paired=F,var.equal=TRUE)[["p.value"]];print(p)}

#dat1$G  =ifelse(dat1$Gender=="Female",1,ifelse(dat1$Gender=="Male",0,NA))

Y=dat1[dat1$Drugs.Response==1,]
N=dat1[dat1$Drugs.Response==0,]

result=NULL
list_train=list()
list_test=list()
coef_all=NULL
i=1
for(i in 1:100){
  set.seed(i)
  Y1<-Y[sample(1:nrow(Y),10,replace = F),]
  Y2<-Y[!c(Y$Sample%in%Y1$Sample),]
  N1<-N[sample(1:nrow(N),10,replace = F),]
  N2<-N[!c(N$Sample%in%N1$Sample),]
  N2<-N2[sample(1:nrow(N2),9,replace = F),]
  train=rbind(Y1,N1)
  test=rbind(Y2,N2)
  y=train$Drugs.Response
  y_test <- test$Drugs.Response
  x <- cbind(train[,c("VAS","SJC","TJC","CRP.1","CBR1","LGALS3BP","MYH9","COL1A1","ECI2")])
  x_test <- cbind(test[,c("VAS","SJC","TJC","CRP.1","CBR1","LGALS3BP","MYH9","COL1A1","ECI2")])
  
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
write.csv(result,"rescaled 100 times ridge_MTXLEF(p-top5)_sample3.csv",row.names = F)
write.csv(coef_all,"rescaled coef 100 times ridge_MTXLEF(p-top5)_sample3.csv",row.names = F)
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)
plot_ROC(list_train,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(049,124,183,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.91-1.00", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.99", size=4)
ggsave("rescaled train 100 times ridge_MTXLEF(p-top5)-SAMPLE3.pdf", width = 3.2, height = 3)

plot_ROC(list_test,show.AUC = F,show.points = F,
         show.eta=FALSE,#不显示截点
         show.sens=FALSE #不显示各种率
)+scale_color_manual(values = rep(rgb(220,109,087,10, maxColorValue = 255),100))+
  theme(axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        panel.grid = element_blank())+
  annotate("text",x=0.65,y=0.2, label="AUROC range: 0.66-1.00", size=4)+
  annotate("text",x=0.65,y=0.1, label="Median AUROC: 0.90", size=4)
# ggsave("test 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled test 100 times ridge_MTXLEF(p-top5)-SAMPLE3.pdf", width = 3.2, height = 3)



result=read.csv("rescaled 100 times ridge_MTXLEF(p-top5).csv")
range(result$train_auc);range(result$test_auc)
median(result$train_auc);median(result$test_auc)
re=reshape2::melt(result,id.vars=1)
ggplot(re,aes(value, fill=variable))+
  geom_density(alpha=.5)+
  geom_vline(xintercept = c(median(result$train_auc), median(result$test_auc)), linetype=2, color=c("#5D669F","#AF322F"))+
  scale_fill_manual(values = c("#317CB7","#DC6D57"),name="", label=c("Train","Test"))+
  theme_minimal()+
  labs(x="AUROC",y="Density")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.text = element_text(color="black", size=13),
        axis.title = element_text(color="black", size=13),
        legend.position = "bottom")
# ggsave("density_plot 100 times ridge 5clinical.pdf", width = 3, height = 3)
ggsave("rescaled density_plot 100 times ridge_MTXLEF(p-top5).pdf",  width = 3, height = 3.4)


########################SFig6F:dot plot-Elisa####################
library(ggbeeswarm);library(reshape2);library(ggpubr);library(dplyr);library(ggsignif);library(ggplot2);library(ggrepel);library(ggbreak)
data<-read.csv("MTX+LEF_ELISA.csv")
{Y<-data[which(data$RESPONSE=="Y"),]
  N<-data[which(data$RESPONSE=="N"),]
  P<- vector(length = 5)
  for(i in 1:5)try({
    P[i]=as.numeric(wilcox.test(as.numeric(Y[,i+3]),as.numeric(N[,i+3]),paired=F,exact=F)[3])
  })
  P<-data.frame(names(data)[c(4:8)],P)
}




{
  data1<- melt(data,  #id.vars表示不需要合并的列；
               measure.vars = colnames(data)[c(4:8)], #measure.vars表示需要合并的列；
               variable.name ="Groups", #variable.name表示列名合并后的新列名；
               value.name ="Value") 
  names(data1)[c(4:5)]<-c("Groups","Value")
  data1 %>% na.omit() %>% 
    group_by(RESPONSE,Groups) %>% 
    summarise(mean_value=mean(Value),
              sd_value=sd(Value)) -> df1
  df1 %>% 
    group_by(RESPONSE,Groups) %>% 
    mutate(new_col=cumsum(mean_value)) -> df2
  df2$mean_value
  ggplot(data=df2,aes(x=Groups,y=mean_value,fill=RESPONSE))+
    geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Groups),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
    geom_bar(aes(y=mean_value,fill=RESPONSE),position = "dodge",stat="identity",width = 0.7,lwd=1)+
    geom_jitter(data=data1,aes(x=data1$Groups,y=data1$Value,color=data1$RESPONSE),shape=21,stroke =1.5,size=3,stat="identity",position = position_jitterdodge(0.2),fill="white")+
    #geom_dotplot(data=data2,aes(x=data2$Gene.name..human.,y=data2$value,colors=data2$Gene.name..human.),size=5,binaxis = 'y', stackdir = 'center',dotsize = 0.5)+
    scale_fill_manual(values = c("N"="#D4D4D4","Y"="#9586BC"),guide=F)+
    scale_color_manual(values = c("N"="#999999","Y"="#604E8C"),guide=F)+
    #geom_signif(data=data1,comparisons =  list(c('N','Y')),test = "t.test",step_increase = 0.1,map_signif_level =T)+
    labs(x="",y="O.D. (450nm)")+
    theme(axis.line=element_line(linetype=1,color="black",size=0.75),
          axis.ticks=element_line(color="black",size=0.75,lineend = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
          title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))+
    facet_wrap( ~Groups, ncol =5,nrow=,scales="free")
}
ggsave("MTXLEF_ELISA.pdf", width =12, height = 3)
data<-read.csv("MTX+HCQ_ELISA.csv")
Y<-data[which(data$RESPONSE=="Y"),]
N<-data[which(data$RESPONSE=="N"),]
P<- vector(length = 2)
for(i in 1:2)try({
  P[i]=as.numeric(wilcox.test(as.numeric(Y[,i+3]),as.numeric(N[,i+3]),paired=F,exact=F)[3])
})
P<-data.frame(names(data)[c(4:5)],P)

{
  data1<- melt(data,  #id.vars表示不需要合并的列；
               measure.vars = colnames(data)[c(4:5)], #measure.vars表示需要合并的列；
               variable.name ="Groups", #variable.name表示列名合并后的新列名；
               value.name ="Value") 
  names(data1)[c(4:5)]<-c("Groups","Value")
  data1 %>% na.omit() %>% 
    group_by(RESPONSE,Groups) %>% 
    summarise(mean_value=mean(Value),
              sd_value=sd(Value)) -> df1
  df1 %>% 
    group_by(RESPONSE,Groups) %>% 
    mutate(new_col=cumsum(mean_value)) -> df2
  df2$mean_value
  ggplot(data=df2,aes(x=Groups,y=mean_value,fill=RESPONSE))+
    geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Groups),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
    geom_bar(aes(y=mean_value,fill=RESPONSE),position = "dodge",stat="identity",width = 0.7,lwd=1)+
    geom_jitter(data=data1,aes(x=data1$Groups,y=data1$Value,color=data1$RESPONSE),shape=21,stroke =1.5,size=3,stat="identity",position = position_jitterdodge(0.2),fill="white")+
    #geom_dotplot(data=data2,aes(x=data2$Gene.name..human.,y=data2$value,colors=data2$Gene.name..human.),size=5,binaxis = 'y', stackdir = 'center',dotsize = 0.5)+
    scale_fill_manual(values = c("N"="#D4D4D4","Y"="#9586BC"),guide=F)+
    scale_color_manual(values = c("N"="#999999","Y"="#604E8C"),guide=F)+
    geom_signif(data=data1,aes(x=data1$Groups,y=data1$Value),comparisons =  list(c('N','Y')),test = "t.test",step_increase = 0.1,map_signif_level =T)+
    labs(x="",y="O.D. (450nm)")+
    theme(axis.line=element_line(linetype=1,color="black",size=0.75),
          axis.ticks=element_line(color="black",size=0.75,lineend = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
          title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))+
    facet_wrap( ~Groups, ncol =5,nrow=,scales="free")
}
ggsave("MTXHCQ_ELISA.pdf", width =5, height = 3)

