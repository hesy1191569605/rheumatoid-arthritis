
####################DAS28 in different gender#########################

ggplot(data,aes(x =data$Gender,y=data$DAS28.CRP.HM)) + 
  geom_violin(aes(fill=data$Gender),width=0.5)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.3,fill="white") +
  #geom_jitter(aes(color=data$Gender),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_fill_manual(values =c('Female'='#AF322F','Male'='#737AAC'),guide=FALSE)+
  geom_signif(comparisons =  list(c("Female", "Male")),test = "wilcox.test",
              y_position = c(9),map_signif_level =T)+
  ylim(0,10)+
  scale_y_continuous(expand = c(0,1.25),breaks = c(0,2,4,6,8,10)) + 
  labs(x="",y="DAS28-CRP")+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28-gender.pdf", width = 2.5, height = 3)


data<-data[which(data$DAS28.CRP.HM!="NA"&data$Age!="NA"),]
ggplot(data,aes(x =as.numeric(data$Age),y=data$DAS28.CRP.HM,color=data$Gender)) + 
  geom_point(aes(color=data$Gender),)+
  geom_smooth(aes(color=data$Gender),method = "lm",size=1.5,se = T)+
  #geom_jitter(aes(color=data$Gender),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_color_manual(values =c('Female'='#AF322F','Male'='#737AAC'),guide=FALSE)+
  stat_cor(data=data, method = "pearson")+
  #ylim(0,10)+
  #scale_y_continuous(expand = c(0,1.25),breaks = c(0,2,4,6,8,10)) + 
  labs(x="Age(Years)",y="DAS28-CRP")+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28 age cor gender.pdf", width = 3, height = 3)

####################DAS28 in different age in female####################------
ggplot(data1,aes(x =data1$group2,y=data1$DAS28.CRP.HM)) + 
  geom_violin(aes(fill=group2),width=0.8)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.2,fill="white") +
  #geom_jitter(aes(color=group2),stroke =1.5,size=0.8,position = position_jitter(0.3))+
  scale_fill_manual(values =c('Female<45'='#eeabab','Female>45'='#d86967',"Male"="#58539f"),guide=FALSE)+
  geom_signif(comparisons =  list(c("Female>45", "Male"),c("Male","Female<45"),c("Female>45", "Female<45")),
              test = "wilcox.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
  #scale_y_continuous(expand = c(0,0)) + 
  labs(x="",y="DAS28-CRP")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("C:/Users/11915/Desktop/RA/913/FIG3/FIG3-D.pdf", width = 3, height = 4)
####################4 clinicalin different age in female####################------
data2<-data1[,c(11,14,15,16,17)]
data3<-reshape::melt(data2,id="group2")
ggplot(data3,aes(x =data3$group2,y=data3$value)) + 
  geom_violin(aes(fill=group2),width=0.8)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.18,fill="white") +
  #geom_jitter(aes(color=group2),stroke =1.5,size=0.8,position = position_jitter(0.3))+
  scale_fill_manual(values =c('Female<45'='#eeabab','Female>45'='#d86967'),guide=FALSE)+
  geom_signif(comparisons =  list(c("Female>45", "Female<45")),
              test = "wilcox.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
  #scale_y_continuous(expand = c(0,0)) + 
  labs(x="",y="DAS28-CRP")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~ variable, ncol =9,scales="free")
ggsave("C:/Users/11915/Desktop/RA/913/FIG3/FIG3-E.pdf", width = 9, height = 4)

####################Calculation  of linearly correlated proteins with clinical indicators#################
setwd("C:/Users/11915/Desktop/RA/913/")
data<-read.csv("cln-data1.csv")
data<-data[which(data$DAS28.CRP.HM!="NA"),]
ALL<-data
P_DAS28<-rep(NA,ncol(ALL)-18)
P_VAS<-rep(NA,ncol(ALL)-18)
P_SJC<-rep(NA,ncol(ALL)-18)
P_TJC<-rep(NA,ncol(ALL)-18)
P_CRP<-rep(NA,ncol(ALL)-18)
LM_DAS28<-rep(NA,ncol(ALL)-18)
LM_VAS<-rep(NA,ncol(ALL)-18)
LM_SJC<-rep(NA,ncol(ALL)-18)
LM_TJC<-rep(NA,ncol(ALL)-18)
LM_CRP<-rep(NA,ncol(ALL)-18)
P_DAS28_g<-rep(NA,ncol(ALL)-18)
P_VAS_g<-rep(NA,ncol(ALL)-18)
P_SJC_g<-rep(NA,ncol(ALL)-18)
P_TJC_g<-rep(NA,ncol(ALL)-18)
P_CRP_g<-rep(NA,ncol(ALL)-18)
LM_DAS28_g<-rep(NA,ncol(ALL)-18)
LM_VAS_g<-rep(NA,ncol(ALL)-18)
LM_SJC_g<-rep(NA,ncol(ALL)-18)
LM_TJC_g<-rep(NA,ncol(ALL)-18)
LM_CRP_g<-rep(NA,ncol(ALL)-18)
P_DAS28_a<-rep(NA,ncol(ALL)-18)
P_VAS_a<-rep(NA,ncol(ALL)-18)
P_SJC_a<-rep(NA,ncol(ALL)-18)
P_TJC_a<-rep(NA,ncol(ALL)-18)
P_CRP_a<-rep(NA,ncol(ALL)-18)
LM_DAS28_a<-rep(NA,ncol(ALL)-18)
LM_VAS_a<-rep(NA,ncol(ALL)-18)
LM_SJC_a<-rep(NA,ncol(ALL)-18)
LM_TJC_a<-rep(NA,ncol(ALL)-18)
LM_CRP_a<-rep(NA,ncol(ALL)-18)
i=1
for(i in 1:(ncol(ALL)-18))try({
  P_DAS28[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$DAS28.CRP.HM)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
  LM_DAS28[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$DAS28.CRP.HM)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
  P_VAS[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
  LM_VAS[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
  P_SJC[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
  LM_SJC[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
  P_TJC[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
  LM_TJC[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
  P_CRP[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
  LM_CRP[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
  P_DAS28_a[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$DAS28.CRP.HM)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
  LM_DAS28_a[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$DAS28.CRP.HM)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
  P_VAS_a[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
  LM_VAS_a[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
  P_SJC_a[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
  LM_SJC_a[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
  P_TJC_a[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
  LM_TJC_a[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
  P_CRP_a[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
  LM_CRP_a[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
  P_DAS28_g[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$DAS28.CRP.HM)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
  LM_DAS28_g[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$DAS28.CRP.HM)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
  P_VAS_g[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
  LM_VAS_g[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
  P_SJC_g[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
  LM_SJC_g[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
  P_TJC_g[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
  LM_TJC_g[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
  P_CRP_g[i] <- coef(summary(lm(scale(ALL[,(i+18)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
  LM_CRP_g[i] <-coefficients(lm(scale(ALL[,(i+18)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
})

test<-data.frame(colnames(ALL)[-c(1:18)],P_DAS28, P_VAS,P_SJC,P_TJC,P_CRP,
                 LM_DAS28,LM_VAS,LM_SJC,LM_TJC,LM_CRP,
                 P_DAS28_g,P_VAS_g,P_SJC_g,P_TJC_g,P_CRP_g,
                 LM_DAS28_g,LM_VAS_g,LM_SJC_g,LM_TJC_g,LM_CRP_g,
                 P_DAS28_a,P_VAS_a,P_SJC_a,P_TJC_a,P_CRP_a,
                 LM_DAS28_a,LM_VAS_a,LM_SJC_a,LM_TJC_a,LM_CRP_a)
FY<-data[which(data$group2=="Female<45"),]
FO<-data[which(data$group2=="Female>45"),]
MY<-data[which(data$group2=="Male"),]
MO<-data[which(data$group2=="Male"),]
FY_median<- apply(FY[,-c(1:18)],2,median,na.rm=T)
FO_median<- apply(FO[,-c(1:18)],2,median,na.rm=T)
MY_median<- apply(MY[,-c(1:18)],2,median,na.rm=T)
MO_median<- apply(MO[,-c(1:18)],2,median,na.rm=T)
FC_FOY<-FO_median/FY_median
FC_MOY<-MO_median/MY_median
FC_YFM<-FY_median/MY_median
FC_OFM<-FO_median/MO_median
P_FOY<-rep(NA,ncol(FO)-18)
P_MOY<-rep(NA,ncol(FO)-18)
P_YFM<-rep(NA,ncol(FO)-18)
P_OFM<-rep(NA,ncol(FO)-18)
name<-rep(NA,ncol(FO)-18)
i=1
for(i in 1:(ncol(FO)-18))try({
  P_FOY[i] <- t.test(as.numeric(FO[,i+18]),as.numeric(FY[,i+18]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_MOY[i] <- t.test(as.numeric(MO[,i+18]),as.numeric(MY[,i+18]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_YFM[i] <- t.test(as.numeric(FY[,i+18]),as.numeric(MY[,i+18]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_OFM[i] <- t.test(as.numeric(FO[,i+18]),as.numeric(MO[,i+18]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  name[i] <- colnames(FO)[i+18]
})
test2<-data.frame(name,P_FOY,FC_FOY,P_MOY,FC_MOY,P_YFM,FC_YFM,P_OFM,FC_OFM,FY_median,FO_median,MY_median,MO_median)
test<-cbind(test,test2)
write.csv(test,'DAS28_ALL_TEST2.csv')


####################The number of linearly correlated proteins with clinical indicators#########
library(dbplyr);library(reshape2);library(ggplot2);library(dplyr)
setwd("C:/Users/11915/Desktop/RA/913/FIG3")
data<-read.csv("cor count.csv")
data$x
data %>%
  mutate(group = factor(group, levels = c("pos","neg"))) %>%
  mutate(x = factor(x, levels = c("DAS28","VAS","TJC","SJC","CRP"))) %>%
  ggplot(aes(y =Count,x= x)) + 
  geom_bar(aes(x= x,fill=group),stat="identity",position=position_dodge(0.7),width = 0.5)+
  scale_fill_manual(values =c('pos'='#AF322F','neg'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  labs(x="")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("5 clinical cor count.pdf", width =4, height = 2.8)
####################DAS28 linear correlation protein calculation###########
setwd("C:/Users/11915/Desktop/RA/913/FIG3/")
test<-read.csv("Clinical cor2.csv")
library(ggplot2)
library(ggrepel)
ggplot(test[which(test$X!="DAS28"),],aes(x =gene,y =X,size=abs(-log10(P)),fill=LM))+
  geom_point(aes(size=abs(-log10(P))),color='black',shape = 21)+
  scale_fill_gradient2(low ="#4569bb",mid ="white",high ="#DD5B51")+
  scale_size_continuous(range=c(6,10))+
  labs(x="",y="")+
  scale_x_discrete(limits=unique(test$gene))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 12, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 12,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28-protein cor 4.pdf", width = 11, height =3.5)

ggplot(test[which(test$X=="DAS28"),],aes(x =gene,y =LM,fill=LM))+
  geom_bar(aes(x =gene,y =LM,fill=LM),stat="identity",position=position_dodge())+
  scale_x_discrete(limits=unique(test$gene))+
  scale_fill_gradient2(low ="#4569bb",mid ="white",high ="#DD5B51")+
  labs(x="",y="")+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 12, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 12,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28-protein cor-das28.pdf", width = 11, height = 3)
####################pathway###########
setwd("C:/Users/11915/Desktop/RA/913/FIG3/")
data<-read.csv("PATHWAY.csv")
library(ggplot2);library(ggrepel)
ggplot(data,aes(x =data$PValue,y =data$Fold.Enrichment1,fill=data$Fold.Enrichment2))+
  geom_point(aes(fill = data$Fold.Enrichment2,size =data$Count),shape=21) + 
  scale_fill_gradient2(high ="#DD5B51",mid ="white",low ="#4569bb")+
  geom_text_repel(aes(label =data$Term), size = 4,show.legend = F)+
  labs(y="Fold enrichment",x="-log10(p value)")+
  scale_size_continuous(range=c(7,14))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28-protein pathway cor.pdf", width = 10, height = 4)
####################4 Clinical scatterplots###########
library(reshape2);library(dplyr);library(ggplot2);library(ggrepel);library(ggbreak)
setwd("C:/Users/11915/Desktop/RA/913/FIG3")
data<-read.csv("4 clinical.csv",header = T)[,-c(2:3)]
n<- 4   #指定比较组数-依据数据进行调整；
i_log2FC<- seq(3, by=2, length.out= n ) #指定log2FC所在的列数；
i_Pvalue<- seq(2, by=2, length.out= n ) #指定Pvalue所在的列数；
#提取差异倍数数据
data_log2FC<- data[c(1,i_log2FC)]
str<- colnames(data)[c(1,i_log2FC)]
colname<- gsub("LM_","",str)
colnames(data_log2FC) <- colname
head(data_log2FC)

#提取Pvalue数据
data_PValue<- data[c(1,i_Pvalue)]
colnames(data_PValue ) <- colname
head(data_PValue)

#将宽型数据转成长型数据#
df1<- melt(data_log2FC,id.vars="X",  #id.vars表示不需要合并的列；
           measure.vars = colnames(data_log2FC)[2:5], #measure.vars表示需要合并的列；
           variable.name ="Groups", #variable.name表示列名合并后的新列名；
           value.name ="log2FC")  #value.name表示数值合并后的新列名。
df2<- melt(data_PValue,id.vars="X",
           measure.vars = colnames(data_PValue)[2:5],
           variable.name ="Groups",
           value.name ="PValue")
df3<- data.frame(df1,df2[3]) #合并两个表格；
head(df3)

df3$color_pre[(df3$PValue>0.05)]<-"no"
df3$color_pre[(df3$PValue<0.05)&(df3$log2FC>0)] <- "up"
df3$color_pre[(df3$PValue<0.05)&(df3$log2FC<0)]  <- "down"

up<- df3 %>% group_by(Groups) %>% summarise(max(-log10(PValue)))
down<-  df3 %>% group_by(Groups) %>% summarise(max(-log10(PValue)))
col_1<- data.frame(up,down[2])
max<- max(max(col_1$max..log10.PValue..))
expan<- max/20

groups<-unique(df3$Groups)
dfbar<-data.frame(x=groups,y=0,label=c(1:length(groups)))
mycolor<- c("#D5ABAC","#5A96B5","#AA92C3","#67867C") 
#绘图——背景柱#
p<-ggplot()+
  geom_col(data = col_1,aes(x = Groups, y =max..log10.PValue..+expan),width =0.7,
           fill="#CCCCCC",alpha=0.4,show.legend = F)+
  geom_jitter(data = df3,aes(x = Groups, y = -log10(PValue), color = color_pre),
              size= 2,width =0.35,show.legend = T)+
  scale_color_manual(name=NULL,values =alpha(c("#737AAC","grey","#AF322F"),0.5))+
  geom_bar(data=dfbar,aes(x=groups,y=y+1,fill=groups), levels = c("VAS","TJC","SJC","CRP"),stat ="identity",width = 0.8)+
  geom_bar(data=dfbar,aes(x=groups,y=y-1,fill=groups), levels = c("VAS","TJC","SJC","CRP"),stat ="identity",width = 0.8)+
  scale_fill_manual(name="Groups",values=alpha(mycolor,1))+
  labs(x="",y="Beta lm")+
  scale_y_continuous(limits = c(-1,16.5), breaks= c(0,5,10,11,12,13,14,15,16), expand= expansion(add = 0))+
  theme_classic()+
  theme(axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),
        axis.line.y = element_line(linetype=1,color="black",size=0.75),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))
p
p3<-p+scale_y_break(c(11,15),scales = 0.1,space = 0.2)
p3
ggsave("4clinic cor.pdf", width = 7, height = 3)
####################9 protein level in different age in female###################------
data1$DAS28.CRP.HM


MTXHCQ=c("A2M","APOC3","TF","AHSG","FGG","RBP4","COL1A2","HP","CRP.1")
d=data1[which(data1$DAS28.CRP.HM!="NA"),colnames(data1)%in%c(MTXHCQ,"gene","group2","DAS28.CRP.HM","DDAS28.HM","VAS","SJC","TJC","CRP")]
d=d[,c(1:8,13,12,9,11,14,17,10,15,16)]
d2<-reshape::melt(d,id=names(d)[1:8])


d3<-d2[which(d2$group2!="Male"),]
A<-ggplot(d3,aes(x =group2,y=log2(value))) + 
  #geom_violin(aes(fill=group2),width=0.5)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.3,fill="white") +
  geom_jitter(aes(color=group2),stroke =1.5,size=1,position = position_jitter(0.3))+
  geom_boxplot(color="black",outlier.colour = NA,width=0.6,fill="white") +
  scale_color_manual(values =c('Female<45'='#eeabab','Female>45'='#d86967'),guide=FALSE)+
  geom_signif(comparisons =  list(c("Female>45", "Female<45")),test = "t.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
  #scale_y_continuous(expand = c(0,0)) + 
  labs(x="",y="DAS28-CRP")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~ variable, ncol =9,scales="free")
#ggsave("C:/Users/11915/Desktop/RA/913/FIG3/FIG3-G.pdf", width = 13, height = 3.5)


B<-ggplot(d2,aes(x=log2(d2$value),y=d2$DAS28.CRP.HM))+
  geom_point(size=2,color="#737AAC") +
  geom_smooth(method="lm",size=1.3, se =T, fullrange = TRUE,color="#58539f")+
  labs(x="log2(intensity)",y="DAS28-CRP")+
  #scale_color_manual(values = c("#AD74B6","#548235"))+
  #@stat_cor(method = "pearson")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +  facet_wrap( ~ variable, ncol =9,scales="free")

C<-ggarrange(A,B,nrow=2,legend = NULL,heights = c(0.55,0.45))
C
ggsave("C:/Users/11915/Desktop/RA/913/FIG3/FIG3-G2.pdf", width = 13, height = 5)

