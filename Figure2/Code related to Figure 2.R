########################calculate the difference###################
setwd("C:/Users/11915/Desktop/RA返修/")
ALL<-read.csv("RA_DATA1.csv",row.names = 1)
ALL[ALL== 0] <- NA
RA<-ALL[which(ALL$Group2=="RA_A"),-c(1:14)]
RA_P<-ALL[which(ALL$Group2=="RA_A"&ALL$Anti.citrullinated.peptide.antibodies=="Postive"),-c(1:14)]
RA_N<-ALL[which(ALL$Group2=="RA_A"&ALL$Anti.citrullinated.peptide.antibodies=="Negative"),-c(1:14)]
HP<-ALL[which(ALL$Group1=="Health"),-c(1:14)]
PRE<-ALL[which(ALL$Group1=="At-risk of RA"&ALL$Anti.citrullinated.peptide.antibodies=="Postive"),-c(1:14)]

RA_median<- apply(RA,2,median,na.rm=T)
HP_median<- apply(HP,2,median,na.rm=T)
PRE_median<- apply(PRE,2,median,na.rm=T)
RA_P_median<- apply(RA_P,2,median,na.rm=T)
RA_N_median<- apply(RA_N,2,median,na.rm=T)

FC_RA_H<-RA_median/HP_median
FC_PRE_H<-PRE_median/HP_median
FC_RA_PRE<-RA_median/PRE_median
FC_RA_P_H<-RA_P_median/HP_median
FC_RA_N_H<-RA_N_median/HP_median
FC_RA_P_N<-RA_P_median/RA_N_median
FC_RA_P_PRE<-RA_P_median/PRE_median
FC_RA_N_PRE<-RA_N_median/PRE_median

P_RA_H<-rep(NA,ncol(RA))
P_PRE_H<-rep(NA,ncol(RA))
P_RA_PRE<-rep(NA,ncol(RA))
P_RA_P_H<-rep(NA,ncol(RA))
P_RA_N_H<-rep(NA,ncol(RA))
P_RA_P_N<-rep(NA,ncol(RA))
P_RA_P_PRE<-rep(NA,ncol(RA))
P_RA_N_PRE<-rep(NA,ncol(RA))
i=1
for(i in 1:ncol(RA))try({
  P_RA_H[i] = t.test(as.numeric(RA[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_PRE_H[i] = t.test(as.numeric(PRE[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
  P_RA_PRE[i] = t.test(as.numeric(PRE[,i]),as.numeric(RA[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_RA_P_H[i] = t.test(as.numeric(RA_P[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_RA_N_H[i] = t.test(as.numeric(RA_N[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_RA_P_N[i] = t.test(as.numeric(RA_P[,i]),as.numeric(RA_N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_RA_P_PRE[i] = t.test(as.numeric(RA_P[,i]),as.numeric(PRE[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_RA_N_PRE[i] = t.test(as.numeric(RA_N[,i]),as.numeric(PRE[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
})
TEST<-cbind(HP_median,PRE_median,RA_median,RA_P_median,RA_N_median,
            FC_RA_H,FC_PRE_H,FC_RA_PRE,FC_RA_P_H,FC_RA_N_H,FC_RA_P_N,FC_RA_P_PRE,FC_RA_N_PRE,
            P_RA_H,P_PRE_H,P_RA_PRE,P_RA_P_H,P_RA_N_H,P_RA_P_N,P_RA_P_PRE,P_RA_N_PRE)
write.csv(TEST,"ALL_TEST.csv")
SS<-ALL[which(ALL$Group1=="SS"),-c(1:14)]
SSc<-ALL[which(ALL$Group1=="SSc"),-c(1:14)]
IIM<-ALL[which(ALL$Group1=="IIM"),-c(1:14)]
SLE<-ALL[which(ALL$Group1=="SLE"),-c(1:14)]
HP<-ALL[which(ALL$Group1=="Health"),-c(1:14)]

SS_median<- apply(SS,2,median,na.rm=T)
SSc_median<- apply(SSc,2,median,na.rm=T)
IIM_median<- apply(IIM,2,median,na.rm=T)
SLE_median<- apply(SLE,2,median,na.rm=T)
HP_median<- apply(HP,2,median,na.rm=T)

FC_SS_H<-SS_median/HP_median
FC_SSc_H<-SSc_median/HP_median
FC_IIM_H<-RA_median/HP_median
FC_SLE_H<-RA_P_median/HP_median


P_SS_H<-rep(NA,ncol(SS))
P_SSc_H<-rep(NA,ncol(SS))
P_IIM_H<-rep(NA,ncol(SS))
P_SLE_H<-rep(NA,ncol(SS))
i=1
for(i in 1:ncol(SS))try({
  P_SS_H[i] = t.test(as.numeric(SS[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_SSc_H[i] = t.test(as.numeric(SSc[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
  P_IIM_H[i] = t.test(as.numeric(IIM[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_SLE_H[i] = t.test(as.numeric(SLE[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  
})
TEST<-cbind(HP_median,SS_median,SSc_median,IIM_median,SLE_median,
            FC_SS_H,FC_SSc_H,FC_IIM_H,FC_SLE_H,
            P_SS_H,P_SSc_H,P_IIM_H,P_SLE_H)
write.csv(TEST,"OTHER_TEST.csv")
ALL<-read.csv("RA_DATA1.csv",row.names = 1)
ALL[ALL== 0] <- NA
table(ALL$Group2)
names(ALL)
A0<-ALL[which(ALL$Group2=="A0"),-c(1:14)]
A1<-ALL[which(ALL$Group2=="A1"),-c(1:14)]

HP<-ALL[which(ALL$Group1=="Health"),-c(1:14)]

A0_median<- apply(A0,2,median,na.rm=T)
A1_median<- apply(A1,2,median,na.rm=T)
HP_median<- apply(HP,2,median,na.rm=T)

FC_A1_A0<-A1_median/A0_median
FC_A1_H<-A1_median/HP_median
FC_A0_H<-A0_median/HP_median



P_A1_A0<-rep(NA,ncol(HP))
P_A1_H<-rep(NA,ncol(HP))
P_A0_H<-rep(NA,ncol(HP))
i=1
for(i in 1:ncol(HP))try({
  P_A1_A0[i] = t.test(as.numeric(A1[,i]),as.numeric(A0[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_A1_H[i] = t.test(as.numeric(A1[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
  P_A0_H[i] = t.test(as.numeric(A0[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  
})
TEST<-cbind(HP_median,A0_median,A1_median,
            FC_A1_A0,FC_A1_H,FC_A0_H,
            P_A1_A0,P_A1_H,P_A0_H)
write.csv(TEST,"PRE_TEST.csv")

ALL<-read.csv("RA_DATA1.csv",row.names = 1)
ALL[ALL== 0] <- NA
table(ALL$Group2)
names(ALL)
A1<-ALL[which(ALL$Group2=="A1"),-c(1:14)]
A1<-A1[c(5:7),]
B1<-ALL[which(ALL$Group2=="B1"),-c(1:14)]
FC1<-data.frame(t(A1[1,]/B1[1,]))
FC2<-data.frame(t(A1[2,]/B1[2,]))
FC3<-data.frame(t(A1[3,]/B1[3,]))
FC<-data.frame(FC1,FC2,FC3)
FC_median<- apply(FC,1,median,na.rm=T)
P<-rep(NA,ncol(B1))
i=1
for(i in 1:ncol(B1))try({
  P[i] = t.test(as.numeric(A1[,i]),as.numeric(B1[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
})
TEST<-cbind(FC_median,FC1,FC2,FC3,P)
write.csv(TEST,"PRE2_TEST.csv")







########################Fig2B:Hierarchical Clustering#########################
library(factoextra)
data=read.csv("CC2.csv",row.names = 1)[,-1]
sample<-read.csv("CC2-sample.csv")
data<-na.omit(data)
data<-data.matrix(data)
data<-t(data)
df <- scale(data)
E.dist  <- dist(df, method = "man")
h.cluster <- hclust(E.dist, method="ward.D2")
A<-fviz_dend(h.cluster, k = 4, cex = 0.5, 
             k_colors = c("#5A96B5", "#D5ABAC", "#67867C","#AA92C3"),
             color_labels_by_k = TRUE, 
             rect = TRUE)
plot(h.cluster)
cut.h.cluster <- cutree(h.cluster, k=4)
sample<-data.frame(sample[,c(1:2)],cut.h.cluster )
cluster_order<-h.cluster[["order"]]
sample2 <- sample[cluster_order, ] 
table(sample$group,sample$cut.h.cluster)
pdf(file="cluster plot.pdf",width = 12,height = 9)
########################Fig2C:Heat map of different groups#################
data<-read.csv("pheatmap.csv",header = T,row.names = 1)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
data_s<-na.omit(data_s)
colnames(data_s)<-c("Health","At-risk RA","APCA+ RA","APCA- RA",)
library(pheatmap)
p <-pheatmap(data_s,scale='none',show_rownames = T, cluster_cols = F,clustering_method = "mcquitty",border_color = NA,cutree_rows=3,
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 0.5,
             breaks=bk)
row_cluster = cutree(p$tree_row,k=5)
newOrder = data_s[p$tree_row$order,]
newOrder[,ncol(newOrder)+1]=row_cluster[match(rownames(newOrder),names(row_cluster))]
colnames(newOrder)[ncol(newOrder)]="Cluster"
head(newOrder)
unique(newOrder$Cluster)
newOrder$Cluster = paste0("cluster",newOrder$Cluster)
newOrder$gene = rownames(newOrder)
head(newOrder)

########################Fig2D:Scatter plot###########
data<-read.csv("PRE_TEST.csv",header = T)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
log2(1.5)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_A1_A0),y=-log10(data$P_A1_A0),color=data$color.pre))+
  geom_point(aes(color=data$color.pre),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'))+
  xlim(-4,4)+#设置坐标轴限制
  geom_text_repel(aes(label =data$names), size = 3,show.legend = F)+
  geom_vline(xintercept = 0.58,linetype=2,color="grey",size=1)+
  geom_vline(xintercept = -0.58,linetype=2,color="grey",size=1)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  #设置辅助线
  theme(panel.background = element_rect(fill='transparent',color = 'black'),
        text =element_blank(),legend.position = "none")
ggsave("scatter plot(A0 vs A1)2.pdf", width =3, height = 3)
########################Fig2E:Pathway###################
data<-read.csv("PRE_PATHWAY.csv")
ggplot(data,aes(x =data$Term,y =-log10(PValue),fill=data$PValue2))+
  geom_bar(stat="identity",width = 0.5)+
  coord_flip()+
  scale_fill_gradient2(high = "#DD5B51",low= "#438CD5",mid = "white")+
  #scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(data$Term))+
  labs(y="Count",x="")+
  #scale_y_continuous(expand = c(0,0),breaks = c(0,1,2,3)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  

ggsave("pre_pathway.pdf", width =4.5, height =6)
########################Fig2F:line graph#############
data<-read.csv("pre对比.csv")
data$group
ggplot(data,aes(x =Group1,y =log2(y)))+
  geom_line(aes(group=Group2),color = "grey60",size = 1.5,alpha=0.6)+
  geom_point(aes(color=Group1),size = 3)+
  scale_color_manual(values =c('A'='#8E93BC','B'='#AF4B87'),guide=FALSE)+
  labs(y="Log2(intensity)",x="")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~group, ncol =8,scales="free")
ggsave("PRE A-B plot.pdf", width = 6, height = 2.5)
########################Fig2G:IGG level in different groups#####################
library(ggplot2);library(ggsignif);library(reshape2);library(dplyr)
data<-read.csv("IG-DATA1.csv")
d2<-melt(data,id=names(data)[1:6])
table(d2$Group2)
d2%>%
  mutate(Group2 = factor(Group2, levels = c("Health","At-risk RA","ACPA+ RA","ACPA- RA"))) %>%
  mutate(variable = factor(variable, levels = c("IGKV3D.20","IGKV4.1","IGHV4.61","IGHV3.15","IGKV3D.15","IGKC"))) %>%
  
  ggplot(aes(x =Group2,y=log2(value))) + 
  geom_violin(aes(fill=Group2),width=0.7)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.3,fill="white") +
  scale_fill_manual(values =c("Health"='#1C65A9',"At-risk of RA"="#8E93BC","ACPA+ RA"="#AF4B87","ACPA- RA"="#E8A85E"),guide=FALSE)+
  geom_signif(comparisons =  list(c("Health", "At-risk RA"),c("Health", "ACPA+ RA"),c("Health","ACPA- RA")),test = "t.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
  #scale_y_continuous(expand = c(0,0)) + 
  labs(x="",y="")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~ variable, ncol =9,scales="free")
ggsave("ig-ccp1.pdf", width = 13, height =3)


########################SFig2A:The correlation between four clinical indicators and age##########
ggplot(data,aes(x =as.numeric(value),y=Age,color=Age)) + 
  geom_point()+
  geom_smooth(method = "lm",size=1.5,se = T)+
  #geom_jitter(aes(color=data$Gender),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_color_manual(values =c('Female'='#AF322F','Male'='#737AAC'),guide=FALSE)+
  stat_cor(data=data, method = "spearman")+
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
########################SFig2B:linearly correlated proteins counts with clinical indicators#########
DATA<-read.csv("DAS28_COR_TEST_ACPA- RA.csv")
data<-data.frame(c("DAS28","VAS","TJC","SJC","CRP","DAS28","VAS","TJC","SJC","CRP"),
                 c("pos","pos","pos","pos","pos","neg","neg","neg","neg","neg"))
data$Count<-c(nrow(DATA[which(DATA$P_DAS28<0.05&DATA$LM_DAS28>0),]),
              nrow(DATA[which(DATA$P_VAS<0.05&DATA$LM_VAS>0),]),
              nrow(DATA[which(DATA$P_TJC<0.05&DATA$LM_TJC>0),]),
              nrow(DATA[which(DATA$P_SJC<0.05&DATA$LM_SJC>0),]),
              nrow(DATA[which(DATA$P_CRP<0.05&DATA$LM_CRP>0),]),
              nrow(DATA[which(DATA$P_DAS28<0.05&DATA$LM_DAS28<0),]),
              nrow(DATA[which(DATA$P_VAS<0.05&DATA$LM_VAS<0),]),
              nrow(DATA[which(DATA$P_TJC<0.05&DATA$LM_TJC<0),]),
              nrow(DATA[which(DATA$P_SJC<0.05&DATA$LM_SJC<0),]),
              nrow(DATA[which(DATA$P_CRP<0.05&DATA$LM_CRP<0),]))
names(data)<-c("x","group","Count")

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
ggsave("5 clinical cor count ACPA- RA.pdf", width =4, height = 2.8)
########################SFig2C:linearly correlated proteins with DAS28-CRP############
test<-read.csv("DAS28_COR_TEST_ACPA- RA.csv")
library(ggplot2)
library(ggrepel)

ggplot(test,aes(x =LM_DAS28,y =-log10(P_DAS28),fill=-log10(P_DAS28)*sign(LM_DAS28)))+
  geom_point(aes(fill=-log10(P_DAS28)*sign(LM_DAS28),size=abs(-log10(P_DAS28))),shape=21,color='black',)+
  scale_fill_gradient2(low ="#4569bb",mid ="white",high ="#DD5B51",guide = FALSE)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  scale_size_continuous(range=c(0,10),guide = FALSE)+
  labs(x="",y="")+
  xlim(-1,1)+
  geom_text_repel(aes(label =test$colnames.ALL...c.1.15..), size = 3,show.legend = F)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28-protein cor-das28 ACPA- RA.pdf", width = 3, height = 3)
########################SFig2D:4 Clinical scatter plots###############
data<-read.csv("DAS28 ACPA-/DAS28_COR_TEST_ACPA- RA.csv",header = T)[,c(2,4:7,9:12)]
names(data)[1]<-"X"
n<- 4   #指定比较组数-依据数据进行调整；
i_log2FC<- seq(6, by=1, length.out= n ) #指定log2FC所在的列数；
i_Pvalue<- seq(2, by=1, length.out= n ) #指定Pvalue所在的列数；
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
  geom_text_repel(data = df3,aes(x = Groups, y = -log10(PValue),label =df3$X), size = 3,show.legend = F)+
  scale_color_manual(name=NULL,values =alpha(c("#737AAC","grey","#AF322F"),0.5))+
  geom_bar(data=dfbar,aes(x=groups,y=y+0.5,fill=groups), levels = c("VAS","TJC","SJC","CRP"),stat ="identity",width = 0.8)+
  geom_bar(data=dfbar,aes(x=groups,y=y-0.5,fill=groups), levels = c("VAS","TJC","SJC","CRP"),stat ="identity",width = 0.8)+
  scale_fill_manual(name="Groups",values=alpha(mycolor,1))+
  labs(x="",y="-Log10(p value)")+
  geom_text(data=dfbar,aes(x=x,y=y,label=x),size=4,color="white")+
  scale_x_discrete(limit=c("VAS","TJC","SJC","CRP"))+
  scale_y_continuous(limits = c(-0.5,8), breaks= c(0,2,4,6,8), expand= expansion(add = 0))+
  theme_classic()+
  theme(axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),
        axis.line.y = element_line(linetype=1,color="black",size=0.75),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))

ggsave("4clinic cor acpa- RA.pdf", width = 7.3, height = 3)
########################SFig2E:pathway#############################################
library(ggplot2);library(ggrepel)
data<-read.csv("pathway acpa+.csv")
ggplot(data,aes(x =LOG10P,y = Fold.Enrichment,size=Count,fill=LOG10P))+
  geom_point(aes(size=Count,fill=LOG10P),shape=21)+
  scale_fill_gradient2(low="#3C82B9",mid="white",high = "#d86967")+
  scale_size_continuous(range=c(6,13))+
  #geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color = "black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("pathway acpa+ ra.pdf", width =7, height = 3)
########################SFig2F:pathway#############################################
data<-read.csv("pathway acpa-.csv")
ggplot(data,aes(x =LOG10P,y = Fold.Enrichment,size=Count,fill=LOG10P))+
  geom_point(aes(size=Count,fill=LOG10P),shape=21)+
  scale_fill_gradient2(low="#3C82B9",mid="white",high = "#d86967")+
  scale_size_continuous(range=c(6,13))+
  #geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color = "black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("pathway acpa- ra.pdf", width =4, height = 3)


















