####################calculate the difference###################
setwd("C:/Users/11915/Desktop/RA/913/")
ALL<-read.csv("cln-data1.csv",row.names = 1)
ALL[ALL== 0] <- NA
A<-ALL[which(ALL$Group=="RA_A"),-c(1:17)]
HP<-ALL[which(ALL$Group=="HP"),-c(1:17)]
PRE<-ALL[which(ALL$Group=="PRA"),-c(1:17)]
CCP_N<-ALL[which(ALL$CCP=="CCP-"),-c(1:17)]
CCP_P<-ALL[which(ALL$CCP=="CCP+"),-c(1:17)]
MTXHCQ=c("FGG","SERPINA1","HP","SERPINA3","CRP.1","A2M","NPC2","AHSG","TF")
d=data[which(data$DAS28.CRP.HM!="NA"),colnames(data)%in%c(MTXHCQ,"gene","group2","DAS28-CRP-HM","DDAS28-HM","Responder-HM","Age","Gender","VAS","SJC","TJC","CRP")]

PS=c("A313","A266","A019","A372","A134","A064","A041","A402","A400","A659","A501","A365","A046","A116")
HS=c("HP029","HP042","HP048","HP004","HP038","HP031","HP092","HP039","HP007","HP097","HP099","HP036","HP017","HP026")
CCP_PS=CCP_P[row.names(CCP_P)%in%c(PS),]
HPS=HP[row.names(HP)%in%c(HS),]
CCP_PS=data.frame(CCP_P[c(65,57,8,79,47,27,15,92,90,167,122,78,17,41),])
HPS=data.frame(HP[c(28,41,47,4,37,30,91,38,7,96,98,35,17,25),])
row.names(HPS)
A<-data.frame(t(data.frame(HPS_median,CCP_PS_median,CCP_N_median)))
data<-rbind(A,HPS)
data<-rbind(data,CCP_PS)
data<-rbind(data,CCP_N)

A_median<- apply(A,2,median,na.rm=T)
HP_median<- apply(HP,2,median,na.rm=T)
PRE_median<- apply(PRE,2,median,na.rm=T)
CCP_PS_median<- apply(CCP_PS,2,median,na.rm=T)
CCP_N_median<- apply(CCP_N,2,median,na.rm=T)
HPS_median<- apply(HPS,2,median,na.rm=T)

FC_A_H<-A_median/HP_median
FC_PRE_H<-PRE_median/HP_median
FC_A_PRE<-A_median/PRE_median
FC_CCP_P_H<-CCP_PS_median/HPS_median
FC_CCP_N_H<-CCP_N_median/HPS_median
FC_CCP_P_N<-CCP_PS_median/CCP_N_median
P_A_H<-rep(NA,ncol(A))
P_PRE_H<-rep(NA,ncol(A))
P_A_PRE<-rep(NA,ncol(A))
P_CCP_P_H<-rep(NA,ncol(A))
P_CCP_N_H<-rep(NA,ncol(A))
P_CCP_P_N<-rep(NA,ncol(A))
i=1
for(i in 1:ncol(A))try({
  P_A_H[i] = t.test(as.numeric(A[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_PRE_H[i] = t.test(as.numeric(PRE[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
  P_A_PRE[i] = t.test(as.numeric(PRE[,i]),as.numeric(A[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_CCP_P_H[i] = t.test(as.numeric(CCP_PS[,i]),as.numeric(HPS[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_CCP_N_H[i] = t.test(as.numeric(CCP_N[,i]),as.numeric(HPS[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_CCP_P_N[i] = t.test(as.numeric(CCP_PS[,i]),as.numeric(CCP_N[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
})
TEST<-cbind(HP_median,PRE_median,A_median,CCP_PS_median,CCP_N_median,HPS_median,
            FC_A_H,FC_PRE_H,FC_A_PRE,FC_CCP_P_H,FC_CCP_N_H,FC_CCP_P_N,
            P_A_H,P_PRE_H,P_A_PRE,P_CCP_P_H,P_CCP_N_H,P_CCP_P_N)
write.csv(TEST,"ALL_TEST.csv")




####################Scatter plot###################
setwd("C:/Users/11915/Desktop/RA/913/FIG2/")


data<-read.csv("ALL_TEST.csv",header = T)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$FC_A_H!="NA")&data$FC_A_H!="NA"),]
data$color_pre[(data$P_A_H> 0.05|data$P_A_H=="NA")|(data$FC_A_H > 0.667& data$FC_A_H<1.5)] <- "no"
data$color_pre[(data$P_A_H<0.05)&(data$FC_A_H >1.5)] <- "up"
data$color_pre[(data$P_A_H<0.05)&(data$FC_A_H<0.667)]  <- "down"
table(data$color_pre)
log2(1.5)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_A_H),y=-log10(data$P_A_H),color=data$color_pre))+
  geom_point(aes(color=data$color_pre),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'))+
  xlim(-6,6)+#设置坐标轴限制
  geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  geom_vline(xintercept = 0.58,linetype=2,color="grey",size=1)+
  geom_vline(xintercept = -0.58,linetype=2,color="grey",size=1)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  #设置辅助线
  theme(panel.background = element_rect(fill='transparent',color = 'black'),
        text =element_blank(),legend.position = "none")
ggsave("scatter plot(A vs H).pdf", width =3, height = 3)

data<-read.csv("ALL_TEST.csv",header = T)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$FC_PRE_H!="NA")&data$FC_PRE_H!="NA"),]
data$color_pre[(data$P_PRE_H> 0.05|data$P_PRE_H=="NA")|(data$FC_PRE_H > 0.667& data$FC_PRE_H<1.5)] <- "no"
data$color_pre[(data$P_PRE_H<0.05)&(data$FC_PRE_H >1.5)] <- "up"
data$color_pre[(data$P_PRE_H<0.05)&(data$FC_PRE_H<0.667)]  <- "down"
table(data$color_pre)
log2(1.5)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_PRE_H),y=-log10(data$P_PRE_H),color=data$color_pre))+
  geom_point(aes(color=data$color_pre),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'))+
  xlim(-6,6)+#设置坐标轴限制
  geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  geom_vline(xintercept = 0.58,linetype=2,color="grey",size=1)+
  geom_vline(xintercept = -0.58,linetype=2,color="grey",size=1)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  theme(panel.background = element_rect(fill='transparent',color = 'black'),
        text =element_blank(),legend.position = "none")
ggsave("scatter plot(pre vs H).pdf", width =3, height = 3)




data<-read.csv("ALL_TEST.csv",header = T)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$FC_A_PRE!="NA")&data$FC_A_PRE!="NA"),]
data$color_pre[(data$P_A_PRE> 0.05|data$P_A_PRE=="NA")|(data$FC_A_PRE > 0.667& data$FC_A_PRE<1.5)] <- "no"
data$color_pre[(data$P_A_PRE<0.05)&(data$FC_A_PRE >1.5)] <- "up"
data$color_pre[(data$P_A_PRE<0.05)&(data$FC_A_PRE<0.667)]  <- "down"
table(data$color_pre)
log2(1.5)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_A_PRE),y=-log10(data$P_A_PRE),color=data$color_pre))+
  geom_point(aes(color=data$color_pre),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'))+
  xlim(-3,3)+#设置坐标轴限制
  geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  geom_vline(xintercept = 0.58,linetype=2,color="grey",size=1)+
  geom_vline(xintercept = -0.58,linetype=2,color="grey",size=1)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  #设置辅助线
  theme(panel.background = element_rect(fill='transparent',color = 'black'),
        text =element_blank(),legend.position = "none")
ggsave("scatter plot(A vs pre).pdf", width =3, height = 3)

####################pheatmap#############

setwd("C:/Users/11915/Desktop/RA/913/FIG2/")
data<-read.csv("304-cluster4.csv",header = T,row.names = 1)
table(data$Group)
data2<-data[,c(1:3)]
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
anno_col<-data[,c(5,6)]
colnames(anno_col)
anno_col
ann_colors = list( Group2= c("Y"="#9586BC","N"="grey95"),
                   Group=c("RA"='#6AA4E0',"PRE"='#FAAAB9',"OVERLAP"="#969CCC"))
library(pheatmap)
p <-pheatmap(data2,scale='none',show_rownames = T, cluster_cols = F,cluster_rows = F,border_color = NA,gaps_row = c(185,218,240),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 1,
             breaks=bk,annotation_row =anno_col,annotation_colors = ann_colors)
p
pdf("PHEATMAP.pdf", width = 13, height = 13);p;dev.off()


setwd("C:/Users/11915/Desktop/RA/913/FIG2/")
data<-read.csv("334-cluster3.csv",header = T,row.names = 1)
data2<-data[,c(1:3)]
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))

p <-pheatmap(data2,scale='none',show_rownames = T, cluster_cols = F,cluster_rows = F,border_color = NA,
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 1,
             breaks=bk)
p
pdf("PHEATMAP2.pdf", width = 13, height = 13);p;dev.off()

####################cluster##########################
setwd("C:/Users/11915/Desktop/RA/913/FIG2/")


data<-read.csv("ALL_TEST.csv",header = T,row.names = 1)
data$Group2<- "N"
data$Group2[(data$P_A_PRE<0.05)&(data$FC_A_PRE<0.667|data$FC_A_PRE>1.5)]<- "Y"
table(data$Group2)
data$Group<- "N"
data$Group[(data$P_A_H<0.05)&(data$FC_A_H<0.667|data$FC_A_H>1.5)]<- "RA"
data$Group[(data$P_PRE_H<0.05)&(data$FC_PRE_H<0.667|data$FC_PRE_H>1.5)]<- "PRE"
data$Group[((data$P_A_H<0.05)&(data$FC_A_H<0.667|data$FC_A_H>1.5))&((data$P_PRE_H<0.05)&(data$FC_PRE_H<0.667|data$FC_PRE_H>1.5))]<-"OVERLAP" 
table(data$Group)
table(data$Group2)
write.csv(data,"ALL_TEST.csv")
data<-data[which(data$Group!="N"),c(1:3,19:20)]
table(data$Group)
data2<-data[,c(1:3)]
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
colnames(data_s)<-c("Health","Pre-RA","RA")
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
anno_col<-data[,c(5,4)]
colnames(anno_col)
anno_col
ann_colors = list( Group2= c("Y"="#9586BC","N"="grey95"),
                   Group=c("RA"='#6AA4E0',"PRE"='#FAAAB9',"OVERLAP"="#969CCC"))
library(pheatmap)
p <-pheatmap(data_s,scale='none',show_rownames = T, cluster_cols = F,clustering_method = "median",border_color = NA,cutree_rows=4, #c(3,6,9,12,15,18,21),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 1,
             breaks=bk,annotation_row =anno_col,annotation_colors = ann_colors)

row_cluster = cutree(p$tree_row,k=4)
newOrder = data_s[p$tree_row$order,]
newOrder[,ncol(newOrder)+1]=row_cluster[match(rownames(newOrder),names(row_cluster))]
colnames(newOrder)[ncol(newOrder)]="Cluster"
head(newOrder)
unique(newOrder$Cluster)
newOrder$Cluster = paste0("cluster",newOrder$Cluster)
newOrder$gene = rownames(newOrder)
head(newOrder)




data<-read.csv("ALL_TEST.csv",header = T,row.names = 1)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data1=data[which((data$P_CCP_P_H<0.05)|data$P_CCP_N_H<0.05),]
data2<-data1[,c(6,4,5)]
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
data_s<-na.omit(data_s)
colnames(data_s)<-c("Health","APCA+ RA","APCA- RA")
library(pheatmap)
p <-pheatmap(data_s,scale='none',show_rownames = T, cluster_cols = F,clustering_method = "mcquitty",border_color = NA,cutree_rows=3,
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 0.5,
             breaks=bk)
row_cluster = cutree(p$tree_row,k=3)
newOrder = data_s[p$tree_row$order,]
newOrder[,ncol(newOrder)+1]=row_cluster[match(rownames(newOrder),names(row_cluster))]
colnames(newOrder)[ncol(newOrder)]="Cluster"
head(newOrder)
unique(newOrder$Cluster)
newOrder$Cluster = paste0("cluster",newOrder$Cluster)
newOrder$gene = rownames(newOrder)
head(newOrder)






####################ssgsea###################
library(GSVA);library(reshape)
setwd("C:/Users/11915/Desktop/RA/")

gene<-melt(genedatabase,id=c())
gene<-gene[which(gene$value!=""),]
expression <- read.csv("ssgsea_data.csv",header = T,row.names = 1)
uni_matrix <- data.frame(t(expression[,-c(1:14)]))
gene_set<-gene 
bg_genes <- split(as.matrix(gene_set)[,2], gene_set[,1])
gsva_matrix <- gsva(as.matrix(uni_matrix), bg_genes, method='ssgsea', kcdf='Gaussian', abs.ranking=TRUE)
write.csv(gsva_matrix,"gsva_pathway1.csv")
library(pheatmap)
gsva_matrix <- read.csv("gsva_pathway1.csv",row.names = 1)
bk <- c(seq(-1.5,0,by=0.01),seq(0.01,1.5,by=0.01))
p <-pheatmap(gsva_matrix,scale='row',show_rownames = T, cluster_cols = F,clustering_method = "ward.D2",border_color = NA,cutree_rows  = T,gaps_col =c(60,132,172), 
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),
             breaks=bk)

A<-gsva_matrix[which(gsva_matrix$group=="A"),-1]
HP<-gsva_matrix[which(gsva_matrix$group=="HP"),-1]
PRE<-gsva_matrix[which(gsva_matrix$group=="PRE=RA"),-1]
A_median<- apply(A,2,median,na.rm=T)
HP_median<- apply(HP,2,median,na.rm=T)
PRE_median<- apply(PRE,2,median,na.rm=T)
FC_A_H<-A_median/HP_median
FC_PRE_H<-PRE_median/HP_median
FC_PRE_A<-PRE_median/A_median
P_A_H<-rep(NA,ncol(A))
P_PRE_H<-rep(NA,ncol(A))
P_PRE_A<-rep(NA,ncol(A))
for(i in 1:ncol(A))try({
  P_A_H[i] = t.test(as.numeric(A[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_PRE_H[i] = t.test(as.numeric(PRE[,i]),as.numeric(HP[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
  P_PRE_A[i] = t.test(as.numeric(PRE[,i]),as.numeric(A[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
})
TEST<-cbind(A_median,HP_median,PRE_median,
            FC_A_H,FC_PRE_H,FC_PRE_A,
            P_A_H,P_PRE_H,P_PRE_A)


library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggsignif)
gsva_matrix <- read.csv("gsva_all2.csv",row.names = 1)[,-2]

da1<-gsva_matrix[,-1]
library(reshape2)
data1<-scale(da1)
data1<-as.data.frame(data1)
data1$group<-gsva_matrix$group
DATA2<-melt(data1,id.vars = "group")
DATA2$group
ggplot(DATA2,aes(x=group,y=value,fill=group))+
  #geom_boxplot(aes(color=GROUP2),width=0.8,fill="white",outlier.shape = NA)+
  geom_boxplot(aes(fill=group),width=0.5,color="black",outlier.shape = NA,alpha=0.8)+
  geom_jitter(aes(color=group),position = position_jitterdodge(0.2),size=0.7,alpha=0.4)+
  scale_fill_manual(values = c("A-HP"="#69A3DD","B-PRE=RA"="#7668BA","C-A"="#F47368"))+
  #ylim(-2,6)+
  geom_signif(comparisons =  list(c("A-HP", "B-PRE=RA")),test = "t.test",y_position =c(max(DATA2$value)-0.5),map_signif_level =T)+
  geom_signif(comparisons =  list(c("A-HP", "C-A")),test = "t.test",y_position =c(max(DATA2$value)+1),map_signif_level =T)+
  geom_signif(comparisons =  list(c("B-PRE=RA", "C-A")),test = "t.test",y_position =c(max(DATA2$value)+0.5),map_signif_level =T)+
  
  facet_wrap(variable~.,scales = "free_y")


####################pathway########################
setwd("C:/Users/11915/Desktop/RA/913/FIG2/")
library(ggplot2)
data<-read.csv("CCP_PATHWAY.csv")
ggplot(data,aes(y =Term,x = data$Fold.Enrichment,size=Count,fill=-log10(data$PValue)))+
  geom_point(aes(size=Count,fill=-log10(data$PValue)),shape=21)+#鐠佸墽鐤嗛悙鍦畱婢堆冪毈娑撳酣顤侀????
  scale_fill_gradient2(low="#3C82B9",mid="white",high = "#d86967")+
  scale_y_discrete(limits=unique(data$Term))+
  scale_size_continuous(range=c(7,16))+ 
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("PATHWAY2.pdf",width = 10,height = 8)

####################boxplot##############
setwd("C:/Users/11915/Desktop/RA/913")
library(ggplot2);library(ggsignif);library(reshape2);library(dplyr);library(ggpubr)
data<-read.csv("IG_DATA-2-2.csv")
d2<-melt(data,id=names(data)[1:2])
table(d2$GROUP)
p1<-d2%>%
  mutate(GROUP = factor(GROUP, levels = c("Health","ACPA+ RA","ACPA- RA"))) %>%
  ggplot(aes(x =GROUP,y=log2(value))) + 
  geom_violin(aes(fill=GROUP),width=0.7)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.3,fill="white") +
  #  geom_jitter(aes(color=GROUP),stroke =1.5,size=0.8,position = position_jitter(0.3))+
  #  geom_boxplot(color="black",outlier.colour = NA,width=0.6,fill="white") +
  scale_fill_manual(values =c("Health"='#69A3DD',"ACPA+ RA"="#9D84AE","ACPA- RA"="#E8A85E"),guide=FALSE)+
  geom_signif(comparisons =  list(c("Health", "ACPA+ RA"),c("Health", "ACPA- RA")),test = "t.test",map_signif_level =T, step_increase = 0.15,na.rm =T, show.legend = NA)+
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
  facet_wrap( ~ variable, ncol =6,scales="free")

ggsave("ig-ccp1.pdf", width = 10, height =4)




setwd("C:/Users/11915/Desktop/RA/913")
library(ggplot2);library(ggsignif);library(reshape2);library(dplyr)
data<-read.csv("IG_DATA-3.csv")
d2<-melt(data,id=names(data)[1:2])
table(d2$GROUP)
d2%>%
  mutate(GROUP = factor(GROUP, levels = c("Health","At-risk of RA","RA"))) %>%
  ggplot(aes(x =GROUP,y=log2(value))) + 
  #geom_violin(aes(fill=GROUP),width=0.7)+
  #geom_boxplot(color="black",outlier.colour = NA,width=0.3,fill="white") +
  geom_jitter(aes(color=GROUP),stroke =1.5,size=0.8,position = position_jitter(0.3))+
  geom_boxplot(color="black",outlier.colour = NA,width=0.6,fill="white") +
  scale_color_manual(values =c("Health"='#69A3DD',"At-risk of RA"="#7668BA","RA"="#F47368"),guide=FALSE)+
  geom_signif(comparisons =  list(c("Health", "At-risk of RA"),c("Health", "RA"),c("At-risk of RA","RA")),test = "t.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
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
#ggarrange(p2,p1,ncol=2,legend = NULL,widths=c(5,6))
ggsave("ig-ccp3.pdf", width =12, height =2.3)
