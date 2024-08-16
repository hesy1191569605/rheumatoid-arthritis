#####################calculate the difference#############################################----

setwd("C:/Users/11915/Desktop/RA/913")
data<-read.csv("cln-data2.csv",header = T)
data<-data[which(data$Drugs.HM=="MTXLEF"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")&data$Responder.HM!="NA"),]
data1<-data[which(data$Responder=="Y"),]
data2<-data[which(data$Responder=="N"),]
A1<-data[c(1:(nrow(data)/2)),-c(1:15)]
A2<-data[-c(1:(nrow(data)/2)),-c(1:15)]
Y1<-data1[c(1:(nrow(data1)/2)),-c(1:15)]
Y2<-data1[-c(1:(nrow(data1)/2)),-c(1:15)]
N1<-data2[c(1:(nrow(data2)/2)),-c(1:15)]
N2<-data2[-c(1:(nrow(data2)/2)),-c(1:15)]
AB_median<- apply(A1,2,median,na.rm=T)
AA_median<- apply(A2,2,median,na.rm=T)
YB_median<- apply(Y1,2,median,na.rm=T)
YA_median<- apply(Y2,2,median,na.rm=T)
NB_median<- apply(N1,2,median,na.rm=T)
NA_median<- apply(N2,2,median,na.rm=T)
FC_A_AB<-AA_median/AB_median
FC_Y_AB<-YA_median/YB_median
FC_N_AB<-NA_median/NB_median
P_A<- vector(length = ncol(A1))
P_Y<- vector(length = ncol(A1))
P_N<- vector(length = ncol(A1))
name <- vector(length = ncol(A1))
for(i in 1:(ncol(A1)))try({
  P_A[i] <- t.test(as.numeric(A1[,i]),as.numeric(A2[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
  P_Y[i] <- t.test(as.numeric(Y1[,i]),as.numeric(Y2[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
  P_N[i] <- t.test(as.numeric(N1[,i]),as.numeric(N2[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
  name[i] <- colnames(A1)[i]
})
test<-data.frame(name,AB_median,AA_median,YB_median,YA_median,NB_median,NA_median,
                 FC_A_AB,FC_Y_AB,FC_N_AB,
                 P_A,P_Y,P_N)
write.csv(test,"test_n_b_MTXLEF.csv")


data<-read.csv("cln-data2.csv",header = T)
data<-data[which(data$Drugs.HM=="MTXHCQ"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")&data$Responder.HM!="NA"),]
data1<-data[which(data$Responder=="Y"),]
data2<-data[which(data$Responder=="N"),]
A1<-data[c(1:(nrow(data)/2)),-c(1:15)]
A2<-data[-c(1:(nrow(data)/2)),-c(1:15)]
Y1<-data1[c(1:(nrow(data1)/2)),-c(1:15)]
Y2<-data1[-c(1:(nrow(data1)/2)),-c(1:15)]
N1<-data2[c(1:(nrow(data2)/2)),-c(1:15)]
N2<-data2[-c(1:(nrow(data2)/2)),-c(1:15)]
AB_median<- apply(A1,2,median,na.rm=T)
AA_median<- apply(A2,2,median,na.rm=T)
YB_median<- apply(Y1,2,median,na.rm=T)
YA_median<- apply(Y2,2,median,na.rm=T)
NB_median<- apply(N1,2,median,na.rm=T)
NA_median<- apply(N2,2,median,na.rm=T)
FC_A_AB<-AA_median/AB_median
FC_Y_AB<-YA_median/YB_median
FC_N_AB<-NA_median/NB_median
P_A<- vector(length = ncol(A1))
P_Y<- vector(length = ncol(A1))
P_N<- vector(length = ncol(A1))
name <- vector(length = ncol(A1))
for(i in 1:(ncol(A1)))try({
  P_A[i] <- t.test(as.numeric(A1[,i]),as.numeric(A2[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
  P_Y[i] <- t.test(as.numeric(Y1[,i]),as.numeric(Y2[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
  P_N[i] <- t.test(as.numeric(N1[,i]),as.numeric(N2[,i]),alternative = c("two.sided"),paired=T,var.equal=TRUE)$p.value;
  name[i] <- colnames(A1)[i]
})
test<-data.frame(name,AB_median,AA_median,YB_median,YA_median,NB_median,NA_median,
                 FC_A_AB,FC_Y_AB,FC_N_AB,
                 P_A,P_Y,P_N)
write.csv(test,"test_n_b_MTXHCQ.csv")



########################Fig6A:scatter plot(MTX+LEF)###################
data<-read.csv("test_n_b_MTXLEF_scatter plot.csv")
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$P_Y!="NA")&data$FC_Y_AB!="NA"),]
data$color_pre[(data$P_Y> 0.05|data$P_Y=="NA")|(data$FC_Y_AB> 1& data$FC_Y_AB < 1)] <- "no"
data$color_pre[(data$P_Y<0.05)&(data$FC_Y_AB>1)] <- "up"
data$color_pre[(data$P_Y<0.05)&(data$FC_Y_AB< 1)]  <- "down"
table(data$color_pre)
log2(1.25)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_Y_AB),y=-log10(data$P_Y),color=data$color_pre))+
  geom_point(aes(color=data$color_pre,shape=data$X.1),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
  scale_shape_manual(values =c("N"=16,"Y"=17),guide=FALSE)+
  geom_text_repel(aes(label =data$X), size = 4,show.legend = F,color="black")+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  xlim(-2,2)+
  labs(x="Log2(After/Before)",y="-log10(P_Value)")+
  #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("Y_N_MTXLEF.pdf",width = 3.2, height = 3)
########################Fig6B:scatter plot(MTX+HCQ)###################
data<-read.csv("test_n_b_MTXHCQ_scatter plot.csv")
data[data=="0"]<-NA
data[data=="Inf"]<-NA
data=data[which((data$P_Y!="NA")&data$FC_Y_AB!="NA"),]
data$color_pre[(data$P_Y> 0.05|data$P_Y=="NA")|(data$FC_Y_AB> 1& data$FC_Y_AB < 1)] <- "no"
data$color_pre[(data$P_Y<0.05)&(data$FC_Y_AB>1)] <- "up"
data$color_pre[(data$P_Y<0.05)&(data$FC_Y_AB< 1)]  <- "down"
table(data$color_pre)
log2(1.25)
library(ggplot2)
library(ggrepel)
ggplot(data,aes(x=log2(data$FC_Y_AB),y=-log10(data$P_Y),color=data$color_pre))+
  geom_point(aes(color=data$color_pre,shape=data$X.1),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
  scale_shape_manual(values =c("N"=16,"Y"=17),guide=FALSE)+
  geom_text_repel(aes(label =data$X), size = 4,show.legend = F,color="black")+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  xlim(-2,2)+
  labs(x="Log2(After/Before)",y="-log10(P_Value)")+
  #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("Y_N_MTXHCQ.pdf",width = 3.2, height = 3)
########################Fig6C:pathway(MTX+LEF)###################

data<-read.csv("pathways_MTXLEF_AB_Y.csv")
library(ggplot2)
ggplot(data,aes(x =Term,y =Count,fill=PValue))+
  geom_bar(fill ="grey", stat = "identity", width = 0.5) + # 注意将width宽度设小
  geom_point(aes(fill = PValue),size =10,shape=21) + 
  coord_flip()+
  scale_fill_gradient2(high = "#d86967",low = "#58539f",mid = "white")+
  scale_x_discrete(limits=data$Term)+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),limits = c(0,15),breaks = c(0,5,10,15)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
ggsave("MTX_LEF(A_B)_Y pathway.pdf", width = 4, height = 7)#


########################Fig6D:pathway(MTX+HCQ)###################

data<-read.csv("pathways_MTXHCQ_AB_Y.csv")
library(ggplot2)
ggplot(data,aes(x =Term,y =Count,fill=PValue))+
  geom_bar(fill ="grey", stat = "identity", width = 0.5) + # 注意将width宽度设小
  geom_point(aes(fill = PValue),size =10,shape=21) + 
  coord_flip()+
  scale_fill_gradient2(high = "#d86967",low = "#58539f",mid = "white")+
  scale_x_discrete(limits=data$Term)+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),limits = c(0,8),breaks = c(0,4,8)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
ggsave("MTX_HCQ(A_B)_Y pathway.pdf", width = 4, height = 7)
########################Fig6E:heatmap(MTX+LEF)###################
data<-read.csv("mtxlef_B_A_GENE.csv",row.names = 1)
library(pheatmap)
bk <- c(seq(-1.5,0,by=0.01),seq(0.01,1.5,by=0.01))
sample<-data.frame(data[,c(12:13)])

bk <- c(seq(-1.5,0,by=0.01),seq(0.01,1.5,by=0.01))
anno_col<-sample
colnames(anno_col)
sample$data...13.
p <-pheatmap(data[,c(1:4)],scale='row',show_rownames = T, cluster_cols = F,cluster_rows  = F,clustering_method = "ward.D2",border_color = NA,#cutree_rows=5, #c(3,6,9,12,15,18,21),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 35,cellheight = 10,
             breaks=bk,annotation_row =anno_col)
pdf("mtxlef_B_A_GENE_pheatmap.pdf", width = 13, height = 13);p;dev.off()
########################Fig6F:heatmap(MTX+HCQ)###################
data<-read.csv("mtxhcq_B_A_GENE.csv",row.names = 1)
library(pheatmap)
bk <- c(seq(-1.5,0,by=0.01),seq(0.01,1.5,by=0.01))
sample<-data.frame(data[,c(10:11)])

bk <- c(seq(-1.5,0,by=0.01),seq(0.01,1.5,by=0.01))
anno_col<-sample
colnames(anno_col)
sample$data...13.
p <-pheatmap(data[,c(1:4)],scale='row',show_rownames = T, cluster_cols = F,cluster_rows  = F,clustering_method = "ward.D2",border_color = NA,#cutree_rows=5, #c(3,6,9,12,15,18,21),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 35,cellheight = 10,
             breaks=bk,annotation_row =anno_col)
pdf("mtxhcq_B_A_GENE_pheatmap.pdf", width = 13, height = 13);p;dev.off()

########################Fig6g:line graph(MTX+LEF)######################
data<-read.csv("cln-data2.csv",header = T)
data<-data[which(data$Drugs.HM=="MTXLEF"&data$Responder.HM!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]
MTXLEF=c("CRP.1","MYH9","APOA4","SUCLG1","TTR","CYCS","XRCC5")
d=data[which(data$Drugs.HM=="MTXLEF"&data$Responder.HM!="NA"),colnames(data)%in%c(MTXLEF,"gene","group","GROUP","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
data1<-d[,-c(4:8)]
data1<-data1[,c(1:3,9,6,5,7,8,4,10)]
data2<-melt(data1,id=names(data1)[c(1:3)])
data2<-rbind(data2,data4)
data2 %>%
  mutate(group = factor(group, levels = c("BY","AY","BN","AN"))) %>%
  ggplot(aes(x =group,y =log2(value)))+
  geom_line(aes(group=GROUP),color = "grey60",size = 1,alpha=0.6)+
  geom_point(size = 1.2)+
  geom_signif(comparisons =  list(c("BY","AY"),c("BN","AN")),test = "t.test",map_signif_level =T)+
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
  facet_wrap( ~variable , ncol =8,scales="free")
ggsave("ALL a_b plot.pdf", width = 16, height = 2.5)
########################Fig6h:line graph(MTX+HCQ)##########################
data<-read.csv("cln-data2.csv",header = T)
data<-data[which(data$Drugs.HM=="MTXHCQ"&data$Responder.HM!="NA"&(data$Class=="moderate_disease_activity"|data$Class=="high_disease_activity")),]
MTXHCQ=c("HNRNPD")
d=data[which(data$Drugs.HM=="MTXHCQ"&data$Responder.HM!="NA"),colnames(data)%in%c(MTXHCQ,"gene","group","GROUP","DDAS28-HM","Responder-HM","Age","Gender","SJC","TJC","CRP")]
data3<-d[,-c(4:8)]
data4<-melt(data1,id=names(data1)[c(1:3)])
data2 %>%
  mutate(group = factor(group, levels = c("BY","AY","BN","AN"))) %>%
  ggplot(aes(x =group,y =log2(value)))+
  geom_line(aes(group=GROUP),color = "grey60",size = 1)+
  geom_point(size = 1)+
  geom_signif(comparisons =  list(c("BY","AY"),c("BN","AN"),c("BY","BN")),test = "t.test",map_signif_level =T,step_increase = 0.1)+
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
  facet_wrap( ~variable , ncol =8,scales="free")
ggsave("MTX_HCQ a_b plot.pdf", width = 2.5, height = 3)

########################SFig6A:pathway###################
data<-read.csv("pathways_MTXLEF_AB_N.csv")
library(ggplot2)
ggplot(data,aes(x =Term,y =Count,fill=PValue))+
  geom_bar(fill ="grey", stat = "identity", width = 0.5) + # 注意将width宽度设小
  geom_point(aes(fill = PValue),size =20,shape=21) + 
  coord_flip()+
  scale_fill_gradient2(high = "#d86967",low = "#58539f",mid = "white")+
  scale_x_discrete(limits=data$Term)+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),limits = c(0,3),breaks = c(0,1,2,3)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
ggsave("MTX_LEF(A_B)_N pathway.pdf", width = 4, height = 7)#
########################SFig6B:pathway#######################
data<-read.csv("pathways_MTXHCQ_AB_N.csv")
library(ggplot2)
ggplot(data,aes(x =Term,y =Count,fill=PValue))+
  geom_bar(fill ="grey", stat = "identity", width = 0.5) + # 注意将width宽度设小
  geom_point(aes(fill = PValue),size =14,shape=21) + 
  coord_flip()+
  scale_fill_gradient2(high = "#d86967",low = "#58539f",mid = "white")+
  scale_x_discrete(limits=data$Term)+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),limits = c(0,10),breaks = c(0,5,10)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
ggsave("MTX_HCQ(A_B)_N pathway.pdf", width = 4, height = 7)
########################SFig6C:scatter plot################
{data<-read.csv("test_n_b_CCP+_SEX_MTXLEF.csv")[,c(1,7:10)]
P<-data[,c(1,4:5)]
FC<-data[,c(1:3)]
str<- colnames(P)
colname<- gsub("P_","",str)
colnames(P) <- colname
df1<- melt(P,  #id.vars表示不需要合并的列；
           measure.vars = colnames(P)[-1], #measure.vars表示需要合并的列；
           variable.name ="Groups", #variable.name表示列名合并后的新列名；
           value.name ="PValue") 
colnames(FC) <- colname
df2<- melt(FC,  #id.vars表示不需要合并的列；
           measure.vars = colnames(FC)[-1], #measure.vars表示需要合并的列；
           variable.name ="Groups", #variable.name表示列名合并后的新列名；
           value.name ="FC") 
df<-merge(df1,df2,by = c("X","Groups"))
df[df=="0"]<-NA
df[df=="Inf"]<-NA

df=df[which((df$PValue!="NA")&df$FC!="NA"),]

df$color_pre[(df$PValue>= 0.05|df$PValue=="NA")|(df$FC> 1& df$FC < 1)] <- "no"
df$color_pre[(df$PValue<0.05)&(df$FC>1)] <- "up"
df$color_pre[(df$PValue<0.05)&(df$FC< 1)]  <- "down"

log2(1.25)
library(ggplot2)
library(ggrepel)
ggplot(df,aes(x=log2(df$FC),y=-log10(df$PValue),color=df$color_pre))+
  geom_point(aes(color=df$color_pre,shape=df$Groups),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
  scale_shape_manual(values =c("N"=16,"Y"=17),guide=FALSE)+
  geom_text_repel(aes(label =df$X), size = 4,show.legend = F,color="black")+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  xlim(-2,2)+
  labs(x="Log2(After/Before)",y="-log10(P_Value)")+
  #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("Y_N_MTXLEF_CCP+.pdf",width = 3.2, height = 3)
}
{data<-read.csv("test_n_b_CCP+_SEX_MTXLEF.csv")[,c(1,15:18)]
  P<-data[,c(1,4:5)]
  FC<-data[,c(1:3)]
  str<- colnames(P)
  colname<- gsub("P_","",str)
  colnames(P) <- colname
  df1<- melt(P,  #id.vars表示不需要合并的列；
             measure.vars = colnames(P)[-1], #measure.vars表示需要合并的列；
             variable.name ="Groups", #variable.name表示列名合并后的新列名；
             value.name ="PValue") 
  colnames(FC) <- colname
  df2<- melt(FC,  #id.vars表示不需要合并的列；
             measure.vars = colnames(FC)[-1], #measure.vars表示需要合并的列；
             variable.name ="Groups", #variable.name表示列名合并后的新列名；
             value.name ="FC") 
  df<-merge(df1,df2,by = c("X","Groups"))
  df[df=="0"]<-NA
  df[df=="Inf"]<-NA
  
  df=df[which((df$PValue!="NA")&df$FC!="NA"),]
  
  df$color_pre[(df$PValue>= 0.05|df$PValue=="NA")|(df$FC> 1& df$FC < 1)] <- "no"
  df$color_pre[(df$PValue<0.05)&(df$FC>1)] <- "up"
  df$color_pre[(df$PValue<0.05)&(df$FC< 1)]  <- "down"
  
  log2(1.25)
  library(ggplot2)
  library(ggrepel)
  ggplot(df,aes(x=log2(df$FC),y=-log10(df$PValue),color=df$color_pre))+
    geom_point(aes(color=df$color_pre,shape=df$Groups),size=2)+
    scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
    scale_shape_manual(values =c("NF"=16,"YF"=17),guide=FALSE)+
    geom_text_repel(aes(label =df$X), size = 4,show.legend = F,color="black")+
    geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
    xlim(-2,2)+
    labs(x="Log2(After/Before)",y="-log10(P_Value)")+
    #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
    theme(axis.line=element_line(linetype=1,color="black",size=0.75),
          axis.ticks=element_line(color="black",size=0.75,lineend = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
          title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
  ggsave("Y_N_MTXLEF_CCP+_female.pdf",width = 2.2, height = 2)
}
{data<-read.csv("test_n_b_CCP+_SEX_MTXLEF.csv")[,c(1,23:26)]
  P<-data[,c(1,4:5)]
  FC<-data[,c(1:3)]
  str<- colnames(P)
  colname<- gsub("P_","",str)
  colnames(P) <- colname
  df1<- melt(P,  #id.vars表示不需要合并的列；
             measure.vars = colnames(P)[-1], #measure.vars表示需要合并的列；
             variable.name ="Groups", #variable.name表示列名合并后的新列名；
             value.name ="PValue") 
  colnames(FC) <- colname
  df2<- melt(FC,  #id.vars表示不需要合并的列；
             measure.vars = colnames(FC)[-1], #measure.vars表示需要合并的列；
             variable.name ="Groups", #variable.name表示列名合并后的新列名；
             value.name ="FC") 
  df<-merge(df1,df2,by = c("X","Groups"))
  df[df=="0"]<-NA
  df[df=="Inf"]<-NA
  
  df=df[which((df$PValue!="NA")&df$FC!="NA"),]
  
  df$color_pre[(df$PValue>= 0.05|df$PValue=="NA")|(df$FC> 1& df$FC < 1)] <- "no"
  df$color_pre[(df$PValue<0.05)&(df$FC>1)] <- "up"
  df$color_pre[(df$PValue<0.05)&(df$FC< 1)]  <- "down"
  
  log2(1.25)
  library(ggplot2)
  library(ggrepel)
  ggplot(df,aes(x=log2(df$FC),y=-log10(df$PValue),color=df$color_pre))+
    geom_point(aes(color=df$color_pre,shape=df$Groups),size=2)+
    scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
    scale_shape_manual(values =c("NM"=16,"YM"=17),guide=FALSE)+
    geom_text_repel(aes(label =df$X), size = 4,show.legend = F,color="black")+
    geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
    xlim(-2.5,2.5)+
    labs(x="Log2(After/Before)",y="-log10(P_Value)")+
    #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
    theme(axis.line=element_line(linetype=1,color="black",size=0.75),
          axis.ticks=element_line(color="black",size=0.75,lineend = 1),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
          title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
  ggsave("Y_N_MTXLEF_CCP+_male.pdf",width = 2.2, height = 2)
}
########################SFig6D:bar plot#####################
A<-c(36,40,16,19,33,7)
A2<-c(0,2,0,0,2,0)
B<-c("A","A","A","B","B","B")
C<-c("All","Female","Male","All","Female","Male")
data<-data.frame(A,A2,B,C)
ggplot()+
  geom_bar(aes(x=data$C,y=data$A,fill=data$B),stat = 'identity', width = 0.55,position = position_dodge(width = 0.75)) + 
  geom_bar(aes(x=data$C,y=data$A2,group=data$B),fill="grey",stat = 'identity', width = 0.55,position = position_dodge(width = 0.75)) + 
  scale_fill_manual(values =alpha(c( "#8051B1","#5A83B1"),0.6),guide = FALSE)+
  labs(x = '', y = 'Number') +
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(limit=rev(unique(data$C)))+
  coord_flip()+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("drug beford_after/MTX_lef_CCP+(A_B)_bar.pdf", width = 3, height =4)
########################SFig6E:pathway#####################
data<-read.csv("drug beford_after/pathway-MTXLEF.csv")

data %>%
  mutate(X.1= factor(X.1, levels = c("All","Female","Male"))) %>%
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
ggsave("drug beford_after/pathway_MTX_lef_CCP+(A_B).pdf", width = 3.5, height = 9)

########################SFig6F:scatter plot#####################
{data<-read.csv("test_n_b_CCP+_SEX_MTXHCQ.csv")[,c(1,7:10)]
P<-data[,c(1,4:5)]
FC<-data[,c(1:3)]
str<- colnames(P)
colname<- gsub("P_","",str)
colnames(P) <- colname
df1<- melt(P,  #id.vars表示不需要合并的列；
           measure.vars = colnames(P)[-1], #measure.vars表示需要合并的列；
           variable.name ="Groups", #variable.name表示列名合并后的新列名；
           value.name ="PValue") 
colnames(FC) <- colname
df2<- melt(FC,  #id.vars表示不需要合并的列；
           measure.vars = colnames(FC)[-1], #measure.vars表示需要合并的列；
           variable.name ="Groups", #variable.name表示列名合并后的新列名；
           value.name ="FC") 
df<-merge(df1,df2,by = c("X","Groups"))
df[df=="0"]<-NA
df[df=="Inf"]<-NA

df=df[which((df$PValue!="NA")&df$FC!="NA"),]

df$color_pre[(df$PValue>= 0.05|df$PValue=="NA")|(df$FC> 1& df$FC < 1)] <- "no"
df$color_pre[(df$PValue<0.05)&(df$FC>1)] <- "up"
df$color_pre[(df$PValue<0.05)&(df$FC< 1)]  <- "down"

log2(1.25)
library(ggplot2)
library(ggrepel)
ggplot(df,aes(x=log2(df$FC),y=-log10(df$PValue),color=df$color_pre))+
  geom_point(aes(color=df$color_pre,shape=df$Groups),size=2)+
  scale_color_manual(values =c('up'='#DD5B51','down'='#438CD5','no'='grey'),guide=FALSE)+
  scale_shape_manual(values =c("N"=16,"Y"=17),guide=FALSE)+
  geom_text_repel(aes(label =df$X), size = 4,show.legend = F,color="black")+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  xlim(-2,2)+
  labs(x="Log2(After/Before)",y="-log10(P_Value)")+
  #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("Y_N_MTXHCQ_CCP+.pdf",width = 3.2, height = 3)
}
########################SFig6G:pathway#####################
ggplot(data,aes(x =Term,y =Count,fill=PValue2))+
  geom_bar(fill ="grey", stat = "identity", width = 0.5) + # 注意将width宽度设小
  geom_point(aes(fill = PValue2),size =10,shape=21) + 
  coord_flip()+
  scale_fill_gradient2(high = "#d86967",low = "#58539f",mid = "white")+
  scale_x_discrete(limits=rev(data$Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),limits = c(0,15),breaks = c(0,5,10,15)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
ggsave("MTX_HCQ(A_B)_Y pathway.pdf", width = 4, height = 7)