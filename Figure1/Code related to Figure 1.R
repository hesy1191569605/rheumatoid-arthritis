########################Fig1B:Sample Age and Gender Distribution Plot#############
library(reshape2);library(dplyr);library(ggplot2);library(ggrepel);library(ggbreak)
data<-read.csv("B1.csv")
data$RATIO
data %>%
  mutate(X = factor(X, levels = c("HC", "PRE","RA","POS","NEG"))) %>%
  ggplot(aes(y =RATIO, x="",fill=group))+
  geom_bar(stat = 'identity',width=0.8,position='stack',size=1,color="white")+
  coord_polar("y", start=0)+
  geom_rect(aes(xmin=0,ymin=0,xmax=0,ymax=0))+
  scale_fill_manual(values=c("#EE8A56","#A8D1E5"),guide=F)+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"))+
  #geom_text(aes(label = sum, size=4))+
  facet_wrap( ~X,ncol = 5)
ggsave("B1.pdf", width =15, height =4.5)
library(reshape2);library(dplyr);library(ggplot2);library(ggrepel);library(ggbreak)
data<-read.csv("B2.csv")
data$RATIO
data %>%
  mutate(X = factor(X, levels = c("HC", "PRE","RA","POS","NEG"))) %>%
  ggplot(aes(y =RATIO, x="",fill=group))+
  geom_bar(stat = 'identity',width=0.6,position='stack',size=1,color="white")+
  #coord_polar("y", start=0)+
  #geom_rect(aes(xmin=0,ymin=0,xmax=0,ymax=0))+
  scale_fill_manual(values=c("#F1C5C5","#E69999","#D16363","#C00000"),guide=F)+
  #theme_minimal()+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  #geom_text(aes(label = sum, size=4))+
  facet_wrap( ~X,ncol = 5)
ggsave("B2.pdf", width =6, height =4.5)


########################Fig1C:Plasma Protein Distribution###############
data<-read.csv("RA_DATA2.csv",row.names = 1)[,-c(1:13)]
data<-data.frame(t(data))
data2<-log2(data)
range(na.omit(data))
pdf("C.pdf", width = 14.32,height = 7.36)
boxplot(scale(data2[478:576]),axes=F,col="white",#浣跨敤data1閲屽墠98鍒楁暟鎹粯鍥?
        border = "#1C65A9",lwd=1.5, cex=.3, #lwd璁剧疆绠辨绮楃粏锛宑ex璁剧疆瀛ょ偣澶у皬
        xlim=c(0,636),ylim=c(-4,4),xlab = "Samples",ylab = "log2(protein)",
        cex.lab=1.4, font.lab=1) #y杞翠笉浠?10寮€濮嬶紝鏄洜涓烘柟渚胯缃甽egend锛屽惁鍒檒egend浼氬拰绠辩嚎鍥鹃噸鍙犲湪涓€璧?boxplot(data[425:523],add = T,axes=F,at=c(425:523),col="white",
boxplot(scale(data2[597:656]),add = T,axes=F,at=c(99:158),col="white",
        border="#7B81B1",lwd=1.5, cex=.3,ylim=c(-4,4))
boxplot(scale(data2[1:478]),add = T,axes=F,at=c(159:636),col="white",
        border="#C00000",lwd=1.5, cex=.3,ylim=c(-4,4))
axis(2,at=c(-4,-2,0,2,4), #2琛ㄧず璁剧疆宸﹀潗鏍囪酱锛坹杞达級
     label=c("-4","-2","0","2","4"),lwd=2,
     lwd.ticks = 2,
     font.axis=1, #鍧愭爣鏍囩鈥?1鈥濅负姝ｅ父锛屸€?2鈥濅负鍔犵矖锛屸€?3鈥濅负鏂滀綋e
     cex.axis=1.3)
dev.off()

########################Fig1D:Cumulative Number of Samples Plot###############
data<-read.csv("ordDATA.csv",row.names = 1)
HP<-data[which(data$Group=="HP"),-c(1:3)]
count=c()
data1<-HP[1,]
sum<-colSums(data1,na.rm = T)
count<-sum(sum!=0,na.rm = T)
for (i in 2:nrow(HP)) {
  data1<-HP[c(1:i),]
  sum<-colSums(data1,na.rm = T)
  c<-sum(sum!=0,na.rm = T)
  count<-c(count,c)
}
count<-data.frame(count)
count$ratio<-count$count/99
count$group<-"HP"
count$rank<-as.numeric(row.names(count))
count$rankratio<-count$rank/99
HP<-count

RA<-data[which(data$Group=="A"),-c(1:3)]
count=c()
data1<-RA[1,]
sum<-colSums(data1,na.rm = T)
count<-sum(sum!=0,na.rm = T)
for (i in 2:nrow(RA)) {
  data1<-RA[c(1:i),]
  sum<-colSums(data1,na.rm = T)
  c<-sum(sum!=0,na.rm = T)
  count<-c(count,c)
}
count<-data.frame(count)
count$ratio<-count$count/265
count$group<-"RA"
count$rank<-as.numeric(row.names(count))
count$rankratio<-count$rank/265
RA<-count



PRE<-data[which(data$Group=="PRE"),-c(1:3)]
count=c()
data1<-PRE[1,]
sum<-colSums(data1,na.rm = T)
count<-sum(sum!=0,na.rm = T)
for (i in 2:nrow(PRE)) {
  data1<-PRE[c(1:i),]
  sum<-colSums(data1,na.rm = T)
  c<-sum(sum!=0,na.rm = T)
  count<-c(count,c)
}
count<-data.frame(count)
count$ratio<-count$count/63
count$group<-"pre-RA"
count$rank<-as.numeric(row.names(count))
count$rankratio<-count$rank/63
pre_RA<-count

ord<-rbind(HP,RA)
ord<-rbind(ord,pre_RA)


write.csv(ord,"ord.csv")
data<-read.csv("ord.csv")
library(ggplot2)
ggplot(data,aes(x=data$rankratio,y=data$count,col=data$group,shape=data$group))+
  geom_point(aes(x=data$rankratio,y=data$count,col=data$group,shape=data$group),size=4,alpha=0.9)+
  geom_line(size=1.5,alpha=0.7)+
  scale_shape_manual(values=c(17,16,18),guide=FALSE)+
  scale_color_manual(values = c("#1C65A9","#7B81B1","#C00000"),guide=FALSE)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("D.pdf",width = 4,height = 4)

########################SFig1A:QC samples Quality Control Scatter Plot########################
library(GGally)
library(rstatix)
data<-read.csv("QC8-15.csv",header = T,row.names = 1)
data2<-log2(data)
data2[data2=="-Inf"]<-NA
data3<-na.omit(data2)
library("ggcorrplot")
ggcorrplot(a, p.mat = cor_pmat(mydata),
           hc.order = TRUE, type = "lower",
           color = c("#1C65A9", "white", "#C00000"),
           outline.col = "white", lab = TRUE)
mean(a)
range(a)

b=pheatmap::pheatmap(a,cluster_cols = FALSE,cluster_rows = F,scale = "none",
                     color = c(colorRampPalette(colors = c("white","firebrick3"))(100)),
                     border=FALSE)

pdf("FIG1-B.pdf");b;dev.off()

########################SFig1B:Internal Standard Correlation Plot###############

data<-read.csv("IS-DATA.csv")[,-c(1:2)]
data[data==0] <-NA
data<-na.omit(data)
mean <- colMeans(data,na.rm = T)
all<-mean(mean)
pare<-mean/all
data2<-data
count<- apply(data,1,function(c)sum(c!=0,na.rm = T))
data3<-data[which(count>=ncol(data)*0.5),]
for (a in  1:(ncol(data))) {
  for (i in  1:(nrow(data))) {
    data2[i,a]<-data[i,a]/pare[a]
  }
}
data3<-data.frame(scale(data2))
colSums(data1,na.rm = T)
colMeans(data2,na.rm = T)
mydata<-as.data.frame(lapply(data3,as.numeric))
a<-cor(mydata, method = "spearman",use="pairwise" )
write.csv(a,"is_cor.csv")
a<-read.csv("is_cor.csv",row.names = 1)
range(a)
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
library(pheatmap)
a[a==1] <-NA

aa=pheatmap(a,cluster_cols =F,scale = "none",cluster_rows =F,
            color = c(colorRampPalette(colors = c("white","firebrick3"))(100)),
            border=FALSE,cellwidth =1,cellheight =1,,fontsize_col = 10,fontsize_row =8)
order_row=aa$tree_row$order
order_col=aa$tree_col$order
datat=data.frame(a[order_row,order_col])
datat=data.frame(rownames(datat),datat,check.names =F)
colnames(datat)[1] = "id" 

b<-melt(a,measure.vars =names(a))
b<-unique(b$value)
write.csv(b,"is_cor.csv")




########################SFig1C:Replicate Sample Correlation Plot###############
library(GGally)
library(rstatix)
data<-read.csv("SAMPLE_REPEAT3.csv",header = T,row.names = 1)
data2<-log2(data)
data2[data2=="-Inf"]<-NA
data3<-na.omit(data2)


mydata<-as.data.frame(lapply(data3,as.numeric))
a<-cor(mydata, method = "spearman",use="pairwise" )

COR<- vector(length = ncol(data)/2)
name <- vector(length = ncol(data)/2)
for(i in 1:ncol(data)/2)try({
  COR[i] <- a[i+125,i]
  name[i]<-names(data3)[i]
})
test<-data.frame(name,COR)
write.csv(test,"SAMPLE-REPEAT2.csv")
write.csv(a,"SAMPLE-COR.csv")



data<-read.csv("SAMPLE_REPEAT3.csv",header = T,row.names = 1)
data2<-log2(data)
data2[data2=="-Inf"]<-NA
data3<-na.omit(data2)


mydata<-as.data.frame(lapply(data3,as.numeric))
a<-cor(mydata, method = "spearman",use="pairwise" )
a<-a[c(11:20),c(1:10)]
# Visualize
a<-read.csv("cor_repeat2.csv",row.names = 1)
mean(a)
range(a)

bk <- c(seq(0.6,0.73,by=0.01))
pheatmap::pheatmap(a,cluster_cols = F,cluster_rows = F,scale = "none",
                   color = c(colorRampPalette(colors = c("#FAEDED","firebrick3"))(length(bk))),border_color = "black",breaks=bk)
col=colorRampPalette(c("navy", "white", "firebrick3"))
corrplot(a,type="lower", method="number",cl.pos="n",col=col(10),addCoefasPercent=F,tl.cex=1,number.cex=1)



########################SFig1D:Analysis of Age Differences Across Different Groups########################
library(ggplot2);library(ggsignif);library(reshape2);library(dplyr)
data<-data[which(data$Group2!="NA"),]
data<-data[which(data$Group2!="IIM"&data$Group2!="SLE"&data$Group2!="SS"&data$Group2!="SSc"),]
F<-data[which(data$Gender=="Female"),]  
M<-data[which(data$Gender=="Male"),]  
data%>%
  mutate(Group2 = factor(Group2, levels = c("Health","At-risk of RA","ACPA+ RA","ACPA- RA"))) %>%
  ggplot(aes(x =Group2,y=as.numeric(Age))) + 
  geom_boxplot(fill="white",width=0.5,color="black",outlier.shape = NA,alpha=0.8)+
  geom_jitter(aes(color=Group2),position = position_jitterdodge(0.8),size=2,alpha=0.8)+
  scale_color_manual(values =c("Health"='#1C65A9',"At-risk of RA"="#8E93BC","ACPA+ RA"="#AF4B87","ACPA- RA"="#E8A85E"),guide=FALSE)+
  geom_signif(comparisons =  list(c("Health", "At-risk of RA"),c("ACPA+ RA","At-risk of RA"),
                                  c("ACPA+ RA","ACPA- RA"),c("Health", "ACPA+ RA"),
                                  c("ACPA- RA","At-risk of RA"),c("Health","ACPA- RA")),
              test = "t.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
  scale_y_continuous(breaks = c(20,40,60,80,100)) + 
  labs(x="",y="")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("AGE.pdf", width =4, height =4)

########################SFig1E:inflammatory response pathway####################

library(GSVA);library(reshape)
gene<- read.csv("GENE LIST.csv",header = T)
expression <- read.csv("RA_DATA1.csv",header = T,row.names = 1)
uni_matrix <- data.frame(t(expression[,-c(1:15)]))
gene_set<-gene 
bg_genes <- split(as.matrix(gene_set)[,1], gene_set[,2])
gsva_matrix <- gsva(as.matrix(uni_matrix), bg_genes, method='ssgsea', kcdf='Gaussian', abs.ranking=TRUE)
gsva_matrix<-data.frame(expression[,c(1:15)],t(gsva_matrix))
write.csv(gsva_matrix,"gsva_pathway1.csv")
library(pheatmap)
gsva_matrix <- read.csv("gsva_pathway1.csv",row.names = 1)
bk <- c(seq(-1.5,0,by=0.01),seq(0.01,1.5,by=0.01))

data<-gsva_matrix[which((gsva_matrix$Group2=="ACPA+ RA"|gsva_matrix$Group2=="ACPA- RA")&gsva_matrix$Group1=="RA_A"),]
P_RA<-gsva_matrix[which(gsva_matrix$Group2=="ACPA+ RA"&gsva_matrix$Group1=="RA_A"),-c(1:15)]
N_RA<-gsva_matrix[which(gsva_matrix$Group2=="ACPA- RA"&gsva_matrix$Group1=="RA_A"),-c(1:15)]
HP<-gsva_matrix[which(gsva_matrix$Group2=="Health"),-c(1:15)]

P_median<- median(P_RA,na.rm=T)
N_median<- median(N_RA,na.rm=T)
HP_median<- median(HP,na.rm=T)
data$Group2<-as.factor(data$Group2)
data<-data[!is.na(data$DAS28.CRP),]
P_P_N = lm(inflammatory~Group2+DAS28.CRP,data=data)

library(reshape2);library(ggplot2);library(ggsignif)

ggplot(data,aes(x=Group2,y=inflammatory,color=Group2))+
  geom_boxplot(aes(color=Group2),width=0.65,fill="white",outlier.shape = NA)+
  geom_jitter(aes(color=Group2),position = position_jitterdodge(0.5),size=1.2,alpha=1)+  scale_fill_manual(values = c("ACPA+ RA"="#E8A85E","ACPA- RA"="#AF4B87"))+
  scale_color_manual(values = c("ACPA+ RA"="#E8A85E","ACPA- RA"="#AF4B87"))+
  geom_signif(comparisons =  list(c("ACPA- RA", "ACPA+ RA")),color="black",test = "t.test",map_signif_level =T,na.rm =T, show.legend = NA)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("inflammatory boxplot.pdf",width = 4,height = 3)

########################SFig1F:scatter plot#####################
data<-read.csv("pheatmap.csv",header = T,row.names = 1)
data[data=="0"]<-NA
data[data=="Inf"]<-NA
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
data_s<-na.omit(data_s)
library(pheatmap)
p <-pheatmap(data_s,scale='none',show_rownames = T, cluster_cols = F,cluster_rows = F,
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 10,
             breaks=bk)

########################SFig1H:pathway######################################
data<-read.csv("pathway.csv",header = T)
table(data$GROUP3)
library(ggplot2);library(reshape)
ggplot(data,aes(x =GROUP1,y =Term,size=Count,fill=LOG10))+
  geom_point(aes(size=Count,fill=LOG10),color='black',shape = 21)+#閻犱礁澧介悿鍡涙倷閸︻厽鐣卞鍫嗗啰姣堝☉鎾抽叄椤や線鎳??
  scale_fill_gradient2(low="#3C82B9",mid = "white",high = "#EE3434")+#闁煎浜滈悾鐐▕婢跺鐟ら柛娆愶耿椤や線?????
  scale_x_discrete(limits=c("At-risk RA vs Healthy","ACPA+ RA vs Healthy","ACPA- RA vs Healthy","ACPA+ RA vs At-risk RA ","ACPA+ RA vs ACPA- RA"))+
  scale_y_discrete(limits=rev(unique(data$Term)))+
  labs(x="",y="")+
  scale_size_continuous(range=c(4,11))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white",color = "black"),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust =1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  facet_wrap( ~ GROUP3, ncol =3,scales="free")
ggsave("pathway.pdf", width = 6, height =13)