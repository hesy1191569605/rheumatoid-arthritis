########################Fig1b########################
setwd("C:/Users/11915/Desktop/RA/913/FIG1/")
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

########################Fig1c###############
setwd("C:\\Users\\11915\\Desktop\\RA\\913\\FIG1/")
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




########################Fig1d###############
setwd("C:/Users/11915/Desktop/RA/913/FIG1/")
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



########################Fig1e###############
setwd("C:\\Users\\11915\\Desktop\\RA\\913\\")
data<-read.csv("cln-dataKNN1.csv",row.names = 1)
pdf("FIG1/D.pdf", width = 14.32,height = 7.36)
data2<-log2(data)
range(na.omit(data))
boxplot(data2[425:523],axes=F,col="white",#浣跨敤data1閲屽墠98鍒楁暟鎹粯鍥?
        border = "#1C65A9",lwd=1.5, cex=.3, #lwd璁剧疆绠辨绮楃粏锛宑ex璁剧疆瀛ょ偣澶у皬
        xlim=c(0,366),ylim=c(-6,4),xlab = "Samples",ylab = "log10 (protein)",
        cex.lab=1.4, font.lab=1) #y杞翠笉浠?10寮€濮嬶紝鏄洜涓烘柟渚胯缃甽egend锛屽惁鍒檒egend浼氬拰绠辩嚎鍥鹃噸鍙犲湪涓€璧?boxplot(data[425:523],add = T,axes=F,at=c(425:523),col="white",
boxplot(data2[524:579],add = T,axes=F,at=c(99:154),col="white",
        border="#7B81B1",lwd=1.5, cex=.3,ylim=c(-6,4))
boxplot(data2[1:212],add = T,axes=F,at=c(155:366),col="white",
        border="#C00000",lwd=1.5, cex=.3,ylim=c(-6,4))


#"Health"="#1C65A9","At risk of RA"='#7B81B1',"RA"="#C00000"
axis(2,at=c(-6,-3,0,3), #2琛ㄧず璁剧疆宸﹀潗鏍囪酱锛坹杞达級
     label=c("-6","-3","0","3"),lwd=2,
     lwd.ticks = 2,
     font.axis=1, #鍧愭爣鏍囩鈥?1鈥濅负姝ｅ父锛屸€?2鈥濅负鍔犵矖锛屸€?3鈥濅负鏂滀綋e
     cex.axis=1.3)
dev.off()

########################Fig1f###############
setwd("C:/Users/11915/Desktop/RA/913/FIG1/")
data<-read.csv("ord_DATA.csv",row.names = 1)
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
HP<-count
write.csv(count,"ord_HP.csv")
RA<-data[which(data$Group=="RA_A"),-c(1:3)]
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
count$ratio<-count$count/212
count$group<-"RA"
RA<-count
write.csv(count,"ord_RA.csv")


PRE<-data[which(data$Group=="PRA"),-c(1:3)]
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
count$ratio<-count$count/56
count$group<-"pre-RA"
pre_RA<-count
write.csv(count,"ord_PRE.csv")

data<-read.csv("ord2.csv")
library(ggplot2)
ggplot(data,aes(x=data$X,y=data$count,col=data$X.1,shape=data$X.1))+
  geom_point(aes(x=data$X,y=data$count,col=data$X.1,shape=data$X.1),size=4,alpha=0.9)+
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
ggsave("C:/Users/11915/Desktop/RA/913/FIG1/F.pdf",width = 4,height = 4)


