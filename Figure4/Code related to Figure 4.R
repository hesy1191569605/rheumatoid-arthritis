##################Calculation of disease activity levels in different groups#############
setwd("C:/Users/11915/Desktop/RA/913/")
{data<-read.csv("cln-data1.csv")
  data<-data[which(data$DAS28.CRP.HM!="NA"),]
  ALL<-data
  H<-data[which(data$Class=="high_disease_activity"),-c(1:18)]
  M<-data[which(data$Class=="moderate_disease_activity"),-c(1:18)]
  L<-data[which(data$Class=="low_disease_activity"),-c(1:18)]
  D<-data[which(data$Class=="disease_remission" ),-c(1:18)]
  H_median<- apply(H,2,median,na.rm=T)
  M_median<- apply(M,2,median,na.rm=T)
  L_median<- apply(L,2,median,na.rm=T)
  D_median<- apply(D,2,median,na.rm=T)
  FC_DH<-D_median/H_median
  FC_DM<-D_median/M_median
  FC_DL<-D_median/L_median
  FC_LH<-L_median/H_median
  FC_LM<-L_median/M_median
  FC_MH<-M_median/H_median
  P_ANOVE<-rep(NA,ncol(ALL)-18)
  P_DH<-rep(NA,ncol(ALL)-18)
  P_DM<-rep(NA,ncol(ALL)-18)
  P_DL<-rep(NA,ncol(ALL)-18)
  P_LH<-rep(NA,ncol(ALL)-18)
  P_LM<-rep(NA,ncol(ALL)-18)
  P_MH<-rep(NA,ncol(ALL)-18)
  i=1
  for(i in 1:(ncol(ALL)-18))try({
    P_DH[i] = t.test(as.numeric(D[,i]),as.numeric(H[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
    P_DM[i] = t.test(as.numeric(D[,i]),as.numeric(M[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
    P_DL[i] = t.test(as.numeric(D[,i]),as.numeric(L[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
    P_LH[i] = t.test(as.numeric(L[,i]),as.numeric(H[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
    P_LM[i] = t.test(as.numeric(L[,i]),as.numeric(M[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
    P_MH[i] = t.test(as.numeric(M[,i]),as.numeric(H[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
    P_ANOVE[i] = summary(aov(ALL[,(i+18)]~ALL$Class,ALL))[[1]][,5] [1]
  })
  test<-data.frame(colnames(ALL)[-c(1:18)],P_ANOVE,P_DH,P_DM,P_DL,P_LH,P_LM,P_MH,
                   FC_DH,FC_DM,FC_DL,FC_LH,FC_LM,FC_MH,D_median,L_median,M_median,H_median)
  write.csv(test,"disease activity class test.csv")
}
##################plot of disease activity levels in different groups#############
library(pheatmap)
setwd("C:/Users/11915/Desktop/RA/913/FIG4/")
data<-read.csv("disease activity class test.csv",row.names = 1)
data1<-data[which(data$P_DL<0.05|data$P_LM<0.05|data$P_MH<0.05|data$P_LH<0.05|data$P_DM<0.05|data$P_DH<0.05),]
data2<-data1[,c(15:18)]
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
colnames(data_s)<-c("Disease remission","Low disease activity","Moderate disease activity","High disease activity")
row.names(data_s)<-row.names(data1)
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
p <-pheatmap(data_s,scale='none',show_rownames = F, cluster_cols = F,clustering_method = "mcquitty",border_color = NA,cutree_rows=6, #c(3,6,9,12,15,18,21),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 2,
             breaks=bk)
pdf("P_PHEATMAP.pdf", width = 13, height = 13);p;dev.off()
newOrder = data_s[p$tree_row$order,]
p1 <-pheatmap(newOrder[,c(1:4)],scale='none',show_rownames = T, cluster_cols = F,cluster_rows  = F,clustering_method = "ward.D2",border_color = NA,#gaps_row =  c(3,6,9,12,15,18,21),
              color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 2,
              breaks=bk)

pdf("P_PHEATMAP2.pdf", width = 13, height = 13);p1;dev.off()
row_cluster = cutree(p$tree_row,k=6)
{
  newOrder = data_s[p$tree_row$order,]
  newOrder[,ncol(newOrder)+1]=row_cluster[match(rownames(newOrder),names(row_cluster))]
  colnames(newOrder)[ncol(newOrder)]="Cluster"
  head(newOrder)
  unique(newOrder$Cluster)
  newOrder$Cluster = paste0("cluster",newOrder$Cluster)
  newOrder$gene = rownames(newOrder)
  head(newOrder)
  write.csv(newOrder,"ALL-cluster8.csv")#瀵煎嚭鏁版嵁鏀瑰悕
  library(reshape2)
  data_new = melt(newOrder)
  head(data_new)
  library(stringr)
  data_new$group1<-data_new$variable
  data_new$group2<-str_c(data_new$gene)
  data_new$group3<-str_c(data_new$Cluster,data_new$variable)
  library(ggplot2)
  library(ggthemes)
  library(ggpubr)
  medians<-aggregate(data_new$value,list(data_new$group3),median)
  medians$variable<-str_sub(medians$Group.1,9,-1)
  medians$new<-str_sub(medians$Group.1,1,8)
  medians$new1<-str_sub(medians$Group.1,1,8)
  
  medians$new
  
  
}
{p1<-ggplot(data_new[data_new$Cluster=="cluster1",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster1",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster1",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  
  p2<-ggplot(data_new[data_new$Cluster=="cluster2",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster2",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster2",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  
  p3<-ggplot(data_new[data_new$Cluster=="cluster3",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster3",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster3",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  
  
  p4<-ggplot(data_new[data_new$Cluster=="cluster4",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster4",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster4",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  
  p5<-ggplot(data_new[data_new$Cluster=="cluster5",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster5",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster5",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  p6<-ggplot(data_new[data_new$Cluster=="cluster6",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster6",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster6",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  p7<-ggplot(data_new[data_new$Cluster=="cluster7",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster7",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster7",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
  p8<-ggplot(data_new[data_new$Cluster=="cluster8",],aes(variable, value, group=group2)) + 
    geom_line(aes(color=Cluster),size=0.8,alpha=0.7) + 
    geom_line(data=medians[medians$new1=="cluster8",],aes(variable,x,group=new),size=2,color="white")+
    geom_line(data=medians[medians$new1=="cluster8",],aes(variable,x,group=new),size=1,color="black")+
    geom_boxplot (aes(group=variable),outlier.shape = NA,width=0.42,size=0.5)+
    geom_hline(yintercept =0,linetype=2,size=0.5,color="gray66") +
    scale_color_manual(values = c(cluster1="#638759",cluster2="#C1AFCC",cluster3="#eca75e",cluster4="#D8ACAC",cluster5="#A1B4D8"),guide=FALSE)+
    labs(x="",y="Z-Score")+
    theme_classic() +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13, color = "black"),
          axis.line=element_line(color="black"),
          axis.ticks=element_line(color="black"),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",  hjust =1,angle = 45),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+ylim(-1.5,1.5)
}
a<-ggarrange(p1,p2,p3,p4,p5,p6,nrow=8,ncol =1,legend = NULL)
a
ggsave("disease activity class cluster6.pdf", width = 2.5, height = 25)
##################LOESS PREDICT OF PROTEIN LEVEL ACROSS DAS28#############
data<-'EXPRESSION DATAFRAME'
data_Z <- data[order(data$DAS28), ]
result <- data.frame(data_Z$DAS28)
for (i in 2:914) {
  loess_fit <- loess(data_Z[, i] ~ data_Z$DAS28, data = data_Z, span = 0.3)
  y_pred <- predict(loess_fit, newdata = data.frame(x = data_Z$DAS28))
  result <- cbind(result, y_pred)
}

##################DE-SWAN#############
DEswan=function(data.df, qt, covariates){
  
  if(missing(data.df)==T){
    print("No input data frame")
    stop
  }
  if(missing(qt)==T){
    print("No quantitative trait")
    stop
  }
  if(missing(covariates)==T){
    covariates  <-  NULL
  }else{
    covariates <- data.frame(covariates)  
  }
  
  pvalues.tot  <-  NULL
  coefficients.tot  <-  NULL
  
  window.center <- seq(0, 8.1, by = 0.01)
  
  for (k in 1:length(window.center)) {
    pvalues <- NULL
    coefficients <- NULL
    idx_below <- which(qt <= window.center[k]) 
    idx_above <- which(qt > window.center[k])
    if (length(idx_below) < 9 || length(idx_above) < 9) {
      next  
    }
    if (length(idx_below) < 10 || length(idx_above) < 10) {
      idx_below <- tail(sort(idx_below), 9)
      idx_above <- head(sort(idx_above), 9)
    }else{ 
      idx_below <- tail(sort(idx_below), 10)  
      idx_above <- head(sort(idx_above), 10) 
    }

    qt.tmp <- rep(NA, length(qt))
    qt.tmp[idx_below] <- 0
    qt.tmp[idx_above] <- 1
    qt.tmp <- factor(qt.tmp)
    for (i in 1:ncol(data.df)) {
      
      if(is.null(covariates) == T){
        deswan.formula = "data.df[, i] ~ qt.tmp"  
      }else{
        deswan.formula  <-  paste(c("data.df[, i] ~ qt.tmp", paste("covariates$",colnames(covariates), collapse  =  " + ", sep = "")), collapse = " + ", sep = "")
      }
      test.glm  <-  NULL
      test.glm  <- try(glm.fit  <-  glm(as.formula(deswan.formula), family  =  gaussian), silent=TRUE)
      if(class(test.glm)[1] !=  "try-error"){
        glm.res  <-  car::Anova(glm.fit,  type  =  "2")
        pvalues  <-  rbind(pvalues, data.frame(variable  =  colnames(data.df)[i], window.center  =  window.center[k], factor  =  rownames(glm.res), pvalue=glm.res$`Pr(>Chisq)`, stringsAsFactors  =  F))
        coefficients  <- rbind(coefficients, data.frame(variable  =  colnames(data.df)[i], window.center  =  window.center[k], factor  = names(coefficients(glm.fit)), 
                                                        coefficient=coefficients(glm.fit), stringsAsFactors  =  F))
      }
    }
    pvalues.tot  =  rbind(pvalues.tot, pvalues)
    coefficients.tot  =  rbind(coefficients.tot, coefficients)
    
    print(paste("window.center  ", k, "/", length(window.center), sep = ""))
  }
  pvalues.tot$factor[which(pvalues.tot$factor=="qt.tmp")]<-"qt"
  pvalues.tot$factor=gsub("^covariates\\$","",pvalues.tot$factor)
  coefficients.tot$factor[which(coefficients.tot$factor=="qt.tmp1")]<-"qt"
  coefficients.tot$factor=gsub("^covariates\\$","",coefficients.tot$factor)
  
  
  results  =  list(p  =  pvalues.tot, coeff  =  coefficients.tot)
  return(results)
}

res.DEswan <- DEswan(
  data = data[,-c(1:4)],
  qt = data[,2],
  covariates = data[,c(3:4)]
)

res.DEswan.wide.p=reshape.DEswan(res.DEswan,parameter = 1,factor = "qt")
res.DEswan.wide.q=q.DEswan(res.DEswan.wide.p,method="BH")
res.DEswan.wide.coeff=reshape.DEswan(res.DEswan,parameter = 2,factor = "qt")
toHeatmap=sign(res.DEswan.wide.coeff[,-1])*-log10(res.DEswan.wide.p[,-1])
rownames(toHeatmap)<-res.DEswan.wide.coeff[,1]
res.DEswan.wide.p.signif=nsignif.DEswan(res.DEswan.wide.p)
res.DEswan.wide.q.signif=nsignif.DEswan(res.DEswan.wide.q)
toPlot=res.DEswan.wide.p.signif[1:3,]
toPlotq=res.DEswan.wide.q.signif[1:3,]

##################PATHWAY###########
setwd("C:/Users/11915/Desktop/RA/913/FIG4/")
data<-read.csv("pathway deswan&lm.csv",header = T)

library(ggplot2);library(reshape)
ggplot(data,aes(x =data$GROUP,y =data$Term,size=data$Count,fill=data$LOG10))+
  geom_point(aes(size=data$Count,fill=data$LOG10),color='black',shape = 21)+#閻犱礁澧介悿鍡涙倷閸︻厽鐣卞鍫嗗啰姣堝☉鎾抽叄椤や線鎳??
  scale_fill_gradient2(low="#3C82B9",mid = "white",high = "#EE3434")+#闁煎浜滈悾鐐▕婢跺鐟ら柛娆愶耿椤や線?????
  scale_x_discrete(limits=c("LM","2.9","4","5.1"))+
  scale_y_discrete(limits=rev(unique(data$Term)))+
  labs(x="",y="")+
  scale_size_continuous(range=c(4,9))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white",color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("pathway deswan&lm.pdf", width = 9, height = 8)

##################Overlap plot of cumulative values across different clinical indicators########################
data <- read.csv("C:\\Users\\Administrator\\Desktop\\累计数据.csv",row.names = 1,check.names = F)
library(ggplot2)
library(dplyr)

a1 <- data[,c(2,6:8)]
a1 <- a1[which((a1$P_VAS<=0.05)),]
a1 <- a1[order(a1$P_VAS),]
a1 <- dplyr::mutate(a1, num = row_number())

library(reshape2)
aa1<-melt(a1[,-1],id=(names(a1)[5]))
library(dplyr)
a3 <- aa1 %>% 
  group_by(variable) %>% 
  arrange(variable,num) %>% 
  mutate(cum_num = cumsum(value)) %>% 
  ungroup()
data$a2.9
p1 <- ggplot(data=a3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,13),breaks =c(0,4,8,12))+
  scale_x_continuous(limits = c(0,90),breaks =c(0,20,40,60,80))+
  labs(x='Proteins rank with VAS',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(a2.9="#5A96B5",a4="#67867C",a5.2="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p1
ggsave("C:\\Users\\Administrator\\Desktop\\VAS.pdf",width = 4.2,height = 3)


#
b1 <- data[,c(3,6:8)]
b1 <- b1[which((b1$P_SJC<=0.05)),]
b1 <- b1[order(b1$P_SJC),]
b1 <- dplyr::mutate(b1, num = row_number())

bb1<-melt(b1[,-1],id=(names(b1)[5]))
library(dplyr)
b3 <- bb1 %>% 
  group_by(variable) %>% 
  arrange(variable,num) %>% 
  mutate(cum_num = cumsum(value)) %>% 
  ungroup()
p2 <- ggplot(data=b3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,6),breaks =c(0,2,4,6))+
  scale_x_continuous(limits = c(0,60),breaks =c(0,20,40,60))+
  labs(x='Proteins rank with SJC',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(a2.9="#5A96B5",a4="#67867C",a5.2="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p2
ggsave("C:\\Users\\Administrator\\Desktop\\SJC.pdf",width = 4.2,height = 3)

#
c1 <- data[,c(4,6:8)]
c1 <- c1[which((c1$P_TJC<=0.05)),]
c1 <- c1[order(c1$P_TJC),]
c1 <- dplyr::mutate(c1, num = row_number())

cc1<-melt(c1[,-1],id=(names(c1)[5]))
library(dplyr)
c3 <- cc1 %>% 
  group_by(variable) %>% 
  arrange(variable,num) %>% 
  mutate(cum_num = cumsum(value)) %>% 
  ungroup()

p3 <- ggplot(data=c3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,8),breaks =c(0,2,4,6,8))+
  scale_x_continuous(limits = c(0,40),breaks =c(0,10,20,30,40))+
  labs(x='Proteins rank with TJC',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(a2.9="#5A96B5",a4="#67867C",a5.2="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p3
ggsave("C:\\Users\\Administrator\\Desktop\\TJC.pdf",width = 4.2,height = 3)

#
d1 <- data[,5:8]
d1 <- d1[which((d1$P_CRP<=0.05)),]
d1 <- d1[order(d1$P_CRP),]
d1 <- dplyr::mutate(d1, num = row_number())

dd1<-melt(d1[,-1],id=(names(d1)[5]))
library(dplyr)
d3 <- dd1 %>% 
  group_by(variable) %>% 
  arrange(variable,num) %>% 
  mutate(cum_num = cumsum(value)) %>% 
  ungroup()

p4 <- ggplot(data=d3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,10),breaks =c(0,2,4,6,8,10))+
  scale_x_continuous(limits = c(0,100),breaks =c(0,20,40,60,80,100))+
  labs(x='Proteins rank with CRP',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(a2.9="#5A96B5",a4="#67867C",a5.2="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p4
ggsave("C:\\Users\\Administrator\\Desktop\\CRP.pdf",width = 4.2,height = 3)

