########################Calculation of disease activity levels in different groups#####################
{ data<-read.csv("RA_DATA1.csv")
data<-data[which(data$Group2=="ACPA+ RA"),]

ALL<-data
H<-data[which(data$Class=="high_disease_activity"),-c(1:15)]
M<-data[which(data$Class=="moderate_disease_activity"),-c(1:15)]
L<-data[which(data$Class=="low_disease_activity"),-c(1:15)]
D<-data[which(data$Class=="disease_remission" ),-c(1:15)]
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
P_ANOVE<-rep(NA,ncol(ALL)-15)
P_DH<-rep(NA,ncol(ALL)-15)
P_DM<-rep(NA,ncol(ALL)-15)
P_DL<-rep(NA,ncol(ALL)-15)
P_LH<-rep(NA,ncol(ALL)-15)
P_LM<-rep(NA,ncol(ALL)-15)
P_MH<-rep(NA,ncol(ALL)-15)
i=1
for(i in 1:(ncol(ALL)-15))try({
  P_DH[i] = t.test(as.numeric(D[,i]),as.numeric(H[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_DM[i] = t.test(as.numeric(D[,i]),as.numeric(M[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value; 
  P_DL[i] = t.test(as.numeric(D[,i]),as.numeric(L[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_LH[i] = t.test(as.numeric(L[,i]),as.numeric(H[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_LM[i] = t.test(as.numeric(L[,i]),as.numeric(M[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_MH[i] = t.test(as.numeric(M[,i]),as.numeric(H[,i]),alternative = c("two.sided"),paired=F,var.equal=TRUE)$p.value;
  P_ANOVE[i] = summary(aov(ALL[,(i+15)]~ALL$Class,ALL))[[1]][,5] [1]
})
test<-data.frame(colnames(ALL)[-c(1:15)],P_ANOVE,P_DH,P_DM,P_DL,P_LH,P_LM,P_MH,
                 FC_DH,FC_DM,FC_DL,FC_LH,FC_LM,FC_MH,D_median,L_median,M_median,H_median)
write.csv(test,"FIG4/disease activity class test ACPA+ RA.csv")
}
########################Fig4A:plot of disease activity levels in different groups################
library(pheatmap)
data<-read.csv("disease activity class test ACPA+ RA.csv",row.names = 1)
data1<-data[which(data$P_DL<0.05|data$P_LM<0.05|data$P_MH<0.05|data$P_LH<0.05|data$P_DM<0.05|data$P_DH<0.05),]
data2<-data1[,c(15:18)]
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
colnames(data_s)<-c("Disease remission","Low disease activity","Moderate disease activity","High disease activity")
row.names(data_s)<-row.names(data1)
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
p <-pheatmap(data_s,scale='none',show_rownames = F, cluster_cols = F,clustering_method = "ward",border_color = NA,cutree_rows=6, #c(3,6,9,12,15,15,21),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 1,
             breaks=bk)
pdf("P_PHEATMAP_ACPA+ RA.pdf", width = 13, height = 13);p;dev.off()
newOrder = data_s[p$tree_row$order,]
p1 <-pheatmap(newOrder[,c(1:4)],scale='none',show_rownames = T, cluster_cols = F,cluster_rows  = F,clustering_method = "ward.D2",border_color = NA,#gaps_row =  c(3,6,9,12,15,15,21),
              color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 2,
              breaks=bk)

pdf("P_PHEATMAP2_ACPA+ RA.pdf", width = 13, height = 13);p1;dev.off()
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
  write.csv(newOrder,"ALL-cluster8 ACPA+ RA.csv")#瀵煎嚭鏁版嵁鏀瑰悕
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
    scale_color_manual(values = c(cluster4="#638759",cluster1="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster6="#A1B4D8"),guide=FALSE)+
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
    scale_color_manual(values = c(cluster4="#638759",cluster1="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster6="#A1B4D8"),guide=FALSE)+    labs(x="",y="Z-Score")+
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
    scale_color_manual(values = c(cluster4="#638759",cluster1="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster6="#A1B4D8"),guide=FALSE)+    labs(x="",y="Z-Score")+
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
    scale_color_manual(values = c(cluster4="#638759",cluster1="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster6="#A1B4D8"),guide=FALSE)+    labs(x="",y="Z-Score")+
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
    scale_color_manual(values = c(cluster4="#638759",cluster1="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster6="#A1B4D8"),guide=FALSE)+    labs(x="",y="Z-Score")+
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
    scale_color_manual(values = c(cluster4="#638759",cluster1="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster6="#A1B4D8"),guide=FALSE)+    labs(x="",y="Z-Score")+
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
ggsave("disease activity class cluster6_ACPA+ RA.pdf", width = 2.5, height = 25)

########################Fig4B:LOESS PREDICT OF PROTEIN LEVEL ACROSS DAS28################
data<-'EXPRESSION DATAFRAME'
data_Z <- data[order(data$DAS28), ]
result <- data.frame(data_Z$DAS28)
for (i in 2:997) {
  loess_fit <- loess(data_Z[, i] ~ data_Z$DAS28, data = data_Z, span = 0.3)
  y_pred <- predict(loess_fit, newdata = data.frame(x = data_Z$DAS28))
  result <- cbind(result, y_pred)
}

########################Fig4C:DE-SWAN################
library(eoffice)
library(DEswan)
DEswan19=function(data.df, qt, covariates){
  
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
  
  window.center <- seq(1, 8, by = 0.1)
  
  for (k in 1:length(window.center)) {
    pvalues <- NULL
    coefficients <- NULL
    # ?ҵ????ӽ???С?ڵ???window.center??ʮ????????λ??
    idx_below <- which(qt <= window.center[k]) 
    # ?ҵ????ӽ??Ҵ???window.center??ʮ????????λ??
    idx_above <- which(qt > window.center[k])
    # ??????һ?˵????????????Ÿ?????ȡ??????ѭ??
    # ??????һ?˵????????????Ÿ?????ȡ??????ѭ??
    if (length(idx_below) < 19 || length(idx_above) < 19) {
      next  # ȡ??????ѭ??
    }
    # ????��?˵Ĵ?????????????ʮ???????ֱ?ȡ?Ÿ?
    if (length(idx_below) < 20 || length(idx_above) < 20) {
      idx_below <- tail(sort(idx_below), 19)
      idx_above <- head(sort(idx_above), 19)
    }else{ 
      idx_below <- tail(sort(idx_below), 20)  # ȡ???window.center??С?ڵ??ڵ?ʮ??????
      idx_above <- head(sort(idx_above), 20)  # ȡ???window.center?Ҵ??ڵ?ʮ??????
    }
    # ????qt.tmp
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

data<-RA_DATA1
dt <- data[data$Group1 == 'RA_A' &
             !is.na(data$`DAS28-CRP`)&!is.na(data$`Anti-citrullinated peptide antibodies`)&data$`Anti-citrullinated peptide antibodies`=='Postive',]
dt<-as.data.frame(dt)

dt$Gender <- ifelse(dt$Gender == "Female", 0, 1)
dt[, c(4,5,8, 16:1011)] <- lapply(dt[, c(4,5,8, 16:1011)], as.numeric)
res.DEswan <- DEswan19(
  data = dt[,c(16:1011)],
  qt =  dt[,8],
  covariates = dt[,c(4:5)]
  
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
x=as.numeric(gsub("X","",colnames(toPlot)))
y<-toPlot[2,]
# Define the breakpoints for the DAS28 categories
breaks <- c(-Inf, 2.6,3.2, 5.0, Inf)

# Define the labels for each category
labels <- c( "????","?ͻ??", "?еȻ??", "?߻??")

# Assume 'data' is your data frame and 'das28' is the column with DAS28 scores
# Categorize the DAS28 scores
categories <- cut(x, breaks = breaks,labels = labels)



# ??ʼ??һ???б?��????ÿ???ȼ???????Yֵ????????
top_indices = numeric()

# ????ÿ??????
for (category in labels) {
  # ?ҵ????ڵ?ǰ??????????
  indices = which(categories == category)
  
  # ????????????ֵ
  if (length(indices) > 0) {
    # ??ȡ??ǰ??????Yֵ
    category_y = y[indices]
    
    # ?ҵ?????ֵ??????
    max_index = indices[which.max(category_y)]
    
    top_indices<-c(top_indices,max_index) 
  }
}

library(ggsci)
library(ggplot2)
color<-pal_npg("nrc")(3)
"#E64B35FF" "#4DBBD5FF" "#00A087FF" "#3C5488FF"
top_indices <- order(y, decreasing = TRUE)[1:3]
gg <- ggplot() +
  geom_line(aes(x = x, y = y), color = "black",size=1.5) +
  theme_linedraw() +
  theme(panel.grid =element_blank()) +
  xlab('DAS28-CRP') +
  ylab('Number of significant proteins 
       (p < 0.05)')
gg
gg <- gg +
  geom_vline(xintercept = x[top_indices[1]], color = color[1], linetype = "solid", size = 1) +
  geom_vline(xintercept = x[top_indices[2]], color = color[2], linetype = "solid", size = 1) +
  geom_vline(xintercept = x[top_indices[3]], color = color[3], linetype = "solid", size = 1) +
  geom_text(aes(label = round(x[top_indices[1]],2)), x = x[top_indices[1]]-0.2, y = 22, color = color[1], vjust = -0.5) +
  geom_text(aes(label = round(x[top_indices[2]],2)), x = x[top_indices[2]]+0.2, y = 22, color = color[2], vjust = -0.5)+
  geom_text(aes(label = round(x[top_indices[3]],2)), x = x[top_indices[3]]+0.2, y = 22, color = color[3], vjust = -0.5)
gg

topptx(gg ,"RA_DATA1_ACPA+covariates for agegender_20eachside.pptx")
########################Fig4E:Pathway################
data<-read.csv("pathway.csv",header = T)

library(ggplot2);library(reshape)
ggplot(data,aes(x =data$X.1,y =data$Term,size=data$Count,fill=data$LOG10))+
  geom_point(aes(size=data$Count,fill=data$LOG10),color='black',shape = 21)+#閻犱礁澧介悿鍡涙倷閸︻厽鐣卞鍫嗗啰姣堝☉鎾抽叄椤や線鎳??
  scale_fill_gradient2(low="#3C82B9",mid = "white",high = "#EE3434")+#闁煎浜滈悾鐐▕婢跺鐟ら柛娆愶耿椤や線?????
  scale_x_discrete(limits=c("lm","a","b","c"))+
  scale_y_discrete(limits=rev(unique(data$Term)))+
  labs(x="",y="")+
  scale_size_continuous(range=c(4,10))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.grid=element_line(linetype = 2,colour = "grey"),
        panel.background = element_rect(fill = "white",color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("pathway deswan&lm.pdf", width = 8.2, height = 10)

########################Fig4F:Overlap plot of cumulative values across different clinical indicators################
data <- read.csv("JILEI.csv",check.names = F)
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
table(a3$variable)
p1 <- ggplot(data=a3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,12),breaks =c(0,3,6,9,12))+
  scale_x_continuous(limits = c(0,90),breaks =c(0,30,60,90))+
  labs(x='Proteins rank with VAS',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(X3.1="#5A96B5",X3.8="#67867C",X5.0="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p1
ggsave("VAS.pdf",width = 4.2,height = 3)


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
table(b3$variable)
p2 <- ggplot(data=b3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,8),breaks =c(0,2,4,6,8))+
  scale_x_continuous(limits = c(0,55),breaks =c(0,25,50))+
  labs(x='Proteins rank with SJC',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(X3.1="#5A96B5",X3.8="#67867C",X5.0="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p2
ggsave("SJC.pdf",width = 4.2,height = 3)

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
table(c3$variable)
p3 <- ggplot(data=c3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,8),breaks =c(0,2,4,6,8))+
  scale_x_continuous(limits = c(0,42),breaks =c(0,10,20,30,40))+
  labs(x='Proteins rank with TJC',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(X3.1="#5A96B5",X3.8="#67867C",X5.0="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p3
ggsave("TJC.pdf",width = 4.2,height = 3)

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
table(d3$variable)
p4 <- ggplot(data=d3, aes(x=num, y=cum_num,group=variable, color=variable))+
  geom_point(size=1.3,fill='white',shape=21,aes(color=variable),lwd=2)+
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,8),breaks =c(0,2,4,6,8))+
  scale_x_continuous(limits = c(0,100),breaks =c(0,20,40,60,80,100))+
  labs(x='Proteins rank with CRP',y="Number of significant proteins 
  in DE-SWAN cut points")+
  scale_color_manual(values=c(X3.1="#5A96B5",X3.8="#67867C",X5.0="#C00000"))+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white",color="black"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
p4
ggsave("CRP.pdf",width = 4.2,height = 3)




















########################SFig4A:pheatmap#######
data<-data.frame(t(read.csv("drug_pheatmap.csv",row.names = 1)[,c(10:12)]))
sample<-read.csv("drug_pheatmap.csv",row.names = 1)[,-c(10:12)]
names(sample)
anno_col<-sample[,c(4,3,2,1)]
colnames(anno_col)
colnames(anno_col)<-c("Responder","DAS28CRP","Gender","Age")
table(sample$Gender)
ann_colors = list( Responder= c(N="#E9EAF2",Y="#83A7CE"),
                   DAS28CRP= c("disease_remission"="#D3D3D3","low_disease_activity"="#E69999","moderate_disease_activity"="#D16363","high_disease_activity"="#C00000"),
                   Gender=c(Female="#f38181",Male="#84b9cb"),
                   Age=c(">60"="#9E79B5","45-60"="#B79BC8","<45" ="#CEBCDA",D="#E7DDEC")
)
library(pheatmap)

aa=pheatmap(data,cluster_cols = F,cluster_rows =F,
            border=FALSE,cellwidth =2,cellheight =15,fontsize_col = 10,fontsize_row =8,
            color = c(colorRampPalette(colors = c("white","grey60"))(100)),
            annotation_col=anno_col,annotation_colors = ann_colors)

pdf("P_PHEATMAP.pdf", width = 13, height = 13);aa;dev.off()
########################SFig4B:bar plot#############
data<-read.csv("drug_numble.csv")
library(ggplot2)
ggplot(data,aes(x=data$ORD,y=data$COUNT,fill=data$FILL))+
  geom_col()+
  scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  theme_classic()+
  labs(x="Drugs",y="Numble")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("drug_numble.pdf", width = 5, height = 3)
########################SFig4C:bar plot+scatter plot########
MTXLEF %>%
  ggplot(aes(x =Responder.HM,y=as.numeric(DAS28.CRP.HM),colors= Responder.HM)) + 
  geom_boxplot(aes(color=Responder.HM),outlier.colour = NA,width=0.4) +
  geom_jitter(aes(color=Responder.HM),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_color_manual(values =c('Y'='#AF322F','N'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  geom_signif(comparisons =  list(c("N", "Y")),test = "wilcox.test",
              y_position = c(9),map_signif_level =T)+
  ylim(c(0,10))+
  labs(y="DAS28-CRP")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  

MTXLEF %>%
  ggplot(aes(x =Responder.HM,y=as.numeric(DDAS28),colors= Responder.HM)) + 
  geom_boxplot(aes(color=Responder.HM),outlier.colour = NA,width=0.4) +
  geom_jitter(aes(color=Responder.HM),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_color_manual(values =c('Y'='#AF322F','N'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  geom_signif(comparisons =  list(c("N", "Y")),test = "wilcox.test",
              y_position = c(7),map_signif_level =T)+
  ylim(c(-4,8))+
  labs(y="ΔDAS28-CRP")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  

########################SFig4D:bar plot+scatter plot########
MTXHCQ %>%
  ggplot(aes(x =Responder.HM,y=as.numeric(DAS28.CRP.HM),colors= Responder.HM)) + 
  geom_boxplot(aes(color=Responder.HM),outlier.colour = NA,width=0.4) +
  geom_jitter(aes(color=Responder.HM),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_color_manual(values =c('Y'='#AF322F','N'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  geom_signif(comparisons =  list(c("N", "Y")),test = "wilcox.test",
              y_position = c(9),map_signif_level =T)+
  ylim(c(0,10))+
  labs(y="DAS28-CRP")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  

MTXHCQ %>%
  ggplot(aes(x =Responder.HM,y=as.numeric(DDAS28),colors= Responder.HM)) + 
  geom_boxplot(aes(color=Responder.HM),outlier.colour = NA,width=0.4) +
  geom_jitter(aes(color=Responder.HM),stroke =1.5,size=1.2,position = position_jitter(0.2))+
  scale_color_manual(values =c('Y'='#AF322F','N'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  geom_signif(comparisons =  list(c("N", "Y")),test = "wilcox.test",
              y_position = c(7),map_signif_level =T)+
  ylim(c(-4,8))+
  labs(y="ΔDAS28-CRP")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  

########################SFig4E:bar plot########
MTXLEF%>%
  mutate(Class = factor(Class, levels = c("disease_remission","low_disease_activity" ,"moderate_disease_activity","high_disease_activity"))) %>%
  ggplot(aes(x =Class )) + 
  geom_bar(aes(x =Class, fill=Responder),stat="count",position=position_dodge(preserve = 'single',0.8),width = 0.4)+
  scale_fill_manual(values =c('Y'='#AF322F','N'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5)) 
########################SFig4F:bar plot########
MTXHCQ%>%
  mutate(Class = factor(Class, levels = c("disease_remission","low_disease_activity" ,"moderate_disease_activity","high_disease_activity"))) %>%
  ggplot(aes(x =Class )) + 
  geom_bar(aes(x =Class, fill=Responder),stat="count",position=position_dodge(preserve = 'single',0.8),width = 0.4)+
  scale_fill_manual(values =c('Y'='#AF322F','N'='#737AAC'),guide=FALSE)+
  scale_y_continuous(expand = c(0,0)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 1,angle = 45),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5)) 
