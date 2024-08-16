########################Calculation  of linearly correlated proteins with clinical indicators#######
data<-data[which((data$Group2=="ACPA+ RA")),]

{ALL<-data
  P_DAS28<-rep(NA,ncol(ALL)-15)
  P_VAS<-rep(NA,ncol(ALL)-15)
  P_SJC<-rep(NA,ncol(ALL)-15)
  P_TJC<-rep(NA,ncol(ALL)-15)
  P_CRP<-rep(NA,ncol(ALL)-15)
  LM_DAS28<-rep(NA,ncol(ALL)-15)
  LM_VAS<-rep(NA,ncol(ALL)-15)
  LM_SJC<-rep(NA,ncol(ALL)-15)
  LM_TJC<-rep(NA,ncol(ALL)-15)
  LM_CRP<-rep(NA,ncol(ALL)-15)
  P_DAS28_g<-rep(NA,ncol(ALL)-15)
  P_VAS_g<-rep(NA,ncol(ALL)-15)
  P_SJC_g<-rep(NA,ncol(ALL)-15)
  P_TJC_g<-rep(NA,ncol(ALL)-15)
  P_CRP_g<-rep(NA,ncol(ALL)-15)
  LM_DAS28_g<-rep(NA,ncol(ALL)-15)
  LM_VAS_g<-rep(NA,ncol(ALL)-15)
  LM_SJC_g<-rep(NA,ncol(ALL)-15)
  LM_TJC_g<-rep(NA,ncol(ALL)-15)
  LM_CRP_g<-rep(NA,ncol(ALL)-15)
  P_DAS28_a<-rep(NA,ncol(ALL)-15)
  P_VAS_a<-rep(NA,ncol(ALL)-15)
  P_SJC_a<-rep(NA,ncol(ALL)-15)
  P_TJC_a<-rep(NA,ncol(ALL)-15)
  P_CRP_a<-rep(NA,ncol(ALL)-15)
  LM_DAS28_a<-rep(NA,ncol(ALL)-15)
  LM_VAS_a<-rep(NA,ncol(ALL)-15)
  LM_SJC_a<-rep(NA,ncol(ALL)-15)
  LM_TJC_a<-rep(NA,ncol(ALL)-15)
  LM_CRP_a<-rep(NA,ncol(ALL)-15)
  i=1
  for(i in 1:(ncol(ALL)-15))try({
    P_DAS28[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_DAS28[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_VAS[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_VAS[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_SJC[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_SJC[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_TJC[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_TJC[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_CRP[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_CRP[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_DAS28_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_DAS28_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_VAS_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_VAS_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_SJC_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_SJC_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_TJC_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_TJC_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_CRP_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_CRP_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_DAS28_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_DAS28_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_VAS_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_VAS_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_SJC_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_SJC_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_TJC_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_TJC_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_CRP_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_CRP_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
  })
  
  test<-data.frame(colnames(ALL)[-c(1:15)],P_DAS28, P_VAS,P_SJC,P_TJC,P_CRP,
                   LM_DAS28,LM_VAS,LM_SJC,LM_TJC,LM_CRP,
                   P_DAS28_g,P_VAS_g,P_SJC_g,P_TJC_g,P_CRP_g,
                   LM_DAS28_g,LM_VAS_g,LM_SJC_g,LM_TJC_g,LM_CRP_g,
                   P_DAS28_a,P_VAS_a,P_SJC_a,P_TJC_a,P_CRP_a,
                   LM_DAS28_a,LM_VAS_a,LM_SJC_a,LM_TJC_a,LM_CRP_a)
}
write.csv(test,'DAS28_COR_TEST_ACPA+ RA.csv')

data<-data[which((data$Group2=="ACPA- RA")),]

{ALL<-data
  P_DAS28<-rep(NA,ncol(ALL)-15)
  P_VAS<-rep(NA,ncol(ALL)-15)
  P_SJC<-rep(NA,ncol(ALL)-15)
  P_TJC<-rep(NA,ncol(ALL)-15)
  P_CRP<-rep(NA,ncol(ALL)-15)
  LM_DAS28<-rep(NA,ncol(ALL)-15)
  LM_VAS<-rep(NA,ncol(ALL)-15)
  LM_SJC<-rep(NA,ncol(ALL)-15)
  LM_TJC<-rep(NA,ncol(ALL)-15)
  LM_CRP<-rep(NA,ncol(ALL)-15)
  P_DAS28_g<-rep(NA,ncol(ALL)-15)
  P_VAS_g<-rep(NA,ncol(ALL)-15)
  P_SJC_g<-rep(NA,ncol(ALL)-15)
  P_TJC_g<-rep(NA,ncol(ALL)-15)
  P_CRP_g<-rep(NA,ncol(ALL)-15)
  LM_DAS28_g<-rep(NA,ncol(ALL)-15)
  LM_VAS_g<-rep(NA,ncol(ALL)-15)
  LM_SJC_g<-rep(NA,ncol(ALL)-15)
  LM_TJC_g<-rep(NA,ncol(ALL)-15)
  LM_CRP_g<-rep(NA,ncol(ALL)-15)
  P_DAS28_a<-rep(NA,ncol(ALL)-15)
  P_VAS_a<-rep(NA,ncol(ALL)-15)
  P_SJC_a<-rep(NA,ncol(ALL)-15)
  P_TJC_a<-rep(NA,ncol(ALL)-15)
  P_CRP_a<-rep(NA,ncol(ALL)-15)
  LM_DAS28_a<-rep(NA,ncol(ALL)-15)
  LM_VAS_a<-rep(NA,ncol(ALL)-15)
  LM_SJC_a<-rep(NA,ncol(ALL)-15)
  LM_TJC_a<-rep(NA,ncol(ALL)-15)
  LM_CRP_a<-rep(NA,ncol(ALL)-15)
  i=1
  for(i in 1:(ncol(ALL)-15))try({
    P_DAS28[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_DAS28[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_VAS[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_VAS[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_SJC[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_SJC[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_TJC[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_TJC[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_CRP[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][2]%>% as.numeric()
    LM_CRP[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[2]
    P_DAS28_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_DAS28_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_VAS_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_VAS_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_SJC_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_SJC_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_TJC_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_TJC_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_CRP_a[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][3]%>% as.numeric()
    LM_CRP_a[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[3]
    P_DAS28_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_DAS28_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$DAS28.CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_VAS_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_VAS_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$VAS)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_SJC_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_SJC_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$SJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_TJC_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_TJC_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$TJC)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
    P_CRP_g[i] <- coef(summary(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL)))[,4][4]%>% as.numeric()
    LM_CRP_g[i] <-coefficients(lm(scale(ALL[,(i+15)])~as.numeric(ALL$CRP)+as.numeric(ALL$Age)+ALL$Gender,ALL))[4]
  })
  
  test<-data.frame(colnames(ALL)[-c(1:15)],P_DAS28, P_VAS,P_SJC,P_TJC,P_CRP,
                   LM_DAS28,LM_VAS,LM_SJC,LM_TJC,LM_CRP,
                   P_DAS28_g,P_VAS_g,P_SJC_g,P_TJC_g,P_CRP_g,
                   LM_DAS28_g,LM_VAS_g,LM_SJC_g,LM_TJC_g,LM_CRP_g,
                   P_DAS28_a,P_VAS_a,P_SJC_a,P_TJC_a,P_CRP_a,
                   LM_DAS28_a,LM_VAS_a,LM_SJC_a,LM_TJC_a,LM_CRP_a)
}
write.csv(test,'DAS28_COR_TEST_ACPA- RA.csv')
########################Fig3A:DAS28 in different gender(ACPA+ RA)#######################
data<-read.csv("RA_DATA1.csv")
data<-data[which(data$Group2=="ACPA+ RA"),]
ggplot(data,aes(x =data$Gender,y=data$DAS28.CRP)) + 
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
ggsave("das28-gender-ACPA+ RA.pdf", width = 2.5, height = 3)

########################Fig3B:DAS28 in different gender(ACPA- RA)###########
data<-read.csv("RA_DATA1.csv")
data<-data[which(data$Group2=="ACPA- RA"),]
ggplot(data,aes(x =data$Gender,y=data$DAS28.CRP)) + 
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
ggsave("das28-gender-ACPA- RA.pdf", width = 2.5, height = 3)
########################Fig3C:DAS28 in different age in female(ACPA+ RA)##########################
ggplot(data,aes(x =as.numeric(Age),y=DAS28.CRP,color=Gender)) + 
  geom_point(aes(color=Gender),)+
  geom_smooth(aes(color=Gender),method = "lm",size=1.5,se = T)+
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
ggsave("das28 age cor gender——ACPA+ RA.pdf", width = 3, height = 3)  
########################Fig3D:DAS28 in different age in female(ACPA- RA)#####
ggplot(data,aes(x =as.numeric(Age),y=DAS28.CRP,color=Gender)) + 
  geom_point(aes(color=Gender),)+
  geom_smooth(aes(color=Gender),method = "lm",size=1.5,se = T)+
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
ggsave("das28 age cor gender——ACPA- RA.pdf", width = 3, height = 3)
########################Fig3E:DESWAN in female####################
DEswanage=function(data.df, qt, covariates){
  
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
  
  window.center <- seq(31, 77, by = 2)
  
  for (k in 1:length(window.center)) {
    pvalues <- NULL
    coefficients <- NULL
    idx_below <- which(qt <= window.center[k]) 
    idx_above <- which(qt > window.center[k])
    
    if (length(idx_below) < 3 || length(idx_above) < 3) {
      next 
    }
    
    if (length(idx_below) < 4 || length(idx_above) < 4) {
      idx_below <- tail(sort(idx_below), 3)
      idx_above <- head(sort(idx_above), 3)
    } else if (length(idx_below) < 5 || length(idx_above) < 5) {
      idx_below <- tail(sort(idx_below), 4)
      idx_above <- head(sort(idx_above), 4)
    } else {
      idx_below <- tail(sort(idx_below), 5)  # Take the 5 closest and smaller or equal samples to window.center
      idx_above <- head(sort(idx_above), 5)  # Take the 5 closest and greater samples to window.center
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
data<-'expression dataframe with age at column1 in female'
res.DEswanage <- DEswanage(
  data = data[,-1]
  qt = data[,'age']
)

age.DEswan.wide.p=reshape.DEswan(res.DEswanage,parameter = 1,factor = "qt")
age.DEswan.wide.q=q.DEswan(age.DEswan.wide.p,method="BH")
age.DEswan.wide.coeff=reshape.DEswan(res.DEswanage,parameter = 2,factor = "qt")
toHeatmap=sign(age.DEswan.wide.coeff[,-1])*-log10(age.DEswan.wide.p[,-1])
rownames(toHeatmap)<-age.DEswan.wide.coeff[,1]
age.DEswan.wide.p.signif=nsignif.DEswan(age.DEswan.wide.p)
age.DEswan.wide.q.signif=nsignif.DEswan(age.DEswan.wide.q)
toPlot=age.DEswan.wide.p.signif[1:3,]
toPlotq=age.DEswan.wide.q.signif[1:3,]
########################Fig3F:4 clinicalin different age in female##############
library(reshape)
data<-read.csv("RA_DATA1.csv")
data<-data[which((data$Group2=="ACPA+ RA")),]
data1<-data[which(data$Gender=="Female"),]
data1$age2[(data1$Age>45)]<-"Female>45"
data1$age2[(data1$Age<45)|(data1$Age==45)]<-"Female<45"
data1<-data1[which(data1$age2=="Female>45"|data1$age2=="Female<45"),]
data2<-data1[,c("DAS28.CRP","VAS","TJC","SJC","CRP","age2")]
data3<-melt(data2, measure.vars = c("DAS28.CRP","VAS","TJC","SJC","CRP"))


ggplot(data3,aes(x =age2,y=value)) + 
  geom_violin(aes(fill=age2),width=0.8)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.2,fill="white") +
  #geom_jitter(aes(color=group2),stroke =1.5,size=0.8,position = position_jitter(0.3))+
  scale_fill_manual(values =c('Female<45'='#eeabab','Female>45'='#d86967'),guide=FALSE)+
  geom_signif(comparisons =  list(c("Female>45", "Female<45")),
              test = "wilcox.test",map_signif_level =T, step_increase = 0.1,na.rm =T, show.legend = NA)+
  #scale_y_continuous(expand = c(0,0)) + 
  labs(x="",y="")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =1,angle = 45),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~ variable, ncol =5,scales="free")
ggsave("clinal 5 female acpa+ra.pdf", width = 12, height = 4)
########################Fig3G:linearly correlated proteins counts with clinical indicators##############
DATA<-read.csv("DAS28_COR_TEST_ACPA+ RA.csv")
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
ggsave("5 clinical cor count ACPA+ RA.pdf", width =4, height = 2.8)
########################Fig3H:linearly correlated proteins with DAS28-CRP###########
test<-read.csv("DAS28_COR_TEST_ACPA+ RA.csv")
library(ggplot2)
library(ggrepel)

ggplot(test,aes(x =LM_DAS28,y =-log10(P_DAS28),fill=-log10(P_DAS28)*sign(LM_DAS28)))+
  geom_point(aes(fill=-log10(P_DAS28)*sign(LM_DAS28),size=abs(-log10(P_DAS28))),shape=21,color='black',)+
  scale_fill_gradient2(low ="#4569bb",mid ="white",high ="#DD5B51",guide = FALSE)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  scale_size_continuous(range=c(0,10),guide = FALSE)+
  labs(x="",y="")+
  xlim(-0.4,0.4)+
  geom_text_repel(aes(label =test$colnames.ALL...c.1.15..), size = 3,show.legend = F)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 
ggsave("das28-protein cor-das28 ACPA+ RA.pdf", width = 3, height = 3)


########################Fig3I:4 Clinical scatter plots################
data<-read.csv("DAS28 ACPA+/DAS28_COR_TEST_ACPA+ RA.csv",header = T)[,c(2,4:7,9:12)]
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
p<-
  ggplot()+
  geom_col(data = col_1,aes(x = Groups, y =max..log10.PValue..+expan),width =0.7,
           fill="#CCCCCC",alpha=0.4,show.legend = F)+
  geom_jitter(data = df3,aes(x = Groups, y = -log10(PValue), color = color_pre),
              size= 2,width =0.35,show.legend = T)+
  #geom_text_repel(data = df3,aes(x = Groups, y = -log10(PValue),label =df3$X), size = 3,show.legend = F)+
  scale_color_manual(name=NULL,values =alpha(c("#737AAC","grey","#AF322F"),0.5))+
  geom_bar(data=dfbar,aes(x=groups,y=y+0.75,fill=groups), levels = c("VAS","TJC","SJC","CRP"),stat ="identity",width = 0.8)+
  geom_bar(data=dfbar,aes(x=groups,y=y-0.75,fill=groups), levels = c("VAS","TJC","SJC","CRP"),stat ="identity",width = 0.8)+
  scale_fill_manual(name="Groups",values=alpha(mycolor,1))+
  labs(x="",y="-Log10(p value)")+
  geom_text(data=dfbar,aes(x=x,y=y,label=x),size=4,color="white")+
  scale_x_discrete(limit=c("VAS","TJC","SJC","CRP"))+
  scale_y_continuous(limits = c(-0.75,13), breaks= c(0,4,8,12), expand= expansion(add = 0))+
  theme_classic()+
  theme(axis.line.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.text.x = element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),
        axis.line.y = element_line(linetype=1,color="black",size=0.75),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))

ggsave("4clinic cor acpa+ RA.pdf", width = 7.3, height = 3)

########################Fig3J:11 protein level in different age in female#########################
data<-read.csv("RA_DATA1.csv",header = T,row.names = 1)

GENE=11_GENE
data<-data[which((data$Group2=="ACPA+ RA"|data$Group2=="ACPA- RA")),]
data$age2[(data$Age>45)&(data$Gender=="Female")]<-"Female>45"
data$age2[((data$Age<45)|(data$Age==45))&(data$Gender=="Female")]<-"Female<45"
d=data[,colnames(data)%in%c(GENE,"Gender","DAS28.CRP","age2")]
names(d)
d=d[,c(1,14,2,3,4,11,12,7,5,9,6,10,13,8)]
d2<-reshape::melt(d,id=names(d)[1:3])


d3<-d2[which(d2$age2!="NA"),]
A<-ggplot(d3,aes(x =age2,y=log2(value))) + 
  #geom_violin(aes(fill=group2),width=0.5)+
  geom_boxplot(color="black",outlier.colour = NA,width=0.3,fill="white") +
  geom_jitter(aes(color=age2),stroke =1.5,size=1,position = position_jitter(0.3))+
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
        axis.text.x= element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~ variable, ncol =11,scales="free")
#ggsave("FIG3/11GENE-1.pdf", width = 18, height = 3)
A

B<-ggplot(d2,aes(x=log2(d2$value),y=d2$DAS28.CRP))+
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
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +  
  facet_wrap( ~ variable, ncol =11,scales="free")

C<-ggarrange(A,B,nrow=2,legend = NULL,heights = c(0.55,0.45))
C
ggsave("FIG3/11GENE-1.pdf", width = 18, height = 5)

########################SFig3:plot of disease activity levels in different groups############
{ data<-read.csv("RA_DATA1.csv")
data<-data[which(data$Group2=="ACPA- RA"),]

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
write.csv(test,"FIG4/disease activity class test ACPA- RA.csv")
}

data<-read.csv("disease activity class test ACPA- RA.csv",row.names = 1)
data1<-data[which(data$P_DL<0.05|data$P_LM<0.05|data$P_MH<0.05|data$P_LH<0.05|data$P_DM<0.05|data$P_DH<0.05),]
data2<-data1[,c(15:18)]
data_s<-data.frame(round(t(apply(data2, 1, scale)),2))
colnames(data_s)<-c("Disease remission","Low disease activity","Moderate disease activity","High disease activity")
row.names(data_s)<-row.names(data1)
bk <- c(seq(-1,0,by=0.01),seq(0.01,1,by=0.01))
p <-pheatmap(data_s,scale='none',show_rownames = F, cluster_cols = F,clustering_method = "ward",border_color = NA,cutree_rows=6, #c(3,6,9,12,15,15,21),
             color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 1,
             breaks=bk)
pdf("P_PHEATMAP_ACPA- RA.pdf", width = 13, height = 13);p;dev.off()
newOrder = data_s[p$tree_row$order,]
p1 <-pheatmap(newOrder[,c(1:4)],scale='none',show_rownames = T, cluster_cols = F,cluster_rows  = F,clustering_method = "ward.D2",border_color = NA,#gaps_row =  c(3,6,9,12,15,15,21),
              color = c(colorRampPalette(colors = c("#3C82B9","white"))(length(bk)/2),colorRampPalette(colors = c("white","#EE3434"))(length(bk)/2)),cellwidth = 25,cellheight = 2,
              breaks=bk)

pdf("P_PHEATMAP2_ACPA- RA.pdf", width = 13, height = 13);p1;dev.off()
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
  write.csv(newOrder,"ALL-cluster8 ACPA_ RA.csv")#瀵煎嚭鏁版嵁鏀瑰悕
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
    scale_color_manual(values = c(cluster6="#638759",cluster4="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster2="#A1B4D8"),guide=FALSE)+
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
    scale_color_manual(values = c(cluster6="#638759",cluster4="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster2="#A1B4D8"),guide=FALSE)+
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
    scale_color_manual(values = c(cluster6="#638759",cluster4="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster2="#A1B4D8"),guide=FALSE)+
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
    scale_color_manual(values = c(cluster6="#638759",cluster4="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster2="#A1B4D8"),guide=FALSE)+
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
    scale_color_manual(values = c(cluster6="#638759",cluster4="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster2="#A1B4D8"),guide=FALSE)+
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
    scale_color_manual(values = c(cluster6="#638759",cluster4="#C1AFCC",cluster3="#eca75e",cluster5="#D8ACAC",cluster2="#A1B4D8"),guide=FALSE)+
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
ggsave("disease activity class cluster6_ACPA- RA.pdf", width = 2.5, height = 25)

