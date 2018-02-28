##########################################################################
##########################################################################

# Title: AI Analysis - RiskPrediction
# Description: fitting regression and generate summary reports
## Functions ===
##	getRefList	67
##	get.mdl.strct	76
##	ClassRef	92
##	getClassRef	98
##	log.reg	110
##	coef.proc	132
##	coef.intpt	150
##	coef.df.to.kable	173
##	reg.cat.vis	202
##	reg.age.vis	219
##	reg.para.var.vis	230
##	find.power	258
##	pred.score.fun	271
##	pred.score.dist.vis	283
##	roc.vis	299
##	perf.vis	322
##	perf.sum.vis	347
##	threshold.sel.vis	356
## ===

##########################################################################
##########################################################################

##########################
# Libraries
##########################

library(knitr)
library(kableExtra)
library(ggplot2)
library(ROCR)
library(dplyr)
library(gridExtra)


##########################
# Lookup table
##########################
# measure.lkup<-readRDS("D:/bdtest/AIwR/Data/measure.lkup.RDS")
measure.lkup<-data.frame(
  measure=c("Accuracy","Error Rate","False Positive Rate (Fallout)","True Positive Rate (Recall/Sensitivity)","False Negative Rate (Miss)","True Negative Rate (Specificity)","Positive Predictive Value (Precision)","Negative Predictive Value","Phi/Matthews Correlation Coefficient","Mutual Information","Precision-Recall F Measure","Lift"),
  code=c("acc","err","fpr","tpr","fnr","tnr","ppv","npv","phi","mi","f","lift"),
  stringsAsFactors = F
)



####################################################
####################################################
# Modeling
####################################################
####################################################


##########################
# Get model structure
##########################
# input data should only have integer/numeric or factor as variable type
# input data should only have outcome and rest as predictors

# get all levels for factors
getRefList<-function(x){
  if(class(x)%in%c("factor","character")){
    unique(as.character(x))%>%paste(collapse=", ")%>%return
  }else{
    return("")
  }
}

# get model structure by summarizing mdl.data and hands-on editing
get.mdl.strct<-function(mdl.data){
  mdl.strct<-data.frame(
    Variable=names(mdl.data),
    rename=names(mdl.data),
    VarGrp="",
    VarClass=sapply(mdl.data,class),
    Reference="",
    Ref.list=sapply(mdl.data,getRefList),
    stringsAsFactors = F
  )%>%edit
  mdl.strct<-mdl.strct[,c("Variable","rename","VarGrp","VarClass","Reference")]
  row.names(mdl.strct)<-NULL
  return(mdl.strct)
}


ClassRef<-function(column){
  colclass<-class(column)
  ref<-ifelse(colclass=="factor",levels(column)[1],"")
  return(c(colclass,ref))
}

getClassRef<-function(data){
  classrefdf<-data.frame(t(sapply(data,ClassRef)))
  names(classrefdf)<-c("VarClass","Reference")
  return(classrefdf)
}




##########################
# Regression
##########################
log.reg<-function(mdl.data){
  fml<-as.formula(paste(names(mdl.data)[1],"~."))
  mdl.glm<-glm(fml,family="binomial",data=mdl.data)
  mdl.glm$rsqrt<-mdl.glm
  return(mdl.glm)
}





####################################################
####################################################
# Coefficient summarization
####################################################
####################################################

##########################
# Coefficient table using knitr
##########################

# from Rcoef table (Estimates, Std, etc...) to (OddsRatio, Upper95, Lower95, Pvalue)
coef.proc<-function(coef.df,conf=.95){
  normqtl<-qnorm(conf+(1-conf)/2)
  coef.df<-data.frame(
    Variable=row.names(coef.df),
    OddsRatio=exp(coef.df[,1]),
    Low=exp(coef.df[,1]-coef.df[,2]*normqtl),
    Up=exp(coef.df[,1]+coef.df[,2]*normqtl),
    Pvalue=coef.df[,4],
    Sig=pval.cut(coef.df[,4]),
    stringsAsFactors = F
  )
  row.names(coef.df)<-NULL
  names(coef.df)[3:4]<-paste(names(coef.df)[3:4],round(conf*100),sep="")
  coef.df<-coef.df[-1,]
  return(coef.df)
}

# adding additional columns according to model structure
coef.intpt<-function(coef.df,mdl.data,mdl.strct){
  fct.row<-which(mdl.strct$VarClass=="factor")
  fct.list<-lapply(fct.row,function(r){
    var.org<-mdl.strct$rename[r]
    data.frame(
      Var.org=var.org,
      VarNCat=paste(mdl.strct$rename[r],as.character(unique(mdl.data[,var.org])),sep=""),
      Cat=as.character(unique(mdl.data[,var.org])),
      stringsAsFactors=F
    )
  })
  fct.lkup<-do.call(rbind.data.frame,fct.list)
  coef.df<-left_join(coef.df,fct.lkup,by=c("Variable"="VarNCat"))
  nfct.row<-is.na(coef.df$Var.org)
  coef.df$Var.org[nfct.row]<-coef.df$Variable[nfct.row]
  coef.df$Var.coef<-coef.df$Variable
  coef.df$Variable[!nfct.row]<-coef.df$Cat[!nfct.row]
  coef.df<-left_join(coef.df,mdl.strct[,c(2,3,5)],by=c("Var.org"="rename"))
  coef.df$color.cd<-as.numeric(as.factor(coef.df$VarGrp))
  return(coef.df)
}

# presenting coefficient table using knitr. Modifications include: header, grouping categorical variables, highlighting significant variables
coef.df.to.kable<-function(coef.df){
  coef.kable<-coef.df%>%
    mutate(
      Variable=cell_spec(Variable, bold=is.na(Cat))
    )%>%select(Variable,OddsRatio,Low95,Up95,Pvalue,Sig)%>%
    kable("html",escape = F,digits=c(0,2,2,2,4,0),align=c("l","r","r","r","r","r"),row.names = F)%>%
    kable_styling(c("striped","hover","condensed","responsive"),full_width=F)%>%
    row_spec(which(coef.df$Pvalue<.05),color = "#1E90FF")%>%
    row_spec(0,color = "white", background = "#1E90FF")
  #coef.kable
  # step 2: grouping for factors
  fct.var<-mdl.strct$rename[mdl.strct$VarClass=="factor"]
  if(length(fct.var)>0){
    for(v in fct.var){
      v.row<-which(coef.df$Var.org==v)
      coef.kable<-coef.kable%>%
        group_rows(v,v.row[1],tail(v.row,1))
    }
  }
  return(coef.kable) 
}



##########################
# Coefficient visualization
##########################

# Categorical variable visualization
reg.cat.vis<-function(var,coef.proc.df,trend=T){
  coef.df<-coef.proc.df[coef.proc.df$Var.org==var,]
  coef.df$Variable<-factor(coef.df$Variable,levels=coef.df$Variable)
  gg<-ggplot(data=coef.df,aes(x=Variable,y=OddsRatio))+
    geom_bar(stat="identity",color="gray",width =.4,alpha=.8,fill="dodgerblue4")+
    geom_errorbar(aes(ymin=Low95, ymax=Up95),size=.5,colour="deeppink",width=.2)+
    geom_line(aes(x=1:nrow(coef.df),y=OddsRatio),colour="deeppink")+
    xlab(var)+ylab("Odds Ratio")+ggtitle(var)+
    theme(legend.position = "bottom",
          axis.ticks.x=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  if(trend)gg<-gg+stat_smooth(aes(x = as.numeric(Variable), y = OddsRatio), method = "lm", se = FALSE,colour="#838B8B")
  return(gg)
}

# Age effect visualization
reg.age.vis<-function(age.effect){
  names(age.effect)<-c("Age","Coefficient")
  coef.min<-min(predict(loess(Coefficient~Age,df,span = .9), age.effect$Age))
  df$Coefficient<-df$Coefficient-coef.min
  gg<-ggplot(df,aes(x=Age,y=Coefficient))+geom_point()+stat_smooth(span = 0.9)
  return(gg)
}

# Parallel binary variable effect visualization
#coef.df<-coef.proc.df[coef.proc.df$VarGrp=="Comorbidity & Complication",]
#coef.df<-coef.df[rev(order(coef.df$Variable)),]
reg.para.var.vis<-function(coef.df,title){
  coef.df$Variable<-factor(coef.df$Variable,levels=coef.df$Variable)
  coef.df$Effect<-"Negative"
  coef.df$Effect[coef.df$OddsRatio>1]<-"Positive"
  gg<-ggplot(data=coef.df)+
    geom_segment(aes(x=Variable,xend=Variable,y=1, yend=OddsRatio,color=Effect), size=8)+
    #geom_bar(mapping=aes(x=Variable,y=OddsRatio),stat="identity",fill="steelblue")+
    geom_point(aes(x=Variable,y=OddsRatio),size=1,colour="deeppink")+
    geom_errorbar(aes(x=Variable,ymin=Low95, ymax=Up95),size=.6,colour="deeppink",width=.4)+
    coord_flip()+ylab("Odds Ratio")+geom_hline(yintercept = 1,size=.8,color="Gray30")+
    ggtitle(title)+xlab("")+
    theme(legend.position = "bottom",
          axis.ticks=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
  return(gg)
}




####################################################
####################################################
# Model Validation
####################################################
####################################################

# find power to most segregate [0,1] variables
find.power<-function(x,plot=F){
  var.seq<-sapply(1:99,function(a)sd(x^(a/100),na.rm = T))
  pwr<-which.max(var.seq)/100
  if(plot){
    print(ggplot()+geom_line(aes(x=seq(.01,.99,.01),y=var.seq),color="steelblue",size=.8)+
            geom_text(aes(x=pwr,y=quantile(var.seq,.05),label=paste("Power at max variance:",pwr)),color="#EE3A8C",angle=90,vjust=1)+
            geom_vline(xintercept = pwr,colour="#EE3A8C")+
            xlab("Power")+ylab("Standard Deviation"))
  }
  return(pwr)
}

# prediction score functions
pred.score.fun<-function(mdl.glm,mdl.data){
  pred<-predict(mdl.glm,newdata = mdl.data,allow.new.levels=T,type = "response")
  pwr<-find.power(pred)
  mdl.pred<-data.frame(
    prediction=pred,
    pred.scale=pred^pwr,
    outcome=sapply(mdl.data[,1],function(x)ifelse(x,"Dead","Alive"))
  )
  return(mdl.pred)
}

# visualizing prediction score segregation
pred.score.dist.vis<-function(mdl.pred){
  gg<-ggplot(mdl.pred, aes(x=pred.scale, color=outcome, fill=outcome)) + 
    geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
    geom_density(alpha=.2)+xlim(0,1)+
    labs(title="Predition score by outcome",x="Scaled prediction",y="Density",
         caption = "*The higher segregated the distributions, the better the predictability")+
    theme(legend.position = "bottom",
          axis.ticks=element_blank(),
          legend.title = element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5)) 
  return(gg)
}


# Calculating and visualizing RoC
roc.vis<-function(mdl.pred){
  pred<-ROCR::prediction(mdl.pred$prediction,mdl.pred$outcome)
  roc.df<-unique(data.frame(
    fpr=round(pred@fp[[1]]/max(pred@fp[[1]])/2,3)*2,
    tpr=round(pred@tp[[1]]/max(pred@tp[[1]])/2,3)*2
  ))
  auc<-ROCR::performance(pred, measure = "auc")@y.values[[1]]
  auc.text<-paste("Area Under Curve (AUC) = ",sprintf("%.02f",auc*100),"%",sep="")
  gg<-ggplot(roc.df,aes(fpr,tpr))+geom_step(size = 1,colour="steelblue", alpha = 0.7)+
    geom_abline(slope = 1,intercept = 0,size = 1,colour="steelblue", alpha = 0.7)+
    geom_text(aes(x=.5,y=.5,label=auc.text),colour="#EE3A8C",angle=45,vjust=-.5,hjust=.5)+
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)")+coord_equal()+
    theme(legend.position = "bottom",
          axis.ticks=element_blank(),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 20, face = "bold",hjust = 0.5),
          panel.border = element_rect(colour = "steelblue", fill=NA))
  return(gg)
}

# Performance visualization, measure see measure.lkup
perf.vis<-function(mdl.pred,measure){
  ind<-which(measure.lkup$code==measure)
  title<-measure.lkup$measure[ind]
  pred<-ROCR::prediction(predictions=mdl.pred$prediction,labels=mdl.pred$outcome)
  msr.result<-ROCR::performance(pred, measure)
  msr.df<-unique(data.frame(
    x=round(msr.result@x.values[[1]],3),
    y=round(msr.result@y.values[[1]],3)
  ))
  ypos<-range(msr.df$y,na.rm=T)%*%c(.7,.3)
  xsel<-mean(msr.df$x[which.max(msr.df$y)])
  ggplot(data=msr.df,aes(x=x,y=y))+
    geom_step(size=1,colour="steelblue")+
    geom_text(aes(x=xsel,y=ypos,label=xsel),color="#EE3A8C",angle=90,vjust=1)+
    geom_vline(xintercept = xsel,colour="#EE3A8C",size=1)+
    labs(title=title,x="",y="")+
    theme(legend.position = "bottom",
          axis.ticks=element_blank(),
          axis.title = element_text(size = 8, face = "bold"),
          plot.title = element_text(size = 12, face = "bold",hjust = 0.5),
          panel.border = element_rect(colour = "steelblue", fill=NA),
          aspect.ratio = 1)
}

# Performance summary on Accuracy, Error, FPR, TPR, FNR, TNR, PPV, NPV
perf.sum.vis<-function(mdl.pred){
  cd<-measure.lkup$code[1:8]
  glist<-lapply(cd,function(x)perf.vis(mdl.pred,x))
  names(glist)<-cd
  grid.arrange(arrangeGrob(grobs=glist,nrow=2,ncol=4,width=1300))
}


# Threshold selection
threshold.sel.vis<-function(mdl.pred){
  cd<-measure.lkup$code[9:11]
  glist<-lapply(cd,function(x)perf.vis(mdl.pred,x))
  names(glist)<-cd
  grid.arrange(arrangeGrob(grobs=glist,nrow=1,ncol=3))
}







