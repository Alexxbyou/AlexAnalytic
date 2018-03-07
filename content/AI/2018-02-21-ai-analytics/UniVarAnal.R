##########################################################################
##########################################################################

# Title: AI Analysis - Visualization
# Description: Univariate visualization & Comparison


##########################################################################
##########################################################################
library(knitr)
library(kableExtra)
options("ReporteRs-font")

##########################
# Age processing
##########################

cutoff2label<-function(cutoff){
  np<-length(cutoff)
  result<-c(
    paste(cutoff[1:(np-2)],"-",cutoff[2:(np-1)]-1,sep=""),
    paste(cutoff[np-1],"+",sep="")
  )
  return(result)
}

age.cut<-function(age,cutoff=c(0,15+0:7*10)){
  cutoff<-c(0,cutoff,999)%>%
    unique()%>%
    sort()
  age.label<-cutoff2label(cutoff)
  
  cut(age,breaks=cutoff,labels=age.label)
}

pval.cut<-function(pval){
  cut(pval,c(-1,.001,.01,.05,.1,1),c("***","**","*",".",""))
}

##########################
# Age Visualization
##########################

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




coef.beaut<-function(coef.df){
  for(c in 2:4){
    coef.df[,c]<-sprintf("%.2f",coef.df[,c])
  }
  coef.df[,5]<-sprintf("%.4f",coef.df[,5])
  return(coef.df)
}

##########################
# Univariate analysis
##########################

reg.anl<-function(
  data, #data.frame(var,group)
  beautify=F
){
  varname<-names(data)[1]
  data[,2]<-as.factor(data[,2])
  fml<-as.formula(paste(names(data)[2:1],collapse="~"))
  mdl<-glm(formula=fml,data=data,family="binomial")
  result<-coef.proc(summary(mdl)$coef)
  row.names(result)<-NULL
  if(beautify)result<-coef.beaut(result)
  return(result)
}


##########################
# Table formating
##########################

coef.temp.func<-function(data,colpattern){
  reg.anl(data[,getcolumn(colpattern,data)])%>%
    kable("html")%>%
    kable_styling("striped", full_width = F)
}


