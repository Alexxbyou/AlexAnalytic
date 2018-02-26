
##########################
# Get model structure
##########################
# input data should only have integer/numeric or factor as variable type
# input data should only have outcome and rest as predictors

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
# Coefficient process function
##########################




##########################
# Knitr function
##########################

# model structure: 1.variable rename, 2. variable grouping
#mdl.strct<-data.frame(
#  Variable=names(mdl.data),
#  rename=c("Death","Age","Gender","Race","Marital","MediFund","DiabetesYear","PatType",names(mdl.data)[9:18],"MajorAmputation","MinorAmputation","BMI",names(mdl.data)[22:27]),
#  VarGrp=c("Outcome",rep("Demographics",4),rep("Medical History",3),rep("Comorbidity & Complication",12),rep("Physiological Measure",4),rep("Score",3)),
#  stringsAsFactors=F
#)

#mdl.strct<-cbind(mdl.strct,getClassRef(mdl.data))

#names(mdl.data)<-mdl.strct$rename

log.reg<-function(mdl.data){
  fml<-as.formula(paste(names(mdl.data)[1],"~."))
  mdl.glm<-glm(fml,family="binomial",data=mdl.data)
  mdl.glm$rsqrt<-mdl.glm
  return(mdl.glm)
}

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
# Visualization
##########################
#<-function(coef.proc.df)



