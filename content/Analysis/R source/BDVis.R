##########################################################################
##########################################################################

# Customize ggplot2

##########################################################################
##########################################################################

require(ggplot2)
require(dplyr)


##########################
# Age Visualization
##########################



cutoff2label<-function(cutoff){
  np<-length(cutoff)
  result<-c(
    paste(cutoff[1:(np-2)],"-",cutoff[2:(np-1)]-1,sep=""),
    paste(cutoff[np-1],"+",sep="")
  )
  return(result)
}

elderly.group<-function(cutoff,elderly){
  ngrp<-length(cutoff)-1
  first.elderly<-ceiling(mean(which(sort(c(cutoff,elderly))==elderly)))-1
  result<-c(
    rep("No",first.elderly-1),
    rep("Yes",ngrp-first.elderly+1)
  )
  result<-factor(result,levels=c("Yes","No"))
  return(result)
}

age.vis<-function(
  age,
  cutoff=c(0,15+0:7*10),
  elderly=65,
  Title="Age"
){
  cutoff<-c(0,cutoff,999)%>%
    unique()%>%
    sort()
  age.label<-cutoff2label(cutoff)
  
  age.group<-age%>%
    cut(breaks=cutoff,labels=age.label)%>%
    table()%>%
    as.data.frame()
  
  names(age.group)<-c("Age","Count")
  age.group$Elderly<-elderly.group(cutoff,elderly)
  
  ggplot()+
    geom_bar(data=age.group,mapping=aes(x=Age,y=Count,fill=Elderly),stat="identity")+
    ggtitle(Title)+
    theme(legend.position = "bottom"
          ,plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
}


age.vis.comp<-function(
  age.n.grp,   # data.frame(age,group)
  cutoff=c(0,15+0:7*10),
  elderly=65,
  Title="Age"
){
  cutoff<-c(0,cutoff,999)%>%
    unique()%>%
    sort()
  age.label<-cutoff2label(cutoff)
  
  age.group.list<-lapply(unique(age.n.grp$group),function(grp){
    age<-age.n.grp$age[age.n.grp$group==grp]
    age.group<-age%>%
      cut(breaks=cutoff,labels=age.label)%>%
      table()%>%
      as.data.frame()
    names(age.group)<-c("Age","Count")
    age.group$Elderly<-elderly.group(cutoff,elderly)
    age.group$Group<-grp
    return(age.group)
  })
  age.group.sum<-do.call(rbind.data.frame,age.group.list)
  age.group.sum$Group<-factor(age.group.sum$Group)
  
  ggplot()+
    geom_bar(data=age.group.sum,mapping=aes(x=Age,y=Count,group=Group,fill=Group,color=Elderly),stat="identity",position="dodge")+
    ggtitle(Title)+
    theme(legend.position = "bottom"
          ,plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
}


##########################
# Gender/Race/oth. Categorical Variables Visualization
##########################
cat.vis.donut<-function(
  data, #data.frame(Category,Count)
  order=c("default","alphabet","desc","asc"),
  Title="Gender"
){
  names(data)<-c("Category","Count")
  data$Perc<-data$Count/sum(data$Count)
  if(order=="default")
    data$Category<-factor(data$Category,levels=rev(data$Category))
  if(order=="alphabet")
    data$Category<-factor(data$Category,levels=rev(sort(as.character(data$Category))))
  if(order=="desc"){
    lvl<-data$Category[order(data$Count)]
    data$Category<-factor(data$Category,levels=lvl)
  }
  if(order=="asc"){
    lvl<-data$Category[order(-data$Count)]
    data$Category<-factor(data$Category,levels=lvl)
  }
  data$label.perc<-paste(sprintf("%.2f",data$Perc*100),"%",sep="")
  
  ggplot() +
    geom_bar(data=data, mapping=aes(x=3.5,y=Perc,fill=Category),stat="identity",colour="grey30") +
    geom_text(data=data,mapping=aes(x=3.5,y=Perc,label=label.perc,vjust=.1))+
    coord_polar(theta="y") +
    xlab("")+ylab("")+ggtitle(Title)+
  xlim(c(0, 4)) +
    theme(panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          legend.position = "bottom"
          ,plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
}



####################################################
# Chronic Condition Prevalence Visualization
####################################################

Prev.vis<-function(
  prev.df,  # data.frame(Disease, Prevalence, Std)
  order=c("default","alphabet","desc","asc"),
  Title="Disease Prevalence",
  confidence=.95
){
  if(order=="default")
    prev.df$Disease<-factor(prev.df$Disease,levels=rev(prev.df$Disease))
  if(order=="alphabet")
    prev.df$Disease<-factor(prev.df$Disease,levels=rev(sort(as.character(prev.df$Disease))))
  if(order=="desc"){
    lvl<-prev.df$Disease[order(prev.df$Prevalence)]
    prev.df$Disease<-factor(prev.df$Disease,levels=lvl)
  }
  if(order=="asc"){
    lvl<-prev.df$Disease[order(-prev.df$Prevalence)]
    prev.df$Disease<-factor(prev.df$Disease,levels=lvl)
  }
  q.conf<-1-(1-confidence)/2
  prev.df$prev.min<-prev.df$Prevalence-prev.df$Std*qnorm(q.conf)
  prev.df$prev.max<-prev.df$Prevalence+prev.df$Std*qnorm(q.conf)
  
  ggplot()+
    geom_bar(data=prev.df,mapping=aes(x=Disease,y=Prevalence),stat="identity",fill="steelblue")+
    geom_errorbar(data=prev.df,mapping=aes(x=Disease,ymin=prev.min, ymax=prev.max),color="#F08080",width=.2)+
    coord_flip()+
    ggtitle(Title)+
    theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5))
}









